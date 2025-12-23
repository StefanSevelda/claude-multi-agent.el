;; -*- lexical-binding: t; -*-
;;; autoload/claude-multi-session.el --- Session persistence for Claude Multi-Agent

;;; Commentary:
;; Session save/restore functionality so users can persist and restore their
;; multi-agent work sessions across Emacs restarts.

;;; Code:

(require 'cl-lib)

;; Forward declarations
(declare-function claude-multi--agents "config")
(declare-function claude-multi--agent-id-counter "config")
(declare-function claude-agent-id "claude-multi-agents")
(declare-function claude-agent-name "claude-multi-agents")
(declare-function claude-agent-task-description "claude-multi-agents")
(declare-function claude-agent-status "claude-multi-agents")
(declare-function claude-agent-color "claude-multi-agents")
(declare-function claude-agent-kitty-window-id "claude-multi-agents")
(declare-function claude-agent-kitty-tab-id "claude-multi-agents")
(declare-function claude-agent-worktree-path "claude-multi-agents")
(declare-function claude-agent-branch-name "claude-multi-agents")
(declare-function claude-agent-working-directory "claude-multi-agents")
(declare-function claude-agent-created-at "claude-multi-agents")
(declare-function claude-agent-completed-at "claude-multi-agents")
(declare-function claude-agent-communication-backend "claude-multi-agents")
(declare-function claude-agent-mcp-enabled "claude-multi-agents")
(declare-function claude-agent-session-id "claude-multi-agents")
(declare-function claude-agent-mcp-request-counter "claude-multi-agents")
(declare-function make-claude-agent "claude-multi-agents")
(declare-function claude-multi--kitty-is-alive "claude-multi-agents")
(declare-function claude-multi--add-agent-section "claude-multi-progress")
(declare-function claude-multi--update-session-stats "claude-multi-progress")
(declare-function claude-multi-ws--is-connected "claude-multi-websocket")
(declare-function claude-multi-ws--get-port-env "claude-multi-websocket")

;; Variables defined in config.el
(defvar claude-multi--agents)
(defvar claude-multi--agent-id-counter)
(defvar claude-multi-session-directory)
(defvar claude-multi-session-autosave-interval)
(defvar claude-multi-session-retention-days)

;;; Global variables

(defvar claude-multi--session-autosave-timer nil
  "Timer for automatic session saving.")

(defvar claude-multi--session-version "1.0"
  "Session file format version.")

;;; Serialization

;;;###autoload
(defun claude-multi-session--serialize-agent (agent)
  "Convert AGENT struct to plist for serialization.
Handles WebSocket fields by storing port number, not connection object."
  (list
   :id (claude-agent-id agent)
   :name (claude-agent-name agent)
   :color (claude-agent-color agent)
   :task-description (claude-agent-task-description agent)
   :status (symbol-name (claude-agent-status agent))
   :kitty-window-id (claude-agent-kitty-window-id agent)
   :kitty-tab-id (claude-agent-kitty-tab-id agent)
   :worktree-path (claude-agent-worktree-path agent)
   :branch-name (claude-agent-branch-name agent)
   :working-directory (claude-agent-working-directory agent)
   :created-at (when (claude-agent-created-at agent)
                 (format-time-string "%Y-%m-%dT%H:%M:%S"
                                   (claude-agent-created-at agent)))
   :completed-at (when (claude-agent-completed-at agent)
                   (format-time-string "%Y-%m-%dT%H:%M:%S"
                                     (claude-agent-completed-at agent)))
   :communication-backend (symbol-name (claude-agent-communication-backend agent))
   :mcp-enabled (claude-agent-mcp-enabled agent)
   :session-id (claude-agent-session-id agent)
   :mcp-request-counter (claude-agent-mcp-request-counter agent)
   ;; Store WebSocket port if available (not connection object)
   :websocket-port (when (and (fboundp 'claude-multi-ws--get-port-env)
                              (claude-multi-ws--get-port-env))
                     (string-to-number (claude-multi-ws--get-port-env)))))

;;;###autoload
(defun claude-multi-session--deserialize-agent (plist)
  "Recreate agent struct from serialized PLIST.
Attempts WebSocket reconnection if port is stored."
  (let* ((id (plist-get plist :id))
         (status-str (plist-get plist :status))
         (status (intern status-str))
         (backend-str (plist-get plist :communication-backend))
         (backend (if backend-str (intern backend-str) 'polling))
         (created-at-str (plist-get plist :created-at))
         (created-at (when created-at-str
                      (date-to-time created-at-str)))
         (completed-at-str (plist-get plist :completed-at))
         (completed-at (when completed-at-str
                        (date-to-time completed-at-str)))
         (agent (make-claude-agent
                 :id id
                 :name (plist-get plist :name)
                 :color (plist-get plist :color)
                 :task-description (plist-get plist :task-description)
                 :status status
                 :kitty-window-id (plist-get plist :kitty-window-id)
                 :kitty-tab-id (plist-get plist :kitty-tab-id)
                 :worktree-path (plist-get plist :worktree-path)
                 :branch-name (plist-get plist :branch-name)
                 :working-directory (plist-get plist :working-directory)
                 :created-at created-at
                 :completed-at completed-at
                 :communication-backend backend
                 :mcp-enabled (plist-get plist :mcp-enabled)
                 :session-id (plist-get plist :session-id)
                 :mcp-request-counter (plist-get plist :mcp-request-counter))))

    ;; Check if kitty window still exists
    (when (and (claude-agent-kitty-window-id agent)
               (not (claude-multi--kitty-is-alive agent)))
      ;; Window is gone - mark as completed/failed
      (setf (claude-agent-status agent)
            (if (eq status 'completed) 'completed 'failed)))

    ;; Attempt WebSocket reconnection if backend was websocket
    (when (and (eq backend 'websocket)
               (plist-get plist :websocket-port))
      ;; WebSocket reconnection would happen when agent sends register message
      ;; For now, just note that connection should be expected
      (message "Agent %s expects WebSocket connection on port %s"
               id (plist-get plist :websocket-port)))

    agent))

;;; Session file operations

(defun claude-multi-session--ensure-directory ()
  "Ensure session directory exists."
  (unless (file-exists-p claude-multi-session-directory)
    (make-directory claude-multi-session-directory t)))

(defun claude-multi-session--generate-filename ()
  "Generate a unique session filename based on current timestamp.
Format: session-YYYY-MM-DD-HHMMSS.el"
  (format "session-%s.el"
          (format-time-string "%Y-%m-%d-%H%M%S")))

(defun claude-multi-session--get-session-path (session-id)
  "Get full path for SESSION-ID.
SESSION-ID can be a filename or just the timestamp portion."
  (let ((filename (if (string-suffix-p ".el" session-id)
                      session-id
                    (format "session-%s.el" session-id))))
    (expand-file-name filename claude-multi-session-directory)))

;;;###autoload
(defun claude-multi-session--save ()
  "Save current session to file.
Returns the session file path on success, nil on failure."
  (condition-case err
      (progn
        (claude-multi-session--ensure-directory)

        ;; Only save if there are agents
        (unless claude-multi--agents
          (message "No agents to save")
          (return-from claude-multi-session--save nil))

        (let* ((filename (claude-multi-session--generate-filename))
               (filepath (expand-file-name filename claude-multi-session-directory))
               (session-data (list
                             :version claude-multi--session-version
                             :timestamp (format-time-string "%Y-%m-%dT%H:%M:%S")
                             :agents (mapcar #'claude-multi-session--serialize-agent
                                           claude-multi--agents)
                             :global-state (list
                                          :agent-id-counter claude-multi--agent-id-counter))))

          ;; Write atomically: write to temp file, then rename
          (let ((temp-file (concat filepath ".tmp")))
            (with-temp-file temp-file
              (prin1 session-data (current-buffer)))
            (rename-file temp-file filepath t))

          (message "Session saved to %s" filepath)
          filepath))
    (error
     (message "Failed to save session: %s" (error-message-string err))
     nil)))

;;;###autoload
(defun claude-multi-session--restore (session-id)
  "Restore session from SESSION-ID.
Returns the number of agents restored."
  (condition-case err
      (let* ((filepath (claude-multi-session--get-session-path session-id))
             (session-data (when (file-exists-p filepath)
                            (with-temp-buffer
                              (insert-file-contents filepath)
                              (read (current-buffer)))))
             (version (plist-get session-data :version))
             (agents-data (plist-get session-data :agents))
             (global-state (plist-get session-data :global-state))
             (restored-count 0))

        (unless session-data
          (error "Session file not found: %s" filepath))

        ;; Validate version
        (unless (string= version claude-multi--session-version)
          (error "Incompatible session version: %s (expected %s)"
                 version claude-multi--session-version))

        ;; Restore global state
        (when global-state
          (setq claude-multi--agent-id-counter
                (plist-get global-state :agent-id-counter)))

        ;; Restore agents
        (dolist (agent-plist agents-data)
          (let ((agent (claude-multi-session--deserialize-agent agent-plist)))
            (push agent claude-multi--agents)
            (setq restored-count (1+ restored-count))

            ;; Add to progress buffer if available
            (when (fboundp 'claude-multi--add-agent-section)
              (claude-multi--add-agent-section agent))))

        ;; Update session statistics
        (when (fboundp 'claude-multi--update-session-stats)
          (claude-multi--update-session-stats))

        (message "Restored %d agent(s) from session %s" restored-count session-id)
        restored-count)
    (error
     (message "Failed to restore session: %s" (error-message-string err))
     0)))

;;;###autoload
(defun claude-multi-session--list-sessions ()
  "Return list of available session IDs (filenames without extension).
Sessions are sorted by modification time, newest first."
  (when (file-exists-p claude-multi-session-directory)
    (let* ((files (directory-files claude-multi-session-directory t "^session-.*\\.el$"))
           (sessions (mapcar (lambda (f)
                              (cons (file-name-nondirectory f)
                                    (file-attribute-modification-time
                                     (file-attributes f))))
                            files)))
      ;; Sort by modification time, newest first
      (setq sessions (cl-sort sessions #'time-less-p :key #'cdr))
      (mapcar #'car (nreverse sessions)))))

;;;###autoload
(defun claude-multi-session--delete-session (session-id)
  "Delete session file for SESSION-ID.
Returns t on success, nil on failure."
  (condition-case err
      (let ((filepath (claude-multi-session--get-session-path session-id)))
        (if (file-exists-p filepath)
            (progn
              (delete-file filepath)
              (message "Deleted session: %s" session-id)
              t)
          (message "Session file not found: %s" filepath)
          nil))
    (error
     (message "Failed to delete session: %s" (error-message-string err))
     nil)))

(defun claude-multi-session--cleanup-old-sessions ()
  "Delete sessions older than `claude-multi-session-retention-days'.
Returns number of sessions deleted."
  (if (not (and (> claude-multi-session-retention-days 0)
                (file-exists-p claude-multi-session-directory)))
      0  ; Return 0 if retention is disabled or directory doesn't exist
    (let* ((cutoff-time (time-subtract (current-time)
                                       (days-to-time claude-multi-session-retention-days)))
           (files (directory-files claude-multi-session-directory t "^session-.*\\.el$"))
           (deleted-count 0))
      (dolist (file files)
        (let ((mtime (file-attribute-modification-time (file-attributes file))))
          (when (time-less-p mtime cutoff-time)
            (condition-case err
                (progn
                  (delete-file file)
                  (setq deleted-count (1+ deleted-count)))
              (error
               (message "Failed to delete old session %s: %s"
                       file (error-message-string err)))))))
      (when (> deleted-count 0)
        (message "Cleaned up %d old session(s)" deleted-count))
      deleted-count)))

;;; Autosave

(defun claude-multi-session--autosave-callback ()
  "Callback function for autosave timer."
  (when claude-multi--agents
    (claude-multi-session--save))
  ;; Cleanup old sessions periodically
  (claude-multi-session--cleanup-old-sessions))

;;;###autoload
(defun claude-multi-session--start-autosave ()
  "Start autosave timer if not already running."
  (when (and (> claude-multi-session-autosave-interval 0)
             (not claude-multi--session-autosave-timer))
    (setq claude-multi--session-autosave-timer
          (run-with-timer claude-multi-session-autosave-interval
                         claude-multi-session-autosave-interval
                         #'claude-multi-session--autosave-callback))
    (message "Session autosave enabled (every %ds)" claude-multi-session-autosave-interval)))

;;;###autoload
(defun claude-multi-session--stop-autosave ()
  "Stop autosave timer."
  (when claude-multi--session-autosave-timer
    (cancel-timer claude-multi--session-autosave-timer)
    (setq claude-multi--session-autosave-timer nil)
    (message "Session autosave disabled")))

;;; Session browser

(defun claude-multi-session--get-session-metadata (session-id)
  "Get metadata for SESSION-ID.
Returns plist with :timestamp, :agent-count, :file-size."
  (let* ((filepath (claude-multi-session--get-session-path session-id))
         (attrs (file-attributes filepath))
         (mtime (file-attribute-modification-time attrs))
         (size (file-attribute-size attrs)))
    (condition-case _err
        (let* ((session-data (with-temp-buffer
                              (insert-file-contents filepath)
                              (read (current-buffer))))
               (agents (plist-get session-data :agents))
               (timestamp (plist-get session-data :timestamp)))
          (list :timestamp timestamp
                :agent-count (length agents)
                :file-size size
                :modified (format-time-string "%Y-%m-%d %H:%M:%S" mtime)))
      (error
       (list :timestamp "unknown"
             :agent-count 0
             :file-size size
             :modified (format-time-string "%Y-%m-%d %H:%M:%S" mtime))))))

(defun claude-multi-session--format-file-size (bytes)
  "Format BYTES as human-readable size."
  (cond
   ((< bytes 1024) (format "%dB" bytes))
   ((< bytes (* 1024 1024)) (format "%.1fKB" (/ bytes 1024.0)))
   (t (format "%.1fMB" (/ bytes 1024.0 1024.0)))))

;;; Interactive commands

;;;###autoload
(defun claude-multi/save-session ()
  "Manually save current session."
  (interactive)
  (if (null claude-multi--agents)
      (message "No active agents to save")
    (if-let ((filepath (claude-multi-session--save)))
        (message "Session saved: %s" (file-name-nondirectory filepath))
      (message "Failed to save session"))))

;;;###autoload
(defun claude-multi/restore-session ()
  "Restore session from file with completion."
  (interactive)
  (let ((sessions (claude-multi-session--list-sessions)))
    (if (null sessions)
        (message "No saved sessions found")
      (let* ((session-id (completing-read "Restore session: " sessions nil t))
             (restored-count (when session-id
                              (claude-multi-session--restore session-id))))
        (if (and restored-count (> restored-count 0))
            (message "Restored %d agent(s)" restored-count)
          (message "Failed to restore session"))))))

;;;###autoload
(defun claude-multi/list-sessions ()
  "Browse available sessions in a dedicated buffer."
  (interactive)
  (let ((sessions (claude-multi-session--list-sessions))
        (buf (get-buffer-create "*Claude Multi-Agent Sessions*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# Claude Multi-Agent Sessions\n\n")

        (if (null sessions)
            (insert "No saved sessions found.\n")
          (insert (format "Found %d session(s):\n\n" (length sessions)))
          (dolist (session-id sessions)
            (let* ((metadata (claude-multi-session--get-session-metadata session-id))
                   (timestamp (plist-get metadata :timestamp))
                   (agent-count (plist-get metadata :agent-count))
                   (file-size (plist-get metadata :file-size))
                   (modified (plist-get metadata :modified)))
              (insert (format "## %s\n" session-id))
              (insert (format "- **Timestamp:** %s\n" timestamp))
              (insert (format "- **Modified:** %s\n" modified))
              (insert (format "- **Agents:** %d\n" agent-count))
              (insert (format "- **Size:** %s\n\n"
                            (claude-multi-session--format-file-size file-size))))))

        (insert "\n---\n\n")
        (insert "Commands:\n")
        (insert "- `r` - Restore session\n")
        (insert "- `d` - Delete session\n")
        (insert "- `q` - Quit\n"))

      (markdown-mode)
      (read-only-mode 1)
      (goto-char (point-min))

      ;; Local keymap
      (use-local-map (make-sparse-keymap))
      (local-set-key (kbd "r") #'claude-multi-session--browser-restore)
      (local-set-key (kbd "d") #'claude-multi-session--browser-delete)
      (local-set-key (kbd "q") #'quit-window))

    (display-buffer buf)))

(defun claude-multi-session--browser-restore ()
  "Restore session at point in session browser."
  (interactive)
  (let ((session-id (claude-multi-session--extract-session-id-at-point)))
    (if session-id
        (progn
          (claude-multi-session--restore session-id)
          (quit-window))
      (message "No session at point"))))

(defun claude-multi-session--browser-delete ()
  "Delete session at point in session browser."
  (interactive)
  (let ((session-id (claude-multi-session--extract-session-id-at-point)))
    (if session-id
        (when (y-or-n-p (format "Delete session %s? " session-id))
          (claude-multi-session--delete-session session-id)
          (claude-multi/list-sessions))
      (message "No session at point"))))

(defun claude-multi-session--extract-session-id-at-point ()
  "Extract session ID from line at point.
Returns session ID (filename) or nil if not found."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "^## \\(session-[^[:space:]]+\\.el\\)" (line-end-position) t)
      (match-string 1))))

;;;###autoload
(defun claude-multi/delete-session ()
  "Delete saved session with completion."
  (interactive)
  (let ((sessions (claude-multi-session--list-sessions)))
    (if (null sessions)
        (message "No saved sessions found")
      (let ((session-id (completing-read "Delete session: " sessions nil t)))
        (when (and session-id
                   (y-or-n-p (format "Really delete session %s? " session-id)))
          (if (claude-multi-session--delete-session session-id)
              (message "Session deleted: %s" session-id)
            (message "Failed to delete session")))))))

;;; Emacs exit hook

(defun claude-multi-session--on-emacs-exit ()
  "Save session on Emacs exit."
  (when claude-multi--agents
    (claude-multi-session--save)))

(add-hook 'kill-emacs-hook #'claude-multi-session--on-emacs-exit)

(provide 'claude-multi-session)
;;; claude-multi-session.el ends here
