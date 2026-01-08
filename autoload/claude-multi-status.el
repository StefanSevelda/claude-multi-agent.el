;; -*- lexical-binding: t; -*-
;;; autoload/claude-multi-status.el --- File-based status tracking for Claude Multi-Agent

;;; Commentary:
;; Uses file-notify (inotify/kqueue) to watch session-specific status files
;; in /tmp/claude-status/ for real-time agent status updates.

;;; Code:

(require 'cl-lib)
(require 'filenotify)
(require 'json)

;; Forward declarations
(declare-function claude-agent-id "claude-multi-agents")
(declare-function claude-agent-name "claude-multi-agents")
(declare-function claude-agent-status "claude-multi-agents")
(declare-function claude-agent-worktree-path "claude-multi-agents")
(declare-function claude-agent-working-directory "claude-multi-agents")
(declare-function claude-agent-session-id "claude-multi-agents")
(declare-function claude-agent-created-at "claude-multi-agents")
(declare-function claude-agent-completed-at "claude-multi-agents")
(declare-function claude-agent-last-status-data "claude-multi-agents")
(declare-function claude-multi--notify-input-needed "claude-multi-notifications")
(declare-function claude-multi--get-status-icon "claude-multi-progress")
(declare-function claude-multi--update-session-stats "claude-multi-progress")
(declare-function claude-multi--update-session-from-status "claude-multi-progress")
(declare-function claude-multi--insert-status-properties "claude-multi-progress")

(defvar claude-multi--agents)
(defvar claude-multi--progress-buffer)

;; Accessors for cl-struct setf (indices based on claude-agent defstruct)
;; Note: cl-defstruct uses 1-based indexing (index 0 is type tag)
;; session-id=18, mcp-request-counter=19, ediff-session=20, last-status-data=21
;; status=11
(gv-define-setter claude-agent-session-id (val agent) `(aset ,agent 18 ,val))
(gv-define-setter claude-agent-last-status-data (val agent) `(aset ,agent 21 ,val))
(gv-define-setter claude-agent-status (val agent) `(aset ,agent 11 ,val))

;;; Variables

(defvar claude-multi-status-directory "/tmp/claude-status/"
  "Directory where Claude hooks write session-specific status files.")

(defvar claude-multi--directory-watcher nil
  "File-notify descriptor for watching the status directory.")

(defvar claude-multi--status-cache (make-hash-table :test 'equal)
  "Cache of parsed status data per session-id.")

(defvar claude-multi--session-to-agent (make-hash-table :test 'equal)
  "Mapping from session-id to agent struct.")

(defvar claude-multi--pending-agents nil
  "List of agents waiting for session-id discovery.")

;;; Directory watcher

;;;###autoload
(defun claude-multi--ensure-status-directory ()
  "Ensure the status directory exists."
  (unless (file-exists-p claude-multi-status-directory)
    (make-directory claude-multi-status-directory t)))

;;;###autoload
(defun claude-multi--start-directory-watcher ()
  "Start watching the status directory for new/changed files."
  (claude-multi--ensure-status-directory)
  (unless claude-multi--directory-watcher
    (setq claude-multi--directory-watcher
          (file-notify-add-watch
           claude-multi-status-directory
           '(change)
           #'claude-multi--handle-directory-event))))

;;;###autoload
(defun claude-multi--stop-directory-watcher ()
  "Stop watching the status directory."
  (when claude-multi--directory-watcher
    (file-notify-rm-watch claude-multi--directory-watcher)
    (setq claude-multi--directory-watcher nil)))

;;; Event handling

(defun claude-multi--handle-directory-event (event)
  "Handle file-notify EVENT for the status directory."
  (let* ((action (nth 1 event))
         (file (nth 2 event)))
    (when (and file (string-match-p "^status-.*\\.json$" (file-name-nondirectory file)))
      (pcase action
        ((or 'created 'changed 'renamed)
         (claude-multi--process-status-file file))
        ('deleted
         (claude-multi--handle-status-deleted file))))))

(defun claude-multi--process-status-file (file)
  "Process a status FILE and update the corresponding agent."
  (when (file-exists-p file)
    (let ((status-data (claude-multi--read-status-file file)))
      (when status-data
        (let* ((session-id (alist-get 'session_id status-data))
               (cwd (alist-get 'cwd status-data))
               (agent (gethash session-id claude-multi--session-to-agent)))
          ;; Cache the data
          (puthash session-id status-data claude-multi--status-cache)

          ;; If we don't have an agent for this session, try to match by cwd
          (unless agent
            (setq agent (claude-multi--find-agent-by-cwd cwd))
            (when agent
              ;; Store the mapping for future updates
              (setf (claude-agent-session-id agent) session-id)
              (puthash session-id agent claude-multi--session-to-agent)
              ;; Remove from pending list
              (setq claude-multi--pending-agents
                    (delq agent claude-multi--pending-agents))))

          ;; Update the agent if found
          (when agent
            (claude-multi--update-agent-from-status agent status-data)))))))

(defun claude-multi--handle-status-deleted (file)
  "Handle deletion of status FILE."
  (let* ((filename (file-name-nondirectory file))
         (session-id (when (string-match "^status-\\(.+\\)\\.json$" filename)
                       (match-string 1 filename))))
    (when session-id
      (remhash session-id claude-multi--status-cache))))

;;; Status file reading

(defun claude-multi--read-status-file (file)
  "Read and parse status JSON from FILE."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file)
        (json-parse-buffer :object-type 'alist))
    (error
     (message "Error reading status file %s: %s" file err)
     nil)))

(defun claude-multi--status-file-path (session-id)
  "Return the status file path for SESSION-ID."
  (expand-file-name (format "status-%s.json" session-id)
                    claude-multi-status-directory))

;;; Agent discovery and matching

(defun claude-multi--normalize-path (path)
  "Normalize PATH by resolving symlinks and removing trailing slashes."
  (when path
    (directory-file-name (file-truename (expand-file-name path)))))

(defun claude-multi--find-agent-by-cwd (cwd)
  "Find an agent whose worktree or directory matches CWD."
  (when cwd
    (let ((normalized-cwd (claude-multi--normalize-path cwd)))
      (cl-find-if
       (lambda (agent)
         (let ((agent-path (or (claude-agent-worktree-path agent)
                               (claude-agent-working-directory agent))))
           (when agent-path
             (string= (claude-multi--normalize-path agent-path)
                      normalized-cwd))))
       claude-multi--agents))))

;;;###autoload
(defun claude-multi--register-agent-for-status (agent)
  "Register AGENT for status updates.
Adds to pending list until session-id is discovered from status file."
  (claude-multi--start-directory-watcher)
  (push agent claude-multi--pending-agents)
  ;; Also try to immediately find existing status file by cwd
  (let ((agent-path (or (claude-agent-worktree-path agent)
                        (claude-agent-working-directory agent)
                        default-directory)))
    (dolist (file (directory-files claude-multi-status-directory t "^status-.*\\.json$"))
      (let ((status-data (claude-multi--read-status-file file)))
        (when status-data
          (let ((cwd (alist-get 'cwd status-data)))
            (when (and cwd (string= (file-truename (expand-file-name cwd))
                                    (file-truename (expand-file-name agent-path))))
              (let ((session-id (alist-get 'session_id status-data)))
                (setf (claude-agent-session-id agent) session-id)
                (puthash session-id agent claude-multi--session-to-agent)
                (setq claude-multi--pending-agents
                      (delq agent claude-multi--pending-agents))
                (claude-multi--update-agent-from-status agent status-data)))))))))

;;;###autoload
(defun claude-multi--unregister-agent-for-status (agent)
  "Unregister AGENT from status updates."
  (when-let ((session-id (claude-agent-session-id agent)))
    (remhash session-id claude-multi--session-to-agent)
    (remhash session-id claude-multi--status-cache))
  (setq claude-multi--pending-agents
        (delq agent claude-multi--pending-agents)))

;;; Agent status updates

(defun claude-multi--update-agent-from-status (agent status-data)
  "Update AGENT struct from STATUS-DATA."
  (let ((claude-status (alist-get 'claude_status status-data))
        (waiting (alist-get 'waiting_for_input status-data)))

    ;; Store last status data
    (setf (claude-agent-last-status-data agent) status-data)

    ;; Update agent status based on Claude status
    (setf (claude-agent-status agent)
          (cond
           (waiting 'waiting-input)
           ((string= claude-status "finished") 'completed)
           ((string= claude-status "error") 'failed)
           (t 'running)))

    ;; Update session buffer
    (claude-multi--update-session-from-status agent status-data)

    ;; Update the status drawer display with latest status
    (when (fboundp 'claude-multi--update-agent-status-display)
      (claude-multi--update-agent-status-display agent))

    ;; Notify if waiting for input
    (when waiting
      (claude-multi--notify-input-needed agent))))

;;; Session buffer updates

(defun claude-multi--update-session-from-status (agent status-data)
  "Update the session buffer for AGENT based on STATUS-DATA."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward
                 (format "^\\*\\* .* %s$" (regexp-quote (claude-agent-name agent)))
                 nil t)
            ;; Update header with status icon
            (beginning-of-line)
            (delete-region (line-beginning-position) (line-end-position))
            (insert (format "** %s %s"
                           (claude-multi--get-status-icon (claude-agent-status agent))
                           (claude-agent-name agent)))

            ;; Find and update properties
            (when (re-search-forward ":PROPERTIES:" nil t)
              (let ((props-start (line-beginning-position)))
                (when (re-search-forward "^:END:" nil t)
                  (let ((props-end (line-end-position)))
                    (delete-region props-start props-end)
                    (goto-char props-start)
                    (claude-multi--insert-status-properties agent status-data)))))

            ;; Update activity line
            (when (re-search-forward "^- Current Activity :: " nil t)
              (delete-region (line-beginning-position) (line-end-position))
              (insert (format "- Current Activity :: %s"
                             (or (alist-get 'goal (alist-get 'current_activity status-data))
                                 "Working..."))))))
        ;; Update session stats
        (claude-multi--update-session-stats)))))

(defun claude-multi--insert-status-properties (agent status-data)
  "Insert property drawer for AGENT with STATUS-DATA."
  (let ((context-window (alist-get 'context_window status-data))
        (git-info (alist-get 'git status-data)))
    (insert ":PROPERTIES:\n")
    (insert (format ":ID: %s\n" (claude-agent-id agent)))
    (insert (format ":STATUS: %s\n" (upcase (symbol-name (claude-agent-status agent)))))

    ;; Context window
    (when context-window
      (insert (format ":TOKENS: %d/%d (%.1f%%)\n"
                     (or (alist-get 'tokens_used context-window) 0)
                     (or (alist-get 'tokens_total context-window) 200000)
                     (or (alist-get 'percentage_used context-window) 0))))

    ;; Git branch
    (when git-info
      (when-let ((branch (alist-get 'branch git-info)))
        (insert (format ":BRANCH: %s\n" branch))))

    ;; Worktree
    (when (claude-agent-worktree-path agent)
      (insert (format ":WORKTREE: %s\n" (claude-agent-worktree-path agent))))

    ;; Timestamps
    (insert (format ":CREATED: %s\n"
                   (format-time-string "[%Y-%m-%d %a %H:%M:%S]"
                                      (claude-agent-created-at agent))))
    (when (claude-agent-completed-at agent)
      (insert (format ":COMPLETED: %s\n"
                     (format-time-string "[%Y-%m-%d %a %H:%M:%S]"
                                        (claude-agent-completed-at agent)))))
    (insert ":END:")))

;;; Cleanup

;;;###autoload
(defun claude-multi--cleanup-status-tracking ()
  "Clean up all status tracking resources."
  (claude-multi--stop-directory-watcher)
  (clrhash claude-multi--status-cache)
  (clrhash claude-multi--session-to-agent)
  (setq claude-multi--pending-agents nil))

;;;###autoload
(defun claude-multi/cleanup-status-files ()
  "Delete all status JSON files from the status directory.
This removes stale status files from previous sessions."
  (interactive)
  (if (not (file-exists-p claude-multi-status-directory))
      (message "Status directory does not exist: %s" claude-multi-status-directory)
    (let* ((files (directory-files claude-multi-status-directory t "^status-.*\\.json$"))
           (count (length files)))
      (if (zerop count)
          (message "No status files to clean up")
        (dolist (file files)
          (delete-file file))
        (message "Cleaned up %d status file%s from %s"
                 count
                 (if (= count 1) "" "s")
                 claude-multi-status-directory)))))

;;; Utility functions

(defun claude-multi--get-cached-status (agent)
  "Get cached status data for AGENT."
  (when-let ((session-id (claude-agent-session-id agent)))
    (gethash session-id claude-multi--status-cache)))

(provide 'claude-multi-status)
;;; claude-multi-status.el ends here
