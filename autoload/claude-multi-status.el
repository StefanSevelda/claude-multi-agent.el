;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; autoload/claude-multi-status.el --- Simplified status tracking for Claude Multi-Agent

;;; Commentary:
;; Uses file-notify (inotify/kqueue) to watch session-specific status files
;; in /tmp/claude-status/ for real-time status updates.
;; Simplified version: just reads all status files and displays them.

;;; Code:

(require 'cl-lib)
(require 'filenotify)
(require 'json)

(defvar claude-multi-status-directory "/tmp/claude-status/"
  "Directory where Claude hooks write session-specific status files.")

(defvar claude-multi-mapping-directory "/tmp/claude-multi-mappings/"
  "Directory for agent-name to kitty-window-id mappings.")

(defvar claude-multi--directory-watcher nil
  "File-notify descriptor for watching the status directory.")

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
    ;; Match both .json files and .tmp files (temp files get renamed to .json)
    (when (and file
               (or (string-match-p "^status-.*\\.json$" (file-name-nondirectory file))
                   (string-match-p "^status-.*\\.tmp$" (file-name-nondirectory file))))
      (pcase action
        ((or 'created 'changed 'renamed 'deleted)
         ;; For all events, just refresh the progress buffer from all status files
         (when (fboundp 'claude-multi--refresh-progress-from-status-files)
           (claude-multi--refresh-progress-from-status-files)))))))

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

(defun claude-multi--get-all-status-files ()
  "Return list of (FILE . STATUS-DATA) pairs sorted by creation time (oldest first)."
  (let ((files (directory-files claude-multi-status-directory t "^status-.*\\.json$"))
        (status-list nil))
    (dolist (file files)
      (when-let ((data (claude-multi--read-status-file file)))
        (push (cons file data) status-list)))
    ;; Sort by creation time (oldest first) for stable ordering
    (sort status-list
          (lambda (a b)
            (let ((started-a (alist-get 'session_started (cdr a)))
                  (started-b (alist-get 'session_started (cdr b))))
              (string< (or started-a "") (or started-b "")))))))

;;; Agent-to-Window Mapping

;;;###autoload
(defun claude-multi--write-agent-mapping (agent-name window-id)
  "Write mapping from AGENT-NAME to WINDOW-ID for persistent tracking."
  (let ((mapping-dir (expand-file-name claude-multi-mapping-directory)))
    (unless (file-exists-p mapping-dir)
      (make-directory mapping-dir t))
    (let ((mapping-file (expand-file-name agent-name mapping-dir)))
      (with-temp-file mapping-file
        (insert window-id)))))

;;;###autoload
(defun claude-multi--read-agent-mapping (agent-name)
  "Read window ID for AGENT-NAME from mapping file."
  (let ((mapping-file (expand-file-name agent-name claude-multi-mapping-directory)))
    (when (file-exists-p mapping-file)
      (with-temp-buffer
        (insert-file-contents mapping-file)
        (string-trim (buffer-string))))))

;;;###autoload
(defun claude-multi--get-all-agent-mappings ()
  "Get all agent-name to window-id mappings.
Returns alist of (agent-name . window-id) pairs."
  (let ((mapping-dir (expand-file-name claude-multi-mapping-directory)))
    (when (file-exists-p mapping-dir)
      (let ((mappings nil))
        (dolist (file (directory-files mapping-dir nil "^[^.]"))
          (let ((window-id (claude-multi--read-agent-mapping file)))
            (when window-id
              (push (cons file window-id) mappings))))
        (nreverse mappings)))))

;;;###autoload
(defun claude-multi--get-sessions-with-windows ()
  "Get all sessions from status files enriched with window IDs from mappings.
Returns list of plists with :session-id, :agent-name, :window-id, :directory, :status."
  (let ((status-files (claude-multi--get-all-status-files))
        (mappings (claude-multi--get-all-agent-mappings))
        (sessions nil))
    (dolist (file-data status-files)
      (let* ((data (cdr file-data))
             (agent-name (alist-get 'agent_name data))
             (session-id (alist-get 'session_id data))
             (directory (alist-get 'cwd data))
             (status (alist-get 'claude_status data))
             (window-id (or (alist-get 'kitty_window_id data)
                           (when agent-name
                             (cdr (assoc agent-name mappings))))))
        (when (or agent-name session-id)
          (push (list :session-id session-id
                     :agent-name agent-name
                     :window-id window-id
                     :directory directory
                     :status status
                     :display-name (or agent-name
                                      (when directory
                                        (file-name-nondirectory directory))
                                      session-id))
                sessions))))
    (nreverse sessions)))

;;; Cleanup

;;;###autoload
(defun claude-multi--cleanup-status-tracking ()
  "Clean up all status tracking resources."
  (claude-multi--stop-directory-watcher))

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

;;; Stateless agent discovery

;;;###autoload
(defun claude-multi--get-agents-from-status-files ()
  "Discover all agents from status files in /tmp/claude-status/.
Returns list of plists with agent information extracted from status files.
This provides a stateless way to know about all running Claude sessions."
  (let ((status-files (claude-multi--get-all-status-files))
        (agents nil))
    (dolist (entry status-files)
      (let* ((data (cdr entry))
             (session-id (alist-get 'session_id data))
             (agent-name (alist-get 'agent_name data))
             (kitty-window-id (alist-get 'kitty_window_id data))
             (kitty-tab-id (alist-get 'kitty_tab_id data))
             (cwd (alist-get 'cwd data))
             (status (alist-get 'claude_status data))
             (timestamp (alist-get 'timestamp data))
             (waiting-for-input (alist-get 'waiting_for_input data))
             (git (alist-get 'git data))
             (branch (when git (alist-get 'branch git)))
             (dir-name (when cwd (file-name-nondirectory (directory-file-name cwd))))
             (display-name (cond
                            (agent-name agent-name)
                            ((and dir-name branch) (format "%s (%s)" dir-name branch))
                            (dir-name dir-name)
                            (t (format "Session %s" (substring session-id 0 8))))))
        (when session-id
          (push (list :session-id session-id
                     :agent-name agent-name
                     :display-name display-name
                     :kitty-window-id kitty-window-id
                     :kitty-tab-id kitty-tab-id
                     :working-directory cwd
                     :status (intern status)
                     :waiting-for-input waiting-for-input
                     :timestamp timestamp
                     :branch-name branch)
                agents))))
    (nreverse agents)))

(provide 'claude-multi-status)
;;; claude-multi-status.el ends here
