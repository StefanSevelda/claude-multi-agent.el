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
    (when (and file (string-match-p "^status-.*\\.json$" (file-name-nondirectory file)))
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
  "Return list of (FILE . STATUS-DATA) pairs sorted by timestamp (newest first)."
  (let ((files (directory-files claude-multi-status-directory t "^status-.*\\.json$"))
        (status-list nil))
    (dolist (file files)
      (when-let ((data (claude-multi--read-status-file file)))
        (push (cons file data) status-list)))
    ;; Sort by timestamp descending (newest first)
    (sort status-list
          (lambda (a b)
            (let ((time-a (alist-get 'timestamp (cdr a)))
                  (time-b (alist-get 'timestamp (cdr b))))
              (string> (or time-a "") (or time-b "")))))))

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

(provide 'claude-multi-status)
;;; claude-multi-status.el ends here
