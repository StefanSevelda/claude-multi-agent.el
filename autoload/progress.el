;; -*- lexical-binding: t; -*-
;;; autoload/progress.el --- Org-mode progress tracking for Claude Multi-Agent

;;; Commentary:
;; Centralized org-mode-based progress tracking for all agents

;;; Code:

(require 's)
(require 'f)
(require 'org)

;;; Progress buffer initialization

;;;###autoload
(defun claude-multi--init-progress-buffer ()
  "Initialize the progress buffer with session header."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: Claude Multi-Agent Session\n"))
        (insert (format "#+AUTHOR: %s\n" (user-full-name)))
        (insert (format "#+DATE: %s\n\n"
                       (format-time-string "%Y-%m-%d %H:%M:%S"
                                         claude-multi--session-start-time)))
        (insert (format "* Session Info\n\n"))
        (insert (format "- Started :: %s\n"
                       (format-time-string "%Y-%m-%d %H:%M:%S"
                                         claude-multi--session-start-time)))
        (insert (format "- Working Directory :: =%s=\n" default-directory))
        (insert (format "- Stats :: %d total | %d running | %d waiting | %d completed | %d failed\n\n"
                       0 0 0 0 0))
        (insert (format "* Agents\n\n"))))))

;;; Agent section management

;;;###autoload
(defun claude-multi--add-agent-section (agent)
  "Add a new section for AGENT to the progress buffer."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n** %s %s\n"
                       (claude-multi--get-status-icon (claude-agent-status agent))
                       (claude-agent-name agent)))
        (insert (format ":PROPERTIES:\n"))
        (insert (format ":ID: %s\n" (claude-agent-id agent)))
        (insert (format ":STATUS: %s\n" (upcase (symbol-name (claude-agent-status agent)))))
        (when (claude-agent-worktree-path agent)
          (insert (format ":WORKTREE: %s\n" (claude-agent-worktree-path agent))))
        (insert (format ":CREATED: %s\n"
                       (format-time-string "[%Y-%m-%d %a %H:%M:%S]"
                                         (claude-agent-created-at agent))))
        (insert (format ":END:\n\n"))
        (insert (format "- Task :: %s\n\n" (claude-agent-task-description agent)))
        (insert (format "*** Progress\n\n"))
        (claude-multi--insert-agent-marker agent)
        (insert "\n")))))

;;; Output appending

;;;###autoload
(defun claude-multi--append-agent-output (agent output)
  "Append OUTPUT from AGENT to its section in the progress buffer."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t)
            (marker-pos (claude-multi--find-agent-marker agent)))
        (when marker-pos
          (goto-char marker-pos)
          ;; Insert before the marker
          (forward-line -1)
          (end-of-line)
          (insert "\n")
          (insert (format "- [%s] %s"
                         (format-time-string "%H:%M:%S")
                         (string-trim output)))
          ;; Color code based on content
          (when (string-match-p "error\\|fail" (downcase output))
            (add-text-properties (line-beginning-position) (line-end-position)
                                '(face (:foreground "red"))))
          (when (string-match-p "success\\|complete" (downcase output))
            (add-text-properties (line-beginning-position) (line-end-position)
                                '(face (:foreground "green"))))
          (when (claude-multi--detect-input-request output)
            (add-text-properties (line-beginning-position) (line-end-position)
                                '(face (:foreground "yellow" :weight bold)))))))))

;;;###autoload
(defun claude-multi--update-agent-status (agent)
  "Update the status of AGENT in the progress buffer."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward
                 (format "^\\*\\* .* %s$" (regexp-quote (claude-agent-name agent)))
                 nil t)
            ;; Update the heading with current status icon
            (beginning-of-line)
            (kill-line)
            (insert (format "** %s %s"
                           (claude-multi--get-status-icon (claude-agent-status agent))
                           (claude-agent-name agent)))

            ;; Update properties drawer
            (forward-line 1)
            (when (looking-at ":PROPERTIES:")
              (let ((props-start (point)))
                (re-search-forward "^:END:" nil t)
                (delete-region props-start (point))
                (goto-char props-start)
                (insert ":PROPERTIES:\n")
                (insert (format ":ID: %s\n" (claude-agent-id agent)))
                (insert (format ":STATUS: %s\n" (upcase (symbol-name (claude-agent-status agent)))))
                (when (claude-agent-worktree-path agent)
                  (insert (format ":WORKTREE: %s\n" (claude-agent-worktree-path agent))))
                (insert (format ":CREATED: %s\n"
                               (format-time-string "[%Y-%m-%d %a %H:%M:%S]"
                                                 (claude-agent-created-at agent))))
                (when (claude-agent-completed-at agent)
                  (insert (format ":COMPLETED: %s\n"
                                 (format-time-string "[%Y-%m-%d %a %H:%M:%S]"
                                                   (claude-agent-completed-at agent))))
                  (insert (format ":DURATION: %s\n"
                                 (claude-multi--format-duration
                                  (claude-agent-created-at agent)
                                  (claude-agent-completed-at agent)))))
                (insert ":END:")))))
        ;; Update session stats
        (claude-multi--update-session-stats)))))

;;;###autoload
(defun claude-multi--remove-agent-section (agent)
  "Remove AGENT's section from the progress buffer."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward
                 (format "^\\*\\* .* %s$" (regexp-quote (claude-agent-name agent)))
                 nil t)
            (let ((section-start (line-beginning-position)))
              ;; Find the end of this section (next ** or end of buffer)
              (forward-line 1)
              (if (re-search-forward "^\\*\\* " nil t)
                  (beginning-of-line)
                (goto-char (point-max)))
              (delete-region section-start (point)))))
        (claude-multi--update-session-stats)))))

;;; Session stats management

(defun claude-multi--update-session-stats ()
  "Update the session statistics in the progress buffer."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t)
            (total 0)
            (running 0)
            (waiting 0)
            (completed 0)
            (failed 0))
        ;; Count agents by status
        (dolist (agent claude-multi--agents)
          (cl-incf total)
          (pcase (claude-agent-status agent)
            ('running (cl-incf running))
            ('waiting-input (cl-incf waiting))
            ('completed (cl-incf completed))
            ('failed (cl-incf failed))))

        ;; Update the stats line
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^- Stats :: " nil t)
            (beginning-of-line)
            (kill-line)
            (insert (format "- Stats :: %d total | %d running | %d waiting | %d completed | %d failed"
                           total running waiting completed failed))))))))

;;; Helper functions

;;;###autoload
(defun claude-multi--get-status-icon (status)
  "Get an emoji icon for the given STATUS."
  (pcase status
    ('running "🟢")
    ('waiting-input "🟡")
    ('completed "🔵")
    ('failed "🔴")
    ('pending "⚪")
    (_ "❓")))

(defun claude-multi--insert-agent-marker (agent)
  "Insert a marker at the end of AGENT's progress section.
This marker is used to find where to append new output."
  (let ((marker-text (format "<!-- agent-marker-%s -->" (claude-agent-id agent))))
    (insert marker-text)))

(defun claude-multi--find-agent-marker (agent)
  "Find the position of AGENT's marker in the progress buffer.
Returns the position or nil if not found."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward
               (format "<!-- agent-marker-%s -->" (regexp-quote (claude-agent-id agent)))
               nil t)
          (point))))))

;;;###autoload
(defun claude-multi--highlight-input-requests ()
  "Highlight all input request lines in the progress buffer."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "🟡\\|WAITING FOR INPUT" nil t)
            (add-text-properties (line-beginning-position) (line-end-position)
                                '(face (:foreground "yellow" :weight bold)))))))))

;;; Auto-scroll functionality

(defun claude-multi--auto-scroll-progress ()
  "Auto-scroll the progress buffer to show latest updates."
  (when (buffer-live-p claude-multi--progress-buffer)
    (let ((windows (get-buffer-window-list claude-multi--progress-buffer nil t)))
      (dolist (window windows)
        (with-selected-window window
          (goto-char (point-max))
          (recenter -3))))))

;;; Export functionality

;;;###autoload
(defun claude-multi/export-progress ()
  "Export the progress buffer to an org file."
  (interactive)
  (when (buffer-live-p claude-multi--progress-buffer)
    (let ((filename (read-file-name "Export to: "
                                    nil
                                    (format "claude-session-%s.org"
                                           (format-time-string "%Y%m%d-%H%M%S")))))
      (with-current-buffer claude-multi--progress-buffer
        (write-region (point-min) (point-max) filename))
      (message "Progress exported to %s" filename))))

(provide 'claude-multi-progress)
;;; progress.el ends here
