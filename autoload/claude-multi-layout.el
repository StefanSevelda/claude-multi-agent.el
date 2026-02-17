;; -*- lexical-binding: t; -*-
;;; autoload/claude-multi-layout.el --- Tiling layout system for Claude Multi-Agent

;;; Commentary:
;; Named layout system for arranging Emacs buffers during triage/planning workflows.
;; Each layout configures the main window area while the progress buffer stays
;; pinned as a bottom side-window via display-buffer-alist.
;;
;; Layouts:
;;   - triage:      email-triage.org | jira-triage.org  (side-by-side)
;;   - triage-all:  same as triage (entry point for full triage flow)
;;   - task-triage:  task-triage.org (full width)
;;   - planning:    week-YYYY-Www.org (full width)
;;   - coding:      restore normal window configuration

;;; Code:

(defvar claude-multi-layout--current nil
  "Name of the currently active layout (symbol or nil).")

(defvar claude-multi-layout--org-base-dir
  (expand-file-name "~/org/planning/")
  "Base directory for org planning files used by layouts.")

(defvar claude-multi-layout--pre-layout-config nil
  "Window configuration saved before entering a layout, for restore.")

;; ──────────────────────────────────────────────────────────────────────────────
;; Helpers
;; ──────────────────────────────────────────────────────────────────────────────

(defun claude-multi-layout--ensure-progress-visible ()
  "Ensure the progress buffer is displayed in its side-window.
Creates the buffer if it doesn't exist."
  (let ((buf (get-buffer-create
              (or (bound-and-true-p claude-multi-progress-buffer-name)
                  "*Claude Multi-Agent Progress*"))))
    (unless (get-buffer-window buf t)
      (display-buffer buf))))

(defun claude-multi-layout--find-org-file (filename)
  "Open FILENAME from the org planning directory, returning the buffer.
Reverts the buffer if it was already visiting the file."
  (let ((path (expand-file-name filename claude-multi-layout--org-base-dir)))
    (if (file-exists-p path)
        (let ((buf (find-file-noselect path)))
          (with-current-buffer buf
            (when (file-exists-p buffer-file-name)
              (revert-buffer t t t)))
          buf)
      (message "Layout: file not found: %s" path)
      nil)))

(defun claude-multi-layout--current-week-file ()
  "Return the filename for the current week's planning file.
Format: week-YYYY-Www.org"
  (let* ((now (current-time))
         (week-num (string-to-number (format-time-string "%V" now)))
         (year (string-to-number (format-time-string "%G" now))))
    (format "week-%d-W%02d.org" year week-num)))

(defun claude-multi-layout--disable-olivetti-in-buffer (buf)
  "Disable olivetti-mode in BUF if it's active, for narrow triage splits."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (bound-and-true-p olivetti-mode)
        (olivetti-mode -1)))))

;; ──────────────────────────────────────────────────────────────────────────────
;; Layout Functions
;; ──────────────────────────────────────────────────────────────────────────────

;;;###autoload
(defun claude-multi-layout/triage ()
  "Activate triage layout: email-triage.org | jira-triage.org side-by-side.
Progress buffer pinned at bottom."
  (interactive)
  (unless claude-multi-layout--pre-layout-config
    (setq claude-multi-layout--pre-layout-config (current-window-configuration)))
  (let ((email-buf (claude-multi-layout--find-org-file "email-triage.org"))
        (jira-buf (claude-multi-layout--find-org-file "jira-triage.org")))
    (when (or email-buf jira-buf)
      (delete-other-windows)
      (when email-buf
        (switch-to-buffer email-buf)
        (claude-multi-layout--disable-olivetti-in-buffer email-buf))
      (when (and email-buf jira-buf)
        (split-window-right)
        (other-window 1)
        (switch-to-buffer jira-buf)
        (claude-multi-layout--disable-olivetti-in-buffer jira-buf)
        (other-window 1))
      (when (and (not email-buf) jira-buf)
        (switch-to-buffer jira-buf)
        (claude-multi-layout--disable-olivetti-in-buffer jira-buf))
      (claude-multi-layout--ensure-progress-visible)
      (setq claude-multi-layout--current 'triage)
      (message "Layout: triage (email + jira)"))))

;;;###autoload
(defun claude-multi-layout/triage-all ()
  "Activate triage-all layout (same as triage, used as entry point for full triage flow)."
  (interactive)
  (claude-multi-layout/triage)
  (setq claude-multi-layout--current 'triage-all)
  (message "Layout: triage-all"))

;;;###autoload
(defun claude-multi-layout/task-triage ()
  "Activate task-triage layout: task-triage.org full width.
Progress buffer pinned at bottom."
  (interactive)
  (unless claude-multi-layout--pre-layout-config
    (setq claude-multi-layout--pre-layout-config (current-window-configuration)))
  (let ((task-buf (claude-multi-layout--find-org-file "task-triage.org")))
    (when task-buf
      (delete-other-windows)
      (switch-to-buffer task-buf)
      (claude-multi-layout--ensure-progress-visible)
      (setq claude-multi-layout--current 'task-triage)
      (message "Layout: task-triage"))))

;;;###autoload
(defun claude-multi-layout/planning ()
  "Activate planning layout: current week file full width.
Progress buffer pinned at bottom."
  (interactive)
  (unless claude-multi-layout--pre-layout-config
    (setq claude-multi-layout--pre-layout-config (current-window-configuration)))
  (let* ((week-file (claude-multi-layout--current-week-file))
         (week-buf (claude-multi-layout--find-org-file week-file)))
    (when week-buf
      (delete-other-windows)
      (switch-to-buffer week-buf)
      (claude-multi-layout--ensure-progress-visible)
      (setq claude-multi-layout--current 'planning)
      (message "Layout: planning (%s)" week-file))))

;;;###autoload
(defun claude-multi-layout/coding ()
  "Restore normal window configuration (exit layout mode).
Progress buffer stays visible if it was showing."
  (interactive)
  (if claude-multi-layout--pre-layout-config
      (progn
        (set-window-configuration claude-multi-layout--pre-layout-config)
        (setq claude-multi-layout--pre-layout-config nil))
    (delete-other-windows))
  (setq claude-multi-layout--current nil)
  (message "Layout: coding (normal)"))

;;;###autoload
(defun claude-multi-layout/switch (layout-name)
  "Switch to LAYOUT-NAME (string).
Valid names: triage, triage-all, task-triage, planning, coding."
  (interactive
   (list (completing-read "Layout: "
                          '("triage" "triage-all" "task-triage" "planning" "coding")
                          nil t)))
  (pcase layout-name
    ("triage"      (claude-multi-layout/triage))
    ("triage-all"  (claude-multi-layout/triage-all))
    ("task-triage" (claude-multi-layout/task-triage))
    ("planning"    (claude-multi-layout/planning))
    ("coding"      (claude-multi-layout/coding))
    (_             (message "Unknown layout: %s" layout-name))))

;;;###autoload
(defun claude-multi-layout/revert-files ()
  "Revert all triage/planning org file buffers from disk.
Useful after workview CLI regenerates files."
  (interactive)
  (let ((files '("email-triage.org" "jira-triage.org" "task-triage.org"))
        (reverted 0))
    ;; Also check for current week file
    (push (claude-multi-layout--current-week-file) files)
    (dolist (filename files)
      (let ((path (expand-file-name filename claude-multi-layout--org-base-dir)))
        (when-let ((buf (find-buffer-visiting path)))
          (with-current-buffer buf
            (revert-buffer t t t)
            (cl-incf reverted)))))
    (message "Reverted %d buffer(s)" reverted)))

(provide 'claude-multi-layout)
;;; claude-multi-layout.el ends here
