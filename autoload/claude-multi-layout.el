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
;;   - morning:     week file (60%) | jira/email/task triage (stacked right)
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
  "Ensure the progress buffer is displayed in a bottom side-window.
Creates the buffer if it doesn't exist.  Any existing non-side window
showing the buffer is replaced by a dedicated side-window so the
progress table stays pinned at the bottom across layout switches."
  (let ((buf (get-buffer-create
              (or (bound-and-true-p claude-multi-progress-buffer-name)
                  "*Claude Multi-Agent Progress*"))))
    ;; Kill any non-side window showing the buffer first
    (when-let ((existing (get-buffer-window buf t)))
      (unless (window-parameter existing 'window-side)
        (delete-window existing)))
    ;; Always display via side-window to guarantee bottom pinning
    (unless (get-buffer-window buf t)
      (display-buffer-in-side-window buf
        '((side . bottom) (slot . 0)
          (window-height . 0.25)
          (preserve-size . (nil . t))
          (dedicated . t))))))

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

(defun claude-multi-layout--find-or-create-org-file (filename)
  "Open FILENAME from the org planning directory, creating a placeholder if missing.
Returns the buffer.  When the file doesn't exist yet (e.g. triage files
that will be exported by the agent), a temporary buffer is created with a
waiting message.  Use `claude-multi-layout/revert-files' to refresh once
the agent has written the real file."
  (let ((path (expand-file-name filename claude-multi-layout--org-base-dir)))
    (if (file-exists-p path)
        (let ((buf (find-file-noselect path)))
          (with-current-buffer buf
            (when (file-exists-p buffer-file-name)
              (revert-buffer t t t)))
          buf)
      ;; Create a placeholder buffer visiting the path
      (let ((buf (find-file-noselect path)))
        (with-current-buffer buf
          (unless (buffer-modified-p)
            (erase-buffer)
            (insert (format "#+TITLE: %s\n\n" (file-name-sans-extension filename)))
            (insert "Waiting for triage export...\n\n")
            (insert "Run =SPC c l r= to refresh once the agent populates this file.\n")
            (set-buffer-modified-p nil)))
        buf))))

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

(defun claude-multi-layout--ensure-kitty-layout ()
  "Ensure the Kitty OS window layout (Emacs + Agents) is initialised.
Shells out to `cma layout init'.  Idempotent — the Go code returns
early when the layout is already active.  Logs a warning if the `cma'
binary is not found."
  (if (and (fboundp 'cma--available-p) (cma--available-p))
      (let ((result (cma--call "layout" "init" "--json")))
        (unless result
          (message "Morning: cma layout init returned nil (already initialised or error)")))
    (message "Morning: cma binary not found — skipping Kitty layout")))

;;;###autoload
(defun claude-multi-layout/morning ()
  "Activate morning layout: weekly plan (left 60%) with triage stack (right).
Right column shows jira-triage, email-triage, and task-triage stacked.
Progress buffer pinned at bottom."
  (interactive)
  (unless claude-multi-layout--pre-layout-config
    (setq claude-multi-layout--pre-layout-config (current-window-configuration)))
  (let* ((week-file (claude-multi-layout--current-week-file))
         (week-buf  (claude-multi-layout--find-or-create-org-file week-file))
         (jira-buf  (claude-multi-layout--find-or-create-org-file "jira-triage.org"))
         (email-buf (claude-multi-layout--find-or-create-org-file "email-triage.org"))
         (task-buf  (claude-multi-layout--find-or-create-org-file "task-triage.org")))
    (delete-other-windows)
    ;; Left: weekly plan (full height)
    (switch-to-buffer week-buf)
    ;; Right column at ~60% width
    (let ((right-win (split-window-right (floor (* (window-width) 0.6)))))
      (select-window right-win)
      ;; Top-right: jira triage
      (switch-to-buffer jira-buf)
      (claude-multi-layout--disable-olivetti-in-buffer jira-buf)
      ;; Middle-right: email triage (~33% of right column height)
      (let ((mid-win (split-window-below (floor (* (window-height) 0.33)))))
        (select-window mid-win)
        (switch-to-buffer email-buf)
        (claude-multi-layout--disable-olivetti-in-buffer email-buf)
        ;; Bottom-right: task triage (remaining ~50% of what's left)
        (let ((bot-win (split-window-below (floor (* (window-height) 0.5)))))
          (select-window bot-win)
          (switch-to-buffer task-buf)
          (claude-multi-layout--disable-olivetti-in-buffer task-buf))))
    ;; Return focus to weekly plan
    (select-window (get-buffer-window week-buf))
    (claude-multi-layout--ensure-progress-visible)
    (setq claude-multi-layout--current 'morning)
    (message "Layout: morning (%s)" week-file)))

;;;###autoload
(defun claude-multi-layout/start-morning ()
  "One-keypress morning startup: Kitty layout, Emacs layout, triage agent.
1. Ensures Kitty OS windows are positioned (Emacs + Agents).
2. Arranges Emacs into the morning layout.
3. Spawns a Claude agent running /workview:triage-all."
  (interactive)
  (claude-multi-layout--ensure-kitty-layout)
  (claude-multi-layout/morning)
  (if (and (fboundp 'cma--available-p) (cma--available-p))
      (cma--call "spawn"
                 "--task" "/workview:triage-all"
                 "--dir" (expand-file-name "~/projects/workview")
                 "--prompt" "/workview:triage-all"
                 "--json")
    (message "Morning: cma not available — skipping agent spawn")))

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
Valid names: triage, triage-all, task-triage, planning, morning, coding."
  (interactive
   (list (completing-read "Layout: "
                          '("triage" "triage-all" "task-triage" "planning" "morning" "coding")
                          nil t)))
  (pcase layout-name
    ("triage"      (claude-multi-layout/triage))
    ("triage-all"  (claude-multi-layout/triage-all))
    ("task-triage" (claude-multi-layout/task-triage))
    ("planning"    (claude-multi-layout/planning))
    ("morning"     (claude-multi-layout/morning))
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
