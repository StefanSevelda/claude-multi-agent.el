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
;;   - focus:       today schedule | task-triage (top), progress (bottom 50%)
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
      (let ((output (cma--call-raw "layout" "init")))
        (when output
          (message "Morning: %s" (car (last (split-string output "\n" t))))))
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
3. Spawns a Claude agent running /workview:workview-triage-all."
  (interactive)
  (claude-multi-layout--ensure-kitty-layout)
  (claude-multi-layout/morning)
  (if (and (fboundp 'cma--available-p) (cma--available-p))
      (cma--call "spawn"
                 "--task" "workview-triage-all"
                 "--dir" (expand-file-name "~/projects/workview")
                 "--prompt" "/workview:workview-triage-all"
                 "--json")
    (message "Morning: cma not available — skipping agent spawn")))

;; ──────────────────────────────────────────────────────────────────────────────
;; Focus Layout — derived today buffer + task triage + large progress
;; ──────────────────────────────────────────────────────────────────────────────

(defvar claude-multi-layout--today-buffer-name "*Today Schedule*"
  "Name of the derived read-only buffer showing today's schedule.")

(defvar claude-multi-layout--time-indicator-timer nil
  "Timer that refreshes the time indicator every 60 seconds.")

(defvar claude-multi-layout--time-indicator-overlays nil
  "List of overlays used for the time indicator across buffers.")

(defface claude-multi-layout-now-indicator
  '((t :background "#2d4f67" :foreground "#ff9e64" :weight bold :extend t))
  "Face for the current time slot indicator in schedule views."
  :group 'claude-multi)

(defun claude-multi-layout--derive-today-buffer ()
  "Extract today's schedule from the week file into a read-only derived buffer.
Returns the buffer.  The buffer is not backed by a file — it is a
transient mirror rendered from the `* Week Overview` section of the
current week's planning file.  A `>>>` marker is placed beside the
current time slot."
  (let* ((week-file (claude-multi-layout--current-week-file))
         (week-path (expand-file-name week-file claude-multi-layout--org-base-dir))
         (today-buf (get-buffer-create claude-multi-layout--today-buffer-name))
         (dow (format-time-string "%a"))  ; e.g. "Mon", "Tue"
         (now-hour (string-to-number (format-time-string "%H")))
         (now-min (string-to-number (format-time-string "%M"))))
    (with-current-buffer today-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: Today's Schedule (%s)\n\n"
                        (format-time-string "%A, %B %d")))
        (if (not (file-exists-p week-path))
            (insert "No week file found. Run /workview:plan-week first.\n")
          ;; Read the week file and extract today's column from the calendar grid
          (let ((week-content (with-temp-buffer
                                (insert-file-contents week-path)
                                (buffer-string)))
                (today-lines nil)
                (in-grid nil)
                (day-col nil))
            ;; Find the "Week at a Glance" table and extract today's column
            (with-temp-buffer
              (insert week-content)
              (goto-char (point-min))
              ;; Find the grid table header row to locate today's column index
              (when (re-search-forward "^|[[:space:]]+| " nil t)
                (beginning-of-line)
                (let* ((header-line (buffer-substring-no-properties
                                     (point) (line-end-position)))
                       (cols (split-string header-line "|" t))
                       (col-idx nil))
                  ;; Find which column matches today's day abbreviation
                  (cl-loop for col in cols
                           for i from 0
                           when (string-match-p (regexp-quote dow) (string-trim col))
                           do (setq col-idx i))
                  (when col-idx
                    (setq day-col col-idx)
                    ;; Now walk each row and extract that column
                    (forward-line 1)
                    (while (and (not (eobp))
                                (looking-at "^|"))
                      (let* ((line (buffer-substring-no-properties
                                     (point) (line-end-position)))
                             (cells (split-string line "|" t)))
                        (when (and (> (length cells) col-idx)
                                   ;; Skip separator lines
                                   (not (string-match-p "^-" (string-trim (nth 0 cells)))))
                          (let* ((time-cell (string-trim (nth 0 cells)))
                                 (day-cell (string-trim (nth col-idx cells)))
                                 (slot-hour nil)
                                 (slot-min nil)
                                 (is-current nil))
                            ;; Parse time from first column (e.g. "09:00", "09:30")
                            (when (string-match "\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" time-cell)
                              (setq slot-hour (string-to-number (match-string 1 time-cell))
                                    slot-min (string-to-number (match-string 2 time-cell)))
                              ;; Check if this is the current time slot
                              ;; Current = slot time <= now < slot time + 30min
                              (let ((slot-total (+ (* slot-hour 60) slot-min))
                                    (now-total (+ (* now-hour 60) now-min)))
                                (setq is-current
                                      (and (>= now-total slot-total)
                                           (< now-total (+ slot-total 30))))))
                            (push (list time-cell day-cell is-current) today-lines))))
                      (forward-line 1))))))
            ;; Render the extracted schedule
            (if (null today-lines)
                (insert "Could not extract today's schedule from the week file.\n"
                        "The Week at a Glance table may not exist yet.\n")
              (setq today-lines (nreverse today-lines))
              (insert "| Time  | Block                  |\n")
              (insert "|-------+------------------------|\n")
              (let ((now-line-start nil))
                (dolist (entry today-lines)
                  (let ((time-str (nth 0 entry))
                        (block-str (nth 1 entry))
                        (is-now (nth 2 entry))
                        (line-start (point)))
                    (insert (format "| %s | %-22s |\n"
                                    time-str
                                    block-str))
                    (when is-now
                      (setq now-line-start line-start))))
                ;; Apply overlay to the current time slot line
                (when now-line-start
                  (let ((ov (make-overlay now-line-start
                                          (save-excursion
                                            (goto-char now-line-start)
                                            (line-end-position)))))
                    (overlay-put ov 'face 'claude-multi-layout-now-indicator)
                    (overlay-put ov 'claude-multi-time-indicator t)
                    (push ov claude-multi-layout--time-indicator-overlays))))
              (insert "\n")
              ;; Also extract today's focus block assignments if present
              (let ((dow-full (format-time-string "%A"))) ; e.g. "Monday"
                (with-temp-buffer
                  (insert week-content)
                  (goto-char (point-min))
                  (when (re-search-forward
                         (format "^\\*\\*\\* %s" dow-full) nil t)
                    (beginning-of-line)
                    (let ((start (point))
                          (end (save-excursion
                                 (if (re-search-forward "^\\*\\*\\* " nil t)
                                     (line-beginning-position)
                                   (point-max)))))
                      (with-current-buffer today-buf
                        (insert "** Focus Assignments\n\n")
                        (insert (with-temp-buffer
                                  (insert-file-contents week-path)
                                  (goto-char (point-min))
                                  (when (re-search-forward
                                         (format "^\\*\\*\\* %s" dow-full) nil t)
                                    (beginning-of-line)
                                    (let ((s (point))
                                          (e (save-excursion
                                               (if (re-search-forward "^\\*\\*\\* " nil t)
                                                   (line-beginning-position)
                                                 (point-max)))))
                                      (buffer-substring-no-properties s e)))
                                  ""))))))))))
        (org-mode)
        (setq buffer-read-only t)
        (goto-char (point-min))))
    today-buf))

(defun claude-multi-layout--clear-time-overlays ()
  "Remove all time indicator overlays from all buffers."
  (dolist (ov claude-multi-layout--time-indicator-overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq claude-multi-layout--time-indicator-overlays nil))

(defun claude-multi-layout--apply-week-file-indicator ()
  "Apply the time indicator overlay to the week file's calendar grid.
Finds today's current time slot row in the Week at a Glance table
and highlights it."
  (let* ((week-file (claude-multi-layout--current-week-file))
         (week-path (expand-file-name week-file claude-multi-layout--org-base-dir))
         (week-buf (find-buffer-visiting week-path))
         (now-hour (string-to-number (format-time-string "%H")))
         (now-min (string-to-number (format-time-string "%M")))
         ;; Snap to 30-min slot
         (slot-time (format "%02d:%02d" now-hour (* (/ now-min 30) 30))))
    (when (and week-buf (buffer-live-p week-buf))
      (with-current-buffer week-buf
        (save-excursion
          (goto-char (point-min))
          ;; Find the Week at a Glance section
          (when (re-search-forward "^\\*\\* Week at a Glance" nil t)
            ;; Find the row matching the current time slot
            (when (re-search-forward
                   (format "^|[[:space:]]*%s[[:space:]]*|" (regexp-quote slot-time))
                   nil t)
              (beginning-of-line)
              (let ((ov (make-overlay (point) (line-end-position))))
                (overlay-put ov 'face 'claude-multi-layout-now-indicator)
                (overlay-put ov 'claude-multi-time-indicator t)
                (push ov claude-multi-layout--time-indicator-overlays)))))))))

(defun claude-multi-layout--update-time-indicator ()
  "Refresh the time indicator in both the today buffer and week file.
Called every 60 seconds by `claude-multi-layout--time-indicator-timer'."
  (when (eq claude-multi-layout--current 'focus)
    ;; Clear old overlays
    (claude-multi-layout--clear-time-overlays)
    ;; Re-derive the today buffer (creates new overlays internally)
    (when (get-buffer claude-multi-layout--today-buffer-name)
      (let ((win (get-buffer-window claude-multi-layout--today-buffer-name)))
        (claude-multi-layout--derive-today-buffer)
        (when win
          (with-selected-window win
            (goto-char (point-min))
            ;; Jump to the highlighted line
            (let ((found nil))
              (dolist (ov claude-multi-layout--time-indicator-overlays)
                (when (and (not found)
                           (eq (overlay-buffer ov) (current-buffer)))
                  (goto-char (overlay-start ov))
                  (recenter)
                  (setq found t))))))))
    ;; Apply indicator to the week file grid if visible
    (claude-multi-layout--apply-week-file-indicator)))

(defun claude-multi-layout--start-time-indicator ()
  "Start the 60-second timer for refreshing the time indicator."
  (claude-multi-layout--stop-time-indicator)
  (setq claude-multi-layout--time-indicator-timer
        (run-with-timer 60 60 #'claude-multi-layout--update-time-indicator)))

(defun claude-multi-layout--stop-time-indicator ()
  "Stop the time indicator refresh timer and remove overlays."
  (when claude-multi-layout--time-indicator-timer
    (cancel-timer claude-multi-layout--time-indicator-timer)
    (setq claude-multi-layout--time-indicator-timer nil))
  (claude-multi-layout--clear-time-overlays))

(defun claude-multi-layout--ensure-kitty-layout-focus ()
  "Ensure the Kitty OS window layout with focus proportions (40/60).
Shells out to `cma layout init' then repositions with focus proportions."
  (if (and (fboundp 'cma--available-p) (cma--available-p))
      (let ((output (cma--call-raw "layout" "init" "--emacs-pct" "40")))
        (when output
          (message "Focus: %s" (car (last (split-string output "\n" t))))))
    (message "Focus: cma binary not found — skipping Kitty layout")))

;;;###autoload
(defun claude-multi-layout/focus ()
  "Activate focus layout for daily focused work.
Top-left: today's schedule (derived read-only buffer).
Top-right: task-triage.org for capturing ideas.
Bottom: progress buffer at 50% height.
Kitty: Emacs 40% | Agents 60%."
  (interactive)
  (unless claude-multi-layout--pre-layout-config
    (setq claude-multi-layout--pre-layout-config (current-window-configuration)))
  (let* ((today-buf (claude-multi-layout--derive-today-buffer))
         (triage-buf (claude-multi-layout--find-or-create-org-file "task-triage.org")))
    (delete-other-windows)
    ;; Top-left: today's schedule
    (switch-to-buffer today-buf)
    ;; Top-right: task triage
    (let ((right-win (split-window-right (floor (* (window-width) 0.5)))))
      (select-window right-win)
      (switch-to-buffer triage-buf)
      (claude-multi-layout--disable-olivetti-in-buffer triage-buf))
    ;; Return focus to today's schedule
    (select-window (get-buffer-window today-buf))
    ;; Progress side-window at 50% height (larger than default 25%)
    (let ((buf (get-buffer-create
                (or (bound-and-true-p claude-multi-progress-buffer-name)
                    "*Claude Multi-Agent Progress*"))))
      (when-let ((existing (get-buffer-window buf t)))
        (unless (window-parameter existing 'window-side)
          (delete-window existing)))
      (display-buffer-in-side-window buf
        '((side . bottom) (slot . 0)
          (window-height . 0.5)
          (preserve-size . (nil . t))
          (dedicated . t))))
    ;; Start time indicator timer and apply initial week file indicator
    (claude-multi-layout--start-time-indicator)
    (claude-multi-layout--apply-week-file-indicator)
    (setq claude-multi-layout--current 'focus)
    (message "Layout: focus (today + triage + progress)")))

;;;###autoload
(defun claude-multi-layout/coding ()
  "Restore normal window configuration (exit layout mode).
Progress buffer stays visible if it was showing."
  (interactive)
  ;; Stop focus-mode timer if active
  (claude-multi-layout--stop-time-indicator)
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
Valid names: triage, triage-all, task-triage, planning, morning, focus, coding."
  (interactive
   (list (completing-read "Layout: "
                          '("triage" "triage-all" "task-triage" "planning" "morning" "focus" "coding")
                          nil t)))
  ;; Stop focus timer when switching away from focus layout
  (when (and (eq claude-multi-layout--current 'focus)
             (not (string= layout-name "focus")))
    (claude-multi-layout--stop-time-indicator))
  (pcase layout-name
    ("triage"      (claude-multi-layout/triage))
    ("triage-all"  (claude-multi-layout/triage-all))
    ("task-triage" (claude-multi-layout/task-triage))
    ("planning"    (claude-multi-layout/planning))
    ("morning"     (claude-multi-layout/morning))
    ("focus"       (claude-multi-layout/focus))
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
