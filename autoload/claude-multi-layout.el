;; -*- lexical-binding: t; -*-
;;; autoload/claude-multi-layout.el --- Tiling layout system for Claude Multi-Agent

;;; Commentary:
;; Named layout system for arranging Emacs buffers during triage/planning workflows.
;; Each layout configures the main window area while the progress buffer stays
;; pinned as a bottom side-window via display-buffer-alist.
;;
;; Layouts:
;;   - agenda:  week file (60%) | email/task/jira triage (stacked right)
;;   - focus:   today schedule | task-triage (top), progress (bottom 50%)
;;   - project: neotree | magit-status (toggle: changed-files | magit-diff)
;;   - exit:    restore normal window configuration
;;
;; Helpers:
;;   - focus-buffer:  focus a specific file's window within the current layout

;;; Code:

(require 'cma-core)

(defvar claude-multi-layout--current nil
  "Name of the currently active layout (symbol or nil).")

(defvar claude-multi-layout--org-base-dir
  (expand-file-name "~/org/planning/")
  "Base directory for org planning files used by layouts.")

(defvar claude-multi-layout--pre-layout-config nil
  "Window configuration saved before entering a layout, for restore.")

(defvar claude-multi-layout--time-override nil
  "When non-nil, an Emacs time value to use instead of `current-time'.
Set via `claude-multi-layout/simulate-time' for testing layouts
at different times of day.  Reset with `claude-multi-layout/reset-time'.")

(defun claude-multi-layout--now ()
  "Return the effective current time, respecting `claude-multi-layout--time-override'."
  (or claude-multi-layout--time-override (current-time)))

;; ──────────────────────────────────────────────────────────────────────────────
;; Helpers
;; ──────────────────────────────────────────────────────────────────────────────

(defun claude-multi-layout--ensure-progress-visible (&optional height)
  "Ensure the progress buffer is displayed in a bottom side-window.
Creates the buffer if it doesn't exist.  Any existing non-side window
showing the buffer is replaced by a dedicated side-window so the
progress table stays pinned at the bottom across layout switches.
HEIGHT defaults to 0.25."
  (let ((buf (get-buffer-create
              (or (bound-and-true-p claude-multi-progress-buffer-name)
                  "*Claude Multi-Agent Progress*")))
        (h (or height 0.25)))
    ;; Kill any non-side window showing the buffer first
    (when-let ((existing (get-buffer-window buf t)))
      (unless (window-parameter existing 'window-side)
        (delete-window existing)))
    ;; Always display via side-window to guarantee bottom pinning
    (unless (get-buffer-window buf t)
      (display-buffer-in-side-window buf
        `((side . bottom) (slot . 0)
          (window-height . ,h)
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
  (let* ((now (claude-multi-layout--now))
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
;; Buffer Focus Helper
;; ──────────────────────────────────────────────────────────────────────────────

;;;###autoload
(defun claude-multi-layout/focus-buffer (filename)
  "Focus the window displaying FILENAME within the current layout.
If visible in a window, select that window. Otherwise switch current window.
Reverts from disk to ensure fresh content."
  (interactive "fFocus file: ")
  (let* ((path (if (file-name-absolute-p filename)
                   filename
                 (expand-file-name filename claude-multi-layout--org-base-dir)))
         (buf (find-file-noselect path)))
    (with-current-buffer buf
      (when (file-exists-p buffer-file-name)
        (revert-buffer t t t)))
    (if-let ((win (get-buffer-window buf t)))
        (select-window win)
      (switch-to-buffer buf))
    (message "Focused: %s" (file-name-nondirectory path))))

;; ──────────────────────────────────────────────────────────────────────────────
;; Layout Functions
;; ──────────────────────────────────────────────────────────────────────────────


(defun claude-multi-layout--reset-kitty-layout (&optional emacs-pct)
  "Re-position Kitty OS windows (Emacs left, Agents right).
EMACS-PCT overrides the default screen-width percentage for Emacs.
Calls `cma layout reset'.  Falls back gracefully if cma is unavailable."
  (when (and (fboundp 'cma--available-p) (cma--available-p))
    (if emacs-pct
        (cma--call-raw "layout" "reset"
                       "--emacs-pct" (number-to-string emacs-pct))
      (cma--call-raw "layout" "reset"))))

;;;###autoload
(defun claude-multi-layout/reset-kitty ()
  "Interactively reposition Kitty OS windows."
  (interactive)
  (if (and (fboundp 'cma--available-p) (cma--available-p))
      (let ((output (cma--call-raw "layout" "reset")))
        (message "%s" (or output "Layout reset")))
    (message "cma binary not found — cannot reset layout")))

(defun claude-multi-layout--setup-layout (name &optional progress-height emacs-pct)
  "Common setup for entering a layout.
Saves window config (first time only), clears existing windows,
resets Kitty positioning, and pins the progress buffer.
NAME is the layout symbol.  PROGRESS-HEIGHT overrides the default 0.25.
EMACS-PCT overrides the default kitty screen-width percentage for Emacs."
  ;; Save pre-layout config (only if not already in a layout)
  (unless claude-multi-layout--pre-layout-config
    (setq claude-multi-layout--pre-layout-config (current-window-configuration)))
  ;; Ensure we're in a regular window (not a side-window) before clearing
  (when (window-parameter (selected-window) 'window-side)
    (select-window (window-main-window)))
  (delete-other-windows)
  ;; Reset Kitty OS window positioning
  (claude-multi-layout--reset-kitty-layout emacs-pct)
  ;; Pin progress buffer at bottom
  (claude-multi-layout--ensure-progress-visible progress-height)
  ;; Track current layout
  (setq claude-multi-layout--current name))

;;;###autoload
(defun claude-multi-layout/agenda ()
  "Activate agenda layout: weekly plan (left 60%) with triage stack (right).
Right column shows email-triage (top), task-triage (middle), jira-triage (bottom).
Progress buffer pinned at bottom."
  (interactive)
  (let* ((week-file (claude-multi-layout--current-week-file))
         (week-buf  (claude-multi-layout--find-or-create-org-file week-file))
         (email-buf (claude-multi-layout--find-or-create-org-file "email-triage.org"))
         (task-buf  (claude-multi-layout--find-or-create-org-file "task-triage.org"))
         (jira-buf  (claude-multi-layout--find-or-create-org-file "jira-triage.org")))
    (claude-multi-layout--setup-layout 'agenda)
    ;; Left: weekly plan (full height)
    (switch-to-buffer week-buf)
    ;; Right column at ~60% width
    (let ((right-win (split-window-right (floor (* (window-width) 0.6)))))
      (select-window right-win)
      ;; Top-right: email triage
      (switch-to-buffer email-buf)
      (claude-multi-layout--disable-olivetti-in-buffer email-buf)
      ;; Middle-right: task triage (~33% of right column height)
      (let ((mid-win (split-window-below (floor (* (window-height) 0.33)))))
        (select-window mid-win)
        (switch-to-buffer task-buf)
        (claude-multi-layout--disable-olivetti-in-buffer task-buf)
        ;; Bottom-right: jira triage (remaining ~50% of what's left)
        (let ((bot-win (split-window-below (floor (* (window-height) 0.5)))))
          (select-window bot-win)
          (switch-to-buffer jira-buf)
          (claude-multi-layout--disable-olivetti-in-buffer jira-buf))))
    ;; Return focus to weekly plan
    (select-window (get-buffer-window week-buf))
    (message "Layout: agenda (%s)" week-file)))

;;;###autoload
(defun claude-multi-layout/start-agenda ()
  "One-keypress agenda startup: Emacs layout + triage agent.
1. Arranges Emacs into the agenda layout (also resets Kitty windows).
2. Spawns a Claude agent running /workview:workview-triage-all."
  (interactive)
  (claude-multi-layout/agenda)
  (if (and (fboundp 'cma--available-p) (cma--available-p))
      (cma--call "spawn"
                 "--task" "workview-triage-all"
                 "--dir" (expand-file-name "~/projects/workview")
                 "--prompt" "/workview:workview-triage-all"
                 "--json")
    (message "Agenda: cma not available — skipping agent spawn")))

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

(defun claude-multi-layout--apply-assignment-indicator (start-pos now-hour now-min)
  "Highlight the focus assignment block covering the current time.
Scans from START-POS for lines matching `- HH:MM-HH:MM` and highlights
the one where NOW-HOUR:NOW-MIN falls within the range, including any
indented continuation lines."
  (let ((now-total (+ (* now-hour 60) now-min)))
    (save-excursion
      (goto-char start-pos)
      (while (re-search-forward
              "^- \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\) "
              nil t)
        (let* ((range-start (+ (* (string-to-number (match-string 1)) 60)
                               (string-to-number (match-string 2))))
               (range-end (+ (* (string-to-number (match-string 3)) 60)
                             (string-to-number (match-string 4)))))
          (when (and (>= now-total range-start)
                     (< now-total range-end))
            (let* ((line-start (line-beginning-position))
                   (block-end (save-excursion
                                (forward-line 1)
                                (while (and (not (eobp))
                                            (looking-at "^  "))
                                  (forward-line 1))
                                (point)))
                   (ov (make-overlay line-start block-end)))
              (overlay-put ov 'face 'claude-multi-layout-now-indicator)
              (overlay-put ov 'priority 100)
              (overlay-put ov 'claude-multi-time-indicator t)
              (push ov claude-multi-layout--time-indicator-overlays))))))))

(defun claude-multi-layout--derive-today-buffer ()
  "Extract today's schedule from the week file into a read-only derived buffer.
Returns the buffer.  The buffer is not backed by a file — it is a
transient mirror rendered from the `* Week Overview` section of the
current week's planning file.  A `>>>` marker is placed beside the
current time slot."
  (let* ((week-file (claude-multi-layout--current-week-file))
         (week-path (expand-file-name week-file claude-multi-layout--org-base-dir))
         (today-buf (get-buffer-create claude-multi-layout--today-buffer-name))
         (now (claude-multi-layout--now))
         (dow (format-time-string "%a" now))  ; e.g. "Mon", "Tue"
         (now-hour (string-to-number (format-time-string "%H" now)))
         (now-min (string-to-number (format-time-string "%M" now))))
    (with-current-buffer today-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: Today's Schedule (%s)\n\n"
                        (format-time-string "%A, %B %d" now)))
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
              (let* ((now-line-start nil)
                     (assignments-start nil)
                     (dow-full (format-time-string "%A" now)))
                ;; Build the time table
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
                (insert "\n")
                ;; Extract and insert focus block assignments
                (let ((day-content
                       (with-temp-buffer
                         (insert week-content)
                         (goto-char (point-min))
                         (when (re-search-forward
                                (format "^\\*\\*\\* %s" dow-full) nil t)
                           (beginning-of-line)
                           (let ((start (point))
                                 (end (save-excursion
                                        (forward-line 1)
                                        (if (re-search-forward "^\\*\\*\\* " nil t)
                                            (line-beginning-position)
                                          (point-max)))))
                             (buffer-substring-no-properties start end))))))
                  (when day-content
                    (setq assignments-start (point))
                    (insert "** Focus Assignments\n\n")
                    (insert day-content)))
                ;; Enable org-mode BEFORE creating overlays (org-mode resets them)
                (org-mode)
                ;; Now apply all overlays after org-mode fontification
                (when now-line-start
                  (let ((ov (make-overlay now-line-start
                                          (save-excursion
                                            (goto-char now-line-start)
                                            (line-end-position)))))
                    (overlay-put ov 'face 'claude-multi-layout-now-indicator)
                    (overlay-put ov 'priority 100)
                    (overlay-put ov 'claude-multi-time-indicator t)
                    (push ov claude-multi-layout--time-indicator-overlays)))
                (when assignments-start
                  (claude-multi-layout--apply-assignment-indicator
                   assignments-start now-hour now-min))))))
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
         (now (claude-multi-layout--now))
         (now-hour (string-to-number (format-time-string "%H" now)))
         (now-min (string-to-number (format-time-string "%M" now)))
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
                (overlay-put ov 'priority 100)
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
  "Start the 60-second timer for refreshing the time indicator.
Only cancels any existing timer — does NOT clear overlays, since
the caller (focus layout) creates overlays before starting the timer."
  (when claude-multi-layout--time-indicator-timer
    (cancel-timer claude-multi-layout--time-indicator-timer))
  (setq claude-multi-layout--time-indicator-timer
        (run-with-timer 60 60 #'claude-multi-layout--update-time-indicator)))

(defun claude-multi-layout--stop-time-indicator ()
  "Stop the time indicator refresh timer and remove overlays."
  (when claude-multi-layout--time-indicator-timer
    (cancel-timer claude-multi-layout--time-indicator-timer)
    (setq claude-multi-layout--time-indicator-timer nil))
  (claude-multi-layout--clear-time-overlays))

;;;###autoload
(defun claude-multi-layout/focus ()
  "Activate focus layout for daily focused work.
Top-left: today's schedule (derived read-only buffer).
Top-right: task-triage.org for capturing ideas.
Bottom: progress buffer at 50% height."
  (interactive)
  ;; Clear any existing time indicator overlays from previous invocation
  (claude-multi-layout--clear-time-overlays)
  (let* ((today-buf (claude-multi-layout--derive-today-buffer))
         (triage-buf (claude-multi-layout--find-or-create-org-file "task-triage.org")))
    (claude-multi-layout--setup-layout 'focus 0.5 40)
    ;; Top-left: today's schedule
    (switch-to-buffer today-buf)
    ;; Top-right: task triage
    (let ((right-win (split-window-right (floor (* (window-width) 0.5)))))
      (select-window right-win)
      (switch-to-buffer triage-buf)
      (claude-multi-layout--disable-olivetti-in-buffer triage-buf))
    ;; Return focus to today's schedule
    (select-window (get-buffer-window today-buf))
    ;; Start time indicator timer and apply initial week file indicator
    (claude-multi-layout--start-time-indicator)
    (claude-multi-layout--apply-week-file-indicator)
    (message "Layout: focus (today + triage + progress)")))

;; ──────────────────────────────────────────────────────────────────────────────
;; Project Layout — neotree + magit (toggleable status/diff + changed files)
;; ──────────────────────────────────────────────────────────────────────────────

(defvar claude-multi-layout--project-view 'status
  "Current view in the project layout.
\\='status — magit-status with neotree (default)
\\='diff   — magit-diff-unstaged with changed-files sidebar")

(defvar claude-multi-layout--project-dir nil
  "Project directory used by the current project layout.")

(defvar claude-multi-layout--project-changed-files-buffer-name
  "*Changed Files*"
  "Name of the sidebar buffer listing changed files in diff view.")

(defvar claude-multi-layout--project-changed-files-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claude-multi-layout--project-show-file-diff)
    (define-key map (kbd "q") #'claude-multi-layout/project-toggle-diff)
    map)
  "Keymap for the changed files sidebar buffer.")

(define-derived-mode claude-multi-layout--project-changed-files-mode
  special-mode "Changed-Files"
  "Mode for the changed files sidebar in project layout.
Press RET on a file to show its diff.  Press q to toggle back to status."
  :keymap claude-multi-layout--project-changed-files-mode-map)

;; Evil keybindings for changed-files sidebar
(with-eval-after-load 'evil
  (evil-define-key 'normal claude-multi-layout--project-changed-files-mode-map
    (kbd "RET") #'claude-multi-layout--project-show-file-diff
    (kbd "g d") #'claude-multi-layout/project-toggle-diff
    (kbd "q")   #'claude-multi-layout/project-toggle-diff))

(defun claude-multi-layout--build-changed-files-buffer (project-dir)
  "Build a buffer listing git-changed files for PROJECT-DIR.
Returns the buffer.  Each file is a clickable line that shows
its diff in the adjacent window when RET is pressed."
  (let* ((changed (or (cma--call "git" "changed-files" "--all"
                                     "--dir" project-dir "--json")
                      '()))
         (buf (get-buffer-create
               claude-multi-layout--project-changed-files-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Changed Files\n" 'face 'bold))
        (insert (make-string 14 ?─) "\n")
        (if (null changed)
            (insert "\n(no changes)")
          (dolist (file (sort changed #'string<))
            (insert (propertize file
                                'face 'link
                                'claude-multi-file file
                                'mouse-face 'highlight)
                    "\n"))))
      (claude-multi-layout--project-changed-files-mode)
      (goto-char (point-min))
      (forward-line 2))
    buf))

(defun claude-multi-layout--project-show-file-diff ()
  "Show the diff for the file at point in the adjacent window."
  (interactive)
  (let ((file (get-text-property (point) 'claude-multi-file)))
    (unless file
      (user-error "No file at point"))
    (let* ((project-dir (or claude-multi-layout--project-dir default-directory))
           (target-win (cl-find-if
                        (lambda (w)
                          (and (not (window-parameter w 'window-side))
                               (not (eq w (selected-window)))))
                        (window-list))))
      (when target-win
        (select-window target-win)
        (let ((default-directory project-dir))
          (magit-diff-unstaged nil (list (expand-file-name file project-dir))))))))

;;;###autoload
(defun claude-multi-layout/project ()
  "Activate project layout: neotree (left) + magit status (right).
Left: neotree file tree (~20% width).
Right: magit-status for the project.
Bottom: progress buffer at 20% height.
Use `claude-multi-layout/project-toggle-diff' (or g d) to switch between
magit-status / neotree and magit-diff / changed-files views."
  (interactive)
  (let ((project-dir (or (and (fboundp 'doom-project-root) (doom-project-root))
                         (vc-root-dir)
                         default-directory)))
    (setq claude-multi-layout--project-dir project-dir)
    (setq claude-multi-layout--project-view 'status)
    (claude-multi-layout--setup-layout 'project 0.2)
    ;; Open magit-status in the main window
    (magit-status project-dir)
    ;; Open neotree on the left for the project
    (neotree-dir project-dir)
    ;; Return focus to magit
    (other-window 1)
    (message "Layout: project (%s) — g d to toggle diff"
             (abbreviate-file-name project-dir))))

;;;###autoload
(defun claude-multi-layout/project-toggle-diff ()
  "Toggle project layout between status and diff views.
Status view: neotree (full tree) + magit-status.
Diff view: changed-files sidebar (only modified files) + magit-diff-unstaged."
  (interactive)
  (unless (eq claude-multi-layout--current 'project)
    (user-error "Not in project layout — activate with SPC c m y p first"))
  (let ((project-dir (or claude-multi-layout--project-dir default-directory)))
    (pcase claude-multi-layout--project-view
      ('status
       ;; --- Switch to diff view ---
       ;; Close neotree
       (when (and (fboundp 'neo-global--window-exists-p)
                  (neo-global--window-exists-p))
         (neotree-hide))
       ;; Switch main window to magit-diff-unstaged
       (let ((main-win (cl-find-if
                        (lambda (w) (not (window-parameter w 'window-side)))
                        (window-list))))
         (when main-win
           (select-window main-win)
           (let ((default-directory project-dir))
             (magit-diff-unstaged))
           ;; Open changed-files sidebar on the left
           (let* ((changed-buf (claude-multi-layout--build-changed-files-buffer project-dir))
                  (left-win (split-window main-win
                                          (floor (* (window-width main-win) 0.2))
                                          'left)))
             (set-window-buffer left-win changed-buf))))
       (setq claude-multi-layout--project-view 'diff)
       (message "Project: diff view (RET on file → show diff, g d to toggle back)"))
      ('diff
       ;; --- Switch back to status view ---
       ;; Kill the changed-files buffer and its window
       (when-let ((cf-buf (get-buffer claude-multi-layout--project-changed-files-buffer-name)))
         (when-let ((cf-win (get-buffer-window cf-buf)))
           (delete-window cf-win))
         (kill-buffer cf-buf))
       ;; Switch main window back to magit-status
       (let ((main-win (cl-find-if
                        (lambda (w) (not (window-parameter w 'window-side)))
                        (window-list))))
         (when main-win
           (select-window main-win)
           (magit-status project-dir)))
       ;; Reopen neotree
       (neotree-dir project-dir)
       ;; Focus magit
       (other-window 1)
       (setq claude-multi-layout--project-view 'status)
       (message "Project: status view (g d to toggle diff)")))))

(defun claude-multi-layout--project-toggle-diff-if-active ()
  "Toggle diff view only when the project layout is active.
Bound to `g d' in magit buffers — falls through gracefully
when not in project layout."
  (interactive)
  (if (eq claude-multi-layout--current 'project)
      (claude-multi-layout/project-toggle-diff)
    (user-error "g d: not in project layout")))

;;;###autoload
(defun claude-multi-layout/exit ()
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
  ;; Re-pin progress buffer as side-window (set-window-configuration
  ;; doesn't reliably preserve side-window parameters)
  (claude-multi-layout--ensure-progress-visible)
  (setq claude-multi-layout--current nil)
  (message "Layout: exit (restored)"))

;;;###autoload
(defun claude-multi-layout/switch (layout-name)
  "Switch to LAYOUT-NAME (string).
Valid names: agenda, focus, project, exit."
  (interactive
   (list (completing-read "Layout: "
                          '("agenda" "focus" "project" "exit")
                          nil t)))
  ;; Stop focus timer when switching away from focus layout
  (when (and (eq claude-multi-layout--current 'focus)
             (not (string= layout-name "focus")))
    (claude-multi-layout--stop-time-indicator))
  (pcase layout-name
    ("agenda"  (claude-multi-layout/agenda))
    ("focus"   (claude-multi-layout/focus))
    ("project" (claude-multi-layout/project))
    ("exit"    (claude-multi-layout/exit))
    (_        (message "Unknown layout: %s" layout-name))))

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

;; ──────────────────────────────────────────────────────────────────────────────
;; Time simulation (for testing layouts at different times)
;; ──────────────────────────────────────────────────────────────────────────────

;;;###autoload
(defun claude-multi-layout/simulate-time (date-time-str)
  "Simulate a different date/time for layout calculations.
DATE-TIME-STR should be like \"2026-02-18 13:00\" (YYYY-MM-DD HH:MM).
Affects week file selection, today schedule derivation, and time indicators.
Use `claude-multi-layout/reset-time' to restore real time."
  (interactive "sSimulate date/time (YYYY-MM-DD HH:MM): ")
  (let ((parsed (parse-time-string date-time-str)))
    ;; parse-time-string returns (SEC MIN HOUR DAY MON YEAR ...)
    ;; Fill in zero seconds if not provided
    (unless (nth 0 parsed) (setf (nth 0 parsed) 0))
    (setq claude-multi-layout--time-override
          (encode-time (nth 0 parsed)   ; sec
                       (nth 1 parsed)   ; min
                       (nth 2 parsed)   ; hour
                       (nth 3 parsed)   ; day
                       (nth 4 parsed)   ; month
                       (nth 5 parsed))) ; year
    (message "Time override: %s (use claude-multi-layout/reset-time to restore)"
             (format-time-string "%A, %B %d %Y %H:%M"
                                 claude-multi-layout--time-override))))

;;;###autoload
(defun claude-multi-layout/reset-time ()
  "Clear the time override, restoring real system time for layouts."
  (interactive)
  (setq claude-multi-layout--time-override nil)
  (message "Time override cleared — using real time"))

(provide 'claude-multi-layout)
;;; claude-multi-layout.el ends here
