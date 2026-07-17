;; -*- lexical-binding: t; -*-
;;; autoload/claude-multi-layout.el --- Tiling layout system for Claude Multi-Agent

;;; Commentary:
;; Named layout system for arranging Emacs buffers during triage/planning workflows.
;; Each layout configures the main window area while the progress buffer stays
;; pinned as a bottom side-window via display-buffer-alist.
;;
;; Layouts:
;;   - agenda:       week file (45%) | triage.org (55%)
;;   - focus:        today schedule (45%) | triage.org (55%) + progress (bottom)
;;   - email-triage: email-triage.org (full width) + progress (bottom)
;;   - project:      neotree | magit-status (toggle: changed-files | magit-diff)
;;   - exit:         restore normal window configuration
;;
;; Helpers:
;;   - focus-buffer:  focus a specific file's window within the current layout

;;; Code:

(require 'cma-core)
(require 'cma-table)

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
  "Ensure the progress buffer is displayed in a window at the bottom.
Creates the buffer if it doesn't exist.  Under Doom, displays via the
popup system (a `set-popup-rule!' registered in config pins it at the
bottom); outside Doom, falls back to a dedicated bottom side-window.
Any existing non-bottom window showing the buffer is replaced, but never
the sole window of its frame.  HEIGHT defaults to 0.25.

Layouts call `delete-other-windows' up front, which removes a Doom popup,
so each layout re-invokes this helper after its splits complete to re-pin
the progress table."
  (let ((buf (get-buffer-create
              (or (bound-and-true-p claude-multi-progress-buffer-name)
                  "*Claude Multi-Agent Progress*")))
        (h (or height 0.25)))
    ;; Activate table mode and refresh.  Never let a revert error abort the
    ;; surrounding layout — pin the (possibly stale) buffer regardless.
    (with-current-buffer buf
      (unless (derived-mode-p 'cma-table-mode)
        (cma-table-mode))
      (condition-case err
          (tabulated-list-revert)
        (error (message "Progress: revert failed: %S" err))))
    ;; Drop any non-side window showing the buffer on THIS frame first (never the
    ;; sole window — that would signal an error).  Scope to the selected frame:
    ;; the progress panel is pinned per-frame, so a window on another frame must
    ;; not make us skip pinning here.
    (when-let ((existing (get-buffer-window buf)))
      (when (and (not (window-parameter existing 'window-side))
                 (> (length (window-list (window-frame existing))) 1))
        (delete-window existing)))
    ;; (Re)display at the bottom of the selected frame.
    (unless (get-buffer-window buf)
      (if (fboundp 'set-popup-rule!)
          ;; Doom: the registered popup rule handles bottom placement; pass the
          ;; per-layout height through the action alist so it overrides the
          ;; rule's default size.
          (display-buffer buf (when height `(nil (window-height . ,h))))
        (display-buffer-in-side-window buf
          `((side . bottom) (slot . 0)
            (window-height . ,h)
            (preserve-size . (nil . t))
            (dedicated . t)))))))

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



(defun claude-multi-layout--setup-layout (name &optional progress-height _emacs-pct)
  "Common setup for entering a layout.
Saves window config (first time only), clears existing windows,
and pins the progress buffer.
NAME is the layout symbol.  PROGRESS-HEIGHT overrides the default 0.25."
  ;; Save pre-layout config (only if not already in a layout)
  (unless claude-multi-layout--pre-layout-config
    (setq claude-multi-layout--pre-layout-config (current-window-configuration)))
  ;; Ensure we're in a regular window (not a side-window) before clearing
  (when (window-parameter (selected-window) 'window-side)
    (select-window (window-main-window)))
  (delete-other-windows)
  ;; Pin progress buffer at bottom
  (claude-multi-layout--ensure-progress-visible progress-height)
  ;; Track current layout
  (setq claude-multi-layout--current name))

;;;###autoload
(defun claude-multi-layout/agenda ()
  "Activate agenda layout: weekly plan (left 45%) | triage.org (right 55%).
Progress buffer pinned at bottom."
  (interactive)
  (let* ((week-file  (claude-multi-layout--current-week-file))
         (week-buf   (claude-multi-layout--find-or-create-org-file week-file))
         (triage-buf (claude-multi-layout--find-or-create-org-file "triage.org")))
    (claude-multi-layout--setup-layout 'agenda)
    ;; Left: weekly plan (full height)
    (switch-to-buffer week-buf)
    ;; Right: triage.org at ~55% width
    (let ((right-win (split-window-right (floor (* (window-width) 0.45)))))
      (select-window right-win)
      (switch-to-buffer triage-buf)
      (claude-multi-layout--disable-olivetti-in-buffer triage-buf))
    ;; Return focus to weekly plan
    (select-window (get-buffer-window week-buf))
    ;; Re-pin progress at the bottom after splits (delete-other-windows above
    ;; removed any pre-existing popup).
    (claude-multi-layout--ensure-progress-visible 0.25)
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
                 "--model" "haiku"
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
transient mirror rendered from the `** Focus Blocks & Work Assignments`
section of the current week's planning file."
  (let* ((week-file (claude-multi-layout--current-week-file))
         (week-path (expand-file-name week-file claude-multi-layout--org-base-dir))
         (today-buf (get-buffer-create claude-multi-layout--today-buffer-name))
         (now (claude-multi-layout--now))
         (dow-full (format-time-string "%A" now))  ; e.g. "Monday", "Tuesday"
         (now-hour (string-to-number (format-time-string "%H" now)))
         (now-min (string-to-number (format-time-string "%M" now))))
    (with-current-buffer today-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: Today's Schedule (%s)\n\n"
                        (format-time-string "%A, %B %d" now)))
        (if (not (file-exists-p week-path))
            (insert "No week file found. Run /workview:plan-week first.\n")
          ;; Read the week file and extract today's section from Focus Blocks
          (let ((week-content (with-temp-buffer
                                (insert-file-contents week-path)
                                (buffer-string)))
                (day-content nil))
            ;; Find the *** {Day} heading inside ** Focus Blocks & Work Assignments
            (with-temp-buffer
              (insert week-content)
              (goto-char (point-min))
              (when (re-search-forward "^\\*\\* Focus Blocks" nil t)
                (when (re-search-forward
                       (format "^\\*\\*\\* %s" (regexp-quote dow-full)) nil t)
                  (beginning-of-line)
                  (let ((start (point))
                        (end (save-excursion
                               (forward-line 1)
                               (if (re-search-forward "^\\*\\*\\* " nil t)
                                   (line-beginning-position)
                                 (point-max)))))
                    (setq day-content
                          (buffer-substring-no-properties start end))))))
            (if (null day-content)
                (insert (format "No focus blocks found for %s.\n" dow-full)
                        "Run /workview:plan-week to generate the week plan.\n")
              ;; Insert the day section and apply time indicator overlay
              (let ((assignments-start (point)))
                (insert day-content)
                ;; Enable org-mode BEFORE creating overlays
                (org-mode)
                ;; Apply time indicator to the current block
                (claude-multi-layout--apply-assignment-indicator
                 assignments-start now-hour now-min))))))
      (setq buffer-read-only t)
      (goto-char (point-min)))
    today-buf))

(defun claude-multi-layout--clear-time-overlays ()
  "Remove all time indicator overlays from all buffers."
  (dolist (ov claude-multi-layout--time-indicator-overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq claude-multi-layout--time-indicator-overlays nil))

(defun claude-multi-layout--apply-week-file-indicator ()
  "Apply the time indicator overlay to today's focus block in the week file.
Finds the current focus block line under today's `*** {Day}` heading
in the `** Focus Blocks & Work Assignments` section and highlights it."
  (let* ((week-file (claude-multi-layout--current-week-file))
         (week-path (expand-file-name week-file claude-multi-layout--org-base-dir))
         (week-buf (find-buffer-visiting week-path))
         (now (claude-multi-layout--now))
         (dow-full (format-time-string "%A" now))
         (now-hour (string-to-number (format-time-string "%H" now)))
         (now-min (string-to-number (format-time-string "%M" now))))
    (when (and week-buf (buffer-live-p week-buf))
      (with-current-buffer week-buf
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^\\*\\* Focus Blocks" nil t)
            (when (re-search-forward
                   (format "^\\*\\*\\* %s" (regexp-quote dow-full)) nil t)
              (let ((section-start (point))
                    (section-end (save-excursion
                                   (forward-line 1)
                                   (if (re-search-forward "^\\*\\*\\* " nil t)
                                       (line-beginning-position)
                                     (point-max)))))
                (claude-multi-layout--apply-assignment-indicator
                 section-start now-hour now-min)))))))))

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
Top-left: today's schedule (derived read-only buffer) at 45% width.
Top-right: triage.org for task reference at 55% width.
Bottom: progress buffer at 50% height."
  (interactive)
  ;; Clear any existing time indicator overlays from previous invocation
  (claude-multi-layout--clear-time-overlays)
  (let* ((today-buf (claude-multi-layout--derive-today-buffer))
         (triage-buf (claude-multi-layout--find-or-create-org-file "triage.org")))
    (claude-multi-layout--setup-layout 'focus 0.5 40)
    ;; Top-left: today's schedule
    (switch-to-buffer today-buf)
    ;; Top-right: task triage (left pane takes 45%)
    (let ((right-win (split-window-right (floor (* (window-width) 0.45)))))
      (select-window right-win)
      (switch-to-buffer triage-buf)
      (claude-multi-layout--disable-olivetti-in-buffer triage-buf))
    ;; Return focus to today's schedule
    (select-window (get-buffer-window today-buf))
    ;; Start time indicator timer and apply initial week file indicator
    (claude-multi-layout--start-time-indicator)
    (claude-multi-layout--apply-week-file-indicator)
    ;; Re-pin progress (taller for focus) after splits complete.
    (claude-multi-layout--ensure-progress-visible 0.5)
    (message "Layout: focus (today + triage + progress)")))

;;;###autoload
(defun claude-multi-layout/email-triage ()
  "Activate email-triage layout: email-triage.org (full width) + progress bottom.
Use for standalone inbox cleanup sessions."
  (interactive)
  (let* ((email-buf (claude-multi-layout--find-or-create-org-file "email-triage.org")))
    (claude-multi-layout--setup-layout 'email-triage 0.3)
    (switch-to-buffer email-buf)
    (claude-multi-layout--disable-olivetti-in-buffer email-buf)
    ;; Re-pin progress at the bottom after the main buffer is shown.
    (claude-multi-layout--ensure-progress-visible 0.3)
    (message "Layout: email-triage (email-triage.org)")))

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
    ;; Re-pin progress after magit/neotree have taken their windows.
    (claude-multi-layout--ensure-progress-visible 0.2)
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
Valid names: agenda, focus, email-triage, project, exit."
  (interactive
   (list (completing-read "Layout: "
                          '("agenda" "focus" "email-triage" "project" "exit")
                          nil t)))
  ;; Stop focus timer when switching away from focus layout
  (when (and (eq claude-multi-layout--current 'focus)
             (not (string= layout-name "focus")))
    (claude-multi-layout--stop-time-indicator))
  (pcase layout-name
    ("agenda"       (claude-multi-layout/agenda))
    ("focus"        (claude-multi-layout/focus))
    ("email-triage" (claude-multi-layout/email-triage))
    ("project"      (claude-multi-layout/project))
    ("exit"         (claude-multi-layout/exit))
    (_              (message "Unknown layout: %s" layout-name))))

;;;###autoload
(defun claude-multi-layout/revert-files ()
  "Revert all triage/planning org file buffers from disk.
Useful after workview CLI regenerates files."
  (interactive)
  (let ((files '("triage.org" "email-triage.org"))
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

;; ──────────────────────────────────────────────────────────────────────────────
;; Triage Filter Shortcuts — sparse-tree views over triage.org
;; ──────────────────────────────────────────────────────────────────────────────

(defun claude-multi-layout--triage-sparse-tree (pred label)
  "Build a sparse tree in triage.org showing entries where PRED returns non-nil.
PRED is called at each org entry with no args.  LABEL appears in the echo area."
  (let* ((path (expand-file-name "triage.org" claude-multi-layout--org-base-dir))
         (buf  (or (find-buffer-visiting path)
                   (and (file-exists-p path) (find-file-noselect path)))))
    (unless buf
      (user-error "triage.org not found at %s" path))
    (let ((win (get-buffer-window buf t)))
      (if win (select-window win) (pop-to-buffer buf)))
    (with-current-buffer buf
      (org-overview)
      (let ((count 0))
        (org-map-entries
         (lambda ()
           (when (funcall pred)
             (cl-incf count)
             (org-show-context 'match)))
         nil 'file)
        (message "Triage filter [%s]: %d entries" label count)))))

;;;###autoload
(defun claude-multi-layout/triage-filter-week ()
  "Sparse tree: TODO/INPROGRESS entries with DEADLINE within the current ISO week."
  (interactive)
  (let* ((now        (claude-multi-layout--now))
         (dow        (string-to-number (format-time-string "%u" now))) ; 1=Mon..7=Sun
         (today-days (time-to-days now))
         (week-start (- today-days (1- dow)))
         (week-end   (+ today-days (- 7 dow))))
    (claude-multi-layout--triage-sparse-tree
     (lambda ()
       (let ((state (org-get-todo-state))
             (dl    (org-entry-get nil "DEADLINE")))
         (and (member state '("TODO" "INPROGRESS"))
              dl
              (let ((d (time-to-days (org-time-string-to-time dl))))
                (and (>= d week-start) (<= d week-end))))))
     "this week")))

;;;###autoload
(defun claude-multi-layout/triage-filter-no-date ()
  "Sparse tree: TODO/INPROGRESS entries that have no DEADLINE."
  (interactive)
  (claude-multi-layout--triage-sparse-tree
   (lambda ()
     (and (member (org-get-todo-state) '("TODO" "INPROGRESS"))
          (not (org-entry-get nil "DEADLINE"))))
   "no date"))

;;;###autoload
(defun claude-multi-layout/triage-filter-postpone ()
  "Sparse tree: all POSTPONE entries."
  (interactive)
  (claude-multi-layout--triage-sparse-tree
   (lambda ()
     (string= (org-get-todo-state) "POSTPONE"))
   "POSTPONE"))

;;;###autoload
(defun claude-multi-layout/triage-filter-clear ()
  "Clear triage sparse-tree filter, expanding all entries."
  (interactive)
  (let* ((path (expand-file-name "triage.org" claude-multi-layout--org-base-dir))
         (buf  (find-buffer-visiting path)))
    (if (null buf)
        (message "Triage filter: triage.org not open")
      (let ((win (get-buffer-window buf t)))
        (if win (select-window win) (pop-to-buffer buf)))
      (with-current-buffer buf
        (org-show-all))
      (message "Triage filter: cleared"))))

(provide 'claude-multi-layout)
;;; claude-multi-layout.el ends here
