;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; autoload/cma-table.el --- Table view backed by cma list --json

;;; Commentary:
;; Tabulated-list-mode view populated from `cma list --json` instead of
;; reading /tmp/claude-status/ directly. Preserves column layout, keybindings,
;; parent-child indentation, and evil mode support from claude-multi-table.el.

;;; Code:

(require 'tabulated-list)
(require 'cma-core)

(defvar cma-table--refresh-timer nil
  "Timer for periodic table refresh.")

(defvar cma-table--refresh-interval 5
  "Seconds between automatic table refreshes.")

;;; Table mode

(defconst cma-table-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'cma-table/focus-agent)
    (define-key map (kbd "f") #'cma-table/focus-agent)
    (define-key map (kbd "r") #'cma-table/rename-agent)
    (define-key map (kbd "g") #'cma-table/refresh)
    map)
  "Keymap for `cma-table-mode'.")

(define-derived-mode cma-table-mode tabulated-list-mode "CMA-Table"
  "Major mode for viewing Claude agents via cma CLI.

Grouped by kitty window ID — agents in the same session appear together.

Keybindings:
  f / RET - Focus on agent at point
  K       - Kill agent at point
  r       - Rename agent at point
  g       - Refresh table"

  (setq tabulated-list-format
        [("Icon"     6 nil :right-align nil)
         ("Window"   8 t :right-align nil)
         ("Name"     25 t :right-align nil)
         ("Location" 30 t :right-align nil)
         ("Status"   14 t :right-align nil)
         ("Model"    8 t :right-align nil)
         ("Time"     10 nil :right-align t)
         ("Ctx"      8 nil :right-align t)])

  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Window" nil))
  (add-hook 'tabulated-list-revert-hook #'cma-table--populate nil t)
  (tabulated-list-init-header)
  (setq buffer-read-only t)

  ;; Evil mode keybindings
  (when (and (fboundp 'evil-define-key)
             (fboundp 'evil-normalize-keymaps))
    (evil-define-key 'normal cma-table-mode-map
      (kbd "f") #'cma-table/focus-agent
      (kbd "RET") #'cma-table/focus-agent
      (kbd "r") #'cma-table/rename-agent
      (kbd "g") #'cma-table/refresh
      (kbd "K") #'cma-table/kill-agent)
    (evil-normalize-keymaps))

  ;; Auto-refresh timer
  (cma-table--start-refresh-timer))

;;; Data population

(defun cma-table--populate ()
  "Populate table from `cma list --json'."
  (let* ((agents (cma--call "list" "--json"))
         ;; Sort by window ID then by session start time
         (sorted (sort (copy-sequence (or agents '()))
                       (lambda (a b)
                         (let ((win-a (or (alist-get 'kitty_window_id a) ""))
                               (win-b (or (alist-get 'kitty_window_id b) ""))
                               (time-a (or (alist-get 'session_started a) ""))
                               (time-b (or (alist-get 'session_started b) "")))
                           (if (string= win-a win-b)
                               (string< time-a time-b)
                             (string< win-a win-b))))))
         (prev-window nil)
         (entries nil))
    (dolist (agent sorted)
      (let* ((window-id (or (alist-get 'kitty_window_id agent) ""))
             (is-child (and prev-window (string= window-id prev-window))))
        (push (cma-table--agent-to-entry agent is-child) entries)
        (setq prev-window window-id)))
    (setq tabulated-list-entries (nreverse entries))))

(defun cma-table--agent-to-entry (agent is-child)
  "Convert AGENT alist to tabulated-list entry.
IS-CHILD adds indentation prefix for child agents."
  (let* ((session-id (alist-get 'session_id agent))
         (name (or (alist-get 'name agent) ""))
         (status-str (or (alist-get 'status agent) "unknown"))
         (cwd (or (alist-get 'cwd agent) ""))
         (model (or (alist-get 'model_name agent) ""))
         (window-id (or (alist-get 'kitty_window_id agent) "—"))
         (git-branch (alist-get 'git_branch agent))
         (context-pct (alist-get 'context_used agent))
         (duration (or (alist-get 'duration agent) ""))
         (waiting (alist-get 'waiting_for_input agent))
         ;; Build location string
         (dir-name (when (not (string-empty-p cwd))
                     (file-name-nondirectory (directory-file-name cwd))))
         (location (cond
                    ((and dir-name git-branch) (format "%s (%s)" dir-name git-branch))
                    (dir-name dir-name)
                    ((not (string-empty-p cwd)) cwd)
                    (t "unknown")))
         ;; Status icon
         (icon (cond
                (waiting "🟡")
                ((string= status-str "running") "🟢")
                ((string= status-str "idle") "⚪")
                ((string= status-str "waiting-input") "🟡")
                ((string= status-str "completed") "🔵")
                ((string= status-str "failed") "🔴")
                (t "⚪")))
         ;; Display name with indentation for children
         (display-name (if is-child (concat "|-> " name) name))
         ;; Context percentage
         (ctx-str (if (and context-pct (> context-pct 0))
                      (format "%.1f%%" context-pct)
                    "")))
    (list session-id
          (vector icon
                  window-id
                  display-name
                  location
                  (upcase status-str)
                  (upcase model)
                  duration
                  ctx-str))))

;;; Refresh timer

(defun cma-table--start-refresh-timer ()
  "Start periodic table refresh timer."
  (cma-table--stop-refresh-timer)
  (setq cma-table--refresh-timer
        (run-with-timer cma-table--refresh-interval
                        cma-table--refresh-interval
                        #'cma-table--auto-refresh)))

(defun cma-table--stop-refresh-timer ()
  "Stop periodic table refresh timer."
  (when cma-table--refresh-timer
    (cancel-timer cma-table--refresh-timer)
    (setq cma-table--refresh-timer nil)))

(defun cma-table--auto-refresh ()
  "Auto-refresh table if the buffer is still alive."
  (let ((buf (get-buffer "*Claude Multi-Agent Progress*")))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (when (derived-mode-p 'cma-table-mode)
          (tabulated-list-revert))))))

;;; Interactive commands

;;;###autoload
(defun cma-table/focus-agent ()
  "Focus on the agent at point."
  (interactive)
  (let ((session-id (tabulated-list-get-id)))
    (if (not session-id)
        (message "No agent at point")
      (cma--call-raw "focus" session-id)
      (message "Focused on agent %s" session-id))))

;;;###autoload
(defun cma-table/kill-agent ()
  "Kill the agent at point."
  (interactive)
  (let ((session-id (tabulated-list-get-id)))
    (if (not session-id)
        (message "No agent at point")
      (when (y-or-n-p (format "Kill agent %s? " session-id))
        (cma--call-raw "kill" session-id)
        (cma-table/refresh)
        (message "Killed agent %s" session-id)))))

;;;###autoload
(defun cma-table/rename-agent ()
  "Rename the agent at point."
  (interactive)
  (let ((session-id (tabulated-list-get-id)))
    (if (not session-id)
        (message "No agent at point")
      (let ((new-name (read-string "New name: ")))
        (when (not (string-empty-p new-name))
          (cma--call-raw "rename" session-id new-name)
          (cma-table/refresh)
          (message "Renamed to %s" new-name))))))

;;;###autoload
(defun cma-table/refresh ()
  "Refresh the table view."
  (interactive)
  (let ((buf (get-buffer "*Claude Multi-Agent Progress*")))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (when (derived-mode-p 'cma-table-mode)
          (tabulated-list-revert))))))

(provide 'cma-table)
;;; cma-table.el ends here
