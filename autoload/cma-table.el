;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; autoload/cma-table.el --- Table view backed by cma list --json

;;; Commentary:
;; Tabulated-list-mode view populated from `cma list --json` instead of
;; reading ~/.cma/status/ directly. Preserves column layout, keybindings,
;; parent-child indentation, and evil mode support from claude-multi-table.el.

;;; Code:

(require 'tabulated-list)
(require 'cma-core)

(defvar cma-table--refresh-timer nil
  "Timer for periodic table refresh.")

(defvar cma-table--refresh-interval 5
  "Seconds between automatic table refreshes.")

(defface cma-table-face-permission
  '((t :foreground "#FF6347"))
  "Face for agents needing tool approval (permission_prompt)."
  :group 'claude-multi)

(defface cma-table-face-elicitation
  '((t :foreground "#FFD700"))
  "Face for agents asking a question (elicitation_dialog)."
  :group 'claude-multi)

(defface cma-table-face-idle
  '((t :foreground "#00FF7F"))
  "Face for agents that finished work (idle_prompt)."
  :group 'claude-multi)

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
         (waiting (eq (alist-get 'waiting_for_input agent) t))
         (notification-type (alist-get 'notification_type agent))
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
                    ""))
         ;; Enhanced status text based on notification type
         (display-status (if (and waiting notification-type)
                             (pcase notification-type
                               ("permission_prompt" "PERMISSION")
                               ("elicitation_dialog" "QUESTION")
                               ("idle_prompt" "IDLE")
                               (_ (upcase status-str)))
                           (upcase status-str)))
         ;; Face for notification-based coloring
         (row-face (when waiting
                     (pcase notification-type
                       ("permission_prompt" 'cma-table-face-permission)
                       ("elicitation_dialog" 'cma-table-face-elicitation)
                       ("idle_prompt" 'cma-table-face-idle)
                       (_ nil)))))
    (list session-id
          (if row-face
              (vector icon
                      (propertize window-id 'face row-face)
                      (propertize display-name 'face row-face)
                      (propertize location 'face row-face)
                      (propertize display-status 'face row-face)
                      (propertize (upcase model) 'face row-face)
                      (propertize duration 'face row-face)
                      (propertize ctx-str 'face row-face))
            (vector icon
                    window-id
                    display-name
                    location
                    display-status
                    (upcase model)
                    duration
                    ctx-str)))))

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

(declare-function cma--maybe-cleanup-worktree "cma-commands")

;;;###autoload
(defun cma-table/kill-agent ()
  "Kill the agent at point.
After killing, offers to clean up associated worktree."
  (interactive)
  (let ((session-id (tabulated-list-get-id)))
    (if (not session-id)
        (message "No agent at point")
      (when (y-or-n-p (format "Kill agent %s? " session-id))
        ;; Look up branch BEFORE killing (kill deletes status files)
        (let* ((agents (cma--call "list" "--json"))
               (agent (cl-find-if (lambda (a)
                                    (string= (alist-get 'session_id a) session-id))
                                  agents))
               (branch (when agent (alist-get 'git_branch agent))))
          (cma--call-raw "kill" session-id)
          (message "Killed agent %s" session-id)
          (when branch
            (cma--maybe-cleanup-worktree branch))
          (cma-table/refresh))))))

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
