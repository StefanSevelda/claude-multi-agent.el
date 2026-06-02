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

Grouped by domain — agents sharing a domain appear together.

Keybindings:
  f / RET - Focus on agent at point
  K       - Kill agent at point
  r       - Rename agent at point
  g       - Refresh table"

  (setq tabulated-list-format
        [("Icon"     6 nil :right-align nil)
         ("Domain"   16 t :right-align nil)
         ("Title"    28 t :right-align nil)
         ("Ctx"      8 nil :right-align t)
         ("Location" 30 t :right-align nil)
         ("Status"   14 t :right-align nil)
         ("Model"    8 t :right-align nil)
         ("Time"     10 nil :right-align t)])

  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Domain" nil))
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
  "Populate table from `cma list --json'.
Agents with the same non-empty domain are grouped together; agents with
no domain (empty string or absent key) are rendered flat in agent_id order."
  (let* ((agents (cma--call "list" "--json"))
         ;; Sort: domain first (so same-domain agents are contiguous), then agent_id
         (sorted (sort (copy-sequence (or agents '()))
                       (lambda (a b)
                         (let ((dom-a (or (alist-get 'domain a) ""))
                               (dom-b (or (alist-get 'domain b) ""))
                               (id-a  (or (alist-get 'agent_id a) ""))
                               (id-b  (or (alist-get 'agent_id b) "")))
                           (if (string= dom-a dom-b)
                               (string< id-a id-b)
                             (string< dom-a dom-b))))))
         (prev-domain nil)
         (entries nil))
    (dolist (agent sorted)
      (let* ((domain (or (alist-get 'domain agent) ""))
             (is-child (and (not (string-empty-p domain))
                            prev-domain
                            (string= domain prev-domain))))
        (push (cma-table--agent-to-entry agent is-child) entries)
        (setq prev-domain (if (string-empty-p domain) nil domain))))
    (setq tabulated-list-entries (nreverse entries))))

(defun cma-table--agent-to-entry (agent is-child)
  "Convert AGENT alist to tabulated-list entry.
IS-CHILD adds indentation prefix indicating the agent shares a domain group."
  (let* ((agent-id (or (alist-get 'agent_id agent)
                       (alist-get 'session_id agent) ; legacy fallback
                       "unknown"))
         (title (or (alist-get 'title agent) (alist-get 'name agent) (alist-get 'agent_id agent) ""))
         (domain (or (alist-get 'domain agent) ""))
         (status-str (or (alist-get 'status agent) "unknown"))
         (cwd (or (alist-get 'cwd agent) ""))
         (model (or (alist-get 'model_name agent) ""))
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
         ;; Indentation for domain-grouped children
         (display-title (if is-child (concat "|-> " title) title))
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
    (list agent-id  ; agent_id is the stable primary key from launch time
          (if row-face
              (vector icon
                      (propertize domain 'face row-face)
                      (propertize display-title 'face row-face)
                      (propertize ctx-str 'face row-face)
                      (propertize location 'face row-face)
                      (propertize display-status 'face row-face)
                      (propertize (upcase model) 'face row-face)
                      (propertize duration 'face row-face))
            (vector icon
                    domain
                    display-title
                    ctx-str
                    location
                    display-status
                    (upcase model)
                    duration)))))

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
  (let ((agent-id (tabulated-list-get-id)))
    (if (not agent-id)
        (message "No agent at point")
      (cma--call-raw "focus" agent-id)
      (message "Focused on agent %s" agent-id))))

(declare-function cma--maybe-cleanup-worktree "cma-commands")

;;;###autoload
(defun cma-table/kill-agent ()
  "Kill the agent at point.
After killing, offers to clean up associated worktree."
  (interactive)
  (let ((agent-id (tabulated-list-get-id)))
    (if (not agent-id)
        (message "No agent at point")
      (when (y-or-n-p (format "Kill agent %s? " agent-id))
        ;; Look up branch BEFORE killing (kill removes the registry entry)
        (let* ((agents (cma--call "list" "--json"))
               (agent (cl-find-if (lambda (a)
                                    (string= (alist-get 'agent_id a) agent-id))
                                  agents))
               (branch (when agent (alist-get 'git_branch agent))))
          (cma--call-raw "kill" agent-id)
          (message "Killed agent %s" agent-id)
          (when branch
            (cma--maybe-cleanup-worktree branch))
          (cma-table/refresh))))))

;;;###autoload
(defun cma-table/rename-agent ()
  "Rename the agent at point."
  (interactive)
  (let ((agent-id (tabulated-list-get-id)))
    (if (not agent-id)
        (message "No agent at point")
      (let ((new-name (read-string "New name: ")))
        (when (not (string-empty-p new-name))
          (cma--call-raw "rename" agent-id new-name)
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
