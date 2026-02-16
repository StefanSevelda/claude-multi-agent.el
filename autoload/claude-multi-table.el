;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; autoload/claude-multi-table.el --- Table view for Claude Multi-Agent

;;; Commentary:
;; Provides a compact table-based view of agents as an alternative to org-mode view
;; Uses Emacs' built-in tabulated-list-mode for sortable columns

;;; Code:

(require 'tabulated-list)

;; Forward declarations
(declare-function claude-multi--get-all-status-files "claude-multi-status")
(declare-function claude-multi--get-status-icon-from-string "claude-multi-progress")
(declare-function claude-multi/focus-agent-at-point "claude-multi-progress")
(declare-function claude-multi/kill-agent-at-point "claude-multi-progress")
(declare-function claude-multi--status-file-path "claude-multi-status")
(declare-function claude-multi--kill-agent-by-session-id "claude-multi-status")

(defvar claude-multi--progress-buffer)

;;; Table view mode

(defvar claude-multi-table-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claude-multi-table/focus-agent)
    (define-key map (kbd "f") #'claude-multi-table/focus-agent)
    (define-key map (kbd "r") #'claude-multi-table/rename-agent)
    (define-key map (kbd "g") #'claude-multi-table/refresh)
    map)
  "Keymap for `claude-multi-table-mode'.")

(define-derived-mode claude-multi-table-mode tabulated-list-mode "Claude-Multi-Table"
  "Major mode for viewing Claude agents in a table format.

Table is grouped by Window ID (kitty_window_id) - agents spawned
in the same session appear together.

Keybindings:
  \\[claude-multi-table/focus-agent] - Focus on agent at point (f, RET)
  \\[claude-multi-table/rename-agent] - Rename agent at point (r)
  \\[claude-multi-table/refresh] - Refresh table (g)

Note: Killing an agent kills all agents in the same session window."

  (setq tabulated-list-format
        [("Icon"     6 nil :right-align nil)
         ("Window"   8 t :right-align nil)
         ("Name"     25 t :right-align nil)
         ("Location" 30 t :right-align nil)
         ("Status"   10 t :right-align nil)
         ("Model"    8 t :right-align nil)
         ("Time"     10 nil :right-align t)
         ("Tokens"   8 nil :right-align t)])

  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Window" nil))
  (add-hook 'tabulated-list-revert-hook 'claude-multi--populate-table-view nil t)
  (tabulated-list-init-header)
  (setq buffer-read-only t)

  ;; Set up evil mode keybindings if evil is loaded
  (when (and (fboundp 'evil-define-key)
             (fboundp 'evil-normalize-keymaps))
    (evil-define-key 'normal claude-multi-table-mode-map
      (kbd "f") #'claude-multi-table/focus-agent
      (kbd "RET") #'claude-multi-table/focus-agent
      (kbd "r") #'claude-multi-table/rename-agent
      (kbd "g") #'claude-multi-table/refresh)
    ;; Update evil's keymap cache
    (evil-normalize-keymaps)))

;;; Table population

;;;###autoload
(defun claude-multi--populate-table-view ()
  "Populate the table view with agent data from status files.
Sorts by window ID (grouping agents in same session) then by timestamp
(oldest first, so parent agents appear before their sub-agents)."
  (when (fboundp 'claude-multi--get-all-status-files)
    (let* ((status-files (claude-multi--get-all-status-files))
           ;; Sort status files: first by window ID, then by session_started timestamp
           (sorted-files
            (sort status-files
                  (lambda (a b)
                    (let* ((data-a (cdr a))
                           (data-b (cdr b))
                           (win-a (or (alist-get 'kitty_window_id data-a) ""))
                           (win-b (or (alist-get 'kitty_window_id data-b) ""))
                           (time-a (or (alist-get 'session_started data-a) ""))
                           (time-b (or (alist-get 'session_started data-b) "")))
                      (if (string= win-a win-b)
                          ;; Same window: sort by timestamp (oldest first)
                          (string< time-a time-b)
                        ;; Different windows: sort by window ID
                        (string< win-a win-b))))))
           ;; Track window IDs to add indentation for child agents
           (prev-window nil)
           (entries nil))
      (dolist (file-data sorted-files)
        (let* ((data (cdr file-data))
               (window-id (alist-get 'kitty_window_id data))
               (is-child (and prev-window (string= window-id prev-window))))
          (push (claude-multi--agent-to-table-entry file-data is-child) entries)
          (setq prev-window window-id)))
      (setq tabulated-list-entries (nreverse entries)))))

(defun claude-multi--agent-to-table-entry (file-data &optional is-child)
  "Convert status FILE-DATA (FILE . STATUS-DATA) to table entry format.
If IS-CHILD is non-nil, adds indentation prefix to show hierarchy."
  (let* ((data (cdr file-data))
         (session-id (alist-get 'session_id data))
         ;; Agent name: ONLY from mapping file (ignores status.json agent_name)
         (mapped-name (when (fboundp 'claude-multi--read-rename-mapping)
                       (claude-multi--read-rename-mapping session-id)))
         (status (alist-get 'claude_status data))
         (cwd (alist-get 'cwd data))
         (model (alist-get 'model_name data))
         (context (alist-get 'context_window data))
         (git-info (alist-get 'git data))
         (timestamp (alist-get 'timestamp data))
         (started (alist-get 'session_started data))
         (tokens-pct (when context (or (alist-get 'percentage_used context) 0)))
         (icon (if (fboundp 'claude-multi--get-status-icon-from-string)
                   (claude-multi--get-status-icon-from-string status)
                 "⚪"))
         (dir-name (when cwd (file-name-nondirectory (directory-file-name cwd))))
         (branch (when git-info (alist-get 'branch git-info)))
         (location (cond
                    ((and dir-name branch) (format "%s (%s)" dir-name branch))
                    (dir-name dir-name)
                    (cwd cwd)
                    (t "unknown")))
         ;; Final display name: mapping file wins, fallback to location
         (base-display-name (or mapped-name location))
         ;; Add indentation prefix for child agents
         (display-name (if is-child
                          (concat "|-> " base-display-name)
                        base-display-name))
         (duration (claude-multi--calculate-duration started))
         (model-str (if model (upcase model) "")))

    (list session-id
          (vector icon
                  (or (alist-get 'kitty_window_id data) "—")
                  display-name
                  location
                  (upcase (or status "unknown"))
                  model-str
                  duration
                  (if tokens-pct (format "%.1f%%" tokens-pct) "")))))

(defun claude-multi--calculate-duration (started-timestamp)
  "Calculate duration from STARTED-TIMESTAMP to now.
Returns formatted string like '12m 34s' or '2h 15m'."
  (if (not started-timestamp)
      ""
    (condition-case nil
        (let* ((started-str (format "%s" started-timestamp))
               ;; Parse ISO 8601 timestamp
               (started-time (date-to-time started-str))
               (elapsed-seconds (float-time (time-subtract nil started-time)))
               (hours (floor (/ elapsed-seconds 3600)))
               (minutes (floor (/ (mod elapsed-seconds 3600) 60)))
               (seconds (floor (mod elapsed-seconds 60))))
          (cond
           ((> hours 0) (format "%dh %dm" hours minutes))
           ((> minutes 0) (format "%dm %ds" minutes seconds))
           (t (format "%ds" seconds))))
      (error ""))))

;;; Interactive commands

;;;###autoload
(defun claude-multi-table/focus-agent ()
  "Focus on the agent at point in the table view."
  (interactive)
  (let ((session-id (tabulated-list-get-id)))
    (if (not session-id)
        (message "No agent at point")
      ;; Get status data for this agent
      (when (fboundp 'claude-multi--get-all-status-files)
        (let* ((status-files (claude-multi--get-all-status-files))
               (agent-entry (cl-find-if
                             (lambda (entry)
                               (string= session-id (alist-get 'session_id (cdr entry))))
                             status-files)))
          (if (not agent-entry)
              (message "Agent not found: %s" session-id)
            (let* ((data (cdr agent-entry))
                   (window-id (alist-get 'kitty_window_id data))
                   ;; Get agent name from mapping file, not status.json
                   (mapped-name (when (fboundp 'claude-multi--read-rename-mapping)
                                 (claude-multi--read-rename-mapping session-id)))
                   (agent-name (or mapped-name session-id))
                   (listen-addr (or (and (boundp 'claude-multi-kitty-listen-address)
                                        claude-multi-kitty-listen-address)
                                   (getenv "KITTY_LISTEN_ON")
                                   "unix:/tmp/kitty-claude")))
              (if (not window-id)
                  (message "No kitty window ID found for %s" (or agent-name session-id))
                (condition-case err
                    (progn
                      (call-process-shell-command
                       (format "kitty @ --to=%s focus-window --match=id:%s"
                              listen-addr window-id)
                       nil 0)
                      (message "Focused on %s (window %s)" (or agent-name session-id) window-id))
                  (error
                   (message "Failed to focus window %s: %s"
                           window-id (error-message-string err))))))))))))

;;;###autoload
(defun claude-multi-table/kill-agent ()
  "Kill the agent at point and all agents in the same session window.
Prompts for confirmation, showing how many agents will be affected."
  (interactive)
  (let ((session-id (tabulated-list-get-id)))
    (if (not session-id)
        (message "No agent at point")
      ;; Get the kitty_window_id for this agent
      (let* ((status-files (claude-multi--get-all-status-files))
             (agent-entry (cl-find-if
                           (lambda (entry)
                             (equal session-id (alist-get 'session_id (cdr entry))))
                           status-files))
             (window-id (alist-get 'kitty_window_id (cdr agent-entry)))
             ;; Find all agents with same window ID
             (session-agents (cl-remove-if-not
                              (lambda (entry)
                                (equal window-id
                                       (alist-get 'kitty_window_id (cdr entry))))
                              status-files))
             (agent-count (length session-agents)))
        (if (not window-id)
            ;; No window ID — kitty window gone or never recorded.
            ;; Still clean up the status file for this single agent.
            (when (y-or-n-p (format "Kill agent %s (no kitty window)? " session-id))
              (require 'claude-multi-status)
              (claude-multi--kill-agent-by-session-id session-id)
              (claude-multi-table/refresh)
              (message "Cleaned up agent %s" session-id))
          ;; Prompt with count
          (when (y-or-n-p (format "Kill %d agent%s in window %s? "
                                  agent-count
                                  (if (> agent-count 1) "s" "")
                                  window-id))
            (require 'claude-multi-status)
            ;; Kill all agents in session
            (dolist (entry session-agents)
              (let ((sid (alist-get 'session_id (cdr entry))))
                (claude-multi--kill-agent-by-session-id sid)))
            (claude-multi-table/refresh)
            (message "Killed %d agent%s from window %s"
                     agent-count
                     (if (> agent-count 1) "s" "")
                     window-id)))))))

;;;###autoload
(defun claude-multi-table/rename-agent ()
  "Rename the agent at point in the table view."
  (interactive)
  (let ((session-id (tabulated-list-get-id)))
    (if (not session-id)
        (message "No agent at point")
      (when (fboundp 'claude-multi--get-all-status-files)
        (let* ((status-files (claude-multi--get-all-status-files))
               (agent-entry (cl-find-if
                             (lambda (entry)
                               (string= session-id (alist-get 'session_id (cdr entry))))
                             status-files)))
          (if (not agent-entry)
              (message "Agent not found: %s" session-id)
            (let* ((data (cdr agent-entry))
                   ;; Get current name from mapping file, not status.json
                   (current-name (if (fboundp 'claude-multi--read-rename-mapping)
                                    (claude-multi--read-rename-mapping session-id)
                                  (alist-get 'agent_name data)))
                   (new-name (read-string "New agent name: " current-name)))
              (when (and new-name (not (string-empty-p (string-trim new-name))))
                ;; Use the rename system from claude-multi-rename.el
                (if (fboundp 'claude-multi--update-status-agent-name)
                    (progn
                      (claude-multi--update-status-agent-name session-id new-name)
                      (message "Renamed agent to: %s" new-name)
                      (claude-multi-table/refresh))
                  ;; Fallback if rename module not loaded
                  (message "Rename module not available - please load claude-multi-rename"))))))))))

;;;###autoload
(defun claude-multi-table/refresh ()
  "Refresh the table view."
  (interactive)
  (when (derived-mode-p 'claude-multi-table-mode)
    (tabulated-list-revert)))

(provide 'claude-multi-table)
;;; claude-multi-table.el ends here
