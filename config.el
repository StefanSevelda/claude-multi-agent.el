;; -*- lexical-binding: t; -*-
;;; tools/claude-multi/config.el

;;; Commentary:
;; Configuration for Claude Multi-Agent Plugin
;; Manages multiple claude-code instances in parallel with worktree isolation

;;; Code:

;; Add autoload directory to load path and load modules
(let ((autoload-dir (expand-file-name "autoload"
                                      (or (and load-file-name
                                               (file-name-directory load-file-name))
                                          (and (boundp 'byte-compile-current-file)
                                               byte-compile-current-file
                                               (file-name-directory byte-compile-current-file))
                                          default-directory))))
  (add-to-list 'load-path autoload-dir)
  (require 'claude-multi-agents)
  (require 'claude-multi-progress)
  (require 'claude-multi-worktree)
  (require 'claude-multi-notifications))

(defgroup claude-multi nil
  "Manage multiple Claude Code agents in parallel."
  :group 'tools
  :prefix "claude-multi-")

(defcustom claude-multi-worktree-location 'adjacent
  "Where to create worktrees for agents.
'adjacent - Create in ../claude-worktrees/
'internal - Create in .git/worktrees/"
  :type '(choice (const :tag "Adjacent directory" adjacent)
                 (const :tag "Internal .git/worktrees" internal))
  :group 'claude-multi)

(defcustom claude-multi-auto-cleanup t
  "Automatically cleanup worktrees when agents complete."
  :type 'boolean
  :group 'claude-multi)

(defcustom claude-multi-claude-command "claude"
  "Command to run Claude Code CLI.
This can be customized to use different binary names (e.g., 'claude26')."
  :type 'string
  :group 'claude-multi)

(defcustom claude-multi-notification-methods '(popup markdown modeline)
  "List of notification methods to use when agents need input.
Available methods: popup, markdown, modeline, sound"
  :type '(set (const :tag "Popup notification" popup)
              (const :tag "Markdown highlight" markdown)
              (const :tag "Mode line indicator" modeline)
              (const :tag "Audio alert" sound))
  :group 'claude-multi)

(defcustom claude-multi-buffer-cleanup 'auto-close-success
  "How to handle eshell buffers when agents complete.
'keep-all - Keep all buffers open
'auto-close-success - Close successful agents, keep failed ones
'ask - Ask before closing each buffer"
  :type '(choice (const :tag "Keep all buffers" keep-all)
                 (const :tag "Auto-close successful" auto-close-success)
                 (const :tag "Ask before closing" ask))
  :group 'claude-multi)

(defcustom claude-multi-agent-colors
  '("#FF6B6B" "#4ECDC4" "#45B7D1" "#FFA07A" "#98D8C8"
    "#F7DC6F" "#BB8FCE" "#85C1E2" "#F8B739" "#52B788")
  "Colors to assign to agents for visual distinction."
  :type '(repeat color)
  :group 'claude-multi)

(defcustom claude-multi-progress-buffer-name "*Claude Multi-Agent Progress.org*"
  "Name of the central progress tracking buffer (org-mode)."
  :type 'string
  :group 'claude-multi)

;; Global variables
(defvar claude-multi--agents nil
  "List of all active agents.")

(defvar claude-multi--agent-id-counter 0
  "Counter for generating unique agent IDs.")

(defvar claude-multi--progress-buffer nil
  "Buffer for displaying agent progress.")

(defvar claude-multi--session-start-time nil
  "Timestamp when the current session started.")

;; Interactive commands

;;;###autoload
(defun claude-multi/start-session ()
  "Initialize a new multi-agent session and open the progress buffer."
  (interactive)
  (setq claude-multi--session-start-time (current-time))
  (setq claude-multi--agents nil)
  (setq claude-multi--agent-id-counter 0)
  ;; Setup notification system
  (claude-multi--setup-notifications)
  ;; Open progress buffer
  (claude-multi/open-progress)
  (message "Claude Multi-Agent session started. Use SPC c m a to spawn agents."))

;;;###autoload
(defun claude-multi/spawn-agent (task-description)
  "Spawn a new Claude agent with TASK-DESCRIPTION."
  (interactive "sTask description: ")
  (unless claude-multi--session-start-time
    (claude-multi/start-session))
  (let ((agent (claude-multi--create-agent task-description)))
    (push agent claude-multi--agents)
    (claude-multi--launch-agent agent)
    (message "Spawned agent: %s" (claude-agent-name agent))))

;;;###autoload
(defun claude-multi/open-progress ()
  "Open the central progress tracking buffer."
  (interactive)
  (setq claude-multi--progress-buffer
        (get-buffer-create claude-multi-progress-buffer-name))
  (with-current-buffer claude-multi--progress-buffer
    (claude-multi-progress-mode)
    (unless (get-buffer-window claude-multi--progress-buffer)
      (display-buffer claude-multi--progress-buffer)))
  (claude-multi--init-progress-buffer))

;;;###autoload
(defun claude-multi/dashboard ()
  "Show a dashboard with all agents and their status."
  (interactive)
  (let ((buf (get-buffer-create "*Claude Multi-Agent Dashboard.org*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "#+TITLE: Claude Multi-Agent Dashboard\n\n")
      (if (null claude-multi--agents)
          (insert "No active agents.\n")
        (dolist (agent (reverse claude-multi--agents))
          (insert (format "* %s %s [%s]\n"
                         (claude-multi--get-status-icon (claude-agent-status agent))
                         (claude-agent-name agent)
                         (upcase (symbol-name (claude-agent-status agent)))))
          (insert (format "- ID :: %s\n" (claude-agent-id agent)))
          (insert (format "- Worktree :: %s\n" (or (claude-agent-worktree-path agent) "N/A")))
          (insert (format "- Created :: %s\n" (format-time-string "[%Y-%m-%d %a %H:%M:%S]"
                                                                 (claude-agent-created-at agent))))
          (when (claude-agent-completed-at agent)
            (insert (format "- Completed :: %s\n" (format-time-string "[%Y-%m-%d %a %H:%M:%S]"
                                                                     (claude-agent-completed-at agent))))
            (insert (format "- Duration :: %s\n"
                           (claude-multi--format-duration
                            (claude-agent-created-at agent)
                            (claude-agent-completed-at agent)))))
          (insert "\n")))
      (org-mode)
      (read-only-mode 1)
      (goto-char (point-min)))
    (display-buffer buf)))

;;;###autoload
(defun claude-multi/send-input ()
  "Send input to an agent waiting for response."
  (interactive)
  (let* ((waiting-agents (cl-remove-if-not
                         (lambda (a) (eq (claude-agent-status a) 'waiting-input))
                         claude-multi--agents))
         (agent (if (= 1 (length waiting-agents))
                   (car waiting-agents)
                 (claude-multi--select-agent waiting-agents "Send input to agent: "))))
    (if agent
        (let ((input (read-string (format "Input for %s: " (claude-agent-name agent)))))
          (claude-multi--send-input-to-agent agent input))
      (message "No agents waiting for input"))))

;;;###autoload
(defun claude-multi/focus-agent ()
  "Switch to a specific agent's eshell buffer."
  (interactive)
  (if (null claude-multi--agents)
      (message "No active agents")
    (let ((agent (claude-multi--select-agent claude-multi--agents "Focus on agent: ")))
      (when agent
        (switch-to-buffer (claude-agent-buffer agent))))))

;;;###autoload
(defun claude-multi/kill-agent ()
  "Kill a specific agent and cleanup resources."
  (interactive)
  (if (null claude-multi--agents)
      (message "No active agents")
    (let ((agent (claude-multi--select-agent claude-multi--agents "Kill agent: ")))
      (when agent
        (when (y-or-n-p (format "Really kill agent %s? " (claude-agent-name agent)))
          (claude-multi--kill-agent agent)
          (message "Killed agent: %s" (claude-agent-name agent)))))))

;;;###autoload
(defun claude-multi/kill-all-agents ()
  "Kill all agents and cleanup all worktrees."
  (interactive)
  (when (and claude-multi--agents
             (y-or-n-p (format "Really kill all %d agents? " (length claude-multi--agents))))
    (dolist (agent claude-multi--agents)
      (claude-multi--kill-agent agent))
    (setq claude-multi--agents nil)
    ;; Teardown notification system
    (claude-multi--teardown-notifications)
    (message "All agents killed")))

;; Progress buffer mode

(define-derived-mode claude-multi-progress-mode org-mode "Claude-Multi-Progress"
  "Major mode for Claude Multi-Agent progress tracking in org-mode format."
  (setq-local auto-revert-interval 0.5)
  (auto-revert-mode 1)
  (read-only-mode 1)
  ;; Enable org-mode features
  (org-indent-mode 1)
  (visual-line-mode 1))

;; Keybindings

(map! :leader
      :prefix ("c m" . "claude-multi")
      :desc "Start session"           "s" #'claude-multi/start-session
      :desc "Spawn agent"             "a" #'claude-multi/spawn-agent
      :desc "Open progress"           "p" #'claude-multi/open-progress
      :desc "Dashboard"               "d" #'claude-multi/dashboard
      :desc "Send input"              "i" #'claude-multi/send-input
      :desc "Focus agent"             "f" #'claude-multi/focus-agent
      :desc "Kill agent"              "k" #'claude-multi/kill-agent
      :desc "Kill all"                "K" #'claude-multi/kill-all-agents
      :desc "Export progress"         "e" #'claude-multi/export-progress
      :desc "Show waiting agents"     "w" #'claude-multi/show-waiting-agents
      :desc "List worktrees"          "t" #'claude-multi/list-worktrees
      :desc "Cleanup worktrees"       "c" #'claude-multi/cleanup-orphaned-worktrees)

(provide 'claude-multi-config)
;;; config.el ends here
