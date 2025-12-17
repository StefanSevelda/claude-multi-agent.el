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

(defcustom claude-multi-kitty-listen-address nil
  "Kitty listen address for remote control.
If nil, uses KITTY_LISTEN_ON environment variable.
Format: unix:/tmp/kitty-claude or tcp:localhost:5555"
  :type '(choice (const :tag "Auto-detect from env" nil)
                 (string :tag "Custom address"))
  :group 'claude-multi)

(defcustom claude-multi-kitty-window-type 'os-window
  "How to create kitty windows for agents.
'os-window - New OS window (separate kitty instance)
'tab - New tab in active kitty window
'window - New kitty split in active tab"
  :type '(choice (const :tag "OS Window" os-window)
                 (const :tag "Tab" tab)
                 (const :tag "Split Window" window))
  :group 'claude-multi)

(defcustom claude-multi-agent-spawn-type 'tab
  "How to spawn agents within the session OS window.
'tab - Each agent gets its own tab (default)
'split - Agents are split within tabs"
  :type '(choice (const :tag "New tab for each agent" tab)
                 (const :tag "Split window for each agent" split))
  :group 'claude-multi)

(defcustom claude-multi-output-throttle-delay 0.5
  "Delay in seconds between progress buffer updates to reduce flashing.
Setting this higher (e.g., 1.0) will reduce flashing but make updates less responsive.
Setting to 0 disables throttling."
  :type 'number
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
  "How to handle kitty windows when agents complete.
'keep-all - Keep all windows open (user closes manually)
'auto-close-success - Auto-cleanup worktrees for successful agents
'ask - Ask before closing kitty windows"
  :type '(choice (const :tag "Keep all windows" keep-all)
                 (const :tag "Auto-cleanup worktrees" auto-close-success)
                 (const :tag "Ask before closing" ask))
  :group 'claude-multi)

(defcustom claude-multi-agent-color-schemes
  '((1  :name "Bright Red"       :color "#FF4444" :text "#FFE5E5" :bg "#1a0808")
    (2  :name "Cyan"             :color "#00D9FF" :text "#E0F8FF" :bg "#081418")
    (3  :name "Medium Purple"    :color "#7B68EE" :text "#EDE8FF" :bg "#100a1a")
    (4  :name "Dark Orange"      :color "#FF8C00" :text "#FFEEDD" :bg "#1a1208")
    (5  :name "Spring Green"     :color "#00FF7F" :text "#E0FFE8" :bg "#081a0e")
    (6  :name "Deep Pink"        :color "#FF1493" :text "#FFE0F0" :bg "#1a0814")
    (7  :name "Gold"             :color "#FFD700" :text "#FFFAE0" :bg "#1a1808")
    (8  :name "Blue Violet"      :color "#8A2BE2" :text "#EFE5FF" :bg "#0e081a")
    (9  :name "Dark Turquoise"   :color "#00CED1" :text "#E0F5F7" :bg "#081416")
    (10 :name "Tomato Red"       :color "#FF6347" :text "#FFE8E0" :bg "#1a0e08"))
  "Color schemes for agents. Each scheme includes:
- :name - Descriptive name
- :color - Main accent color (cursor, tab, selection, border)
- :text - Terminal text color
- :bg - Terminal background color"
  :type 'list
  :group 'claude-multi)

;; Legacy compatibility - extract just colors for simple access
(defcustom claude-multi-agent-colors
  '("#FF4444" "#00D9FF" "#7B68EE" "#FF8C00" "#00FF7F"
    "#FF1493" "#FFD700" "#8A2BE2" "#00CED1" "#FF6347")
  "Colors to assign to agents for visual distinction (extracted from schemes)."
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

(defvar claude-multi--current-session-window-id nil
  "Kitty OS window ID for the current session.
Set when the first agent is spawned, used to target subsequent agents.")

(defvar claude-multi--current-session-tab-ids nil
  "List of kitty tab IDs for tabs created in the current session.
Used for round-robin split placement or intelligent tab management.")

;; Interactive commands

;;;###autoload
(defun claude-multi/start-session ()
  "Initialize a new multi-agent session and open the progress buffer."
  (interactive)
  (setq claude-multi--session-start-time (current-time))
  (setq claude-multi--agents nil)
  (setq claude-multi--agent-id-counter 0)
  (setq claude-multi--current-session-window-id nil)
  (setq claude-multi--current-session-tab-ids nil)
  ;; Setup notification system
  (claude-multi--setup-notifications)
  ;; Open progress buffer
  (claude-multi/open-progress)
  (message "Claude Multi-Agent session started. Use SPC c m a to spawn agents."))

;;;###autoload
(defun claude-multi/spawn-agent (task-description &optional directory branch)
  "Spawn a new Claude agent with TASK-DESCRIPTION.
Optional DIRECTORY specifies the working directory.
Optional BRANCH specifies the git branch (creates a worktree if provided)."
  (interactive "sTask description: ")
  (unless claude-multi--session-start-time
    (claude-multi/start-session))
  (let ((agent (claude-multi--create-agent task-description))
        (default-directory (or directory default-directory)))
    ;; Set directory as worktree path (will be used as working directory)
    (when directory
      (setf (claude-agent-worktree-path agent) (expand-file-name directory)))
    ;; Only set branch if provided (this triggers worktree creation)
    (when (and branch (not (string-empty-p branch)))
      (setf (claude-agent-branch-name agent) branch))
    (push agent claude-multi--agents)
    (claude-multi--launch-agent agent)
    (message "Spawned agent: %s" (claude-agent-name agent))))

;;;###autoload
(defun claude-multi/spawn-agent-tab (task-description)
  "Spawn a new Claude agent in a kitty TAB with TASK-DESCRIPTION."
  (interactive "sTask description: ")
  (unless claude-multi--session-start-time
    (claude-multi/start-session))
  (let ((claude-multi-kitty-window-type 'tab)
        (agent (claude-multi--create-agent task-description)))
    (push agent claude-multi--agents)
    (claude-multi--launch-agent agent)
    (message "Spawned agent in tab: %s" (claude-agent-name agent))))

;;;###autoload
(defun claude-multi/spawn-agent-split (task-description)
  "Spawn a new Claude agent in a kitty SPLIT WINDOW with TASK-DESCRIPTION."
  (interactive "sTask description: ")
  (unless claude-multi--session-start-time
    (claude-multi/start-session))
  (let ((claude-multi-kitty-window-type 'window)
        (agent (claude-multi--create-agent task-description)))
    (push agent claude-multi--agents)
    (claude-multi--launch-agent agent)
    (message "Spawned agent in split: %s" (claude-agent-name agent))))

;;;###autoload
(defun claude-multi/spawn-agent-with-worktree ()
  "Spawn agent with custom directory and branch for worktree."
  (interactive)
  (let* ((task (read-string "Task description: "))
         (directory (read-directory-name "Directory (for worktree): " nil nil t))
         (branch (read-string "Branch name (optional): ")))
    (claude-multi/spawn-agent
     task
     directory
     (if (string-empty-p branch) nil branch))))

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

;; Note: claude-multi/send-input removed - user types directly in kitty terminal

;;;###autoload
(defun claude-multi/focus-agent ()
  "Switch focus to a specific agent's kitty window."
  (interactive)
  (if (null claude-multi--agents)
      (message "No active agents")
    (let ((agent (claude-multi--select-agent claude-multi--agents "Focus on agent: ")))
      (when agent
        (let* ((window-id (claude-agent-kitty-window-id agent))
               (listen-addr (or claude-multi-kitty-listen-address
                               (getenv "KITTY_LISTEN_ON")
                               "unix:/tmp/kitty-claude")))
          (call-process-shell-command
           (format "kitty @ --to=%s focus-window --match=id:%s"
                  listen-addr window-id)
           nil 0))))))

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
    ;; Close session OS window if it exists
    (when claude-multi--current-session-window-id
      (let ((listen-addr (or claude-multi-kitty-listen-address
                             (getenv "KITTY_LISTEN_ON")
                             "unix:/tmp/kitty-claude")))
        (call-process-shell-command
         (format "kitty @ --to=%s close-window --match=id:%s"
                 listen-addr
                 claude-multi--current-session-window-id)
         nil 0))
      (setq claude-multi--current-session-window-id nil))
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
      :desc "Spawn agent in tab"      "t" #'claude-multi/spawn-agent-tab
      :desc "Spawn agent in split"    "w" #'claude-multi/spawn-agent-split
      :desc "Spawn with worktree"     "W" #'claude-multi/spawn-agent-with-worktree
      :desc "Open progress"           "p" #'claude-multi/open-progress
      :desc "Dashboard"               "d" #'claude-multi/dashboard
      :desc "Focus agent"             "f" #'claude-multi/focus-agent
      :desc "Kill agent"              "k" #'claude-multi/kill-agent
      :desc "Kill all"                "K" #'claude-multi/kill-all-agents
      :desc "Export progress"         "e" #'claude-multi/export-progress
      :desc "List worktrees"          "T" #'claude-multi/list-worktrees
      :desc "Cleanup worktrees"       "c" #'claude-multi/cleanup-orphaned-worktrees)

(provide 'claude-multi-config)
;;; config.el ends here
