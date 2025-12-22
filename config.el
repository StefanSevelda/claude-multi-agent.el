;; -*- lexical-binding: t; -*-
;;; tools/claude-multi/config.el

;;; Commentary:
;; Configuration for Claude Multi-Agent Plugin
;; Manages multiple claude-code instances in parallel with worktree isolation

;;; Code:

(eval-and-compile
  (require 'subr-x))  ; For string-empty-p, string-trim

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
\\='adjacent - Create in ../claude-worktrees/
\\='internal - Create in .git/worktrees/"
  :type '(choice (const :tag "Adjacent directory" adjacent)
                 (const :tag "Internal .git/worktrees" internal))
  :group 'claude-multi)

(defcustom claude-multi-claude-command "claude"
  "Command to run Claude Code CLI.
This can be customized to use different binary names (e.g., \\='claude26\\=')."
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
\\='os-window - New OS window (separate kitty instance)
\\='tab - New tab in active kitty window
\\='window - New kitty split in active tab"
  :type '(choice (const :tag "OS Window" os-window)
                 (const :tag "Tab" tab)
                 (const :tag "Split Window" window))
  :group 'claude-multi)

(defcustom claude-multi-agent-spawn-type 'tab
  "How to spawn agents within the session OS window.
\\='tab - Each agent gets its own tab (default)
\\='split - Agents are split within tabs"
  :type '(choice (const :tag "New tab for each agent" tab)
                 (const :tag "Split window for each agent" split))
  :group 'claude-multi)

(defcustom claude-multi-output-throttle-delay 0.5
  "Delay in seconds between progress buffer updates to reduce flashing.
Setting this higher (e.g., 1.0) will reduce flashing but make updates
less responsive.  Setting to 0 disables throttling."
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
\\='keep-all - Keep all windows open (user closes manually)
\\='auto-close-success - Auto-cleanup worktrees for successful agents
\\='ask - Ask before closing kitty windows"
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
  :type '(repeat (list integer
                       (plist :key-type symbol :value-type string)))
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

;; Forward declarations for functions in other modules
(declare-function claude-multi--stop-all-status-watches "claude-multi-progress")
(declare-function claude-multi--setup-notifications "claude-multi-notifications")
(declare-function claude-multi--teardown-notifications "claude-multi-notifications")
(declare-function claude-multi--kill-agent "claude-multi-agents")
(declare-function claude-multi--format-duration "claude-multi-agents")
(declare-function claude-multi--get-status-icon "claude-multi-progress")
(declare-function claude-agent-kitty-window-id "claude-multi-agents")
(declare-function claude-agent-name "claude-multi-agents")
(declare-function claude-agent-status "claude-multi-agents")
(declare-function claude-multi--select-agent "claude-multi-agents")
(declare-function claude-multi--create-agent "claude-multi-agents")
(declare-function claude-multi--launch-agent "claude-multi-agents")
(declare-function claude-agent-worktree-path "claude-multi-agents")
(declare-function claude-agent-branch-name "claude-multi-agents")
(declare-function claude-agent-created-at "claude-multi-agents")
(declare-function claude-agent-completed-at "claude-multi-agents")
(declare-function claude-agent-id "claude-multi-agents")
(declare-function claude-multi--init-progress-buffer "claude-multi-progress")

;; cl-lib setf accessors for struct
(gv-define-setter claude-agent-worktree-path (val agent) `(aset ,agent 7 ,val))
(gv-define-setter claude-agent-branch-name (val agent) `(aset ,agent 8 ,val))

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
(defun claude-multi/spawn-agent ()
  "Spawn a new Claude agent in a kitty tab.
Prompts for task description and working directory.
Agent will cd into the directory before launching Claude."
  (interactive)
  (unless claude-multi--session-start-time
    (claude-multi/start-session))
  ;; Ensure progress buffer exists
  (unless (and claude-multi--progress-buffer
               (buffer-live-p claude-multi--progress-buffer))
    (claude-multi/open-progress))
  (let* ((task (read-string "Task description: "))
         (directory (read-directory-name "Working directory: " default-directory nil t))
         (agent (claude-multi--create-agent task)))
    ;; Set directory as worktree path (will be used as working directory)
    (setf (claude-agent-worktree-path agent) (expand-file-name directory))
    (push agent claude-multi--agents)
    (claude-multi--launch-agent agent)
    (message "Spawned agent: %s in %s" (claude-agent-name agent) directory)))

;;;###autoload
(defun claude-multi/spawn-agent-with-worktree ()
  "Spawn agent with git worktree isolation in a kitty tab.
Prompts for task description, directory, and branch name.
Creates a new git worktree for isolated parallel development.
Agent will cd into the worktree directory before launching Claude."
  (interactive)
  (unless claude-multi--session-start-time
    (claude-multi/start-session))
  ;; Ensure progress buffer exists
  (unless (and claude-multi--progress-buffer
               (buffer-live-p claude-multi--progress-buffer))
    (claude-multi/open-progress))
  (let* ((task (read-string "Task description: "))
         (directory (read-directory-name "Worktree directory: " nil nil t))
         (branch (read-string "Branch name: "))
         (agent (claude-multi--create-agent task)))
    ;; Set directory as worktree path
    (setf (claude-agent-worktree-path agent) (expand-file-name directory))
    ;; Set branch name to trigger worktree creation
    (when (and branch (not (string-empty-p branch)))
      (setf (claude-agent-branch-name agent) branch))
    (push agent claude-multi--agents)
    (claude-multi--launch-agent agent)
    (message "Spawned agent: %s in worktree %s (branch: %s)"
             (claude-agent-name agent) directory branch)))

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

;;; Drawer visibility core logic (testable, no org-mode dependencies)

(defun claude-multi--find-agent-headlines ()
  "Find all agent headline positions in the progress buffer.
Returns a list of buffer positions where agent headlines start."
  (when (and claude-multi--progress-buffer
             (buffer-live-p claude-multi--progress-buffer))
    (with-current-buffer claude-multi--progress-buffer
      (save-excursion
        (let ((positions nil))
          (goto-char (point-min))
          (while (re-search-forward "^\\*\\* " nil t)
            (push (line-beginning-position) positions))
          (nreverse positions))))))

(defun claude-multi--apply-to-headlines (action-fn)
  "Apply ACTION-FN to each agent headline in the progress buffer.
ACTION-FN is called with point at the beginning of each headline."
  (when (and claude-multi--progress-buffer
             (buffer-live-p claude-multi--progress-buffer))
    (with-current-buffer claude-multi--progress-buffer
      (save-excursion
        (dolist (pos (claude-multi--find-agent-headlines))
          (goto-char pos)
          (funcall action-fn))))))

;;; Display functions (org-mode specific, thin wrappers)

(defun claude-multi--org-show-subtree-at-point ()
  "Show org subtree at point. Wrapper for org-mode function."
  (when (fboundp 'org-show-subtree)
    (org-show-subtree)))

(defun claude-multi--org-hide-drawer-at-point ()
  "Hide org drawer at point. Wrapper for org-mode function."
  (when (fboundp 'org-hide-drawer-all)
    (org-hide-drawer-all)))

(defun claude-multi--org-cycle-at-point ()
  "Cycle org visibility at point. Wrapper for org-mode function."
  (when (fboundp 'org-cycle)
    (org-cycle)))

;;; User commands

;;;###autoload
(defun claude-multi/toggle-all-status-drawers ()
  "Toggle visibility of all agent STATUS drawers in the progress buffer."
  (interactive)
  (claude-multi--apply-to-headlines #'claude-multi--org-cycle-at-point))

;;;###autoload
(defun claude-multi/show-all-status-drawers ()
  "Show all agent STATUS drawers in the progress buffer."
  (interactive)
  (claude-multi--apply-to-headlines #'claude-multi--org-show-subtree-at-point))

;;;###autoload
(defun claude-multi/hide-all-status-drawers ()
  "Hide all agent STATUS drawers in the progress buffer."
  (interactive)
  (claude-multi--apply-to-headlines #'claude-multi--org-hide-drawer-at-point))

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
    ;; Stop all status file watches
    (claude-multi--stop-all-status-watches)
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

(defvar auto-revert-interval)  ; Defined in autorevert.el
(declare-function org-indent-mode "org-indent")  ; Defined in org-indent.el

(define-derived-mode claude-multi-progress-mode org-mode "Claude-Multi-Progress"
  "Major mode for Claude Multi-Agent progress tracking in org-mode format."
  (setq-local auto-revert-interval 0.5)
  (auto-revert-mode 1)
  (read-only-mode 1)
  ;; Enable org-mode features
  (when (fboundp 'org-indent-mode)
    (org-indent-mode 1))
  (visual-line-mode 1))

;; Keybindings
;; Note: Keybindings using map! macro should be set up in packages.el
;; to avoid byte-compilation issues with Doom Emacs-specific macros

(provide 'claude-multi-config)
;;; config.el ends here
