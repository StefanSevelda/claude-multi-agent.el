;; -*- lexical-binding: t; -*-
;;; tools/claude-multi/config.el

;;; Commentary:
;; Configuration for Claude Multi-Agent Plugin
;; Manages multiple claude-code instances in parallel with worktree isolation
;; All agent orchestration is handled by the cma Go CLI binary.

;;; Code:

(eval-and-compile
  (require 'subr-x))  ; For string-empty-p, string-trim

;;; Define defgroup and all variables BEFORE loading modules
(defgroup claude-multi nil
  "Manage multiple Claude Code agents in parallel."
  :group 'tools
  :prefix "claude-multi-")

;;; Customization variables
(defcustom claude-multi-default-model "sonnet"
  "Default Claude model for new agents.
Model aliases: haiku (fast), sonnet (balanced, default), opus (advanced),
opusplan (hybrid opus planning + sonnet execution)."
  :type 'string
  :group 'claude-multi)

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

(defcustom claude-multi-session-directory
  (expand-file-name "claude-multi-sessions" user-emacs-directory)
  "Directory for storing session files."
  :type 'directory
  :group 'claude-multi)

(defcustom claude-multi-session-autosave-interval 300
  "Seconds between automatic session saves (0 to disable)."
  :type 'integer
  :group 'claude-multi)

(defcustom claude-multi-session-retention-days 30
  "Number of days to keep old sessions (0 for unlimited)."
  :type 'integer
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

(defcustom claude-multi-progress-buffer-name "*Claude Multi-Agent Progress*"
  "Name of the central progress tracking buffer (table view)."
  :type 'string
  :group 'claude-multi)

;; Forward declarations for cma modules
(declare-function cma/spawn-agent "cma-commands")
(declare-function cma/spawn-agent-with-worktree "cma-commands")
(declare-function cma/kill-agent "cma-commands")
(declare-function cma/kill-all-agents "cma-commands")
(declare-function cma/focus-agent "cma-commands")
(declare-function cma/rename-agent "cma-commands")
(declare-function cma/save-session "cma-commands")
(declare-function cma/restore-session "cma-commands")
(declare-function cma/list-sessions "cma-commands")
(declare-function cma/delete-session "cma-commands")
(declare-function cma/list-worktrees "cma-commands")
(declare-function cma/worktree-create "cma-commands")
(declare-function cma/worktree-remove "cma-commands")
(declare-function cma-modeline--start "cma-modeline")

;; Global variables (surviving)
(defvar claude-multi--progress-buffer nil
  "Buffer for displaying agent progress.")

;;; Load autoload modules
(let ((autoload-dir (expand-file-name "autoload"
                                      (or (and load-file-name
                                               (file-name-directory (file-truename load-file-name)))
                                          (and (boundp 'byte-compile-current-file)
                                               byte-compile-current-file
                                               (file-name-directory (file-truename byte-compile-current-file)))
                                          default-directory))))
  (add-to-list 'load-path autoload-dir)

  ;; CMA backend modules (required)
  (condition-case err
      (progn
        (load (expand-file-name "cma-core.el" autoload-dir) nil 'nomessage)
        (load (expand-file-name "cma-commands.el" autoload-dir) nil 'nomessage)
        (load (expand-file-name "cma-table.el" autoload-dir) nil 'nomessage)
        (load (expand-file-name "cma-modeline.el" autoload-dir) nil 'nomessage))
    (error (message ">>> CLAUDE-MULTI: Error loading cma modules: %S" err)))

  ;; Surviving modules (ediff, mcp, layout)
  (condition-case err
      (progn
        (load (expand-file-name "claude-multi-mcp.el" autoload-dir) nil t)
        (load (expand-file-name "claude-multi-ediff.el" autoload-dir) nil t)
        (load (expand-file-name "claude-multi-layout.el" autoload-dir) nil t))
    (error (message ">>> CLAUDE-MULTI: Error loading surviving modules: %S" err)))

  ;; Startup guard
  (unless (executable-find "cma")
    (warn "CLAUDE-MULTI: cma binary not found on PATH. Install it to use agent orchestration."))

  ;; Start cma modeline
  (when (fboundp 'cma-modeline--start)
    (cma-modeline--start)))

;; Interactive commands — thin wrappers calling cma-commands.el

;;;###autoload
(defun claude-multi/spawn-agent ()
  "Spawn a new Claude agent in a kitty tab.
Prompts for task description and working directory."
  (interactive)
  (cma/spawn-agent))

;;;###autoload
(defun claude-multi/spawn-agent-with-worktree ()
  "Spawn agent with git worktree isolation in a kitty tab.
Prompts for task description, directory, and branch name."
  (interactive)
  (cma/spawn-agent-with-worktree))

;;;###autoload
(defun claude-multi/open-progress ()
  "Open the central progress tracking buffer (table view)."
  (interactive)
  (let ((buf (get-buffer-create claude-multi-progress-buffer-name)))
    (setq claude-multi--progress-buffer buf)
    (with-current-buffer buf
      (unless (derived-mode-p 'cma-table-mode)
        (cma-table-mode))
      (tabulated-list-revert)
      (unless (get-buffer-window buf)
        (display-buffer buf)))))

;;;###autoload
(defun claude-multi/focus-agent ()
  "Switch focus to a specific agent's kitty window."
  (interactive)
  (cma/focus-agent))

;;;###autoload
(defun claude-multi/kill-agent ()
  "Kill a specific agent and cleanup resources."
  (interactive)
  (cma/kill-agent))

;;;###autoload
(defun claude-multi/kill-all-agents ()
  "Kill all agents and cleanup all worktrees."
  (interactive)
  (cma/kill-all-agents))

;;;###autoload
(defun claude-multi/save-session ()
  "Save the current agent session."
  (interactive)
  (cma/save-session))

;;;###autoload
(defun claude-multi/restore-session ()
  "Restore a saved agent session."
  (interactive)
  (cma/restore-session))

;;;###autoload
(defun claude-multi/list-sessions ()
  "List saved agent sessions."
  (interactive)
  (cma/list-sessions))

;;;###autoload
(defun claude-multi/delete-session ()
  "Delete a saved agent session."
  (interactive)
  (cma/delete-session))

;;;###autoload
(defun claude-multi/list-worktrees ()
  "List git worktrees managed by agents."
  (interactive)
  (cma/list-worktrees))

;; ──────────────────────────────────────────────────────────────────────────────
;; Progress Buffer Pinning (side-window)
;; ──────────────────────────────────────────────────────────────────────────────

;; Allow 1 bottom side-window slot for the progress buffer
(setq window-sides-slots '(nil nil 1 nil))

;; Pin progress buffer as a dedicated bottom side-window that survives delete-other-windows
(add-to-list 'display-buffer-alist
  `(,(regexp-quote (or (bound-and-true-p claude-multi-progress-buffer-name)
                       "*Claude Multi-Agent Progress*"))
    (display-buffer-in-side-window)
    (side . bottom) (slot . 0) (window-height . 0.25)
    (preserve-size . (nil . t)) (dedicated . t)))

;; ──────────────────────────────────────────────────────────────────────────────
;; Keybindings
;; ──────────────────────────────────────────────────────────────────────────────

;; Following https://rameezkhan.me/posts/2020/2020-07-03--adding-keybindings-to-doom-emacs/
;; Just use map! directly - Doom handles the timing
(map! :leader
      (:prefix-map ("c" . "code")
       (:prefix ("m" . "claude-multi")
        :desc "Spawn agent"             "a" #'claude-multi/spawn-agent
        :desc "Spawn with worktree"     "w" #'claude-multi/spawn-agent-with-worktree
        :desc "Open progress"           "p" #'claude-multi/open-progress
        :desc "Focus agent"             "f" #'claude-multi/focus-agent
        :desc "Kill agent"              "k" #'claude-multi/kill-agent
        :desc "Kill all"                "K" #'claude-multi/kill-all-agents
        :desc "List worktrees"          "l" #'claude-multi/list-worktrees
        (:prefix ("W" . "worktrees")
         :desc "Create worktree"         "c" #'cma/worktree-create
         :desc "Remove worktree"         "r" #'cma/worktree-remove
         :desc "List worktrees"          "l" #'claude-multi/list-worktrees)
        :desc "Save session"            "S" #'claude-multi/save-session
        :desc "Restore session"         "R" #'claude-multi/restore-session
        :desc "List sessions"           "L" #'claude-multi/list-sessions
        :desc "Delete session"          "D" #'claude-multi/delete-session
        (:prefix ("r" . "review")
         :desc "Review agent changes"   "r" #'claude-multi/review-agent-changes
         :desc "Accept current diff"    "a" #'claude-multi/accept-current-diff
         :desc "Reject current diff"    "x" #'claude-multi/reject-current-diff
         :desc "Next diff file"         "n" #'claude-multi/next-diff-file)
        :desc "Triage (agenda + agent)" "t" #'claude-multi-layout/start-agenda
        (:prefix ("y" . "layout")
         :desc "Agenda layout"           "a" #'claude-multi-layout/start-agenda
         :desc "Focus layout"            "f" #'claude-multi-layout/focus
         :desc "Project layout"          "p" #'claude-multi-layout/project
         :desc "Toggle status/diff"      "d" #'claude-multi-layout/project-toggle-diff
         :desc "Exit layout"             "e" #'claude-multi-layout/exit
         :desc "Switch layout"           "l" #'claude-multi-layout/switch
         :desc "Revert files"            "r" #'claude-multi-layout/revert-files
         :desc "Reset Kitty layout"     "R" #'claude-multi-layout/reset-kitty))))

;; Quick g d toggle for project layout in magit buffers
(with-eval-after-load 'magit
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal magit-status-mode-map
      (kbd "g d") #'claude-multi-layout--project-toggle-diff-if-active)
    (evil-define-key 'normal magit-diff-mode-map
      (kbd "g d") #'claude-multi-layout--project-toggle-diff-if-active)))

;; ──────────────────────────────────────────────────────────────────────────────
;; Auto-show progress buffer at startup
;; ──────────────────────────────────────────────────────────────────────────────

(defun claude-multi--auto-show-progress ()
  "Create and display the progress buffer at startup."
  (let ((buf (get-buffer-create
              (or (bound-and-true-p claude-multi-progress-buffer-name)
                  "*Claude Multi-Agent Progress*"))))
    (display-buffer buf)))

(if (boundp 'doom-after-init-hook)
    (add-hook 'doom-after-init-hook #'claude-multi--auto-show-progress)
  (add-hook 'emacs-startup-hook #'claude-multi--auto-show-progress))

(provide 'claude-multi-config)
;;; config.el ends here
