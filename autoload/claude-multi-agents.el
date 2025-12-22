;; -*- lexical-binding: t; -*-
;;; autoload/agents.el --- Agent management for Claude Multi-Agent

;;; Commentary:
;; Core agent lifecycle management including creation, monitoring, and cleanup

;;; Code:

(require 'cl-lib)

;; Forward declarations for variables defined in config.el
(defvar claude-multi--agent-id-counter)
(defvar claude-multi-agent-colors)
(defvar claude-multi-agent-color-schemes)
(defvar claude-multi-kitty-listen-address)
(defvar claude-multi--current-session-window-id)
(defvar claude-multi-agent-spawn-type)
(defvar claude-multi-claude-command)

;;; Agent structure

(cl-defstruct claude-agent
  "Structure representing a Claude agent instance."
  id                    ; Unique identifier (string)
  name                  ; Human-readable name (string)
  color                 ; Hex color code for visual distinction (string)
  kitty-window-id       ; Kitty window ID (string)
  kitty-tab-id          ; Kitty tab ID (string or nil)
  context-buffer        ; Emacs buffer for context/notes
  status-timer          ; Timer for polling kitty status
  worktree-path         ; Git worktree path (string or nil)
  branch-name           ; Git branch name for worktree (string or nil)
  working-directory     ; Working directory where agent was launched (string)
  status                ; Agent status symbol: running, waiting-input, completed, failed
  task-description      ; Original task description (string)
  created-at            ; Timestamp when created
  completed-at)         ; Timestamp when finished (or nil)

;;; Agent creation

;;;###autoload
(defun claude-multi--create-agent (task-description)
  "Create a new agent structure for TASK-DESCRIPTION.
Does not launch the agent process yet."
  (let* ((id (format "agent-%d" (cl-incf claude-multi--agent-id-counter)))
         (name (format "claude-%s" id))
         (color (claude-multi--assign-color id)))
    (make-claude-agent
     :id id
     :name name
     :color color
     :task-description task-description
     :working-directory default-directory
     :status 'pending
     :created-at (current-time))))

(defun claude-multi--assign-color (_id)
  "Assign a color to an agent based on its ID."
  (let ((colors claude-multi-agent-colors)
        (index (mod claude-multi--agent-id-counter (length claude-multi-agent-colors))))
    (nth index colors)))

(defun claude-multi--get-agent-color-scheme (agent)
  "Get the full color scheme for AGENT based on its agent ID counter.
Returns a plist with :name, :color, :text, :bg properties."
  (let* ((agent-num (string-to-number
                     (replace-regexp-in-string "agent-" "" (claude-agent-id agent))))
         (index (mod (1- agent-num) (length claude-multi-agent-color-schemes)))
         (scheme (nth index claude-multi-agent-color-schemes)))
    (cdr scheme)))

;;; Agent launching

;;;###autoload
(defun claude-multi--launch-agent (agent)
  "Launch AGENT in kitty terminal."
  (condition-case err
      (let* ((agent-id (claude-agent-id agent))
             (task-short (if (> (length (claude-agent-task-description agent)) 40)
                            (concat (substring (claude-agent-task-description agent) 0 37) "...")
                          (claude-agent-task-description agent)))
             (session-name (format "[%s] %s" agent-id task-short))
             (listen-addr (or claude-multi-kitty-listen-address
                             (getenv "KITTY_LISTEN_ON")
                             "unix:/tmp/kitty-claude")))

        ;; Calculate worktree path if branch name is specified
        ;; (Worktree will be created in kitty terminal, not here)
        (when (and (claude-multi--in-git-repo-p)
                   (claude-agent-branch-name agent))
          (let* ((repo-root (claude-multi--get-git-root))
                 (repo-name (file-name-nondirectory repo-root))
                 (branch-name (or (claude-agent-branch-name agent)
                                 (format "claude/%s" (claude-agent-id agent))))
                 (worktree-path (claude-multi--determine-worktree-path repo-root repo-name branch-name)))
            (setf (claude-agent-worktree-path agent) worktree-path)))

        (let* ((use-worktree-p (and (claude-multi--in-git-repo-p)
                                   (claude-agent-branch-name agent)))
               ;; Determine starting directory:
               ;; 1. If using worktree: repo root (will cd to worktree later)
               ;; 2. If worktree-path is set but no branch: use that directory directly
               ;; 3. Otherwise: use agent's working-directory
               (starting-dir (cond
                              (use-worktree-p (claude-multi--get-git-root))
                              ((claude-agent-worktree-path agent)
                               (claude-agent-worktree-path agent))
                              (t (or (claude-agent-working-directory agent)
                                     default-directory))))
               ;; Check if session window still exists (might have been closed manually)
               (session-window-exists
                (and claude-multi--current-session-window-id
                     (zerop (call-process-shell-command
                             (format "kitty @ --to=%s ls --match=id:%s 2>/dev/null"
                                     listen-addr
                                     claude-multi--current-session-window-id)
                             nil nil))))
               ;; If session window was closed, reset it
               (_ (unless session-window-exists
                    (setq claude-multi--current-session-window-id nil)))
               ;; Determine if this is the first agent (session window doesn't exist)
               (is-first-agent (null claude-multi--current-session-window-id))
               ;; For first agent: create OS window. For subsequent: create tab or split
               (window-type (if is-first-agent
                                "os-window"
                              (if (eq claude-multi-agent-spawn-type 'split)
                                  "window"
                                "tab")))
               ;; For subsequent agents, target the session window with --match
               (match-clause (if is-first-agent
                                 ""
                               (format " --match=id:%s" claude-multi--current-session-window-id)))
               ;; Launch kitty and get window ID
               (launch-output
                (shell-command-to-string
                 (format "kitty @ --to=%s launch --type=%s%s --cwd=%s --title='%s'"
                        listen-addr
                        window-type
                        match-clause
                        (shell-quote-argument starting-dir)
                        session-name)))
               (window-id (string-trim launch-output)))

          ;; Store session window ID if this is the first agent
          (when is-first-agent
            (setq claude-multi--current-session-window-id window-id))

          ;; Store kitty session info
          (setf (claude-agent-kitty-window-id agent) window-id)
          (setf (claude-agent-context-buffer agent)
                (generate-new-buffer (format "*claude-context-%s*" agent-id)))
          (setf (claude-agent-status agent) 'running)

          ;; Set tab and terminal colors using agent's color scheme
          (let* ((color-scheme (claude-multi--get-agent-color-scheme agent))
                 (accent-color (plist-get color-scheme :color))
                 (text-color (plist-get color-scheme :text))
                 (bg-color (plist-get color-scheme :bg))
                 (wid window-id)
                 (addr listen-addr))
            (run-with-timer 0.2 nil
                           `(lambda ()
                              ;; Set tab color with white text
                              (call-process-shell-command
                               (format "kitty @ --to=%s set-tab-color --match=id:%s active_bg=\"%s\" active_fg=\"#FFFFFF\""
                                      ,addr ,wid ,accent-color)
                               nil 0)
                              ;; Set terminal colors using full color scheme (only for this window)
                              (call-process-shell-command
                               (format "kitty @ --to=%s set-colors --match=id:%s foreground=\"%s\" background=\"%s\" cursor=\"%s\" cursor_text_color=\"#000000\" active_border_color=\"%s\" selection_background=\"%s\" selection_foreground=\"#000000\""
                                      ,addr ,wid
                                      ,text-color    ; Terminal text color from scheme
                                      ,bg-color      ; Terminal background from scheme
                                      ,accent-color  ; Cursor in agent accent color
                                      ,accent-color  ; Border in agent accent color
                                      ,accent-color) ; Selection in agent accent color
                               nil 0))))

          ;; Initialize context buffer with task info
          (with-current-buffer (claude-agent-context-buffer agent)
            (insert (format "# Agent: %s\n" (claude-agent-name agent)))
            (insert (format "Task: %s\n" (claude-agent-task-description agent)))
            (insert (format "Worktree: %s\n" (or (claude-agent-worktree-path agent) "N/A")))
            (insert (format "Started: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
            (org-mode))

          ;; Start lightweight status monitoring
          (claude-multi--setup-kitty-status-monitor agent)

          ;; Send initial command after delay
          (let ((use-wt use-worktree-p)  ; Capture for lambda closure
                (agt agent)  ; Capture agent reference
                ;; Capture git context before timer (default-directory may change)
                (repo-root (when use-worktree-p (claude-multi--get-git-root)))
                (worktree-path (claude-agent-worktree-path agent))
                (branch-name (when use-worktree-p
                              (or (claude-agent-branch-name agent)
                                  (format "claude/%s" (claude-agent-id agent))))))
            (run-with-timer 0.5 nil
                           (lambda ()
                             (if use-wt
                                 ;; Build and send worktree creation command chain
                                 (let ((command (claude-multi--build-worktree-command
                                                agt repo-root worktree-path branch-name)))
                                  (claude-multi--send-to-kitty agt command))
                               ;; No worktree - just start Claude normally
                               (claude-multi--send-to-kitty agt claude-multi-claude-command)))))

          ;; Update progress buffer
          (claude-multi--add-agent-section agent)

          ;; Update session statistics (agent counts)
          (claude-multi--update-session-stats)

          ;; Start watching agent's status.md file
          (claude-multi--watch-agent-status-file agent)))

    (error
     (setf (claude-agent-status agent) 'failed)
     (message "Failed to launch agent %s: %s"
             (claude-agent-name agent)
             (error-message-string err)))))

(defun claude-multi--send-to-kitty (agent command)
  "Send initial COMMAND to AGENT's kitty window to start Claude."
  (let* ((window-id (claude-agent-kitty-window-id agent))
         (listen-addr (or claude-multi-kitty-listen-address
                         (getenv "KITTY_LISTEN_ON")
                         "unix:/tmp/kitty-claude"))
         ;; Escape single quotes for shell: ' becomes '\''
         (escaped-cmd (replace-regexp-in-string "'" "'\\\\''" command)))
    (call-process-shell-command
     (format "kitty @ --to=%s send-text --match=id:%s '%s\n'"
            listen-addr window-id escaped-cmd)
     nil 0)))

;;; Agent monitoring

(defun claude-multi--setup-kitty-status-monitor (agent)
  "Setup lightweight status monitoring for AGENT's kitty window."
  (let ((timer (run-with-timer
                5.0  ; Check every 5 seconds
                5.0
                (lambda () (claude-multi--check-kitty-status agent)))))
    (setf (claude-agent-status-timer agent) timer)))

(defun claude-multi--check-kitty-status (agent)
  "Check basic status of AGENT's kitty window."
  (unless (claude-multi--kitty-is-alive agent)
    ;; Window closed - mark agent as completed
    (when (cl-member (claude-agent-status agent) '(running waiting-input))
      (claude-multi--handle-agent-completion agent))))

(defun claude-multi--kitty-is-alive (agent)
  "Check if AGENT's kitty window still exists."
  (let* ((window-id (claude-agent-kitty-window-id agent))
         (listen-addr (or claude-multi-kitty-listen-address
                         (getenv "KITTY_LISTEN_ON")
                         "unix:/tmp/kitty-claude")))
    (= 0 (call-process-shell-command
          (format "kitty @ --to=%s ls --match=id:%s 2>/dev/null"
                 listen-addr window-id)
          nil nil))))

;;;###autoload
(defun claude-multi--handle-agent-completion (agent)
  "Handle completion of AGENT."
  (setf (claude-agent-status agent) 'completed)
  (setf (claude-agent-completed-at agent) (current-time))

  ;; Update progress buffer
  (claude-multi--update-agent-status agent)

  ;; Handle buffer cleanup
  (claude-multi--handle-buffer-cleanup agent))

(defun claude-multi--handle-buffer-cleanup (agent)
  "Handle cleanup of AGENT's kitty window based on configuration."
  (pcase claude-multi-buffer-cleanup
    ('keep-all nil)  ; User manually closes kitty windows
    ('auto-close-success
     (when (eq (claude-agent-status agent) 'completed)
       ;; Leave kitty window for user to review
       nil))
    ('ask
     (when (y-or-n-p (format "Close kitty window for %s? " (claude-agent-name agent)))
       (claude-multi--kill-agent agent)))))

;;; Agent interaction

;; Note: claude-multi--send-input-to-agent is no longer needed
;; User interacts directly in kitty terminal

;;;###autoload
(defun claude-multi--kill-agent (agent)
  "Kill AGENT and cleanup all resources (worktree, status watch, progress buffer)."
  (when agent
    ;; Mark agent as failed/killed and update status in progress buffer
    (setf (claude-agent-status agent) 'failed)
    (setf (claude-agent-completed-at agent) (current-time))
    (claude-multi--update-agent-status agent)

    ;; Cancel status timer
    (when-let ((timer (claude-agent-status-timer agent)))
      (cancel-timer timer))

    ;; Stop watching agent's status file
    (claude-multi--stop-watching-agent-status agent)

    ;; Close kitty window
    (let* ((window-id (claude-agent-kitty-window-id agent))
           (listen-addr (or claude-multi-kitty-listen-address
                           (getenv "KITTY_LISTEN_ON")
                           "unix:/tmp/kitty-claude")))
      (when window-id
        (call-process-shell-command
         (format "kitty @ --to=%s close-window --match=id:%s 2>/dev/null"
                listen-addr window-id)
         nil 0)))

    ;; Cleanup context buffer
    (when-let ((buf (claude-agent-context-buffer agent)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))

    ;; Remove from agents list
    (setq claude-multi--agents
          (delq agent claude-multi--agents))

    ;; If this was the last agent, reset session window tracking
    (when (null claude-multi--agents)
      (setq claude-multi--current-session-window-id nil)
      (setq claude-multi--current-session-tab-ids nil))

    ;; Update session stats after removal
    (claude-multi--update-session-stats)))

;;; Agent listing and selection

;;;###autoload
(defun claude-multi--list-agents ()
  "Return a list of all active agents."
  claude-multi--agents)

;;;###autoload
(defun claude-multi--select-agent (agents prompt)
  "Prompt user to select an agent from AGENTS list with PROMPT."
  (when agents
    (let* ((choices (mapcar (lambda (a)
                             (cons (format "%s [%s]"
                                         (claude-agent-name a)
                                         (claude-agent-status a))
                                   a))
                           agents))
           (choice (completing-read prompt choices nil t)))
      (cdr (assoc choice choices)))))

;;;###autoload
(defun claude-multi--get-agent-by-id (id)
  "Get agent by its ID."
  (cl-find-if (lambda (a) (string= (claude-agent-id a) id))
             claude-multi--agents))

;;; Helper utilities

(defun claude-multi--format-agent-status (agent)
  "Format the status of AGENT for display."
  (pcase (claude-agent-status agent)
    ('running "🟢 RUNNING")
    ('waiting-input "🟡 WAITING FOR INPUT")
    ('completed "🔵 COMPLETED")
    ('failed "🔴 FAILED")
    ('pending "⚪ PENDING")
    (_ "❓ UNKNOWN")))

(defun claude-multi--format-duration (start-time &optional end-time)
  "Format duration between START-TIME and END-TIME (or current time).
Returns a human-readable string with visual warnings for long-running agents."
  (let* ((end (or end-time (current-time)))
         (duration (time-subtract end start-time))
         (seconds (floor (time-to-seconds duration)))
         (minutes (/ seconds 60))
         (hours (/ seconds 3600))
         (days (/ seconds 86400))
         (weeks (/ seconds 604800)))
    (cond
     ;; Less than 1 minute
     ((< seconds 60)
      (format "%ds" seconds))
     ;; Less than 1 hour - show minutes and seconds
     ((< seconds 3600)
      (let ((m (/ seconds 60))
            (s (mod seconds 60)))
        (if (zerop s)
            (format "%dm" m)
          (format "%dm %ds" m s))))
     ;; Less than 24 hours - show hours and minutes
     ((< seconds 86400)
      (let* ((h (/ seconds 3600))
             (m (/ (mod seconds 3600) 60))
             (warning (if (>= hours 4) "⚠️ " "")))
        (if (zerop m)
            (format "%s%dh" warning h)
          (format "%s%dh %dm" warning h m))))
     ;; Less than 7 days - show days and hours with red flag
     ((< seconds 604800)
      (let* ((d (/ seconds 86400))
             (h (/ (mod seconds 86400) 3600)))
        (if (zerop h)
            (format "🔴 %dd" d)
          (format "🔴 %dd %dh" d h))))
     ;; 7 days or more - show weeks and days
     (t
      (let* ((w (/ seconds 604800))
             (d (/ (mod seconds 604800) 86400)))
        (if (zerop d)
            (format "🔴 %dw" w)
          (format "🔴 %dw %dd" w d)))))))

(provide 'claude-multi-agents)
;;; agents.el ends here
