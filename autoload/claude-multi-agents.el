;; -*- lexical-binding: t; -*-
;;; autoload/agents.el --- Agent management for Claude Multi-Agent

;;; Commentary:
;; Core agent lifecycle management including creation, monitoring, and cleanup

;;; Code:

(require 'cl-lib)

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
     :status 'pending
     :created-at (current-time))))

(defun claude-multi--assign-color (id)
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

        ;; Create worktree only if:
        ;; 1. We're in a git repo AND
        ;; 2. A branch name is specified (meaning user wants a worktree)
        (when (and (claude-multi--in-git-repo-p)
                   (claude-agent-branch-name agent))
          (let ((worktree-path (claude-multi--create-worktree agent)))
            (setf (claude-agent-worktree-path agent) worktree-path)))

        (let* ((worktree-path (or (claude-agent-worktree-path agent) default-directory))
               ;; Launch kitty and get window ID
               (window-type (symbol-name (or claude-multi-kitty-window-type 'os-window)))
               (launch-output
                (shell-command-to-string
                 (format "kitty @ --to=%s launch --type=%s --cwd=%s --title='%s'"
                        listen-addr
                        window-type
                        (shell-quote-argument worktree-path)
                        session-name)))
               (window-id (string-trim launch-output)))

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
            (insert (format "Worktree: %s\n" (or worktree-path "N/A")))
            (insert (format "Started: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
            (org-mode))

          ;; Start lightweight status monitoring
          (claude-multi--setup-kitty-status-monitor agent)

          ;; Send initial command after delay
          (run-with-timer 0.5 nil
                         (lambda ()
                           (claude-multi--send-to-kitty
                            agent
                            ;; Just start claude26 without arguments
                            claude-multi-claude-command)))

          ;; Update progress buffer
          (claude-multi--add-agent-section agent)))

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
         (escaped-cmd (replace-regexp-in-string "'" "'\\''" command)))
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

  ;; Cleanup worktree if configured
  (when (and claude-multi-auto-cleanup
             (claude-agent-worktree-path agent))
    (claude-multi--delete-worktree agent))

  ;; Handle buffer cleanup
  (claude-multi--handle-buffer-cleanup agent))

(defun claude-multi--handle-buffer-cleanup (agent)
  "Handle cleanup of AGENT's kitty window based on configuration."
  (pcase claude-multi-buffer-cleanup
    ('keep-all nil)  ; User manually closes kitty windows
    ('auto-close-success
     (when (eq (claude-agent-status agent) 'completed)
       ;; Only cleanup worktree, leave kitty window for user to review
       (when (claude-agent-worktree-path agent)
         (claude-multi--delete-worktree agent))))
    ('ask
     (when (y-or-n-p (format "Close kitty window for %s? " (claude-agent-name agent)))
       (claude-multi--kill-agent agent)))))

;;; Agent interaction

;; Note: claude-multi--send-input-to-agent is no longer needed
;; User interacts directly in kitty terminal

;;;###autoload
(defun claude-multi--kill-agent (agent)
  "Kill AGENT and cleanup all resources."
  (when agent
    ;; Cancel status timer
    (when-let ((timer (claude-agent-status-timer agent)))
      (cancel-timer timer))

    ;; Close kitty window
    (let* ((window-id (claude-agent-kitty-window-id agent))
           (listen-addr (or claude-multi-kitty-listen-address
                           (getenv "KITTY_LISTEN_ON")
                           "unix:/tmp/kitty-claude")))
      (call-process-shell-command
       (format "kitty @ --to=%s close-window --match=id:%s"
              listen-addr window-id)
       nil 0))

    ;; Cleanup context buffer
    (when-let ((buf (claude-agent-context-buffer agent)))
      (kill-buffer buf))

    ;; Cleanup worktree
    (when (claude-agent-worktree-path agent)
      (claude-multi--delete-worktree agent))

    ;; Remove from agents list
    (setq claude-multi--agents
          (delq agent claude-multi--agents))

    ;; Update progress buffer
    (claude-multi--remove-agent-section agent)))

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
  "Format duration between START-TIME and END-TIME (or current time)."
  (let* ((end (or end-time (current-time)))
         (duration (time-subtract end start-time))
         (seconds (time-to-seconds duration)))
    (cond
     ((< seconds 60) (format "%.0fs" seconds))
     ((< seconds 3600) (format "%.1fm" (/ seconds 60.0)))
     (t (format "%.1fh" (/ seconds 3600.0))))))

(provide 'claude-multi-agents)
;;; agents.el ends here
