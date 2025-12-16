;; -*- lexical-binding: t; -*-
;;; autoload/agents.el --- Agent management for Claude Multi-Agent

;;; Commentary:
;; Core agent lifecycle management including creation, monitoring, and cleanup

;;; Code:

(require 'cl-lib)
(require 'vterm)

;;; Agent structure

(cl-defstruct claude-agent
  "Structure representing a Claude agent instance."
  id                    ; Unique identifier (string)
  name                  ; Human-readable name (string)
  color                 ; Hex color code for visual distinction (string)
  process               ; vterm process
  buffer                ; vterm buffer
  worktree-path         ; Git worktree path (string or nil)
  status                ; Agent status symbol: running, waiting-input, completed, failed
  last-output           ; Last output line (string)
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

;;; Agent launching

;;;###autoload
(defun claude-multi--launch-agent (agent)
  "Launch the AGENT by creating a worktree and starting claude-code in vterm."
  (condition-case err
      (progn
        ;; Create worktree if in a git repo
        (when (claude-multi--in-git-repo-p)
          (let ((worktree-path (claude-multi--create-worktree agent)))
            (setf (claude-agent-worktree-path agent) worktree-path)))

        ;; Create vterm buffer and start process
        (let* ((vterm-buffer-name (format "*vterm-%s*" (claude-agent-name agent)))
               (default-directory (or (claude-agent-worktree-path agent) default-directory))
               (vterm-buf (save-window-excursion
                            (vterm t))))
          (setf (claude-agent-buffer agent) vterm-buf)
          (setf (claude-agent-status agent) 'running)

          ;; Setup output monitoring
          (with-current-buffer vterm-buf
            (rename-buffer vterm-buffer-name t)
            ;; Set buffer-local variable to track which agent this is
            (setq-local claude-multi--current-agent agent)
            ;; Setup process output filter
            (claude-multi--setup-vterm-output-monitor agent))

          ;; Start claude-code (after a short delay to ensure vterm is ready)
          (run-with-timer 0.5 nil
                         (lambda ()
                           (claude-multi--send-to-vterm vterm-buf
                                                       (format "%s \"%s\""
                                                              claude-multi-claude-command
                                                              (claude-agent-task-description agent)))))

          ;; Update progress buffer
          (claude-multi--add-agent-section agent)))

    (error
     (setf (claude-agent-status agent) 'failed)
     (message "Failed to launch agent %s: %s"
             (claude-agent-name agent)
             (error-message-string err)))))

(defun claude-multi--send-to-vterm (buffer command)
  "Send COMMAND to vterm BUFFER."
  (with-current-buffer buffer
    (vterm-send-string command)
    (vterm-send-return)))

;;; Agent monitoring

(defun claude-multi--setup-vterm-output-monitor (agent)
  "Setup output monitoring for AGENT's vterm buffer using process filter."
  (let* ((buffer (claude-agent-buffer agent))
         (process (get-buffer-process buffer)))
    (when process
      ;; Store the original filter if any
      (let ((original-filter (process-filter process)))
        (setf (claude-agent-process agent) process)
        ;; Set up our custom filter
        (set-process-filter
         process
         (lambda (proc string)
           ;; Call original filter first if it exists
           (when (and original-filter (functionp original-filter))
             (funcall original-filter proc string))
           ;; Process output for our agent monitoring
           (claude-multi--process-vterm-output agent string)))))))

(defun claude-multi--process-vterm-output (agent output)
  "Process OUTPUT from AGENT's vterm buffer."
  (when agent
    ;; Update last output
    (setf (claude-agent-last-output agent) output)

    ;; Check for input requests
    (when (claude-multi--detect-input-request output)
      (setf (claude-agent-status agent) 'waiting-input)
      (claude-multi--notify-input-needed agent))

    ;; Check for completion
    (when (claude-multi--detect-completion output)
      (claude-multi--handle-agent-completion agent))

    ;; Check for errors
    (when (claude-multi--detect-error output)
      (setf (claude-agent-status agent) 'failed))

    ;; Update progress buffer
    (claude-multi--append-agent-output agent output)))

(defun claude-multi--detect-completion (output)
  "Return non-nil if OUTPUT indicates the agent has completed successfully."
  (or (string-match-p "Task completed successfully" output)
      (string-match-p "All tasks completed" output)
      (string-match-p "\\$ $" output))) ; Back at shell prompt

(defun claude-multi--detect-error (output)
  "Return non-nil if OUTPUT indicates an error occurred."
  (or (string-match-p "Error:" output)
      (string-match-p "Failed:" output)
      (string-match-p "Exception:" output)))

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
  "Handle cleanup of AGENT's vterm buffer based on configuration."
  (pcase claude-multi-buffer-cleanup
    ('keep-all nil) ; Do nothing
    ('auto-close-success
     (when (eq (claude-agent-status agent) 'completed)
       (kill-buffer (claude-agent-buffer agent))))
    ('ask
     (when (y-or-n-p (format "Close buffer for %s? " (claude-agent-name agent)))
       (kill-buffer (claude-agent-buffer agent))))))

;;; Agent interaction

;;;###autoload
(defun claude-multi--send-input-to-agent (agent input)
  "Send INPUT to AGENT."
  (when (buffer-live-p (claude-agent-buffer agent))
    (claude-multi--send-to-vterm (claude-agent-buffer agent) input)
    (setf (claude-agent-status agent) 'running)
    (claude-multi--clear-notifications agent)
    (claude-multi--update-agent-status agent)))

;;;###autoload
(defun claude-multi--kill-agent (agent)
  "Kill AGENT and cleanup all resources."
  (when agent
    ;; Kill the process
    (when (and (claude-agent-buffer agent)
               (buffer-live-p (claude-agent-buffer agent)))
      (let ((proc (get-buffer-process (claude-agent-buffer agent))))
        (when proc
          (delete-process proc)))
      (kill-buffer (claude-agent-buffer agent)))

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
