;; -*- lexical-binding: t; -*-
;;; autoload/notifications.el --- Input notification system for Claude Multi-Agent

;;; Commentary:
;; Detects when agents need user input and provides multiple notification methods
;; Includes popup notifications, mode-line indicators, and visual highlights

;;; Code:

(require 'alert nil t)

;; Forward declarations
(declare-function claude-agent-last-output "claude-multi-agents")
(declare-function claude-multi--get-status-icon "claude-multi-progress")
(declare-function claude-multi/send-input "config")

;; Forward declarations for variables defined in config.el
(defvar claude-multi-notification-methods)
(defvar claude-multi--progress-buffer)
(defvar claude-multi--agents)

;;; Notification state

(defvar claude-multi--waiting-agents nil
  "List of agents currently waiting for input.")

(defvar claude-multi--notification-timer nil
  "Timer for periodic notification checks.")

;;; Input detection

;;;###autoload
(defun claude-multi--detect-input-request (output)
  "Return non-nil if OUTPUT indicates the agent needs user input.
This is the main detection function that checks for various input patterns."
  (or (string-match-p "\\[.*Request interrupted.*\\]" output)
      (string-match-p "AskUserQuestion" output)
      (string-match-p "(y/n)\\?" output)
      (string-match-p "(yes/no)\\?" output)
      (string-match-p "Continue\\?" output)
      (string-match-p "Enter your choice:" output)
      (string-match-p "Please provide:" output)
      (string-match-p "Waiting for input" output)
      (string-match-p "Press .* to continue" output)
      (string-match-p "\\[\\?\\]" output)))

;;; Notification triggers

;;;###autoload
(defun claude-multi--notify-input-needed (agent)
  "Trigger all configured notification methods for AGENT needing input."
  ;; Add to waiting list if not already there
  (unless (memq agent claude-multi--waiting-agents)
    (push agent claude-multi--waiting-agents))

  ;; Trigger each configured notification method
  (dolist (method claude-multi-notification-methods)
    (pcase method
      ('popup (claude-multi--notify-popup agent))
      ('markdown (claude-multi--notify-markdown agent))
      ('modeline (claude-multi--notify-modeline))
      ('sound (claude-multi--notify-sound))))

  ;; Update progress buffer to highlight the request
  (claude-multi--highlight-input-requests))

;;;###autoload
(defun claude-multi--clear-notifications (agent)
  "Clear notifications for AGENT after input has been provided."
  (setq claude-multi--waiting-agents
        (delq agent claude-multi--waiting-agents))

  ;; Update mode line
  (claude-multi--notify-modeline)

  ;; Force mode line update
  (force-mode-line-update t))

;;; Popup notifications

(defun claude-multi--notify-popup (agent)
  "Show a popup notification for AGENT needing input."
  (let ((title "Claude Agent Needs Input")
        (message (format "%s is waiting for your response"
                        (claude-agent-name agent))))
    (cond
     ;; If alert package is available, use it
     ((featurep 'alert)
      (alert message
             :title title
             :category 'claude-multi
             :severity 'normal
             :persistent t))

     ;; Fallback to notifications.el (built-in on Linux/macOS)
     ((fboundp 'notifications-notify)
      (notifications-notify
       :title title
       :body message
       :urgency 'normal))

     ;; Final fallback to message
     (t
      (message "%s: %s" title message)))))

;;; Markdown/org-mode notifications

(defun claude-multi--notify-markdown (agent)
  "Add visual notification in the progress buffer for AGENT."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward
                 (format "^\\*\\* .* %s$" (regexp-quote (claude-agent-name agent)))
                 nil t)
            ;; Add a prominent notification line
            (end-of-line)
            (insert "\n\n")
            (insert "#+BEGIN_NOTICE\n")
            (insert (format "🔔 *INPUT REQUIRED* - %s is waiting for your response!\n"
                           (claude-agent-name agent)))
            (insert "Use =SPC c m i= to respond.\n")
            (insert "#+END_NOTICE\n")

            ;; Highlight the entire notice block
            (save-excursion
              (forward-line -4)
              (let ((start (point)))
                (forward-line 4)
                (add-text-properties start (point)
                                   '(face (:background "#3a3a00" :weight bold)))))))))))

;;; Mode-line notifications

(defvar claude-multi--modeline-string ""
  "String to display in the mode line.")

(defun claude-multi--notify-modeline ()
  "Update the mode line to show waiting agent count."
  (let ((count (length claude-multi--waiting-agents)))
    (setq claude-multi--modeline-string
          (if (> count 0)
              (propertize (format " [Claude:%d⏳]" count)
                         'face '(:foreground "yellow" :weight bold))
            "")))
  (force-mode-line-update t))

;;;###autoload
(defun claude-multi--setup-modeline ()
  "Add Claude Multi-Agent indicator to the mode line."
  (unless (member '(:eval claude-multi--modeline-string) mode-line-format)
    (setq-default mode-line-format
                  (append mode-line-format
                          '((:eval claude-multi--modeline-string))))))

;;; Sound notifications

(defun claude-multi--notify-sound ()
  "Play a sound alert (if configured and available)."
  (when (and (fboundp 'play-sound)
             (display-graphic-p))
    (condition-case nil
        (play-sound '(sound :file "/System/Library/Sounds/Glass.aiff"))
      (error nil))))

;;; Interactive response functions

;;;###autoload
(defun claude-multi/respond-to-agent ()
  "Interactively respond to an agent waiting for input.
This is a convenience wrapper around claude-multi/send-input."
  (interactive)
  (if claude-multi--waiting-agents
      (claude-multi/send-input)
    (message "No agents are waiting for input")))

;;;###autoload
(defun claude-multi/show-waiting-agents ()
  "Show a list of all agents waiting for input."
  (interactive)
  (if claude-multi--waiting-agents
      (let ((buf (get-buffer-create "*Claude Waiting Agents.org*")))
        (with-current-buffer buf
          (read-only-mode -1)
          (erase-buffer)
          (insert "#+TITLE: Agents Waiting for Input\n\n")
          (dolist (agent claude-multi--waiting-agents)
            (insert (format "* %s %s\n"
                           (claude-multi--get-status-icon (claude-agent-status agent))
                           (claude-agent-name agent)))
            (insert (format "- Status :: %s\n" (upcase (symbol-name (claude-agent-status agent)))))
            (insert (format "- Last output :: %s\n"
                           (or (claude-agent-last-output agent) "N/A")))
            (insert (format "- Waiting since :: %s\n\n"
                           (format-time-string "[%Y-%m-%d %a %H:%M:%S]"
                                             (claude-agent-created-at agent)))))
          (org-mode)
          (goto-char (point-min))
          (read-only-mode 1))
        (display-buffer buf))
    (message "No agents are waiting for input")))

;;; Notification cleanup

;;;###autoload
(defun claude-multi--cleanup-notifications ()
  "Clean up notification state when agents are killed or complete."
  ;; Remove any agents that no longer exist
  (setq claude-multi--waiting-agents
        (cl-remove-if-not
         (lambda (agent)
           (and (memq agent claude-multi--agents)
                (eq (claude-agent-status agent) 'waiting-input)))
         claude-multi--waiting-agents))

  ;; Update mode line
  (claude-multi--notify-modeline))

;;; Periodic checks

(defun claude-multi--start-notification-timer ()
  "Start a timer to periodically check for notifications.
This provides a backup in case output filtering misses something."
  (unless claude-multi--notification-timer
    (setq claude-multi--notification-timer
          (run-with-timer 5 5 #'claude-multi--check-waiting-agents))))

(defun claude-multi--stop-notification-timer ()
  "Stop the notification checking timer."
  (when claude-multi--notification-timer
    (cancel-timer claude-multi--notification-timer)
    (setq claude-multi--notification-timer nil)))

(defun claude-multi--check-waiting-agents ()
  "Check all agents for waiting status and trigger notifications if needed.
This is called periodically by the notification timer."
  (dolist (agent claude-multi--agents)
    (when (and (eq (claude-agent-status agent) 'waiting-input)
               (not (memq agent claude-multi--waiting-agents)))
      (claude-multi--notify-input-needed agent))))

;;; Input pattern customization

(defcustom claude-multi-input-patterns
  '("\\[.*Request interrupted.*\\]"
    "AskUserQuestion"
    "(y/n)\\?"
    "(yes/no)\\?"
    "Continue\\?"
    "Enter your choice:"
    "Please provide:"
    "Waiting for input"
    "Press .* to continue"
    "\\[\\?\\]")
  "Regular expressions that indicate an agent needs input."
  :type '(repeat regexp)
  :group 'claude-multi)

;;;###autoload
(defun claude-multi/add-input-pattern (pattern)
  "Add a new input detection PATTERN."
  (interactive "sInput pattern (regexp): ")
  (add-to-list 'claude-multi-input-patterns pattern)
  (message "Added input pattern: %s" pattern))

;;; Notification history

(defvar claude-multi--notification-history nil
  "History of all notifications sent.")

(defun claude-multi--record-notification (agent)
  "Record a notification for AGENT in the history."
  (push (list :agent (claude-agent-name agent)
              :time (current-time)
              :output (claude-agent-last-output agent))
        claude-multi--notification-history))

;;;###autoload
(defun claude-multi/show-notification-history ()
  "Display the notification history."
  (interactive)
  (if claude-multi--notification-history
      (let ((buf (get-buffer-create "*Claude Notification History.org*")))
        (with-current-buffer buf
          (read-only-mode -1)
          (erase-buffer)
          (insert "#+TITLE: Claude Multi-Agent Notification History\n\n")
          (dolist (entry (reverse claude-multi--notification-history))
            (insert (format "* %s\n"
                           (format-time-string "[%Y-%m-%d %a %H:%M:%S]"
                                             (plist-get entry :time))))
            (insert (format "- Agent :: %s\n" (plist-get entry :agent)))
            (insert (format "- Output :: %s\n\n" (plist-get entry :output))))
          (org-mode)
          (goto-char (point-min))
          (read-only-mode 1))
        (display-buffer buf))
    (message "No notification history")))

;;; Integration hooks

;;;###autoload
(defun claude-multi--setup-notifications ()
  "Setup notification system for the session."
  (claude-multi--setup-modeline)
  (claude-multi--start-notification-timer))

;;;###autoload
(defun claude-multi--teardown-notifications ()
  "Teardown notification system when session ends."
  (claude-multi--stop-notification-timer)
  (setq claude-multi--waiting-agents nil)
  (setq claude-multi--modeline-string "")
  (force-mode-line-update t))

(provide 'claude-multi-notifications)
;;; notifications.el ends here
