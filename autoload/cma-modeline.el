;; -*- lexical-binding: t; -*-
;;; autoload/cma-modeline.el --- Modeline indicator for waiting agents

;;; Commentary:
;; Polls `cma list --json` on a timer to update the modeline when
;; any agent has waiting_for_input: true.  Detects state transitions
;; to fire desktop alerts via the `alert' package.

;;; Code:

(require 'cma-core)
(require 'alert nil t)

(defvar cma-modeline--timer nil
  "Timer for modeline refresh.")

(defvar cma-modeline--text ""
  "Current modeline text for waiting agents.")

(defvar cma-modeline--interval 5
  "Seconds between modeline checks.")

(defvar cma-modeline--previous-waiting nil
  "List of session-ids that were waiting on the last poll.
Used for detecting state transitions to avoid duplicate alerts.")

;;;###autoload
(defun cma-modeline--start ()
  "Start modeline polling timer."
  (cma-modeline--stop)
  (setq cma-modeline--timer
        (run-with-timer 2 cma-modeline--interval #'cma-modeline--update)))

;;;###autoload
(defun cma-modeline--stop ()
  "Stop modeline polling timer."
  (when cma-modeline--timer
    (cancel-timer cma-modeline--timer)
    (setq cma-modeline--timer nil)))

(defun cma-modeline--update ()
  "Check for waiting agents and update modeline.
Detects newly-waiting agents and fires desktop alerts."
  (cma--call-async
   (lambda (agents)
     (let* ((waiting (when agents
                       (seq-filter (lambda (a) (eq (alist-get 'waiting_for_input a) t))
                                   agents)))
            (waiting-ids (mapcar (lambda (a) (alist-get 'session_id a)) waiting))
            (newly-waiting (cl-set-difference waiting-ids
                                              cma-modeline--previous-waiting
                                              :test #'string=)))
       ;; Fire alerts for newly-waiting agents
       (when (and newly-waiting
                  (boundp 'claude-multi-notification-methods)
                  (memq 'popup claude-multi-notification-methods))
         (dolist (sid newly-waiting)
           (let ((agent (cl-find-if (lambda (a) (string= (alist-get 'session_id a) sid))
                                    waiting)))
             (when agent
               (cma-modeline--alert agent)))))
       ;; Update tracked state
       (setq cma-modeline--previous-waiting waiting-ids)
       ;; Update modeline text
       (setq cma-modeline--text
             (if waiting
                 (format " [CMA:%d waiting]" (length waiting))
               ""))
       (force-mode-line-update t)))))

(defun cma-modeline--alert (agent)
  "Send desktop alert for AGENT that just started waiting.
Uses the `alert' package if available.  Message varies by notification type."
  (when (fboundp 'alert)
    (let* ((name (or (alist-get 'name agent) (alist-get 'session_id agent) "Agent"))
           (ntype (alist-get 'notification_type agent))
           (message (pcase ntype
                      ("idle_prompt" (format "Agent %s finished work" name))
                      ("permission_prompt" (format "Agent %s needs tool approval" name))
                      ("elicitation_dialog" (format "Agent %s is asking a question" name))
                      (_ (format "Agent %s needs attention" name))))
           (severity (if (member ntype '("permission_prompt" "elicitation_dialog"))
                         'high
                       'normal)))
      (alert message
             :title "Claude Multi-Agent"
             :severity severity
             :category 'claude-multi))))

;;;###autoload
(defun cma-modeline--format ()
  "Return modeline string for Claude Multi-Agent status."
  cma-modeline--text)

(provide 'cma-modeline)
;;; cma-modeline.el ends here
