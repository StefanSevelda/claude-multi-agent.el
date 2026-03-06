;; -*- lexical-binding: t; -*-
;;; test-cma-modeline.el --- Tests for modeline notification handling

;;; Commentary:
;; Tests for state diffing, alert firing, and notification-based behavior.

;;; Code:

(require 'buttercup)
(load-file "test/test-helper.el")

;; Provide notification-methods variable used by modeline
(defvar claude-multi-notification-methods '(popup modeline))

(require 'cma-core)
(require 'cma-modeline)

(describe "CMA Modeline Notification Handling"

  (before-each
    (setq cma-modeline--previous-waiting nil)
    (spy-on 'force-mode-line-update))

  (describe "cma-modeline--alert"

    (it "fires alert for idle_prompt"
      (spy-on 'alert)
      (let ((agent '((session_id . "s1")
                     (name . "test-agent")
                     (notification_type . "idle_prompt"))))
        (cma-modeline--alert agent)
        (expect 'alert :to-have-been-called)
        (expect (car (spy-calls-args-for 'alert 0))
                :to-match "finished work")))

    (it "fires alert with high severity for permission_prompt"
      (spy-on 'alert)
      (let ((agent '((session_id . "s1")
                     (name . "test-agent")
                     (notification_type . "permission_prompt"))))
        (cma-modeline--alert agent)
        (expect (plist-get (cdr (spy-calls-args-for 'alert 0)) :severity)
                :to-equal 'high)))

    (it "fires alert for elicitation_dialog"
      (spy-on 'alert)
      (let ((agent '((session_id . "s1")
                     (name . "test-agent")
                     (notification_type . "elicitation_dialog"))))
        (cma-modeline--alert agent)
        (expect (car (spy-calls-args-for 'alert 0))
                :to-match "asking a question")))

    (it "uses fallback message for unknown notification type"
      (spy-on 'alert)
      (let ((agent '((session_id . "s1")
                     (name . "test-agent")
                     (notification_type . "unknown"))))
        (cma-modeline--alert agent)
        (expect (car (spy-calls-args-for 'alert 0))
                :to-match "needs attention")))

    (it "uses session_id when name is nil"
      (spy-on 'alert)
      (let ((agent '((session_id . "abc-123")
                     (notification_type . "idle_prompt"))))
        (cma-modeline--alert agent)
        (expect (car (spy-calls-args-for 'alert 0))
                :to-match "abc-123"))))

  (describe "state diffing"

    (it "detects newly-waiting agents"
      (spy-on 'alert)
      (spy-on 'cma--call-async
              :and-call-fake
              (lambda (cb &rest _)
                (funcall cb '(((session_id . "s1")
                               (waiting_for_input . t)
                               (name . "agent-1")
                               (notification_type . "idle_prompt"))))))
      (cma-modeline--update)
      (expect 'alert :to-have-been-called))

    (it "does not re-alert for already-waiting agents"
      (spy-on 'alert)
      (setq cma-modeline--previous-waiting '("s1"))
      (spy-on 'cma--call-async
              :and-call-fake
              (lambda (cb &rest _)
                (funcall cb '(((session_id . "s1")
                               (waiting_for_input . t)
                               (name . "agent-1")
                               (notification_type . "idle_prompt"))))))
      (cma-modeline--update)
      (expect 'alert :not :to-have-been-called))

    (it "tracks waiting session-ids across polls"
      (spy-on 'alert)
      (spy-on 'cma--call-async
              :and-call-fake
              (lambda (cb &rest _)
                (funcall cb '(((session_id . "s1")
                               (waiting_for_input . t)
                               (name . "agent-1")
                               (notification_type . "idle_prompt"))
                              ((session_id . "s2")
                               (waiting_for_input . t)
                               (name . "agent-2")
                               (notification_type . "permission_prompt"))))))
      (cma-modeline--update)
      (expect cma-modeline--previous-waiting :to-have-same-items-as '("s1" "s2")))

    (it "clears previous-waiting when no agents are waiting"
      (setq cma-modeline--previous-waiting '("s1"))
      (spy-on 'cma--call-async
              :and-call-fake
              (lambda (cb &rest _)
                (funcall cb '(((session_id . "s1")
                               (waiting_for_input . nil))))))
      (cma-modeline--update)
      (expect cma-modeline--previous-waiting :to-equal nil))

    (it "does not alert when popup not in notification-methods"
      (spy-on 'alert)
      (let ((claude-multi-notification-methods '(modeline)))
        (spy-on 'cma--call-async
                :and-call-fake
                (lambda (cb &rest _)
                  (funcall cb '(((session_id . "s1")
                                 (waiting_for_input . t)
                                 (name . "agent-1")
                                 (notification_type . "idle_prompt"))))))
        (cma-modeline--update)
        (expect 'alert :not :to-have-been-called))))

  (describe "modeline text"

    (it "shows waiting count"
      (spy-on 'alert)
      (spy-on 'cma--call-async
              :and-call-fake
              (lambda (cb &rest _)
                (funcall cb '(((session_id . "s1")
                               (waiting_for_input . t))))))
      (cma-modeline--update)
      (expect cma-modeline--text :to-equal " [CMA:1 waiting]"))

    (it "shows empty text when no agents waiting"
      (spy-on 'cma--call-async
              :and-call-fake
              (lambda (cb &rest _)
                (funcall cb '(((session_id . "s1")
                               (waiting_for_input . nil))))))
      (cma-modeline--update)
      (expect cma-modeline--text :to-equal ""))

    (it "does not count agents with :false (json-parse-string false) as waiting"
      ;; json-parse-string maps JSON false to :false, which is truthy — regression test
      (spy-on 'alert)
      (spy-on 'cma--call-async
              :and-call-fake
              (lambda (cb &rest _)
                (funcall cb '(((session_id . "s1")
                               (waiting_for_input . :false))))))
      (cma-modeline--update)
      (expect cma-modeline--text :to-equal ""))))

(provide 'test-cma-modeline)
;;; test-cma-modeline.el ends here
