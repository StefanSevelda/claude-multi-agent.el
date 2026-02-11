;; -*- lexical-binding: t; -*-
;;; test-notifications.el --- Tests for claude-multi-notifications module

;;; Commentary:
;; Comprehensive tests for notification system including input detection,
;; notification triggers, popup/modeline/markdown notifications, and cleanup

;;; Code:

(require 'buttercup)
(load-file "test/test-helper.el")
(require 'claude-multi-agents)
(require 'claude-multi-notifications)

(describe "Notification Module"

  ;; Test fixtures
  (before-each
    (setq claude-multi--waiting-agents nil)
    (setq claude-multi--modeline-string "")
    (setq claude-multi--agents nil)
    (setq claude-multi-notification-methods '(popup markdown modeline))

    ;; Mock external functions
    (spy-on 'alert)
    (spy-on 'message)
    (spy-on 'notifications-notify)
    (spy-on 'force-mode-line-update)
    (spy-on 'claude-multi--highlight-input-requests))

  (after-each
    (setq claude-multi--waiting-agents nil)
    (setq claude-multi--modeline-string "")
    (setq claude-multi--agents nil))

  ;;; Input Detection Tests
  (describe "claude-multi--detect-input-request"

    (it "detects Request interrupted pattern"
      (expect (claude-multi--detect-input-request "[Request interrupted - waiting for input]")
              :to-be-truthy))

    (it "detects AskUserQuestion pattern"
      (expect (claude-multi--detect-input-request "Using AskUserQuestion tool")
              :to-be-truthy))

    (it "detects y/n question pattern"
      (expect (claude-multi--detect-input-request "Continue with this action? (y/n)?")
              :to-be-truthy))

    (it "detects yes/no question pattern"
      (expect (claude-multi--detect-input-request "Proceed? (yes/no)?")
              :to-be-truthy))

    (it "detects Continue? pattern"
      (expect (claude-multi--detect-input-request "Continue?")
              :to-be-truthy))

    (it "detects Enter your choice pattern"
      (expect (claude-multi--detect-input-request "Enter your choice: 1, 2, or 3")
              :to-be-truthy))

    (it "detects Please provide pattern"
      (expect (claude-multi--detect-input-request "Please provide: your input here")
              :to-be-truthy))

    (it "detects Waiting for input pattern"
      (expect (claude-multi--detect-input-request "Waiting for input from user")
              :to-be-truthy))

    (it "detects Press key to continue pattern"
      (expect (claude-multi--detect-input-request "Press any key to continue")
              :to-be-truthy))

    (it "detects [?] pattern"
      (expect (claude-multi--detect-input-request "Command output [?] what next")
              :to-be-truthy))

    (it "ignores normal output without input patterns"
      (expect (claude-multi--detect-input-request "Processing files...")
              :not :to-be-truthy))

    (it "ignores empty strings"
      (expect (claude-multi--detect-input-request "")
              :not :to-be-truthy))

    (it "ignores output with similar but non-matching patterns"
      (expect (claude-multi--detect-input-request "Continued working on task")
              :not :to-be-truthy)))

  ;;; Notification Trigger Tests
  (describe "claude-multi--notify-input-needed"

    (it "adds agent to waiting list"
      (let ((agent (make-claude-agent :id "test-1" :task-description "Test task")))
        (claude-multi--notify-input-needed agent)
        (expect (memq agent claude-multi--waiting-agents) :to-be-truthy)))

    (it "does not add agent twice to waiting list"
      (let ((agent (make-claude-agent :id "test-1" :task-description "Test task")))
        (claude-multi--notify-input-needed agent)
        (claude-multi--notify-input-needed agent)
        (expect (length claude-multi--waiting-agents) :to-equal 1)))

    (it "calls highlight-input-requests function"
      (let ((agent (make-claude-agent :id "test-1" :task-description "Test task")))
        (claude-multi--notify-input-needed agent)
        (expect 'claude-multi--highlight-input-requests :to-have-been-called)))

    (it "triggers popup notification when configured"
      (let ((agent (make-claude-agent :id "test-1" :task-description "Test task")))
        (setq claude-multi-notification-methods '(popup))
        ;; Mock alert package availability
        (spy-on 'featurep :and-return-value t)
        (claude-multi--notify-input-needed agent)
        (expect 'alert :to-have-been-called)))

    (it "triggers modeline notification when configured"
      (let ((agent (make-claude-agent :id "test-1" :task-description "Test task")))
        (setq claude-multi-notification-methods '(modeline))
        (claude-multi--notify-input-needed agent)
        (expect 'force-mode-line-update :to-have-been-called)))

    (it "triggers multiple notification methods when configured"
      (let ((agent (make-claude-agent :id "test-1" :task-description "Test task")))
        (setq claude-multi-notification-methods '(popup modeline))
        (spy-on 'featurep :and-return-value t)
        (claude-multi--notify-input-needed agent)
        (expect 'alert :to-have-been-called)
        (expect 'force-mode-line-update :to-have-been-called))))

  ;;; Notification Clearing Tests
  (describe "claude-multi--clear-notifications"

    (it "removes agent from waiting list"
      (let ((agent (make-claude-agent :id "test-1" :task-description "Test task")))
        (push agent claude-multi--waiting-agents)
        (claude-multi--clear-notifications agent)
        (expect (memq agent claude-multi--waiting-agents) :not :to-be-truthy)))

    (it "updates modeline after clearing"
      (let ((agent (make-claude-agent :id "test-1" :task-description "Test task")))
        (push agent claude-multi--waiting-agents)
        (claude-multi--clear-notifications agent)
        (expect 'force-mode-line-update :to-have-been-called)))

    (it "handles clearing agent not in waiting list gracefully"
      (let ((agent (make-claude-agent :id "test-1" :task-description "Test task")))
        ;; Should not throw when agent is not in waiting list
        (expect (claude-multi--clear-notifications agent) :not :to-throw))))

  ;;; Popup Notification Tests
  (describe "claude-multi--notify-popup"

    (it "uses alert package when available"
      (let ((agent (make-claude-agent :id "test-1" :task-description "Test task")))
        (spy-on 'featurep :and-call-fake
                (lambda (feature) (eq feature 'alert)))
        (claude-multi--notify-popup agent)
        (expect 'alert :to-have-been-called)))

    (it "passes correct message to alert"
      (let ((agent (make-claude-agent :id "test-1" :task-description "Test task")))
        (spy-on 'featurep :and-call-fake
                (lambda (feature) (eq feature 'alert)))
        (claude-multi--notify-popup agent)
        (let ((calls (spy-calls-all 'alert)))
          (expect (length calls) :to-be-greater-than 0)
          (expect (car (spy-calls-args-for 'alert 0))
                  :to-match "waiting for your response"))))

    (it "falls back to notifications-notify when alert not available"
      (let ((agent (make-claude-agent :id "test-1" :task-description "Test task")))
        (spy-on 'featurep :and-return-value nil)
        (spy-on 'fboundp :and-call-fake
                (lambda (fn) (eq fn 'notifications-notify)))
        (claude-multi--notify-popup agent)
        (expect 'notifications-notify :to-have-been-called)))

    (it "falls back to message when no notification system available"
      (let ((agent (make-claude-agent :id "test-1" :task-description "Test task")))
        (spy-on 'featurep :and-return-value nil)
        (spy-on 'fboundp :and-return-value nil)
        (claude-multi--notify-popup agent)
        (expect 'message :to-have-been-called))))

  ;;; Modeline Notification Tests
  (describe "claude-multi--notify-modeline"

    (it "sets modeline string to empty when no waiting agents"
      (setq claude-multi--waiting-agents nil)
      (claude-multi--notify-modeline)
      (expect claude-multi--modeline-string :to-equal ""))

    (it "sets modeline string with count when agents waiting"
      (let ((agent1 (make-claude-agent :id "test-1"))
            (agent2 (make-claude-agent :id "test-2")))
        (setq claude-multi--waiting-agents (list agent1 agent2))
        (claude-multi--notify-modeline)
        (expect claude-multi--modeline-string :to-match "\\[Claude:2⏳\\]")))

    (it "updates modeline string when waiting agent count changes"
      (let ((agent1 (make-claude-agent :id "test-1")))
        (setq claude-multi--waiting-agents (list agent1))
        (claude-multi--notify-modeline)
        (expect claude-multi--modeline-string :to-match "\\[Claude:1⏳\\]")
        (setq claude-multi--waiting-agents nil)
        (claude-multi--notify-modeline)
        (expect claude-multi--modeline-string :to-equal "")))

    (it "forces modeline update"
      (claude-multi--notify-modeline)
      (expect 'force-mode-line-update :to-have-been-called)))

  ;;; Interactive Functions Tests
  (describe "claude-multi/show-waiting-agents"

    (it "displays buffer with waiting agents"
      (let ((agent1 (make-claude-agent :id "test-1" :status 'waiting-input)))
        (setq claude-multi--waiting-agents (list agent1))
        (spy-on 'display-buffer)
        ;; Mock the missing function
        (spy-on 'claude-multi--get-status-icon :and-return-value "🟡")
        (claude-multi/show-waiting-agents)
        (expect 'display-buffer :to-have-been-called)
        (let ((buf (get-buffer "*Claude Waiting Agents.org*")))
          (expect buf :to-be-truthy)
          (when buf (kill-buffer buf)))))

    (it "shows message when no agents waiting"
      (setq claude-multi--waiting-agents nil)
      (claude-multi/show-waiting-agents)
      (expect 'message :to-have-been-called-with "No agents are waiting for input")))

  ;;; Cleanup Tests
  (describe "claude-multi--cleanup-notifications"

    (it "removes agents not in active agent list"
      (let ((agent1 (make-claude-agent :id "test-1" :status 'waiting-input))
            (agent2 (make-claude-agent :id "test-2" :status 'waiting-input)))
        (setq claude-multi--waiting-agents (list agent1 agent2))
        (setq claude-multi--agents (list agent1))  ; Only agent1 is active
        (claude-multi--cleanup-notifications)
        (expect (memq agent1 claude-multi--waiting-agents) :to-be-truthy)
        (expect (memq agent2 claude-multi--waiting-agents) :not :to-be-truthy)))

    (it "removes agents not in waiting-input status"
      (let ((agent1 (make-claude-agent :id "test-1" :status 'completed))
            (agent2 (make-claude-agent :id "test-2" :status 'waiting-input)))
        (setq claude-multi--waiting-agents (list agent1 agent2))
        (setq claude-multi--agents (list agent1 agent2))
        (claude-multi--cleanup-notifications)
        (expect (memq agent1 claude-multi--waiting-agents) :not :to-be-truthy)
        (expect (memq agent2 claude-multi--waiting-agents) :to-be-truthy)))

    (it "updates modeline after cleanup"
      (claude-multi--cleanup-notifications)
      (expect 'force-mode-line-update :to-have-been-called)))

  ;;; Periodic Check Tests
  (describe "claude-multi--check-waiting-agents"

    (it "triggers notifications for agents in waiting-input status"
      (let ((agent1 (make-claude-agent :id "test-1" :status 'waiting-input))
            (agent2 (make-claude-agent :id "test-2" :status 'running)))
        (setq claude-multi--agents (list agent1 agent2))
        (setq claude-multi--waiting-agents nil)
        (spy-on 'featurep :and-return-value t)
        (claude-multi--check-waiting-agents)
        ;; Agent1 should be added to waiting list
        (expect (memq agent1 claude-multi--waiting-agents) :to-be-truthy)
        ;; Agent2 should not be added
        (expect (memq agent2 claude-multi--waiting-agents) :not :to-be-truthy)))

    (it "does not trigger notifications for already waiting agents"
      (let ((agent1 (make-claude-agent :id "test-1" :status 'waiting-input)))
        (setq claude-multi--agents (list agent1))
        (setq claude-multi--waiting-agents (list agent1))
        (spy-on 'featurep :and-return-value t)
        (let ((call-count-before (spy-calls-count 'alert)))
          (claude-multi--check-waiting-agents)
          ;; Alert should not be called again
          (expect (spy-calls-count 'alert) :to-equal call-count-before))))))

(provide 'test-notifications)
;;; test-notifications.el ends here
