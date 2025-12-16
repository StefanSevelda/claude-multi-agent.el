;; -*- lexical-binding: t; -*-
;;; test-kitty-integration.el --- Tests for kitty integration

;;; Commentary:
;; Unit tests for kitty window management and remote control integration

;;; Code:

(require 'buttercup)
(require 'claude-multi-agents)

(describe "Kitty Integration"

  (before-each
    ;; Mock shell commands
    (spy-on 'call-process-shell-command :and-return-value 0)
    (spy-on 'shell-command-to-string :and-return-value "12345"))

  (describe "claude-multi--launch-agent"
    (it "creates kitty window with correct parameters"
      (let ((agent (make-claude-agent
                    :id "test-1"
                    :name "claude-test-1"
                    :task-description "Test task"
                    :status 'pending)))
        (cl-letf (((symbol-function 'claude-multi--in-git-repo-p)
                   (lambda () nil))
                  ((symbol-function 'claude-multi--setup-kitty-status-monitor)
                   (lambda (agent) nil))
                  ((symbol-function 'run-with-timer)
                   (lambda (secs repeat func) nil))
                  ((symbol-function 'claude-multi--add-agent-section)
                   (lambda (agent) nil)))
          (claude-multi--launch-agent agent)
          (expect 'shell-command-to-string :to-have-been-called)
          (expect (claude-agent-kitty-window-id agent) :to-equal "12345")
          (expect (claude-agent-status agent) :to-equal 'running)
          (expect (claude-agent-context-buffer agent) :to-be-truthy)
          (when (claude-agent-context-buffer agent)
            (kill-buffer (claude-agent-context-buffer agent)))))))

  (describe "claude-multi--send-to-kitty"
    (it "sends initial command to kitty"
      (let ((agent (make-claude-agent :kitty-window-id "123")))
        (claude-multi--send-to-kitty agent "claude 'test task'")
        (expect 'call-process-shell-command :to-have-been-called)
        (expect (spy-calls-args-for 'call-process-shell-command 0)
                :to-match "send-text"))))

  (describe "claude-multi--kitty-is-alive"
    (it "checks window existence successfully"
      (let ((agent (make-claude-agent :kitty-window-id "123")))
        (spy-on 'call-process-shell-command :and-return-value 0)
        (expect (claude-multi--kitty-is-alive agent) :to-be-truthy)))

    (it "detects when window is closed"
      (let ((agent (make-claude-agent :kitty-window-id "123")))
        (spy-on 'call-process-shell-command :and-return-value 1)
        (expect (claude-multi--kitty-is-alive agent) :not :to-be-truthy))))

  (describe "claude-multi--check-kitty-status"
    (it "marks agent completed when window closes"
      (let ((agent (make-claude-agent
                    :kitty-window-id "123"
                    :status 'running)))
        (cl-letf (((symbol-function 'claude-multi--kitty-is-alive)
                   (lambda (agent) nil))
                  ((symbol-function 'claude-multi--handle-agent-completion)
                   (lambda (agent) (setf (claude-agent-status agent) 'completed))))
          (claude-multi--check-kitty-status agent)
          (expect (claude-agent-status agent) :to-equal 'completed)))))

  (describe "claude-multi--kill-agent"
    (it "closes kitty window and cleanups"
      (let* ((context-buf (generate-new-buffer "*test-context*"))
             (agent (make-claude-agent
                     :kitty-window-id "123"
                     :context-buffer context-buf
                     :status-timer (run-with-timer 10 nil #'ignore)))
             (claude-multi--agents (list agent)))
        (cl-letf (((symbol-function 'claude-multi--delete-worktree)
                   (lambda (agent) nil))
                  ((symbol-function 'claude-multi--remove-agent-section)
                   (lambda (agent) nil)))
          (claude-multi--kill-agent agent)
          (expect 'call-process-shell-command :to-have-been-called)
          (expect (spy-calls-args-for 'call-process-shell-command 0)
                  :to-match "close-window")
          (expect claude-multi--agents :to-equal nil)))))

  (describe "claude-multi--setup-kitty-status-monitor"
    (it "creates status timer for agent"
      (let ((agent (make-claude-agent :kitty-window-id "123"))
            (timer-created nil))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (secs repeat func)
                     (setq timer-created t)
                     (list 'timer secs repeat func))))
          (claude-multi--setup-kitty-status-monitor agent)
          (expect timer-created :to-be-truthy)
          (expect (claude-agent-status-timer agent) :to-be-truthy))))))

(provide 'test-kitty-integration)
;;; test-kitty-integration.el ends here
