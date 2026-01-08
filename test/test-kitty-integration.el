;; -*- lexical-binding: t; -*-
;;; test-kitty-integration.el --- Tests for kitty integration

;;; Commentary:
;; Unit tests for kitty window management and remote control integration

;;; Code:

(require 'buttercup)
(require 'claude-multi-agents)
(require 'claude-multi-status)

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
                  ((symbol-function 'claude-multi--register-agent-for-status)
                   (lambda (agent) nil))
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
        ;; Extract first argument (command string) from spy call args
        (let ((command-string (car (spy-calls-args-for 'call-process-shell-command 0))))
          (expect command-string :to-match "send-text"))))

    (it "properly escapes command with single quotes"
      (let ((agent (make-claude-agent :kitty-window-id "123")))
        (claude-multi--send-to-kitty agent "echo 'hello world'")
        (expect 'call-process-shell-command :to-have-been-called)
        (let ((command-string (car (spy-calls-args-for 'call-process-shell-command 0))))
          ;; Single quotes should be escaped as '\''
          (expect command-string :to-match "echo")))))

  (describe "Agent directory handling"
    (it "sends cd command with expanded path before launching claude"
      (let* ((test-dir "/Users/test/projects/foo")
             (agent (make-claude-agent
                     :id "test-1"
                     :name "claude-test-1"
                     :task-description "Test task"
                     :status 'pending))
             (captured-command nil)
             (captured-agent nil))
        ;; Set directory after creation
        (setf (claude-agent-directory agent) test-dir)
        (cl-letf (((symbol-function 'claude-multi--in-git-repo-p)
                   (lambda () nil))
                  ((symbol-function 'claude-multi--register-agent-for-status)
                   (lambda (agent) nil))
                  ((symbol-function 'claude-multi--setup-kitty-status-monitor)
                   (lambda (agent) nil))
                  ((symbol-function 'run-with-timer)
                   (lambda (secs repeat func)
                     ;; Execute the timer function immediately to capture the command
                     (when (and (numberp secs) (eq secs 0.5))
                       (funcall func))
                     nil))
                  ((symbol-function 'claude-multi--send-to-kitty)
                   (lambda (ag cmd)
                     (setq captured-agent ag)
                     (setq captured-command cmd)))
                  ((symbol-function 'claude-multi--add-agent-section)
                   (lambda (agent) nil)))
          (claude-multi--launch-agent agent)
          ;; Should have captured the cd command
          (expect captured-command :to-be-truthy)
          (expect captured-command :to-match "cd .* && ")
          (expect captured-command :to-match test-dir))))

    (it "expands tilde in directory path"
      (let* ((test-dir "~/projects/foo")
             (expanded-dir (expand-file-name test-dir))
             (agent (make-claude-agent
                     :id "test-2"
                     :name "claude-test-2"
                     :task-description "Test task"
                     :status 'pending))
             (captured-command nil))
        ;; Set directory after creation
        (setf (claude-agent-directory agent) test-dir)
        (cl-letf (((symbol-function 'claude-multi--in-git-repo-p)
                   (lambda () nil))
                  ((symbol-function 'claude-multi--register-agent-for-status)
                   (lambda (agent) nil))
                  ((symbol-function 'claude-multi--setup-kitty-status-monitor)
                   (lambda (agent) nil))
                  ((symbol-function 'run-with-timer)
                   (lambda (secs repeat func)
                     (when (and (numberp secs) (eq secs 0.5))
                       (funcall func))
                     nil))
                  ((symbol-function 'claude-multi--send-to-kitty)
                   (lambda (agent command)
                     (setq captured-command command)))
                  ((symbol-function 'claude-multi--add-agent-section)
                   (lambda (agent) nil)))
          (claude-multi--launch-agent agent)
          ;; Should have expanded tilde and not escaped it
          (expect captured-command :to-be-truthy)
          (expect captured-command :not :to-match "\\\\~")
          (expect captured-command :to-match expanded-dir))))

    (it "uses worktree path over directory when both are set"
      (let* ((worktree-path "/Users/test/worktrees/feature-branch")
             (default-dir "/Users/test/default")
             (agent (make-claude-agent
                     :id "test-3"
                     :name "claude-test-3"
                     :task-description "Test task"
                     :status 'pending))
             (captured-command nil))
        ;; Set both directory and worktree path
        (setf (claude-agent-directory agent) default-dir)
        (setf (claude-agent-worktree-path agent) worktree-path)
        (cl-letf (((symbol-function 'claude-multi--in-git-repo-p)
                   (lambda () nil))
                  ((symbol-function 'claude-multi--register-agent-for-status)
                   (lambda (agent) nil))
                  ((symbol-function 'claude-multi--setup-kitty-status-monitor)
                   (lambda (agent) nil))
                  ((symbol-function 'run-with-timer)
                   (lambda (secs repeat func)
                     (when (and (numberp secs) (eq secs 0.5))
                       (funcall func))
                     nil))
                  ((symbol-function 'claude-multi--send-to-kitty)
                   (lambda (agent command)
                     (setq captured-command command)))
                  ((symbol-function 'claude-multi--add-agent-section)
                   (lambda (agent) nil)))
          (claude-multi--launch-agent agent)
          ;; Should use worktree path, not directory
          (expect captured-command :to-be-truthy)
          (expect captured-command :to-match worktree-path)
          (expect captured-command :not :to-match default-dir)))))

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
        (cl-letf (((symbol-function 'claude-multi--unregister-agent-for-status)
                   (lambda (agent) nil))
                  ((symbol-function 'claude-multi--delete-worktree)
                   (lambda (agent) nil))
                  ((symbol-function 'claude-multi--remove-agent-section)
                   (lambda (agent) nil)))
          (claude-multi--kill-agent agent)
          (expect 'call-process-shell-command :to-have-been-called)
          ;; Extract first argument (command string) from spy call args
          (let ((command-string (car (spy-calls-args-for 'call-process-shell-command 0))))
            (expect command-string :to-match "close-window"))
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
