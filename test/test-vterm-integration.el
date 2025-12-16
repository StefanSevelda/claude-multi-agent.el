;; -*- lexical-binding: t; -*-
;;; test-vterm-integration.el --- Tests for vterm integration

;;; Commentary:
;; Unit tests for vterm buffer management and process integration

;;; Code:

(require 'buttercup)
(require 'claude-multi-agents)

(describe "Vterm Integration"

  (describe "claude-multi--send-to-vterm"
    (it "sends command to vterm buffer and sends return"
      (let ((test-buffer (generate-new-buffer "*vterm-test*"))
            (vterm-send-string-called nil)
            (vterm-send-return-called nil))
        (with-current-buffer test-buffer
          ;; Mock vterm functions
          (cl-letf (((symbol-function 'vterm-send-string)
                     (lambda (string)
                       (setq vterm-send-string-called string)))
                    ((symbol-function 'vterm-send-return)
                     (lambda ()
                       (setq vterm-send-return-called t))))
            (claude-multi--send-to-vterm test-buffer "test command")
            (expect vterm-send-string-called :to-equal "test command")
            (expect vterm-send-return-called :to-be-truthy)))
        (kill-buffer test-buffer))))

  (describe "claude-multi--setup-vterm-output-monitor"
    (it "sets up process filter for agent monitoring"
      (let* ((agent (make-claude-agent
                     :id "test-agent-1"
                     :name "test-agent"
                     :buffer (generate-new-buffer "*vterm-monitor-test*")
                     :status 'running))
             (mock-process (make-process
                           :name "test-vterm-process"
                           :buffer (claude-agent-buffer agent)
                           :command '("echo" "test")
                           :connection-type 'pipe))
             (filter-set nil))
        (cl-letf (((symbol-function 'get-buffer-process)
                   (lambda (buf) mock-process))
                  ((symbol-function 'set-process-filter)
                   (lambda (proc filter)
                     (setq filter-set (cons proc filter)))))
          (claude-multi--setup-vterm-output-monitor agent)
          (expect (car filter-set) :to-equal mock-process)
          (expect (cdr filter-set) :to-be-truthy)
          (expect (functionp (cdr filter-set)) :to-be-truthy)
          (expect (claude-agent-process agent) :to-equal mock-process))
        (delete-process mock-process)
        (kill-buffer (claude-agent-buffer agent)))))

  (describe "claude-multi--process-vterm-output"
    (before-each
      (setq test-agent (make-claude-agent
                        :id "test-agent-2"
                        :name "test-agent-2"
                        :status 'running)))

    (it "updates agent's last output"
      (claude-multi--process-vterm-output test-agent "test output")
      (expect (claude-agent-last-output test-agent) :to-equal "test output"))

    (it "detects input requests and updates status"
      (cl-letf (((symbol-function 'claude-multi--detect-input-request)
                 (lambda (output) t))
                ((symbol-function 'claude-multi--notify-input-needed)
                 (lambda (agent) nil))
                ((symbol-function 'claude-multi--append-agent-output)
                 (lambda (agent output) nil)))
        (claude-multi--process-vterm-output test-agent "Please provide input:")
        (expect (claude-agent-status test-agent) :to-equal 'waiting-input)))

    (it "detects completion and handles it"
      (let ((completion-handled nil))
        (cl-letf (((symbol-function 'claude-multi--detect-completion)
                   (lambda (output) t))
                  ((symbol-function 'claude-multi--handle-agent-completion)
                   (lambda (agent) (setq completion-handled t)))
                  ((symbol-function 'claude-multi--append-agent-output)
                   (lambda (agent output) nil)))
          (claude-multi--process-vterm-output test-agent "Task completed successfully")
          (expect completion-handled :to-be-truthy))))

    (it "detects errors and updates status"
      (cl-letf (((symbol-function 'claude-multi--detect-error)
                 (lambda (output) t))
                ((symbol-function 'claude-multi--append-agent-output)
                 (lambda (agent output) nil)))
        (claude-multi--process-vterm-output test-agent "Error: something failed")
        (expect (claude-agent-status test-agent) :to-equal 'failed))))

  (describe "claude-multi--launch-agent with vterm"
    (it "creates vterm buffer with correct naming"
      (let* ((agent (make-claude-agent
                     :id "test-agent-3"
                     :name "claude-agent-3"
                     :status 'pending
                     :task-description "Test task"))
             (vterm-created nil)
             (vterm-buffer-name nil))
        (cl-letf (((symbol-function 'claude-multi--in-git-repo-p)
                   (lambda () nil))
                  ((symbol-function 'vterm)
                   (lambda (arg)
                     (setq vterm-created t)
                     (generate-new-buffer "*vterm-test*")))
                  ((symbol-function 'rename-buffer)
                   (lambda (name &optional unique)
                     (setq vterm-buffer-name name)))
                  ((symbol-function 'claude-multi--setup-vterm-output-monitor)
                   (lambda (agent) nil))
                  ((symbol-function 'run-with-timer)
                   (lambda (secs repeat func) nil))
                  ((symbol-function 'claude-multi--add-agent-section)
                   (lambda (agent) nil)))
          (claude-multi--launch-agent agent)
          (expect vterm-created :to-be-truthy)
          (expect vterm-buffer-name :to-equal (format "*vterm-%s*" (claude-agent-name agent)))
          (expect (claude-agent-status agent) :to-equal 'running)
          (when (claude-agent-buffer agent)
            (kill-buffer (claude-agent-buffer agent)))))))

  (describe "claude-multi--kill-agent with vterm"
    (it "kills vterm process and buffer correctly"
      (let* ((test-buffer (generate-new-buffer "*vterm-kill-test*"))
             (mock-process (make-process
                           :name "test-vterm-kill"
                           :buffer test-buffer
                           :command '("cat")
                           :connection-type 'pipe))
             (agent (make-claude-agent
                     :id "test-agent-4"
                     :name "test-agent-4"
                     :buffer test-buffer
                     :status 'running))
             (process-deleted nil)
             (claude-multi--agents (list agent)))
        (cl-letf (((symbol-function 'get-buffer-process)
                   (lambda (buf) mock-process))
                  ((symbol-function 'delete-process)
                   (lambda (proc)
                     (setq process-deleted t)
                     (delete-process proc)))
                  ((symbol-function 'claude-multi--delete-worktree)
                   (lambda (agent) nil))
                  ((symbol-function 'claude-multi--remove-agent-section)
                   (lambda (agent) nil)))
          (claude-multi--kill-agent agent)
          (expect process-deleted :to-be-truthy)
          (expect claude-multi--agents :to-equal nil)))))

  (describe "claude-multi--send-input-to-agent with vterm"
    (it "sends input to vterm buffer and updates agent status"
      (let* ((test-buffer (generate-new-buffer "*vterm-input-test*"))
             (agent (make-claude-agent
                     :id "test-agent-5"
                     :name "test-agent-5"
                     :buffer test-buffer
                     :status 'waiting-input))
             (input-sent nil))
        (cl-letf (((symbol-function 'claude-multi--send-to-vterm)
                   (lambda (buffer input)
                     (setq input-sent input)))
                  ((symbol-function 'claude-multi--clear-notifications)
                   (lambda (agent) nil))
                  ((symbol-function 'claude-multi--update-agent-status)
                   (lambda (agent) nil)))
          (claude-multi--send-input-to-agent agent "user input")
          (expect input-sent :to-equal "user input")
          (expect (claude-agent-status agent) :to-equal 'running))
        (kill-buffer test-buffer)))))

(provide 'test-vterm-integration)
;;; test-vterm-integration.el ends here
