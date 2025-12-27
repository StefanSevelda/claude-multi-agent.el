;; -*- lexical-binding: t; -*-
;;; test-progress-buffer-sync.el --- Tests for progress buffer synchronization

;;; Commentary:
;; Tests to verify that agent information is properly synchronized to the
;; progress buffer when agents are created, launched, and removed.
;; These tests mock external dependencies (kitty, file-notify) so they don't
;; require Claude Code or kitty to be installed.

;;; Code:

(require 'buttercup)

;; Load test helper
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'test-helper)

;; Load modules under test
(add-to-list 'load-path (expand-file-name "../autoload" (file-name-directory load-file-name)))
(require 'claude-multi-agents)
(require 'claude-multi-progress)

;; Mock external commands to prevent actual execution
(defvar test-mock-shell-commands nil
  "List to track shell commands that were called during tests.")

(defun test-mock-shell-command-to-string (command)
  "Mock version of shell-command-to-string for testing."
  (push command test-mock-shell-commands)
  (cond
   ;; Mock kitty launch - return fake window ID
   ((string-match "kitty @ .* launch" command)
    "test-window-12345\n")
   ;; Mock kitty ls check (for window existence)
   ((string-match "kitty @ .* ls" command)
    "test-window-12345\n")
   ;; Other commands
   (t "")))

(defun test-mock-call-process-shell-command (command &optional infile destination display)
  "Mock version of call-process-shell-command for testing."
  (push command test-mock-shell-commands)
  ;; Return 0 (success) for all commands
  0)

(describe "Progress Buffer Synchronization"

  (before-each
    ;; Reset state
    (setq claude-multi--agents nil)
    (setq claude-multi--agent-id-counter 0)
    (setq claude-multi--current-session-window-id nil)
    (setq claude-multi--progress-buffer nil)
    (setq claude-multi-claude-command "claude")
    (setq test-mock-shell-commands nil)

    ;; Mock external functions
    (spy-on 'shell-command-to-string :and-call-fake #'test-mock-shell-command-to-string)
    (spy-on 'call-process-shell-command :and-call-fake #'test-mock-call-process-shell-command)
    (spy-on 'run-with-timer :and-return-value nil)

    ;; Create progress buffer (set session start time BEFORE init)
    (setq claude-multi--session-start-time (current-time))
    (setq claude-multi--progress-buffer
          (get-buffer-create "*test-progress-buffer*"))
    (claude-multi--init-progress-buffer))

  (after-each
    ;; Clean up
    (when claude-multi--agents
      (dolist (agent claude-multi--agents)
        (when-let ((buf (claude-agent-context-buffer agent)))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))
    (when (and claude-multi--progress-buffer
               (buffer-live-p claude-multi--progress-buffer))
      (kill-buffer claude-multi--progress-buffer))
    (setq claude-multi--agents nil)
    (setq test-mock-shell-commands nil))

  (describe "when adding agent sections to progress buffer"

    (it "initializes progress buffer with session info"
      (with-current-buffer claude-multi--progress-buffer
        (let ((content (buffer-string)))
          ;; Should have session info section (use escaped asterisk for org-mode heading)
          (expect (string-match-p "\\* Session Info" content)
                 :to-be-truthy)
          ;; Should have agents section
          (expect (string-match-p "\\* Agents" content)
                 :to-be-truthy)
          ;; Initial stats should be 0
          (expect (string-match-p "Stats :: 0 total" content)
                 :to-be-truthy))))

    (it "adds agent section when agent is created"
      (let ((agent (claude-multi--create-agent "Test task")))
        ;; Add agent section
        (claude-multi--add-agent-section agent)

        (with-current-buffer claude-multi--progress-buffer
          (let ((content (buffer-string)))
            ;; Agent ID should appear in content
            (expect (string-match-p (regexp-quote (claude-agent-id agent)) content)
                   :to-be-truthy)
            ;; Task description should appear
            (expect (string-match-p "Test task" content)
                   :to-be-truthy)
            ;; Should have status drawer
            (expect (string-match-p ":STATUS:" content)
                   :to-be-truthy)))))

    (it "adds multiple agent sections correctly"
      (let ((agent1 (claude-multi--create-agent "Task 1"))
            (agent2 (claude-multi--create-agent "Task 2"))
            (agent3 (claude-multi--create-agent "Task 3")))

        (claude-multi--add-agent-section agent1)
        (claude-multi--add-agent-section agent2)
        (claude-multi--add-agent-section agent3)

        (with-current-buffer claude-multi--progress-buffer
          (let ((content (buffer-string)))
            ;; All three agent IDs should be present
            (expect (string-match-p (regexp-quote (claude-agent-id agent1)) content)
                   :to-be-truthy)
            (expect (string-match-p (regexp-quote (claude-agent-id agent2)) content)
                   :to-be-truthy)
            (expect (string-match-p (regexp-quote (claude-agent-id agent3)) content)
                   :to-be-truthy)
            ;; All three tasks should be present
            (expect (string-match-p "Task 1" content) :to-be-truthy)
            (expect (string-match-p "Task 2" content) :to-be-truthy)
            (expect (string-match-p "Task 3" content) :to-be-truthy)))))

    (it "shows correct status icon for each agent state"
      (let ((agent (claude-multi--create-agent "Test task")))
        ;; Test different status values
        (setf (claude-agent-status agent) 'pending)
        (claude-multi--add-agent-section agent)
        (with-current-buffer claude-multi--progress-buffer
          (expect (string-match-p "⚪" (buffer-string)) :to-be-truthy))

        ;; Clear and test running status
        (erase-buffer)
        (claude-multi--init-progress-buffer)
        (setf (claude-agent-status agent) 'running)
        (claude-multi--add-agent-section agent)
        (with-current-buffer claude-multi--progress-buffer
          (expect (string-match-p "🟢" (buffer-string)) :to-be-truthy)))))

  (describe "when updating session statistics"

    (it "updates stats when agents are added"
      (let ((agent1 (claude-multi--create-agent "Task 1"))
            (agent2 (claude-multi--create-agent "Task 2")))
        ;; Add to session
        (push agent1 claude-multi--agents)
        (push agent2 claude-multi--agents)

        ;; Update stats
        (claude-multi--update-session-stats)

        (with-current-buffer claude-multi--progress-buffer
          (let ((content (buffer-string)))
            (expect (string-match-p "Stats :: 2 total" content)
                   :to-be-truthy)))))

    (it "correctly counts agents by status"
      (let ((agent1 (claude-multi--create-agent "Task 1"))
            (agent2 (claude-multi--create-agent "Task 2"))
            (agent3 (claude-multi--create-agent "Task 3")))

        ;; Set different statuses
        (setf (claude-agent-status agent1) 'running)
        (setf (claude-agent-status agent2) 'waiting-input)
        (setf (claude-agent-status agent3) 'completed)

        ;; Add to session
        (push agent1 claude-multi--agents)
        (push agent2 claude-multi--agents)
        (push agent3 claude-multi--agents)

        ;; Update stats
        (claude-multi--update-session-stats)

        (with-current-buffer claude-multi--progress-buffer
          (let ((content (buffer-string)))
            (expect (string-match-p "Stats :: 3 total" content) :to-be-truthy)
            (expect (string-match-p "1 running" content) :to-be-truthy)
            (expect (string-match-p "1 waiting" content) :to-be-truthy)
            (expect (string-match-p "1 completed" content) :to-be-truthy)))))

    (it "updates stats to 0 when all agents are removed"
      (let ((agent (claude-multi--create-agent "Task")))
        (push agent claude-multi--agents)
        (claude-multi--update-session-stats)

        ;; Remove agent
        (setq claude-multi--agents nil)
        (claude-multi--update-session-stats)

        (with-current-buffer claude-multi--progress-buffer
          (expect (string-match-p "Stats :: 0 total" (buffer-string))
                 :to-be-truthy)))))

  (describe "when updating agent status"

    (it "updates agent headline when status changes"
      (let ((agent (claude-multi--create-agent "Test task")))
        ;; Add initial section with pending status
        (setf (claude-agent-status agent) 'pending)
        (claude-multi--add-agent-section agent)

        ;; Change status to running
        (setf (claude-agent-status agent) 'running)
        (claude-multi--update-agent-status agent)

        (with-current-buffer claude-multi--progress-buffer
          (let ((content (buffer-string)))
            ;; Should show running emoji
            (expect (string-match-p "🟢" content) :to-be-truthy)
            ;; Should still have agent ID
            (expect (string-match-p (regexp-quote (claude-agent-id agent)) content)
                   :to-be-truthy)))))

    (it "preserves other agents when updating one agent"
      (let ((agent1 (claude-multi--create-agent "Task 1"))
            (agent2 (claude-multi--create-agent "Task 2")))

        ;; Add both agents
        (claude-multi--add-agent-section agent1)
        (claude-multi--add-agent-section agent2)

        ;; Update only agent1
        (setf (claude-agent-status agent1) 'completed)
        (claude-multi--update-agent-status agent1)

        (with-current-buffer claude-multi--progress-buffer
          (let ((content (buffer-string)))
            ;; Both agents should still be present
            (expect (string-match-p (regexp-quote (claude-agent-id agent1)) content)
                   :to-be-truthy)
            (expect (string-match-p (regexp-quote (claude-agent-id agent2)) content)
                   :to-be-truthy))))))

  (describe "integration: simulated agent launch"

    (it "updates progress buffer when agent is launched"
      (let* ((agent (claude-multi--create-agent "Integration test task"))
             (test-dir "/tmp/test-agent"))

        ;; Set up agent like spawn-agent would
        (setf (claude-agent-worktree-path agent) test-dir)
        (push agent claude-multi--agents)

        ;; Simulate launch (but skip actual kitty commands)
        ;; Just do the progress buffer updates
        (claude-multi--add-agent-section agent)
        (claude-multi--update-session-stats)

        (with-current-buffer claude-multi--progress-buffer
          (let ((content (buffer-string)))
            ;; Verify agent appears in buffer
            (expect (string-match-p (regexp-quote (claude-agent-id agent)) content)
                   :to-be-truthy)
            ;; Verify task description
            (expect (string-match-p "Integration test task" content)
                   :to-be-truthy)
            ;; Verify stats updated
            (expect (string-match-p "Stats :: 1 total" content)
                   :to-be-truthy)))))))

(provide 'test-progress-buffer-sync)
;;; test-progress-buffer-sync.el ends here
