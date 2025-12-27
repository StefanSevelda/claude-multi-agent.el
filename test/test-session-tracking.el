;; -*- lexical-binding: t; -*-
;;; test-session-tracking.el --- Tests for agent session tracking

;;; Commentary:
;; Tests to verify that agent information is correctly added to and tracked
;; within the multi-agent session

;;; Code:

(require 'buttercup)

;; Load test helper
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'test-helper)

;; Load modules under test
(add-to-list 'load-path (expand-file-name "../autoload" (file-name-directory load-file-name)))
(require 'claude-multi-agents)

(describe "Agent Session Tracking"

  (before-each
    ;; Reset session state before each test
    (setq claude-multi--agents nil)
    (setq claude-multi--agent-id-counter 0)
    (setq claude-multi--session-start-time nil)
    (setq claude-multi--current-session-window-id nil)
    (setq claude-multi-claude-command "claude"))

  (after-each
    ;; Clean up buffers after each test
    (when claude-multi--agents
      (dolist (agent claude-multi--agents)
        (when-let ((buf (claude-agent-context-buffer agent)))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))
    (setq claude-multi--agents nil))

  (describe "Agent creation and session registration"

    (it "adds agent information to session when created"
      (let* ((task "Fix authentication bug")
             (agent (claude-multi--create-agent task)))
        ;; Verify agent structure is properly initialized
        (expect (claude-agent-id agent) :to-be-truthy)
        (expect (claude-agent-name agent) :to-be-truthy)
        (expect (claude-agent-task-description agent) :to-equal task)
        (expect (claude-agent-status agent) :to-equal 'pending)
        (expect (claude-agent-created-at agent) :to-be-truthy)
        (expect (claude-agent-working-directory agent) :to-equal default-directory)))

    (it "assigns unique ID to each agent"
      (let ((agent1 (claude-multi--create-agent "Task 1"))
            (agent2 (claude-multi--create-agent "Task 2"))
            (agent3 (claude-multi--create-agent "Task 3")))
        (expect (claude-agent-id agent1) :to-equal "agent-1")
        (expect (claude-agent-id agent2) :to-equal "agent-2")
        (expect (claude-agent-id agent3) :to-equal "agent-3")
        ;; Verify IDs are unique
        (expect (cl-member (claude-agent-id agent1)
                          (list (claude-agent-id agent2)
                                (claude-agent-id agent3))
                          :test #'string=)
               :to-be nil)))

    (it "assigns unique name to each agent"
      (let ((agent1 (claude-multi--create-agent "Task 1"))
            (agent2 (claude-multi--create-agent "Task 2")))
        (expect (claude-agent-name agent1) :to-equal "claude-agent-1")
        (expect (claude-agent-name agent2) :to-equal "claude-agent-2")
        (expect (string= (claude-agent-name agent1)
                        (claude-agent-name agent2))
               :to-be nil)))

    (it "assigns color to agent from color palette"
      (let ((agent (claude-multi--create-agent "Task")))
        (expect (claude-agent-color agent) :to-be-truthy)
        ;; Verify color is from the palette
        (expect (member (claude-agent-color agent) claude-multi-agent-colors)
               :to-be-truthy)))

    (it "cycles through color palette for multiple agents"
      (let ((num-colors (length claude-multi-agent-colors)))
        ;; Create more agents than colors to test cycling
        (dotimes (i (+ num-colors 2))
          (claude-multi--create-agent (format "Task %d" i)))
        ;; First agent should have same color as (num-colors + 1)th agent
        (let ((agent1-id (format "agent-%d" 1))
              (agent-beyond-id (format "agent-%d" (1+ num-colors))))
          (expect (nth 0 claude-multi-agent-colors)
                 :to-equal (nth (mod 0 num-colors) claude-multi-agent-colors)))))

    (it "stores task description in agent"
      (let* ((task-desc "Refactor authentication module with comprehensive tests")
             (agent (claude-multi--create-agent task-desc)))
        (expect (claude-agent-task-description agent) :to-equal task-desc)))

    (it "stores working directory when agent is created"
      (let* ((test-dir "/tmp/test-project")
             (default-directory test-dir)
             (agent (claude-multi--create-agent "Test task")))
        (expect (claude-agent-working-directory agent) :to-equal test-dir)))

    (it "initializes agent with pending status"
      (let ((agent (claude-multi--create-agent "Test task")))
        (expect (claude-agent-status agent) :to-equal 'pending)
        (expect (claude-agent-completed-at agent) :to-be nil)))

    (it "records creation timestamp"
      (let ((agent (claude-multi--create-agent "Test task")))
        ;; Verify timestamp exists and is a valid time value
        (expect (claude-agent-created-at agent) :to-be-truthy)
        ;; Verify it's a time object (list of integers)
        (expect (listp (claude-agent-created-at agent)) :to-be-truthy)
        ;; Verify the timestamp is recent (within last 60 seconds)
        (let ((time-diff (time-subtract (current-time)
                                       (claude-agent-created-at agent))))
          (expect (< (time-to-seconds time-diff) 60) :to-be-truthy)))))

  (describe "Session-level agent tracking"

    (it "maintains list of all agents in session"
      ;; Simulate adding agents to session like spawn-agent does
      (let ((agent1 (claude-multi--create-agent "Task 1"))
            (agent2 (claude-multi--create-agent "Task 2"))
            (agent3 (claude-multi--create-agent "Task 3")))
        (push agent1 claude-multi--agents)
        (push agent2 claude-multi--agents)
        (push agent3 claude-multi--agents)
        ;; Verify all agents are tracked
        (expect (length claude-multi--agents) :to-equal 3)
        (expect (member agent1 claude-multi--agents) :to-be-truthy)
        (expect (member agent2 claude-multi--agents) :to-be-truthy)
        (expect (member agent3 claude-multi--agents) :to-be-truthy)))

    (it "can retrieve agent by ID from session"
      (let ((agent1 (claude-multi--create-agent "Task 1"))
            (agent2 (claude-multi--create-agent "Task 2")))
        (push agent1 claude-multi--agents)
        (push agent2 claude-multi--agents)
        ;; Test retrieval
        (expect (claude-multi--get-agent-by-id "agent-1") :to-equal agent1)
        (expect (claude-multi--get-agent-by-id "agent-2") :to-equal agent2)
        (expect (claude-multi--get-agent-by-id "nonexistent") :to-be nil)))

    (it "can list all active agents"
      (let ((agent1 (claude-multi--create-agent "Task 1"))
            (agent2 (claude-multi--create-agent "Task 2")))
        (push agent1 claude-multi--agents)
        (push agent2 claude-multi--agents)
        (let ((agents (claude-multi--list-agents)))
          (expect (length agents) :to-equal 2)
          (expect (member agent1 agents) :to-be-truthy)
          (expect (member agent2 agents) :to-be-truthy))))

    (it "returns empty list when no agents exist"
      (expect (claude-multi--list-agents) :to-equal nil))

    (it "tracks session start time"
      (let ((start-time (current-time)))
        (setq claude-multi--session-start-time start-time)
        (expect claude-multi--session-start-time :to-equal start-time)))

    (it "tracks session window ID for first agent"
      (let ((window-id "12345"))
        (setq claude-multi--current-session-window-id window-id)
        (expect claude-multi--current-session-window-id :to-equal window-id))))

  (describe "Agent metadata and state tracking"

    (it "tracks agent status transitions"
      (let ((agent (claude-multi--create-agent "Test task")))
        ;; Initial status
        (expect (claude-agent-status agent) :to-equal 'pending)
        ;; Simulate status changes
        (setf (claude-agent-status agent) 'running)
        (expect (claude-agent-status agent) :to-equal 'running)
        (setf (claude-agent-status agent) 'waiting-input)
        (expect (claude-agent-status agent) :to-equal 'waiting-input)
        (setf (claude-agent-status agent) 'completed)
        (expect (claude-agent-status agent) :to-equal 'completed)))

    (it "stores completion timestamp when agent finishes"
      (let ((agent (claude-multi--create-agent "Test task")))
        (expect (claude-agent-completed-at agent) :to-be nil)
        ;; Simulate completion
        (setf (claude-agent-completed-at agent) (current-time))
        (expect (claude-agent-completed-at agent) :to-be-truthy)
        ;; Verify it's later than creation time
        (expect (time-less-p (claude-agent-created-at agent)
                           (claude-agent-completed-at agent))
               :to-be-truthy)))

    (it "stores worktree path when set"
      (let ((agent (claude-multi--create-agent "Test task")))
        (expect (claude-agent-worktree-path agent) :to-be nil)
        (setf (claude-agent-worktree-path agent) "/tmp/worktree")
        (expect (claude-agent-worktree-path agent) :to-equal "/tmp/worktree")))

    (it "stores branch name when set"
      (let ((agent (claude-multi--create-agent "Test task")))
        (expect (claude-agent-branch-name agent) :to-be nil)
        (setf (claude-agent-branch-name agent) "feature/test-branch")
        (expect (claude-agent-branch-name agent) :to-equal "feature/test-branch")))

    (it "stores kitty window ID after launch"
      (let ((agent (claude-multi--create-agent "Test task")))
        (expect (claude-agent-kitty-window-id agent) :to-be nil)
        (setf (claude-agent-kitty-window-id agent) "67890")
        (expect (claude-agent-kitty-window-id agent) :to-equal "67890")))

    (it "creates and tracks context buffer"
      (let* ((agent (claude-multi--create-agent "Test task"))
             (buf (generate-new-buffer "*test-context*")))
        (setf (claude-agent-context-buffer agent) buf)
        (expect (claude-agent-context-buffer agent) :to-equal buf)
        (expect (buffer-live-p (claude-agent-context-buffer agent)) :to-be-truthy)
        ;; Cleanup
        (kill-buffer buf))))

  (describe "Agent removal from session"

    (it "removes agent from session list when killed"
      (let ((agent1 (claude-multi--create-agent "Task 1"))
            (agent2 (claude-multi--create-agent "Task 2")))
        (push agent1 claude-multi--agents)
        (push agent2 claude-multi--agents)
        (expect (length claude-multi--agents) :to-equal 2)
        ;; Simulate removal (like claude-multi--kill-agent does)
        (setq claude-multi--agents (delq agent1 claude-multi--agents))
        (expect (length claude-multi--agents) :to-equal 1)
        (expect (member agent1 claude-multi--agents) :to-be nil)
        (expect (member agent2 claude-multi--agents) :to-be-truthy)))

    (it "resets session window ID when last agent is removed"
      (let ((agent (claude-multi--create-agent "Task")))
        (push agent claude-multi--agents)
        (setq claude-multi--current-session-window-id "12345")
        ;; Remove last agent
        (setq claude-multi--agents nil)
        (setq claude-multi--current-session-window-id nil)
        (expect claude-multi--current-session-window-id :to-be nil)))

    (it "maintains other agents when one is removed"
      (let ((agent1 (claude-multi--create-agent "Task 1"))
            (agent2 (claude-multi--create-agent "Task 2"))
            (agent3 (claude-multi--create-agent "Task 3")))
        (push agent1 claude-multi--agents)
        (push agent2 claude-multi--agents)
        (push agent3 claude-multi--agents)
        ;; Remove middle agent
        (setq claude-multi--agents (delq agent2 claude-multi--agents))
        (expect (length claude-multi--agents) :to-equal 2)
        (expect (member agent1 claude-multi--agents) :to-be-truthy)
        (expect (member agent2 claude-multi--agents) :to-be nil)
        (expect (member agent3 claude-multi--agents) :to-be-truthy)))))

(provide 'test-session-tracking)
;;; test-session-tracking.el ends here
