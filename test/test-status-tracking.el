;; -*- lexical-binding: t; -*-
;;; test-status-tracking.el --- Tests for agent status tracking integration

;;; Commentary:
;; Regression tests to verify status tracking works correctly for both
;; new agents and restored agents. Tests the fixes for:
;; 1. Path matching using working-directory instead of default-directory
;; 2. Status registration for restored agents

;;; Code:

(require 'buttercup)
(require 'json)

;; Load test helper
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'test-helper)

;; Load modules under test
(add-to-list 'load-path (expand-file-name "../autoload" (file-name-directory load-file-name)))
(require 'claude-multi-agents)
(require 'claude-multi-status)
(require 'claude-multi-session)

(describe "Status Tracking Integration"

  (before-each
    ;; Reset all state before each test
    (setq claude-multi--agents nil)
    (setq claude-multi--agent-id-counter 0)
    (setq claude-multi--session-start-time nil)
    (setq claude-multi--current-session-window-id nil)
    (setq claude-multi-claude-command "claude")
    ;; Clean up any existing mock status files
    (test-helper--cleanup-mock-status-files)
    ;; Reset status tracking state
    (when (boundp 'claude-multi--session-to-agent)
      (clrhash claude-multi--session-to-agent))
    (when (boundp 'claude-multi--status-cache)
      (clrhash claude-multi--status-cache))
    (when (boundp 'claude-multi--pending-agents)
      (setq claude-multi--pending-agents nil)))

  (after-each
    ;; Clean up buffers and files after each test
    (when claude-multi--agents
      (dolist (agent claude-multi--agents)
        (when-let ((buf (claude-agent-context-buffer agent)))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))
    (setq claude-multi--agents nil)
    (test-helper--cleanup-mock-status-files))

  (describe "Path matching for status file registration"

    (it "matches agent by working-directory when no worktree"
      ;; Create agent with specific working directory
      (let* ((test-dir "/tmp/test-project-a")
             (_ (make-directory test-dir t))
             (default-directory test-dir)
             (agent (claude-multi--create-agent "Test task"))
             (session-id "test-session-123"))
        ;; Verify working-directory was stored
        (expect (claude-agent-working-directory agent) :to-equal test-dir)

        ;; Create mock status file with matching cwd
        (test-helper--create-mock-status-file session-id test-dir)

        ;; Change default-directory to simulate buffer switch
        (let ((default-directory "/tmp/different-directory"))
          ;; Register agent for status (this should use working-directory, not default-directory)
          (claude-multi--register-agent-for-status agent)

          ;; Verify agent was matched and session-id was set
          (expect (claude-agent-session-id agent) :to-equal session-id)
          ;; Verify agent was added to session-to-agent mapping
          (expect (gethash session-id claude-multi--session-to-agent) :to-equal agent))))

    (it "matches agent by worktree-path when worktree exists"
      ;; Create agent with both working-directory and worktree-path
      (let* ((working-dir "/tmp/project-main")
             (worktree-dir "/tmp/project-worktree")
             (_ (progn (make-directory working-dir t) (make-directory worktree-dir t)))
             (default-directory working-dir)
             (agent (claude-multi--create-agent "Test task"))
             (session-id "test-session-456"))
        ;; Set worktree path (simulating worktree creation)
        (setf (claude-agent-worktree-path agent) worktree-dir)

        ;; Create mock status file with worktree cwd (Claude runs in worktree)
        (test-helper--create-mock-status-file session-id worktree-dir)

        ;; Register agent for status
        (claude-multi--register-agent-for-status agent)

        ;; Verify agent was matched using worktree-path (not working-directory)
        (expect (claude-agent-session-id agent) :to-equal session-id)
        (expect (gethash session-id claude-multi--session-to-agent) :to-equal agent)))

    (it "does not match when directories differ"
      ;; Create agent in one directory
      (let* ((agent-dir "/tmp/test-project-a")
             (status-dir "/tmp/test-project-b")
             (_ (progn (make-directory agent-dir t) (make-directory status-dir t)))
             (default-directory agent-dir)
             (agent (claude-multi--create-agent "Test task"))
             (session-id "test-session-789"))
        ;; Create mock status file with different cwd
        (test-helper--create-mock-status-file session-id status-dir)

        ;; Register agent for status
        (claude-multi--register-agent-for-status agent)

        ;; Verify agent was NOT matched (session-id should still be nil)
        (expect (claude-agent-session-id agent) :to-be nil)
        (expect (gethash session-id claude-multi--session-to-agent) :to-be nil))))

  (describe "Session restore with status tracking"

    (it "registers agents for status tracking after restore"
      ;; Create and serialize an agent
      (let* ((test-dir "/tmp/test-restore-project")
             (_ (make-directory test-dir t))
             (default-directory test-dir)
             (agent (claude-multi--create-agent "Test restore task"))
             (session-id "restore-session-001"))
        ;; Set some agent properties
        (setf (claude-agent-status agent) 'running)
        (setf (claude-agent-session-id agent) session-id)
        (setf (claude-agent-kitty-window-id agent) "12345")

        ;; Serialize the agent
        (let* ((agent-plist (claude-multi-session--serialize-agent agent))
               ;; Create mock status file
               (status-file (test-helper--create-mock-status-file session-id test-dir)))

          ;; Verify status file exists
          (expect (file-exists-p status-file) :to-be-truthy)

          ;; Reset state to simulate fresh session
          (setq claude-multi--agents nil)
          (clrhash claude-multi--session-to-agent)
          (setq claude-multi--pending-agents nil)

          ;; Deserialize and register the agent (simulating restore)
          (let ((restored-agent (claude-multi-session--deserialize-agent agent-plist)))
            ;; Manually call registration (this is what the fix adds to restore flow)
            (claude-multi--register-agent-for-status restored-agent)

            ;; Verify agent was registered and matched to status file
            (expect (claude-agent-session-id restored-agent) :to-equal session-id)
            (expect (gethash session-id claude-multi--session-to-agent) :to-equal restored-agent)))))

    (it "picks up existing status files after restore"
      ;; Create agent and status file
      (let* ((test-dir "/tmp/test-pickup-project")
             (_ (make-directory test-dir t))
             (default-directory test-dir)
             (agent (claude-multi--create-agent "Test pickup task"))
             (session-id "pickup-session-002"))
        (setf (claude-agent-status agent) 'running)
        (setf (claude-agent-session-id agent) session-id)
        (setf (claude-agent-kitty-window-id agent) "67890")

        ;; Create status file before restore
        (test-helper--create-mock-status-file session-id test-dir)

        ;; Serialize agent
        (let ((agent-plist (claude-multi-session--serialize-agent agent)))
          ;; Reset state
          (setq claude-multi--agents nil)
          (clrhash claude-multi--session-to-agent)
          (clrhash claude-multi--status-cache)
          (setq claude-multi--pending-agents nil)

          ;; Deserialize and register
          (let ((restored-agent (claude-multi-session--deserialize-agent agent-plist)))
            (claude-multi--register-agent-for-status restored-agent)

            ;; Verify agent picked up the status data
            (expect (claude-agent-last-status-data restored-agent) :to-be-truthy)
            ;; Verify status data has expected fields
            (let ((status-data (claude-agent-last-status-data restored-agent)))
              (expect (alist-get 'session_id status-data) :to-equal session-id)
              (expect (alist-get 'cwd status-data) :to-equal test-dir)
              (expect (alist-get 'claude_status status-data) :to-equal "running"))))))

    (it "updates progress buffer with status after restore"
      ;; This test verifies the full integration
      (let* ((test-dir "/tmp/test-full-integration")
             (_ (make-directory test-dir t))
             (default-directory test-dir)
             (agent (claude-multi--create-agent "Full integration test"))
             (session-id "integration-session-003"))
        (setf (claude-agent-status agent) 'running)
        (setf (claude-agent-session-id agent) session-id)
        (setf (claude-agent-kitty-window-id agent) "99999")

        ;; Create status file with context_window data
        (let ((status-file (expand-file-name
                            (format "status-%s.json" session-id)
                            claude-multi-status-directory)))
          (make-directory claude-multi-status-directory t)
          (with-temp-file status-file
            (insert (json-encode `((cwd . ,test-dir)
                                  (session_id . ,session-id)
                                  (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S"))
                                  (claude_status . "running")
                                  (waiting_for_input . :json-false)
                                  (context_window . ((tokens_used . 5000)
                                                   (tokens_total . 200000)
                                                   (percentage_used . 2.5)))
                                  (git . ((branch . "feature/test"))))))))

          ;; Serialize agent
          (let ((agent-plist (claude-multi-session--serialize-agent agent)))
            ;; Reset state
            (setq claude-multi--agents nil)
            (clrhash claude-multi--session-to-agent)
            (clrhash claude-multi--status-cache)
            (setq claude-multi--pending-agents nil)

            ;; Deserialize and register
            (let ((restored-agent (claude-multi-session--deserialize-agent agent-plist)))
              (claude-multi--register-agent-for-status restored-agent)

              ;; Verify status data was loaded
              (expect (claude-agent-last-status-data restored-agent) :to-be-truthy)
              (let ((status-data (claude-agent-last-status-data restored-agent)))
                ;; Verify context window data
                (let ((context (alist-get 'context_window status-data)))
                  (expect (alist-get 'tokens_used context) :to-equal 5000)
                  (expect (alist-get 'percentage_used context) :to-equal 2.5))
                ;; Verify git data
                (let ((git-info (alist-get 'git status-data)))
                  (expect (alist-get 'branch git-info) :to-equal "feature/test")))))))))

  (describe "Edge cases and error handling"

    (it "handles missing status files gracefully"
      ;; Create agent without corresponding status file
      (let* ((test-dir "/tmp/test-no-status")
             (_ (make-directory test-dir t))
             (default-directory test-dir)
             (agent (claude-multi--create-agent "No status file")))
        ;; Register without creating status file
        (claude-multi--register-agent-for-status agent)

        ;; Agent should be in pending list
        (expect (member agent claude-multi--pending-agents) :to-be-truthy)
        ;; Session ID should still be nil
        (expect (claude-agent-session-id agent) :to-be nil)))

    (it "handles malformed status files gracefully"
      ;; Create agent and malformed status file
      (let* ((test-dir "/tmp/test-malformed")
             (_ (make-directory test-dir t))
             (default-directory test-dir)
             (agent (claude-multi--create-agent "Malformed status"))
             (session-id "malformed-session-999")
             (status-file (expand-file-name
                          (format "status-%s.json" session-id)
                          claude-multi-status-directory)))
        ;; Create malformed JSON file
        (make-directory claude-multi-status-directory t)
        (with-temp-file status-file
          (insert "{ invalid json }"))

        ;; Register agent - should not crash
        (expect (claude-multi--register-agent-for-status agent) :not :to-throw 'error)

        ;; Agent should remain in pending state
        (expect (claude-agent-session-id agent) :to-be nil))))

(provide 'test-status-tracking)
;;; test-status-tracking.el ends here
