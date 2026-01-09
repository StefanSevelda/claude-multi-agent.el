;;; test-multi-agent-same-dir.el --- Tests for multiple agents in same directory -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'claude-multi-agents)
(require 'claude-multi-status)

(describe "Multiple agents in same directory"

  (before-each
    ;; Clear all state
    (setq claude-multi--agents nil)
    (setq claude-multi--pending-agents nil)
    (clrhash claude-multi--session-to-agent)
    (clrhash claude-multi--status-cache))

  (describe "claude-multi--find-agent-by-cwd"

    (it "finds first unmapped agent in directory"
      (let* ((dir "/tmp/test-dir")
             (agent1 (make-claude-agent :name "agent-1"
                                       :id "agent-1"
                                       :working-directory dir
                                       :session-id nil))
             (agent2 (make-claude-agent :name "agent-2"
                                       :id "agent-2"
                                       :working-directory dir
                                       :session-id nil)))
        (setq claude-multi--agents (list agent1 agent2))

        ;; Should find agent1 (first unmapped)
        (let ((found (claude-multi--find-agent-by-cwd dir)))
          (expect (claude-agent-name found) :to-equal "agent-1"))))

    (it "skips already mapped agents"
      (let* ((dir "/tmp/test-dir")
             (agent1 (make-claude-agent :name "agent-1"
                                       :id "agent-1"
                                       :working-directory dir
                                       :session-id "session-123"))  ; Already mapped
             (agent2 (make-claude-agent :name "agent-2"
                                       :id "agent-2"
                                       :working-directory dir
                                       :session-id nil)))
        (setq claude-multi--agents (list agent1 agent2))

        ;; Should find agent2 (agent1 is already mapped)
        (let ((found (claude-multi--find-agent-by-cwd dir)))
          (expect (claude-agent-name found) :to-equal "agent-2"))))

    (it "returns nil when all agents are mapped"
      (let* ((dir "/tmp/test-dir")
             (agent1 (make-claude-agent :name "agent-1"
                                       :id "agent-1"
                                       :working-directory dir
                                       :session-id "session-123"))
             (agent2 (make-claude-agent :name "agent-2"
                                       :id "agent-2"
                                       :working-directory dir
                                       :session-id "session-456")))
        (setq claude-multi--agents (list agent1 agent2))

        ;; Should return nil (all mapped)
        (let ((found (claude-multi--find-agent-by-cwd dir)))
          (expect found :to-be nil))))

    (it "normalizes paths correctly"
      (let* ((dir "/tmp/test-dir/")  ; With trailing slash
             (agent (make-claude-agent :name "agent-1"
                                      :id "agent-1"
                                      :working-directory "/tmp/test-dir"  ; Without trailing slash
                                      :session-id nil)))
        (setq claude-multi--agents (list agent))

        ;; Should still find agent despite different path format
        (let ((found (claude-multi--find-agent-by-cwd dir)))
          (expect (claude-agent-name found) :to-equal "agent-1")))))

  (describe "Session mapping with multiple agents"

    (it "maps different sessions to agents in same directory"
      (let* ((dir "/tmp/test-dir")
             (agent1 (make-claude-agent :name "agent-1"
                                       :id "agent-1"
                                       :working-directory dir
                                       :session-id nil))
             (agent2 (make-claude-agent :name "agent-2"
                                       :id "agent-2"
                                       :working-directory dir
                                       :session-id nil))
             (session1 "session-ABC")
             (session2 "session-DEF"))

        (setq claude-multi--agents (list agent1 agent2))

        ;; Simulate first status file appearing
        (let ((agent (claude-multi--find-agent-by-cwd dir)))
          (setf (claude-agent-session-id agent) session1)
          (puthash session1 agent claude-multi--session-to-agent))

        ;; Simulate second status file appearing
        (let ((agent (claude-multi--find-agent-by-cwd dir)))
          (setf (claude-agent-session-id agent) session2)
          (puthash session2 agent claude-multi--session-to-agent))

        ;; Verify both agents have different sessions
        (expect (claude-agent-session-id agent1) :to-equal session1)
        (expect (claude-agent-session-id agent2) :to-equal session2)

        ;; Verify hash table has correct mappings
        (expect (claude-agent-name (gethash session1 claude-multi--session-to-agent))
                :to-equal "agent-1")
        (expect (claude-agent-name (gethash session2 claude-multi--session-to-agent))
                :to-equal "agent-2")))

    (it "prevents double-mapping of same session"
      (let* ((dir "/tmp/test-dir")
             (agent1 (make-claude-agent :name "agent-1"
                                       :id "agent-1"
                                       :working-directory dir
                                       :session-id nil))
             (agent2 (make-claude-agent :name "agent-2"
                                       :id "agent-2"
                                       :working-directory dir
                                       :session-id nil))
             (session "session-ABC"))

        (setq claude-multi--agents (list agent1 agent2))

        ;; Map session to agent1
        (setf (claude-agent-session-id agent1) session)
        (puthash session agent1 claude-multi--session-to-agent)

        ;; Try to find agent for same session again (should fail if already claimed)
        (let ((already-mapped (gethash session claude-multi--session-to-agent)))
          (expect already-mapped :not :to-be nil)
          (expect (claude-agent-name already-mapped) :to-equal "agent-1"))

        ;; Agent2 should not be mapped to this session
        (expect (claude-agent-session-id agent2) :to-be nil))))

  (describe "claude-multi/reset-agent-mappings"

    (it "clears all mappings and session IDs"
      (let* ((agent1 (make-claude-agent :name "agent-1"
                                       :id "agent-1"
                                       :working-directory "/tmp/dir1"
                                       :session-id "session-123"))
             (agent2 (make-claude-agent :name "agent-2"
                                       :id "agent-2"
                                       :working-directory "/tmp/dir2"
                                       :session-id "session-456")))

        (setq claude-multi--agents (list agent1 agent2))
        (puthash "session-123" agent1 claude-multi--session-to-agent)
        (puthash "session-456" agent2 claude-multi--session-to-agent)

        ;; Verify initial state
        (expect (hash-table-count claude-multi--session-to-agent) :to-equal 2)
        (expect (claude-agent-session-id agent1) :to-equal "session-123")
        (expect (claude-agent-session-id agent2) :to-equal "session-456")

        ;; Reset (without rescan since we don't have real status files)
        (clrhash claude-multi--session-to-agent)
        (dolist (agent claude-multi--agents)
          (setf (claude-agent-session-id agent) nil))
        (setq claude-multi--pending-agents (copy-sequence claude-multi--agents))

        ;; Verify reset state
        (expect (hash-table-count claude-multi--session-to-agent) :to-equal 0)
        (expect (claude-agent-session-id agent1) :to-be nil)
        (expect (claude-agent-session-id agent2) :to-be nil)
        (expect (length claude-multi--pending-agents) :to-equal 2))))

  (describe "Integration: Three agents in same directory"

    (it "maps each agent to a unique session"
      (let* ((dir "/tmp/test-dir")
             (agent1 (make-claude-agent :name "agent-1"
                                       :id "agent-1"
                                       :working-directory dir
                                       :session-id nil))
             (agent2 (make-claude-agent :name "agent-2"
                                       :id "agent-2"
                                       :working-directory dir
                                       :session-id nil))
             (agent3 (make-claude-agent :name "agent-3"
                                       :id "agent-3"
                                       :working-directory dir
                                       :session-id nil))
             (sessions '("session-A" "session-B" "session-C")))

        (setq claude-multi--agents (list agent1 agent2 agent3))

        ;; Simulate three status files appearing
        (dolist (session sessions)
          (let ((agent (claude-multi--find-agent-by-cwd dir)))
            (when agent
              (setf (claude-agent-session-id agent) session)
              (puthash session agent claude-multi--session-to-agent))))

        ;; Verify all agents have different sessions
        (expect (claude-agent-session-id agent1) :not :to-be nil)
        (expect (claude-agent-session-id agent2) :not :to-be nil)
        (expect (claude-agent-session-id agent3) :not :to-be nil)

        (let ((session-ids (list (claude-agent-session-id agent1)
                                (claude-agent-session-id agent2)
                                (claude-agent-session-id agent3))))
          ;; All should be unique
          (expect (length (delete-dups session-ids)) :to-equal 3)))))

  (describe "Edge cases"

    (it "handles agent with worktree path vs working directory"
      (let* ((dir "/tmp/test-dir")
             (agent1 (make-claude-agent :name "agent-1"
                                       :id "agent-1"
                                       :worktree-path dir
                                       :working-directory nil
                                       :session-id nil))
             (agent2 (make-claude-agent :name "agent-2"
                                       :id "agent-2"
                                       :worktree-path nil
                                       :working-directory dir
                                       :session-id nil)))

        (setq claude-multi--agents (list agent1 agent2))

        ;; Should find agent1 first (worktree-path takes precedence)
        (let ((found (claude-multi--find-agent-by-cwd dir)))
          (expect (claude-agent-name found) :to-equal "agent-1"))

        ;; Map agent1
        (setf (claude-agent-session-id agent1) "session-1")

        ;; Should now find agent2
        (let ((found (claude-multi--find-agent-by-cwd dir)))
          (expect (claude-agent-name found) :to-equal "agent-2"))))

    (it "handles nil working directory"
      (let* ((agent (make-claude-agent :name "agent-1"
                                      :id "agent-1"
                                      :working-directory nil
                                      :session-id nil)))
        (setq claude-multi--agents (list agent))

        ;; Should not crash, should return nil
        (let ((found (claude-multi--find-agent-by-cwd "/tmp/test-dir")))
          (expect found :to-be nil))))))

(provide 'test-multi-agent-same-dir)
;;; test-multi-agent-same-dir.el ends here
