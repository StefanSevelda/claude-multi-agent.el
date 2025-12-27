;; -*- lexical-binding: t; -*-
;;; test-session.el --- Tests for session persistence

;;; Commentary:
;; Comprehensive tests for session save/restore functionality

;;; Code:

(require 'buttercup)

;; Load test helper
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'test-helper)

;; Load modules under test
(add-to-list 'load-path (expand-file-name "../autoload" (file-name-directory load-file-name)))
(require 'claude-multi-agents)
(require 'claude-multi-session)

;;; Test fixtures

(defvar test-session-temp-dir nil
  "Temporary directory for session tests.")

(defun test-session--setup ()
  "Setup test environment."
  ;; Create temporary directory for sessions
  (setq test-session-temp-dir (make-temp-file "claude-session-test-" t))
  (setq claude-multi-session-directory test-session-temp-dir)
  (setq claude-multi-session-autosave-interval 0)  ; Disable autosave during tests
  (setq claude-multi-session-retention-days 0)  ; Disable cleanup during tests
  ;; Reset global state
  (setq claude-multi--agents nil)
  (setq claude-multi--agent-id-counter 0))

(defun test-session--teardown ()
  "Cleanup test environment."
  ;; Delete temporary directory
  (when (and test-session-temp-dir
             (file-exists-p test-session-temp-dir))
    (delete-directory test-session-temp-dir t))
  (setq test-session-temp-dir nil)
  ;; Reset global state
  (setq claude-multi--agents nil)
  (setq claude-multi--agent-id-counter 0))

(defun test-session--create-test-agent (&optional overrides)
  "Create a test agent with optional OVERRIDES plist."
  (let ((agent (make-claude-agent
                :id (or (plist-get overrides :id) "agent-1")
                :name (or (plist-get overrides :name) "claude-agent-1")
                :color (or (plist-get overrides :color) "#FF4444")
                :task-description (or (plist-get overrides :task) "Test task")
                :status (or (plist-get overrides :status) 'running)
                :kitty-window-id (or (plist-get overrides :window-id) "test-window-1")
                :kitty-tab-id (or (plist-get overrides :tab-id) "test-tab-1")
                :worktree-path (or (plist-get overrides :worktree) "/tmp/test-worktree")
                :branch-name (or (plist-get overrides :branch) "test-branch")
                :working-directory (or (plist-get overrides :dir) "/tmp")
                :created-at (or (plist-get overrides :created) (current-time))
                :completed-at (plist-get overrides :completed)
                :communication-backend (or (plist-get overrides :backend) 'polling)
                :mcp-enabled (or (plist-get overrides :mcp) nil)
                :session-id (plist-get overrides :session-id)
                :mcp-request-counter (or (plist-get overrides :counter) 0))))
    agent))

;;; Tests

(describe "Session Serialization"

  (before-each
    (test-session--setup))

  (after-each
    (test-session--teardown))

  (it "serializes agent to plist"
    (let* ((agent (test-session--create-test-agent))
           (plist (claude-multi-session--serialize-agent agent)))
      (expect (plist-get plist :id) :to-equal "agent-1")
      (expect (plist-get plist :name) :to-equal "claude-agent-1")
      (expect (plist-get plist :color) :to-equal "#FF4444")
      (expect (plist-get plist :task-description) :to-equal "Test task")
      (expect (plist-get plist :status) :to-equal "running")
      (expect (plist-get plist :kitty-window-id) :to-equal "test-window-1")
      (expect (plist-get plist :kitty-tab-id) :to-equal "test-tab-1")
      (expect (plist-get plist :worktree-path) :to-equal "/tmp/test-worktree")
      (expect (plist-get plist :branch-name) :to-equal "test-branch")
      (expect (plist-get plist :working-directory) :to-equal "/tmp")
      (expect (plist-get plist :communication-backend) :to-equal "polling")
      (expect (plist-get plist :mcp-enabled) :to-be nil)
      (expect (plist-get plist :mcp-request-counter) :to-equal 0)))

  (it "serializes timestamp as ISO8601 string"
    (let* ((test-time (encode-time 0 30 14 23 12 2025))
           (agent (test-session--create-test-agent (list :created test-time)))
           (plist (claude-multi-session--serialize-agent agent))
           (timestamp (plist-get plist :created-at)))
      (expect timestamp :to-be-truthy)
      (expect (stringp timestamp) :to-be-truthy)
      (expect timestamp :to-match "2025-12-23T14:30:00")))

  (it "handles nil timestamps"
    (let* ((agent (test-session--create-test-agent (list :completed nil)))
           (plist (claude-multi-session--serialize-agent agent)))
      (expect (plist-get plist :completed-at) :to-be nil)))

  (it "serializes WebSocket backend correctly"
    (let* ((agent (test-session--create-test-agent (list :backend 'websocket :mcp t)))
           (plist (claude-multi-session--serialize-agent agent)))
      (expect (plist-get plist :communication-backend) :to-equal "websocket")
      (expect (plist-get plist :mcp-enabled) :to-be t)))

  (it "deserializes plist to agent"
    (let* ((plist '(:id "agent-2"
                    :name "claude-agent-2"
                    :color "#00D9FF"
                    :task-description "Another test"
                    :status "completed"
                    :kitty-window-id "test-window-2"
                    :kitty-tab-id "test-tab-2"
                    :worktree-path "/tmp/worktree2"
                    :branch-name "branch2"
                    :working-directory "/tmp/dir2"
                    :created-at "2025-12-23T10:00:00"
                    :completed-at "2025-12-23T11:00:00"
                    :communication-backend "polling"
                    :mcp-enabled nil
                    :session-id "test-session"
                    :mcp-request-counter 5))
           (agent (claude-multi-session--deserialize-agent plist)))
      (expect (claude-agent-id agent) :to-equal "agent-2")
      (expect (claude-agent-name agent) :to-equal "claude-agent-2")
      (expect (claude-agent-color agent) :to-equal "#00D9FF")
      (expect (claude-agent-task-description agent) :to-equal "Another test")
      (expect (claude-agent-status agent) :to-equal 'completed)
      (expect (claude-agent-kitty-window-id agent) :to-equal "test-window-2")
      (expect (claude-agent-worktree-path agent) :to-equal "/tmp/worktree2")
      (expect (claude-agent-branch-name agent) :to-equal "branch2")
      (expect (claude-agent-working-directory agent) :to-equal "/tmp/dir2")
      (expect (claude-agent-communication-backend agent) :to-equal 'polling)
      (expect (claude-agent-mcp-enabled agent) :to-be nil)
      (expect (claude-agent-session-id agent) :to-equal "test-session")
      (expect (claude-agent-mcp-request-counter agent) :to-equal 5)))

  (it "deserializes timestamps correctly"
    (let* ((plist '(:id "agent-3"
                    :name "test"
                    :color "#FF0000"
                    :task-description "task"
                    :status "running"
                    :created-at "2025-12-23T14:30:00"
                    :communication-backend "polling"
                    :mcp-enabled nil
                    :mcp-request-counter 0))
           (agent (claude-multi-session--deserialize-agent plist))
           (created-time (claude-agent-created-at agent)))
      (expect created-time :to-be-truthy)
      (expect (time-to-seconds created-time) :to-be-greater-than 0)))

  (it "round-trips serialization/deserialization"
    (let* ((original (test-session--create-test-agent))
           (serialized (claude-multi-session--serialize-agent original))
           (deserialized (claude-multi-session--deserialize-agent serialized)))
      (expect (claude-agent-id deserialized) :to-equal (claude-agent-id original))
      (expect (claude-agent-name deserialized) :to-equal (claude-agent-name original))
      (expect (claude-agent-color deserialized) :to-equal (claude-agent-color original))
      ;; Note: status may change during deserialization if kitty window check fails
      ;; So we just verify the status is one of the valid states
      (expect (memq (claude-agent-status deserialized)
                    '(running waiting-input completed failed pending))
              :to-be-truthy)
      (expect (claude-agent-worktree-path deserialized)
              :to-equal (claude-agent-worktree-path original)))))

(describe "Session File Operations"

  (before-each
    (test-session--setup))

  (after-each
    (test-session--teardown))

  (it "ensures session directory exists"
    (claude-multi-session--ensure-directory)
    (expect (file-exists-p claude-multi-session-directory) :to-be-truthy)
    (expect (file-directory-p claude-multi-session-directory) :to-be-truthy))

  (it "generates unique filenames"
    (let ((filename1 (claude-multi-session--generate-filename)))
      (sleep-for 1.1)  ; Ensure different timestamp (need >1 second for timestamp format)
      (let ((filename2 (claude-multi-session--generate-filename)))
        (expect filename1 :to-match "^session-")
        (expect filename2 :to-match "^session-")
        (expect filename1 :not :to-equal filename2))))

  (it "gets correct session path"
    (let ((path (claude-multi-session--get-session-path "2025-12-23-143000")))
      (expect path :to-match "session-2025-12-23-143000\\.el$")))

  (it "handles session-id with .el extension"
    (let ((path1 (claude-multi-session--get-session-path "session-2025-12-23-143000.el"))
          (path2 (claude-multi-session--get-session-path "2025-12-23-143000")))
      (expect path1 :to-match "session-2025-12-23-143000\\.el$")
      (expect path2 :to-match "session-2025-12-23-143000\\.el$")))

  (it "saves session with agents"
    (let ((agent (test-session--create-test-agent)))
      (push agent claude-multi--agents)
      (let ((filepath (claude-multi-session--save)))
        (expect filepath :to-be-truthy)
        (expect (file-exists-p filepath) :to-be-truthy))))

  (it "does not save when no agents exist"
    (setq claude-multi--agents nil)
    (let ((filepath (claude-multi-session--save)))
      (expect filepath :to-be nil)))

  (it "saves valid elisp data"
    (let ((agent (test-session--create-test-agent)))
      (push agent claude-multi--agents)
      (setq claude-multi--agent-id-counter 5)
      (let* ((filepath (claude-multi-session--save))
             (data (with-temp-buffer
                    (insert-file-contents filepath)
                    (read (current-buffer)))))
        (expect (plist-get data :version) :to-equal "1.0")
        (expect (plist-get data :timestamp) :to-be-truthy)
        (expect (length (plist-get data :agents)) :to-equal 1)
        (expect (plist-get (plist-get data :global-state) :agent-id-counter)
                :to-equal 5))))

  (it "restores session from file"
    ;; Save a session
    (let ((agent1 (test-session--create-test-agent (list :id "agent-1")))
          (agent2 (test-session--create-test-agent (list :id "agent-2" :name "claude-agent-2"))))
      (push agent1 claude-multi--agents)
      (push agent2 claude-multi--agents)
      (setq claude-multi--agent-id-counter 10)
      (let ((filepath (claude-multi-session--save)))
        ;; Clear state
        (setq claude-multi--agents nil)
        (setq claude-multi--agent-id-counter 0)
        ;; Restore
        (let ((count (claude-multi-session--restore (file-name-nondirectory filepath))))
          (expect count :to-equal 2)
          (expect (length claude-multi--agents) :to-equal 2)
          (expect claude-multi--agent-id-counter :to-equal 10)))))

  (it "returns 0 when restoring non-existent session"
    (let ((count (claude-multi-session--restore "nonexistent-session.el")))
      (expect count :to-equal 0)))

  (it "validates session version"
    ;; Create a session file with wrong version
    (let ((filepath (expand-file-name "test-session.el" claude-multi-session-directory))
          (bad-data '(:version "999.0" :timestamp "2025-12-23" :agents nil)))
      (with-temp-file filepath
        (prin1 bad-data (current-buffer)))
      (let ((count (claude-multi-session--restore "test-session.el")))
        (expect count :to-equal 0)))))

(describe "Session Listing and Deletion"

  (before-each
    (test-session--setup))

  (after-each
    (test-session--teardown))

  (it "lists sessions sorted by modification time"
    ;; Create multiple sessions
    (dotimes (i 3)
      (let ((agent (test-session--create-test-agent (list :id (format "agent-%d" i)))))
        (push agent claude-multi--agents)
        (claude-multi-session--save)
        (setq claude-multi--agents nil)
        (sleep-for 1.1)))  ; Ensure different timestamps (>1 second for filename uniqueness)
    (let ((sessions (claude-multi-session--list-sessions)))
      (expect (length sessions) :to-equal 3)
      (expect (car sessions) :to-match "^session-")))

  (it "returns empty list when no sessions exist"
    (let ((sessions (claude-multi-session--list-sessions)))
      (expect sessions :to-equal nil)))

  (it "deletes session successfully"
    (let ((agent (test-session--create-test-agent)))
      (push agent claude-multi--agents)
      (let* ((filepath (claude-multi-session--save))
             (session-id (file-name-nondirectory filepath)))
        (expect (file-exists-p filepath) :to-be-truthy)
        (let ((result (claude-multi-session--delete-session session-id)))
          (expect result :to-be t)
          (expect (file-exists-p filepath) :to-be nil)))))

  (it "returns nil when deleting non-existent session"
    (let ((result (claude-multi-session--delete-session "nonexistent.el")))
      (expect result :to-be nil))))

(describe "Session Cleanup"

  (before-each
    (test-session--setup))

  (after-each
    (test-session--teardown))

  (it "cleans up old sessions"
    ;; Create an old session by creating a file with old timestamp
    (let* ((old-file (expand-file-name "session-2020-01-01-120000.el"
                                       claude-multi-session-directory))
           (old-data '(:version "1.0" :timestamp "2020-01-01" :agents nil))
           (old-time (encode-time 0 0 0 1 1 2020)))  ; Jan 1, 2020
      (with-temp-file old-file
        (prin1 old-data (current-buffer)))
      ;; Set file modification time to the past
      (set-file-times old-file old-time)
      ;; Set retention to 1 day
      (setq claude-multi-session-retention-days 1)
      ;; Run cleanup
      (let ((deleted (claude-multi-session--cleanup-old-sessions)))
        (expect deleted :to-equal 1)
        (expect (file-exists-p old-file) :to-be nil))))

  (it "does not clean up recent sessions"
    (let ((agent (test-session--create-test-agent)))
      (push agent claude-multi--agents)
      (let ((filepath (claude-multi-session--save)))
        (setq claude-multi-session-retention-days 1)
        (let ((deleted (claude-multi-session--cleanup-old-sessions)))
          (expect deleted :to-equal 0)
          (expect (file-exists-p filepath) :to-be-truthy)))))

  (it "does not cleanup when retention is 0"
    (let* ((old-file (expand-file-name "session-2020-01-01-120000.el"
                                       claude-multi-session-directory))
           (old-data '(:version "1.0" :timestamp "2020-01-01" :agents nil)))
      (with-temp-file old-file
        (prin1 old-data (current-buffer)))
      (setq claude-multi-session-retention-days 0)
      (let ((deleted (claude-multi-session--cleanup-old-sessions)))
        (expect deleted :to-equal 0)
        (expect (file-exists-p old-file) :to-be-truthy)))))

(describe "Session Metadata"

  (before-each
    (test-session--setup))

  (after-each
    (test-session--teardown))

  (it "extracts session metadata"
    (let ((agent (test-session--create-test-agent)))
      (push agent claude-multi--agents)
      (let* ((filepath (claude-multi-session--save))
             (session-id (file-name-nondirectory filepath))
             (metadata (claude-multi-session--get-session-metadata session-id)))
        (expect (plist-get metadata :timestamp) :to-be-truthy)
        (expect (plist-get metadata :agent-count) :to-equal 1)
        (expect (plist-get metadata :file-size) :to-be-greater-than 0)
        (expect (plist-get metadata :modified) :to-be-truthy))))

  (it "formats file size correctly"
    (expect (claude-multi-session--format-file-size 500) :to-equal "500B")
    (expect (claude-multi-session--format-file-size 1024) :to-equal "1.0KB")
    (expect (claude-multi-session--format-file-size (* 2.5 1024)) :to-equal "2.5KB")
    (expect (claude-multi-session--format-file-size (* 1024 1024)) :to-equal "1.0MB")
    (expect (claude-multi-session--format-file-size (* 3.7 1024 1024)) :to-equal "3.7MB")))

(describe "Session ID Extraction"

  (it "extracts session ID from browser line"
    (with-temp-buffer
      (insert "## session-2025-12-23-143000.el\n")
      (goto-char (point-min))
      (let ((session-id (claude-multi-session--extract-session-id-at-point)))
        (expect session-id :to-equal "session-2025-12-23-143000.el"))))

  (it "returns nil when no session ID at point"
    (with-temp-buffer
      (insert "Some random text\n")
      (goto-char (point-min))
      (let ((session-id (claude-multi-session--extract-session-id-at-point)))
        (expect session-id :to-be nil)))))

(describe "Autosave Timer"

  (before-each
    (test-session--setup))

  (after-each
    (test-session--teardown)
    (claude-multi-session--stop-autosave))

  (it "starts autosave timer"
    (setq claude-multi-session-autosave-interval 10)
    (claude-multi-session--start-autosave)
    (expect claude-multi--session-autosave-timer :to-be-truthy))

  (it "does not start timer when interval is 0"
    (setq claude-multi-session-autosave-interval 0)
    (claude-multi-session--start-autosave)
    (expect claude-multi--session-autosave-timer :to-be nil))

  (it "stops autosave timer"
    (setq claude-multi-session-autosave-interval 10)
    (claude-multi-session--start-autosave)
    (expect claude-multi--session-autosave-timer :to-be-truthy)
    (claude-multi-session--stop-autosave)
    (expect claude-multi--session-autosave-timer :to-be nil)))

(provide 'test-session)
;;; test-session.el ends here
