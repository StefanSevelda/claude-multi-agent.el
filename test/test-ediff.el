;; -*- lexical-binding: t; -*-
;;; test-ediff.el --- Tests for claude-multi-ediff

;;; Commentary:
;; Comprehensive tests for the Ediff Integration module

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Add parent directory to load path
(let ((project-root (file-name-directory
                     (directory-file-name
                      (file-name-directory load-file-name)))))
  (add-to-list 'load-path project-root)
  (add-to-list 'load-path (expand-file-name "autoload" project-root)))

(require 'claude-multi-ediff)

;;; Test helpers

;; Use the real claude-agent struct
(require 'claude-multi-agents)

(defun test-ediff--make-agent (&optional id)
  "Create a mock agent for testing."
  (make-claude-agent
   :id (or id "test-agent-123")
   :name "Test Agent"
   :status 'running
   :working-directory default-directory
   :ediff-session nil
   :created-at (current-time)
   :kitty-window-id nil
   :worktree-path nil
   :branch-name nil
   :completed-at nil
   :color "#FF4444"
   :kitty-tab-id nil
   :context-buffer nil
   :status-timer nil
   :task-description "Test task"
   :websocket-connection nil
   :communication-backend nil
   :mcp-enabled nil
   :session-id nil
   :mcp-request-counter 0))

;;; Test suite

(describe "Ediff Session Management"

  (describe "claude-multi-ediff--create-session"
    (it "creates a session with provided files"
      (let* ((agent (test-ediff--make-agent))
             (files '("/tmp/file1.el" "/tmp/file2.el"))
             (session (claude-multi-ediff--create-session agent files)))
        (expect session :not :to-be nil)
        (expect (plist-get session :files-to-review) :to-equal files)
        (expect (plist-get session :agent-id) :to-equal "test-agent-123")
        (expect (plist-get session :accepted-files) :to-equal nil)
        (expect (plist-get session :rejected-files) :to-equal nil)))

    (it "detects changed files when none provided"
      (spy-on 'claude-multi-ediff--get-changed-files
              :and-return-value '("/tmp/changed.el"))
      (let* ((agent (test-ediff--make-agent))
             (session (claude-multi-ediff--create-session agent)))
        (expect (plist-get session :files-to-review)
                :to-equal '("/tmp/changed.el"))))

    (it "stores session in agent struct"
      (let* ((agent (test-ediff--make-agent))
             (files '("/tmp/test.el"))
             (session (claude-multi-ediff--create-session agent files)))
        (expect (claude-agent-ediff-session agent) :to-equal session))))

  (describe "claude-multi-ediff--get-changed-files"
    (it "returns list of changed files"
      (spy-on 'shell-command-to-string
              :and-return-value "file1.el\nfile2.el\n")
      (let* ((agent (test-ediff--make-agent))
             (files (claude-multi-ediff--get-changed-files agent)))
        (expect (length files) :to-be 2)
        (expect (car files) :to-match "file1.el$")
        (expect (cadr files) :to-match "file2.el$")))

    (it "returns nil when no changes"
      (spy-on 'shell-command-to-string :and-return-value "")
      (let* ((agent (test-ediff--make-agent))
             (files (claude-multi-ediff--get-changed-files agent)))
        (expect files :to-be nil)))

    (it "uses agent's working directory"
      (spy-on 'shell-command-to-string :and-return-value "")
      (let* ((agent (make-claude-agent
                     :id "test"
                     :name "Test"
                     :status 'running
                     :working-directory "/custom/path"
                     :ediff-session nil
                     :created-at (current-time)
                     :kitty-window-id nil
                     :worktree-path nil
                     :branch-name nil
                     :completed-at nil
                     :color "#FF4444"
                     :kitty-tab-id nil
                     :context-buffer nil
                     :status-timer nil
                     :task-description "Test"
                     :websocket-connection nil
                     :communication-backend nil
                     :mcp-enabled nil
                     :session-id nil
                     :mcp-request-counter 0)))
        (claude-multi-ediff--get-changed-files agent)
        (expect 'shell-command-to-string :to-have-been-called))))

  (describe "claude-multi-ediff--get-worktree-diff"
    (it "returns unified diff output"
      (spy-on 'shell-command-to-string
              :and-return-value "diff --git a/file.el b/file.el\n...")
      (let* ((agent (test-ediff--make-agent))
             (diff (claude-multi-ediff--get-worktree-diff agent)))
        (expect diff :to-match "^diff --git")))))

(describe "File Accept/Reject Operations"

  (describe "claude-multi-ediff--accept-changes"
    (it "adds file to accepted list"
      (let* ((agent (test-ediff--make-agent))
             (session (list :current-file "/tmp/test.el"
                           :files-to-review nil
                           :agent-id "test-agent-123"
                           :accepted-files nil
                           :rejected-files nil)))
        (setf (claude-agent-ediff-session agent) session)
        (spy-on 'claude-multi-ediff--next-or-complete)
        (claude-multi-ediff--accept-changes agent)
        (expect (plist-get session :accepted-files)
                :to-equal '("/tmp/test.el"))
        (expect 'claude-multi-ediff--next-or-complete
                :to-have-been-called-with agent)))

    (it "does nothing when no current file"
      (let* ((agent (test-ediff--make-agent))
             (session (list :current-file nil
                           :accepted-files nil)))
        (setf (claude-agent-ediff-session agent) session)
        (spy-on 'claude-multi-ediff--next-or-complete)
        (claude-multi-ediff--accept-changes agent)
        (expect (plist-get session :accepted-files) :to-equal nil))))

  (describe "claude-multi-ediff--reject-changes"
    (it "adds file to rejected list and reverts"
      (let* ((agent (test-ediff--make-agent))
             (session (list :current-file "/tmp/test.el"
                           :files-to-review nil
                           :agent-id "test-agent-123"
                           :accepted-files nil
                           :rejected-files nil)))
        (setf (claude-agent-ediff-session agent) session)
        (spy-on 'claude-multi-ediff--revert-file)
        (spy-on 'claude-multi-ediff--next-or-complete)
        (claude-multi-ediff--reject-changes agent)
        (expect (plist-get session :rejected-files)
                :to-equal '("/tmp/test.el"))
        (expect 'claude-multi-ediff--revert-file
                :to-have-been-called-with "/tmp/test.el" agent)
        (expect 'claude-multi-ediff--next-or-complete
                :to-have-been-called-with agent)))

    (it "does nothing when no current file"
      (let* ((agent (test-ediff--make-agent))
             (session (list :current-file nil
                           :rejected-files nil)))
        (setf (claude-agent-ediff-session agent) session)
        (spy-on 'claude-multi-ediff--revert-file)
        (claude-multi-ediff--reject-changes agent)
        (expect 'claude-multi-ediff--revert-file :not :to-have-been-called))))

  (describe "claude-multi-ediff--revert-file"
    (it "runs git checkout to revert file"
      (spy-on 'shell-command :and-return-value 0)
      (spy-on 'get-file-buffer :and-return-value nil)
      (let ((agent (test-ediff--make-agent)))
        (claude-multi-ediff--revert-file "/tmp/test.el" agent)
        (expect 'shell-command :to-have-been-called)))

    (it "reverts buffer if file is open"
      (spy-on 'shell-command :and-return-value 0)
      (with-temp-buffer
        (spy-on 'get-file-buffer :and-return-value (current-buffer))
        (spy-on 'revert-buffer)
        (let ((agent (test-ediff--make-agent)))
          (claude-multi-ediff--revert-file "/tmp/test.el" agent)
          (expect 'revert-buffer :to-have-been-called-times 1))))))

(describe "Review Workflow"

  (describe "claude-multi-ediff--next-or-complete"
    (it "shows next file when files remaining"
      (let* ((agent (test-ediff--make-agent))
             (session (list :current-file "/tmp/file1.el"
                           :files-to-review '("/tmp/file2.el" "/tmp/file3.el")
                           :agent-id "test-agent-123")))
        (setf (claude-agent-ediff-session agent) session)
        (spy-on 'ediff-quit)
        (spy-on 'claude-multi-ediff--show-diff)
        (setq ediff-control-buffer nil)
        (claude-multi-ediff--next-or-complete agent)
        (expect (plist-get session :files-to-review)
                :to-equal '("/tmp/file3.el"))
        (expect 'claude-multi-ediff--show-diff
                :to-have-been-called-with agent "/tmp/file2.el")))

    (it "completes review when no files remaining"
      (let* ((agent (test-ediff--make-agent))
             (session (list :current-file "/tmp/file1.el"
                           :files-to-review nil
                           :agent-id "test-agent-123")))
        (setf (claude-agent-ediff-session agent) session)
        (spy-on 'claude-multi-ediff--complete-review)
        (setq ediff-control-buffer nil)
        (claude-multi-ediff--next-or-complete agent)
        (expect 'claude-multi-ediff--complete-review
                :to-have-been-called-with agent)))

    (it "quits ediff control buffer if active"
      (with-temp-buffer
        (setq ediff-control-buffer (current-buffer))
        (spy-on 'ediff-quit)
        (let* ((agent (test-ediff--make-agent))
               (session (list :files-to-review nil)))
          (setf (claude-agent-ediff-session agent) session)
          (spy-on 'claude-multi-ediff--complete-review)
          (claude-multi-ediff--next-or-complete agent)
          (expect 'ediff-quit :to-have-been-called)))))

  (describe "claude-multi-ediff--complete-review"
    (it "sends MCP response with results"
      (spy-on 'claude-multi-mcp--complete-deferred-response)
      (let* ((agent (test-ediff--make-agent))
             (session (list :mcp-request-id "req-123"
                           :accepted-files '("/tmp/a.el" "/tmp/b.el")
                           :rejected-files '("/tmp/c.el")
                           :agent-id "test-agent-123")))
        (setf (claude-agent-ediff-session agent) session)
        (claude-multi-ediff--complete-review agent)
        (expect 'claude-multi-mcp--complete-deferred-response
                :to-have-been-called-times 1)))

    (it "clears session state"
      (let* ((agent (test-ediff--make-agent))
             (session (list :mcp-request-id nil
                           :accepted-files '("/tmp/a.el")
                           :rejected-files nil)))
        (setf (claude-agent-ediff-session agent) session)
        (setq claude-multi--current-ediff-agent agent)
        (setq claude-multi--ediff-control-buffer (current-buffer))
        (claude-multi-ediff--complete-review agent)
        (expect (claude-agent-ediff-session agent) :to-be nil)
        (expect claude-multi--current-ediff-agent :to-be nil)
        (expect claude-multi--ediff-control-buffer :to-be nil)))

    (it "handles case with no MCP request ID"
      (spy-on 'claude-multi-mcp--complete-deferred-response)
      (let* ((agent (test-ediff--make-agent))
             (session (list :mcp-request-id nil
                           :accepted-files nil
                           :rejected-files nil)))
        (setf (claude-agent-ediff-session agent) session)
        (claude-multi-ediff--complete-review agent)
        (expect 'claude-multi-mcp--complete-deferred-response
                :not :to-have-been-called)))))

(describe "MCP Integration"

  (describe "claude-multi-ediff--set-mcp-request-id"
    (it "sets request ID in session"
      (let* ((agent (test-ediff--make-agent))
             (session (list :mcp-request-id nil)))
        (setf (claude-agent-ediff-session agent) session)
        (claude-multi-ediff--set-mcp-request-id agent "req-456")
        (expect (plist-get session :mcp-request-id) :to-equal "req-456")))

    (it "does nothing when no session exists"
      (let ((agent (test-ediff--make-agent)))
        (setf (claude-agent-ediff-session agent) nil)
        (expect (lambda ()
                  (claude-multi-ediff--set-mcp-request-id agent "req-789"))
                :not :to-throw)))))

(describe "Interactive Commands"

  (before-each
    (setq claude-multi--agents nil))

  (describe "claude-multi/review-agent-changes"
    (it "shows message when no agents"
      (setq claude-multi--agents nil)
      (spy-on 'message)
      (claude-multi/review-agent-changes)
      (expect 'message :to-have-been-called-with
              "No active agents to review"))

    (it "shows message when agent has no changes"
      (let ((agent (test-ediff--make-agent)))
        (setq claude-multi--agents (list agent))
        (spy-on 'claude-multi--select-agent :and-return-value agent)
        (spy-on 'claude-multi-ediff--get-changed-files :and-return-value nil)
        (spy-on 'message)
        (claude-multi/review-agent-changes)
        (expect 'message :to-have-been-called-with
                "No changes to review for %s" "Test Agent")))

    (it "starts review when agent has changes"
      (let ((agent (test-ediff--make-agent)))
        (setq claude-multi--agents (list agent))
        (spy-on 'claude-multi--select-agent :and-return-value agent)
        (spy-on 'claude-multi-ediff--get-changed-files
                :and-return-value '("/tmp/file1.el" "/tmp/file2.el"))
        (spy-on 'claude-multi-ediff--show-diff)
        (claude-multi/review-agent-changes)
        (expect 'claude-multi-ediff--show-diff
                :to-have-been-called-with agent "/tmp/file1.el"))))

  (describe "claude-multi/accept-current-diff"
    (it "accepts changes when agent active"
      (let ((agent (test-ediff--make-agent)))
        (setq claude-multi--current-ediff-agent agent)
        (spy-on 'claude-multi-ediff--accept-changes)
        (claude-multi/accept-current-diff)
        (expect 'claude-multi-ediff--accept-changes
                :to-have-been-called-with agent)))

    (it "shows message when no active session"
      (setq claude-multi--current-ediff-agent nil)
      (spy-on 'message)
      (claude-multi/accept-current-diff)
      (expect 'message :to-have-been-called-with
              "No active ediff review session")))

  (describe "claude-multi/reject-current-diff"
    (it "rejects changes when agent active"
      (let ((agent (test-ediff--make-agent)))
        (setq claude-multi--current-ediff-agent agent)
        (spy-on 'claude-multi-ediff--reject-changes)
        (claude-multi/reject-current-diff)
        (expect 'claude-multi-ediff--reject-changes
                :to-have-been-called-with agent)))

    (it "shows message when no active session"
      (setq claude-multi--current-ediff-agent nil)
      (spy-on 'message)
      (claude-multi/reject-current-diff)
      (expect 'message :to-have-been-called-with
              "No active ediff review session")))

  (describe "claude-multi/next-diff-file"
    (it "moves to next file when agent active"
      (let ((agent (test-ediff--make-agent)))
        (setq claude-multi--current-ediff-agent agent)
        (spy-on 'claude-multi-ediff--next-or-complete)
        (claude-multi/next-diff-file)
        (expect 'claude-multi-ediff--next-or-complete
                :to-have-been-called-with agent)))

    (it "shows message when no active session"
      (setq claude-multi--current-ediff-agent nil)
      (spy-on 'message)
      (claude-multi/next-diff-file)
      (expect 'message :to-have-been-called-with
              "No active ediff review session"))))

(describe "Ediff Display"

  (describe "claude-multi-ediff--get-main-version"
    (it "creates temp file with main branch content"
      (spy-on 'shell-command :and-return-value 0)
      (let* ((agent (test-ediff--make-agent))
             (temp-file (claude-multi-ediff--get-main-version "/tmp/test.el" agent)))
        (expect (file-exists-p temp-file) :to-be-truthy)
        (delete-file temp-file)))

    (it "creates empty temp file when git show fails"
      (spy-on 'shell-command :and-return-value 1)
      (let* ((agent (test-ediff--make-agent))
             (temp-file (claude-multi-ediff--get-main-version "/tmp/test.el" agent)))
        (expect (file-exists-p temp-file) :to-be-truthy)
        (expect (with-temp-buffer
                  (insert-file-contents temp-file)
                  (buffer-string))
                :to-equal "")
        (delete-file temp-file)))))

(provide 'test-ediff)
;;; test-ediff.el ends here
