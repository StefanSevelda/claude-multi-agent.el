;; -*- lexical-binding: t; -*-
;;; test-ediff.el --- Tests for claude-multi-ediff (cma backend)

;;; Commentary:
;; Tests for the Ediff Integration module, using cma CLI backend.
;; Agents are alists from `cma list --json`, not cl-defstructs.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(load-file "test/test-helper.el")

(require 'cma-core)
(require 'claude-multi-ediff)

;;; Test helpers

(defun test-ediff--make-agent-alist (&optional id)
  "Create a mock agent alist for testing."
  `((session_id . ,(or id "test-session-123"))
    (name . "Test Agent")
    (status . "running")
    (cwd . ,(expand-file-name default-directory))))

;;; Test suite

(describe "Ediff Session Management"

  (before-each
    (setq claude-multi--ediff-session nil)
    (setq claude-multi--current-ediff-agent nil)
    (setq claude-multi--ediff-control-buffer nil))

  (describe "claude-multi-ediff--create-session"
    (it "creates a session with provided files"
      (let* ((agent (test-ediff--make-agent-alist))
             (files '("/tmp/file1.el" "/tmp/file2.el"))
             (session (claude-multi-ediff--create-session agent files)))
        (expect session :not :to-be nil)
        (expect (plist-get session :files-to-review) :to-equal files)
        (expect (plist-get session :agent-id) :to-equal "test-session-123")
        (expect (plist-get session :accepted-files) :to-equal nil)
        (expect (plist-get session :rejected-files) :to-equal nil)))

    (it "detects changed files when none provided"
      (spy-on 'claude-multi-ediff--get-changed-files
              :and-return-value '("/tmp/changed.el"))
      (let* ((agent (test-ediff--make-agent-alist))
             (session (claude-multi-ediff--create-session agent)))
        (expect (plist-get session :files-to-review)
                :to-equal '("/tmp/changed.el"))))

    (it "stores session in global variable"
      (let* ((agent (test-ediff--make-agent-alist))
             (files '("/tmp/test.el"))
             (session (claude-multi-ediff--create-session agent files)))
        (expect claude-multi--ediff-session :to-equal session))))

  (describe "claude-multi-ediff--get-changed-files"
    (it "returns list of changed files via cma"
      (spy-on 'cma--call :and-return-value '("file1.el" "file2.el"))
      (let ((files (claude-multi-ediff--get-changed-files "/tmp/project")))
        (expect (length files) :to-be 2)
        (expect (car files) :to-match "file1.el$")
        (expect (cadr files) :to-match "file2.el$")))

    (it "returns nil when no changes"
      (spy-on 'cma--call :and-return-value nil)
      (let ((files (claude-multi-ediff--get-changed-files "/tmp/project")))
        (expect files :to-be nil)))

    (it "passes correct args to cma"
      (spy-on 'cma--call :and-return-value nil)
      (claude-multi-ediff--get-changed-files "/custom/path")
      (let ((call-args (spy-calls-args-for 'cma--call 0)))
        (expect (car call-args) :to-equal "git")
        (expect (nth 1 call-args) :to-equal "changed-files")
        (expect (member "--dir" call-args) :to-be-truthy)
        (expect (member "--json" call-args) :to-be-truthy))))

  (describe "claude-multi-ediff--get-worktree-diff"
    (it "returns unified diff output via cma"
      (spy-on 'cma--call-raw
              :and-return-value "diff --git a/file.el b/file.el\n...")
      (let ((diff (claude-multi-ediff--get-worktree-diff "/tmp/project")))
        (expect diff :to-match "^diff --git")))))

(describe "File Accept/Reject Operations"

  (before-each
    (setq claude-multi--ediff-session nil)
    (setq claude-multi--current-ediff-agent nil)
    (setq claude-multi--ediff-control-buffer nil))

  (describe "claude-multi-ediff--accept-changes"
    (it "adds file to accepted list"
      (setq claude-multi--ediff-session
            (list :current-file "/tmp/test.el"
                  :files-to-review nil
                  :agent-id "test-session-123"
                  :accepted-files nil
                  :rejected-files nil
                  :working-directory "/tmp"
                  :mcp-request-id nil))
      (spy-on 'claude-multi-ediff--next-or-complete)
      (claude-multi-ediff--accept-changes)
      (expect (plist-get claude-multi--ediff-session :accepted-files)
              :to-equal '("/tmp/test.el"))
      (expect 'claude-multi-ediff--next-or-complete :to-have-been-called))

    (it "does nothing when no current file"
      (setq claude-multi--ediff-session
            (list :current-file nil :accepted-files nil))
      (spy-on 'claude-multi-ediff--next-or-complete)
      (claude-multi-ediff--accept-changes)
      (expect (plist-get claude-multi--ediff-session :accepted-files) :to-equal nil)))

  (describe "claude-multi-ediff--reject-changes"
    (it "adds file to rejected list and reverts"
      (setq claude-multi--ediff-session
            (list :current-file "/tmp/test.el"
                  :files-to-review nil
                  :agent-id "test-session-123"
                  :accepted-files nil
                  :rejected-files nil
                  :working-directory "/tmp"
                  :mcp-request-id nil))
      (spy-on 'claude-multi-ediff--revert-file)
      (spy-on 'claude-multi-ediff--next-or-complete)
      (claude-multi-ediff--reject-changes)
      (expect (plist-get claude-multi--ediff-session :rejected-files)
              :to-equal '("/tmp/test.el"))
      (expect 'claude-multi-ediff--revert-file
              :to-have-been-called-with "/tmp/test.el"))

    (it "does nothing when no current file"
      (setq claude-multi--ediff-session
            (list :current-file nil :rejected-files nil))
      (spy-on 'claude-multi-ediff--revert-file)
      (claude-multi-ediff--reject-changes)
      (expect 'claude-multi-ediff--revert-file :not :to-have-been-called)))

  (describe "claude-multi-ediff--revert-file"
    (it "calls cma git checkout to revert file"
      (setq claude-multi--ediff-session
            (list :working-directory "/tmp/project"))
      (spy-on 'cma--call-raw)
      (spy-on 'get-file-buffer :and-return-value nil)
      (claude-multi-ediff--revert-file "/tmp/project/test.el")
      (expect 'cma--call-raw :to-have-been-called))

    (it "reverts buffer if file is open"
      (setq claude-multi--ediff-session
            (list :working-directory "/tmp/project"))
      (spy-on 'cma--call-raw)
      (with-temp-buffer
        (spy-on 'get-file-buffer :and-return-value (current-buffer))
        (spy-on 'revert-buffer)
        (claude-multi-ediff--revert-file "/tmp/project/test.el")
        (expect 'revert-buffer :to-have-been-called-times 1)))))

(describe "Review Workflow"

  (before-each
    (setq claude-multi--ediff-session nil)
    (setq claude-multi--current-ediff-agent nil)
    (setq claude-multi--ediff-control-buffer nil))

  (describe "claude-multi-ediff--next-or-complete"
    (it "shows next file when files remaining"
      (setq claude-multi--ediff-session
            (list :current-file "/tmp/file1.el"
                  :files-to-review '("/tmp/file2.el" "/tmp/file3.el")
                  :agent-id "test-session-123"
                  :working-directory "/tmp"))
      (spy-on 'claude-multi-ediff--show-diff)
      (setq ediff-control-buffer nil)
      (claude-multi-ediff--next-or-complete)
      (expect (plist-get claude-multi--ediff-session :files-to-review)
              :to-equal '("/tmp/file3.el"))
      (expect 'claude-multi-ediff--show-diff
              :to-have-been-called-with "/tmp/file2.el"))

    (it "completes review when no files remaining"
      (setq claude-multi--ediff-session
            (list :current-file "/tmp/file1.el"
                  :files-to-review nil
                  :agent-id "test-session-123"
                  :accepted-files nil
                  :rejected-files nil
                  :mcp-request-id nil))
      (spy-on 'claude-multi-ediff--complete-review)
      (setq ediff-control-buffer nil)
      (claude-multi-ediff--next-or-complete)
      (expect 'claude-multi-ediff--complete-review :to-have-been-called))

    (it "quits ediff control buffer if active"
      (with-temp-buffer
        (setq ediff-control-buffer (current-buffer))
        (spy-on 'ediff-quit)
        (setq claude-multi--ediff-session
              (list :files-to-review nil
                    :accepted-files nil
                    :rejected-files nil
                    :mcp-request-id nil))
        (spy-on 'claude-multi-ediff--complete-review)
        (claude-multi-ediff--next-or-complete)
        (expect 'ediff-quit :to-have-been-called))))

  (describe "claude-multi-ediff--complete-review"
    (it "sends MCP response with results"
      (spy-on 'claude-multi-mcp--complete-deferred-response)
      (setq claude-multi--ediff-session
            (list :mcp-request-id "req-123"
                  :accepted-files '("/tmp/a.el" "/tmp/b.el")
                  :rejected-files '("/tmp/c.el")
                  :agent-id "test-session-123"))
      (claude-multi-ediff--complete-review)
      (expect 'claude-multi-mcp--complete-deferred-response
              :to-have-been-called-times 1))

    (it "clears session state"
      (setq claude-multi--ediff-session
            (list :mcp-request-id nil
                  :accepted-files '("/tmp/a.el")
                  :rejected-files nil
                  :agent-id "test"))
      (setq claude-multi--current-ediff-agent '((name . "test")))
      (setq claude-multi--ediff-control-buffer (current-buffer))
      (claude-multi-ediff--complete-review)
      (expect claude-multi--ediff-session :to-be nil)
      (expect claude-multi--current-ediff-agent :to-be nil)
      (expect claude-multi--ediff-control-buffer :to-be nil))

    (it "handles case with no MCP request ID"
      (spy-on 'claude-multi-mcp--complete-deferred-response)
      (setq claude-multi--ediff-session
            (list :mcp-request-id nil
                  :accepted-files nil
                  :rejected-files nil
                  :agent-id "test"))
      (claude-multi-ediff--complete-review)
      (expect 'claude-multi-mcp--complete-deferred-response
              :not :to-have-been-called))))

(describe "MCP Integration"

  (before-each
    (setq claude-multi--ediff-session nil))

  (describe "claude-multi-ediff--set-mcp-request-id"
    (it "sets request ID in session"
      (setq claude-multi--ediff-session (list :mcp-request-id nil))
      (claude-multi-ediff--set-mcp-request-id "req-456")
      (expect (plist-get claude-multi--ediff-session :mcp-request-id)
              :to-equal "req-456"))

    (it "does nothing when no session exists"
      (setq claude-multi--ediff-session nil)
      ;; Should not throw when no ediff session exists
      (expect (claude-multi-ediff--set-mcp-request-id "req-789")
              :not :to-throw))))

(describe "Interactive Commands"

  (before-each
    (setq claude-multi--ediff-session nil)
    (setq claude-multi--current-ediff-agent nil))

  (describe "claude-multi/review-agent-changes"
    (it "shows message when agent has no changes"
      (spy-on 'cma--call :and-return-value
              '(((session_id . "s1") (name . "Test") (status . "running") (cwd . "/tmp"))))
      (spy-on 'completing-read :and-return-value "Test [RUNNING] - /tmp")
      (spy-on 'claude-multi-ediff--get-changed-files :and-return-value nil)
      (spy-on 'message)
      (claude-multi/review-agent-changes)
      (expect 'message :to-have-been-called))

    (it "starts review when agent has changes"
      (spy-on 'cma--call :and-return-value
              '(((session_id . "s1") (name . "Test") (status . "running") (cwd . "/tmp"))))
      (spy-on 'completing-read :and-return-value "Test [RUNNING] - /tmp")
      (spy-on 'claude-multi-ediff--get-changed-files
              :and-return-value '("/tmp/file1.el" "/tmp/file2.el"))
      (spy-on 'claude-multi-ediff--show-diff)
      (claude-multi/review-agent-changes)
      (expect 'claude-multi-ediff--show-diff
              :to-have-been-called-with "/tmp/file1.el")))

  (describe "claude-multi/accept-current-diff"
    (it "accepts changes when session active"
      (setq claude-multi--ediff-session (list :current-file "/tmp/test.el"))
      (spy-on 'claude-multi-ediff--accept-changes)
      (claude-multi/accept-current-diff)
      (expect 'claude-multi-ediff--accept-changes :to-have-been-called))

    (it "shows message when no active session"
      (setq claude-multi--ediff-session nil)
      (spy-on 'message)
      (claude-multi/accept-current-diff)
      (expect 'message :to-have-been-called-with
              "No active ediff review session")))

  (describe "claude-multi/reject-current-diff"
    (it "rejects changes when session active"
      (setq claude-multi--ediff-session (list :current-file "/tmp/test.el"))
      (spy-on 'claude-multi-ediff--reject-changes)
      (claude-multi/reject-current-diff)
      (expect 'claude-multi-ediff--reject-changes :to-have-been-called))

    (it "shows message when no active session"
      (setq claude-multi--ediff-session nil)
      (spy-on 'message)
      (claude-multi/reject-current-diff)
      (expect 'message :to-have-been-called-with
              "No active ediff review session")))

  (describe "claude-multi/next-diff-file"
    (it "moves to next file when session active"
      (setq claude-multi--ediff-session (list :current-file "/tmp/test.el"))
      (spy-on 'claude-multi-ediff--next-or-complete)
      (claude-multi/next-diff-file)
      (expect 'claude-multi-ediff--next-or-complete :to-have-been-called))

    (it "shows message when no active session"
      (setq claude-multi--ediff-session nil)
      (spy-on 'message)
      (claude-multi/next-diff-file)
      (expect 'message :to-have-been-called-with
              "No active ediff review session"))))

(provide 'test-ediff)
;;; test-ediff.el ends here
