;;; test-multi-agent-same-dir.el --- Tests for multiple agents in same directory -*- lexical-binding: t; -*-

;;; Commentary:
;; These tests are obsolete after the architecture simplification.
;; The old architecture used complex agent-to-session matching which
;; had issues with multiple agents in the same directory.
;;
;; The new simplified architecture (commit 89855ee) displays ALL status
;; files from /tmp/claude-status/ regardless of agent tracking, so multiple
;; agents in the same directory work automatically without special handling.
;;
;; Test coverage now provided by test-status-syntax.el which tests:
;; - claude-multi--get-all-status-files (reads all sessions)
;; - claude-multi--handle-directory-event (refreshes on changes)

;;; Code:

(require 'buttercup)

(describe "Multiple agents in same directory (deprecated tests)"

  (it "is no longer applicable with simplified architecture"
    ;; The new architecture doesn't use agent-to-session matching
    ;; See test-status-syntax.el for relevant tests
    (expect t :to-be-truthy)))

(provide 'test-multi-agent-same-dir)
;;; test-multi-agent-same-dir.el ends here
