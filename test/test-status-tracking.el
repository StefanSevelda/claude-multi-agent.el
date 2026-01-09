;;; test-status-tracking.el --- Tests for status tracking system -*- lexical-binding: t; -*-

;;; Commentary:
;; These tests are obsolete after the architecture simplification.
;; The old architecture used:
;; - claude-multi--session-to-agent hash table
;; - claude-multi--status-cache hash table
;; - claude-multi--pending-agents list
;; - Complex matching by path normalization
;; - Self-healing rescan mechanisms
;;
;; The new simplified architecture (commit 89855ee) removed all of this.
;; It simply reads all status files from /tmp/claude-status/ and displays
;; them sorted by timestamp. No agent-to-session matching needed.
;;
;; Test coverage now provided by test-status-syntax.el which tests:
;; - claude-multi--get-all-status-files (reads and sorts status files)
;; - claude-multi--handle-directory-event (handles file-notify events)
;; - claude-multi--read-status-file (parses JSON)

;;; Code:

(require 'buttercup)

(describe "Status tracking (deprecated tests)"

  (it "is no longer applicable with simplified architecture"
    ;; The new architecture doesn't use complex matching
    ;; See test-status-syntax.el for relevant tests
    (expect t :to-be-truthy)))

(provide 'test-status-tracking)
;;; test-status-tracking.el ends here
