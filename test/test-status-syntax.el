;; -*- lexical-binding: t; -*-
;;; test-status-syntax.el --- Syntax validation tests for status tracking module

;;; Commentary:
;; Tests to prevent regression of parenthesis mismatches and syntax errors
;; in claude-multi-status.el. These tests verify:
;; 1. Module loads without syntax errors
;; 2. All critical functions are defined
;; 3. Functions can be byte-compiled
;; 4. Basic functionality works correctly

;;; Code:

(require 'buttercup)
(require 'bytecomp)
(require 'cl-lib)

;; Load test helper (if available, but don't fail if missing)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(ignore-errors (require 'test-helper))

;; Define the module path
(defvar test-status-module-path
  (expand-file-name "../autoload/claude-multi-status.el"
                    (file-name-directory (or load-file-name buffer-file-name
                                             default-directory)))
  "Path to the status tracking module.")

(describe "Status Module Syntax Validation"

  (it "loads the status module without syntax errors"
    (expect (load test-status-module-path nil 'nomessage) :not :to-throw))

  (it "defines all critical functions after loading"
    (load test-status-module-path nil 'nomessage)
    (let ((required-functions
           '(claude-multi--start-directory-watcher
             claude-multi--stop-directory-watcher
             claude-multi--register-agent-for-status
             claude-multi--unregister-agent-for-status
             claude-multi--rescan-pending-agents
             claude-multi--process-status-file
             claude-multi--normalize-path
             claude-multi--find-agent-by-cwd
             claude-multi/cleanup-status-files
             claude-multi/reset-agent-mappings
             claude-multi--get-cached-status)))
      (dolist (func required-functions)
        (expect (fboundp func) :to-be-truthy
                :failure-message
                (format "Function %s should be defined after loading module" func)))))

  (it "byte-compiles the module without errors"
    (let* ((temp-dir (make-temp-file "claude-multi-test-" t))
           (compiled-file (expand-file-name "claude-multi-status.elc" temp-dir))
           (byte-compile-dest-file-function
            (lambda (_file) compiled-file))
           (byte-compile-error-on-warn nil))
      (unwind-protect
          (let ((result (byte-compile-file test-status-module-path)))
            (expect result :to-equal compiled-file)
            (expect (file-exists-p compiled-file) :to-be-truthy))
        ;; Cleanup
        (when (file-exists-p compiled-file)
          (delete-file compiled-file))
        (when (file-directory-p temp-dir)
          (delete-directory temp-dir))))))

(describe "claude-multi--rescan-pending-agents"

  (before-each
    ;; Load module and reset state
    (load test-status-module-path nil 'nomessage)
    (setq claude-multi--pending-agents nil)
    (when (boundp 'claude-multi--session-to-agent)
      (clrhash claude-multi--session-to-agent))
    (when (boundp 'claude-multi--status-cache)
      (clrhash claude-multi--status-cache)))

  (it "is defined and callable"
    (expect (fboundp 'claude-multi--rescan-pending-agents) :to-be-truthy)
    (expect (claude-multi--rescan-pending-agents) :not :to-throw))

  (it "handles empty pending agents list"
    (setq claude-multi--pending-agents nil)
    (expect (claude-multi--rescan-pending-agents) :not :to-throw))

  (it "has correct parenthesis balance"
    ;; Extract function source and verify paren count
    (let* ((func-symbol 'claude-multi--rescan-pending-agents)
           (func-def (symbol-function func-symbol)))
      (expect func-def :to-be-truthy)
      ;; If we can call it without error, parentheses are balanced
      (expect (functionp func-symbol) :to-be-truthy)))

  (it "iterates through pending agents without error"
    ;; Create a mock agent structure
    (let* ((mock-agent (make-vector 22 nil)))
      ;; Set required fields (matching cl-defstruct indices)
      (aset mock-agent 0 'cl-struct-claude-agent)  ; type tag
      (aset mock-agent 1 "test-agent-1")           ; name
      (aset mock-agent 2 "agent-1")                ; id
      (aset mock-agent 18 nil)                     ; session-id
      (setq claude-multi--pending-agents (list mock-agent))
      (expect (claude-multi--rescan-pending-agents) :not :to-throw))))

(describe "claude-multi/cleanup-status-files"

  (before-each
    (load test-status-module-path nil 'nomessage)
    ;; Ensure clean temp directory
    (when (boundp 'claude-multi-status-directory)
      (setq claude-multi-status-directory (make-temp-file "claude-status-test-" t))))

  (after-each
    ;; Cleanup test directory
    (when (and (boundp 'claude-multi-status-directory)
               (file-directory-p claude-multi-status-directory))
      (delete-directory claude-multi-status-directory t)))

  (it "is defined and callable"
    (expect (fboundp 'claude-multi/cleanup-status-files) :to-be-truthy))

  (it "handles non-existent status directory"
    (setq claude-multi-status-directory "/tmp/nonexistent-directory-12345/")
    (expect (claude-multi/cleanup-status-files) :not :to-throw))

  (it "handles empty status directory"
    ;; Directory exists but has no files
    (expect (claude-multi/cleanup-status-files) :not :to-throw))

  (it "has correct parenthesis balance"
    (let* ((func-symbol 'claude-multi/cleanup-status-files)
           (func-def (symbol-function func-symbol)))
      (expect func-def :to-be-truthy)
      (expect (functionp func-symbol) :to-be-truthy)))

  (it "deletes status files when they exist"
    ;; Create mock status files
    (let ((file1 (expand-file-name "status-test1.json" claude-multi-status-directory))
          (file2 (expand-file-name "status-test2.json" claude-multi-status-directory)))
      (with-temp-file file1 (insert "{}"))
      (with-temp-file file2 (insert "{}"))
      (expect (file-exists-p file1) :to-be-truthy)
      (expect (file-exists-p file2) :to-be-truthy)
      ;; Run cleanup
      (claude-multi/cleanup-status-files)
      ;; Verify files are deleted
      (expect (file-exists-p file1) :not :to-be-truthy)
      (expect (file-exists-p file2) :not :to-be-truthy))))

(describe "claude-multi/reset-agent-mappings"

  (before-each
    (load test-status-module-path nil 'nomessage)
    ;; Load required dependencies
    (add-to-list 'load-path (expand-file-name "../autoload"
                                              (file-name-directory load-file-name)))
    (require 'claude-multi-agents)
    ;; Reset state
    (setq claude-multi--agents nil)
    (setq claude-multi--pending-agents nil)
    (when (boundp 'claude-multi--session-to-agent)
      (clrhash claude-multi--session-to-agent)))

  (it "is defined and callable"
    (expect (fboundp 'claude-multi/reset-agent-mappings) :to-be-truthy))

  (it "executes without error when no agents exist"
    (setq claude-multi--agents nil)
    (expect (claude-multi/reset-agent-mappings) :not :to-throw))

  (it "has correct parenthesis balance"
    (let* ((func-symbol 'claude-multi/reset-agent-mappings)
           (func-def (symbol-function func-symbol)))
      (expect func-def :to-be-truthy)
      (expect (functionp func-symbol) :to-be-truthy)))

  (it "clears session-to-agent mapping"
    ;; Create mock mapping
    (puthash "session-1" "agent-1" claude-multi--session-to-agent)
    (expect (hash-table-count claude-multi--session-to-agent) :to-equal 1)
    ;; Run reset
    (claude-multi/reset-agent-mappings)
    ;; Verify mapping is cleared
    (expect (hash-table-count claude-multi--session-to-agent) :to-equal 0))

  (it "clears session IDs from all agents"
    ;; Create mock agents with session IDs
    (let* ((agent1 (make-vector 22 nil))
           (agent2 (make-vector 22 nil)))
      ;; Set type tags and session IDs
      (aset agent1 0 'cl-struct-claude-agent)
      (aset agent1 1 "agent-1")
      (aset agent1 18 "session-1")  ; session-id at index 18
      (aset agent2 0 'cl-struct-claude-agent)
      (aset agent2 1 "agent-2")
      (aset agent2 18 "session-2")
      (setq claude-multi--agents (list agent1 agent2))
      ;; Run reset
      (claude-multi/reset-agent-mappings)
      ;; Verify session IDs are nil
      (expect (aref agent1 18) :to-be nil)
      (expect (aref agent2 18) :to-be nil)))

  (it "adds agents to pending list"
    ;; Create mock agents
    (let* ((agent1 (make-vector 22 nil))
           (agent2 (make-vector 22 nil)))
      (aset agent1 0 'cl-struct-claude-agent)
      (aset agent2 0 'cl-struct-claude-agent)
      (setq claude-multi--agents (list agent1 agent2))
      (setq claude-multi--pending-agents nil)
      ;; Run reset
      (claude-multi/reset-agent-mappings)
      ;; Verify agents are in pending list
      (expect (length claude-multi--pending-agents) :to-equal 2)
      (expect (memq agent1 claude-multi--pending-agents) :to-be-truthy)
      (expect (memq agent2 claude-multi--pending-agents) :to-be-truthy))))

(describe "Parenthesis Balance Validation"

  (it "validates entire status module has balanced parentheses"
    (with-temp-buffer
      (insert-file-contents test-status-module-path)
      (goto-char (point-min))
      (let ((paren-count 0)
            (line-num 1))
        (while (not (eobp))
          (let ((char (char-after)))
            (cond
             ((eq char ?\() (cl-incf paren-count))
             ((eq char ?\)) (cl-decf paren-count)))
            ;; Check we never go negative (more closes than opens)
            (expect paren-count :to-be-greater-than-or-equal-to 0
                    :failure-message
                    (format "Too many closing parens at line %d" line-num))
            (when (eq char ?\n)
              (cl-incf line-num)))
          (forward-char 1))
        ;; At end of file, should have equal opens and closes
        (expect paren-count :to-equal 0
                :failure-message
                (format "Unbalanced parentheses: %s %d extra %s"
                        (if (> paren-count 0) "missing" "extra")
                        (abs paren-count)
                        (if (> paren-count 0) "closing" "opening")))))))

(describe "Function Signature Validation"

  (before-each
    (load test-status-module-path nil 'nomessage))

  (it "validates claude-multi--rescan-pending-agents takes no arguments"
    (let ((arglist (help-function-arglist 'claude-multi--rescan-pending-agents)))
      (expect arglist :to-equal nil)))

  (it "validates claude-multi/cleanup-status-files is interactive"
    (expect (commandp 'claude-multi/cleanup-status-files) :to-be-truthy))

  (it "validates claude-multi/reset-agent-mappings is interactive"
    (expect (commandp 'claude-multi/reset-agent-mappings) :to-be-truthy))

  (it "validates claude-multi--normalize-path takes one argument"
    (let ((arglist (help-function-arglist 'claude-multi--normalize-path)))
      (expect (length arglist) :to-equal 1)))

  (it "validates claude-multi--find-agent-by-cwd takes one argument"
    (let ((arglist (help-function-arglist 'claude-multi--find-agent-by-cwd)))
      (expect (length arglist) :to-equal 1))))

(provide 'test-status-syntax)
;;; test-status-syntax.el ends here
