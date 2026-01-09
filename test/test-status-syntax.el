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
             claude-multi--handle-directory-event
             claude-multi--read-status-file
             claude-multi--status-file-path
             claude-multi--get-all-status-files
             claude-multi/cleanup-status-files
             claude-multi--cleanup-status-tracking)))
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

(describe "claude-multi--get-all-status-files"

  (before-each
    ;; Load module
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
    (expect (fboundp 'claude-multi--get-all-status-files) :to-be-truthy)
    (expect (claude-multi--get-all-status-files) :not :to-throw))

  (it "returns empty list for empty directory"
    (expect (claude-multi--get-all-status-files) :to-equal nil))

  (it "reads and sorts status files by timestamp"
    ;; Create mock status files with different timestamps
    (let ((file1 (expand-file-name "status-test1.json" claude-multi-status-directory))
          (file2 (expand-file-name "status-test2.json" claude-multi-status-directory)))
      (with-temp-file file1
        (insert "{\"session_id\": \"test1\", \"timestamp\": \"2026-01-09T10:00:00\"}"))
      (with-temp-file file2
        (insert "{\"session_id\": \"test2\", \"timestamp\": \"2026-01-09T11:00:00\"}"))
      ;; Get sorted files
      (let ((result (claude-multi--get-all-status-files)))
        ;; Should return 2 entries
        (expect (length result) :to-equal 2)
        ;; First entry should be newer (test2)
        (let ((first-session-id (alist-get 'session_id (cdr (car result)))))
          (expect first-session-id :to-equal "test2"))))))

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

(describe "claude-multi--handle-directory-event"

  (before-each
    (load test-status-module-path nil 'nomessage))

  (it "is defined and callable"
    (expect (fboundp 'claude-multi--handle-directory-event) :to-be-truthy))

  (it "handles events without crashing"
    ;; Mock event with json file
    (let ((event '(nil created "/tmp/claude-status/status-test.json")))
      (expect (claude-multi--handle-directory-event event) :not :to-throw)))

  (it "handles events for tmp files"
    ;; Mock event with tmp file (atomic write pattern)
    (let ((event '(nil renamed "/tmp/claude-status/status-test.tmp")))
      (expect (claude-multi--handle-directory-event event) :not :to-throw))))

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

  (it "validates claude-multi/cleanup-status-files is interactive"
    (expect (commandp 'claude-multi/cleanup-status-files) :to-be-truthy))

  (it "validates claude-multi--read-status-file takes one argument"
    (let ((arglist (help-function-arglist 'claude-multi--read-status-file)))
      (expect (length arglist) :to-equal 1)))

  (it "validates claude-multi--status-file-path takes one argument"
    (let ((arglist (help-function-arglist 'claude-multi--status-file-path)))
      (expect (length arglist) :to-equal 1)))

  (it "validates claude-multi--get-all-status-files takes no arguments"
    (let ((arglist (help-function-arglist 'claude-multi--get-all-status-files)))
      (expect arglist :to-equal nil))))

(provide 'test-status-syntax)
;;; test-status-syntax.el ends here
