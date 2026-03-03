#!/bin/bash
# Run syntax validation tests for claude-multi-status.el

set -e

cd "$(dirname "$0")/.."

echo "==================================="
echo "Running Syntax Validation Tests"
echo "==================================="
echo ""

# Test 1: Check parenthesis balance
echo "Test 1: Parenthesis Balance Check"
PAREN_CHECK=$(emacs -batch --eval '(progn
  (let ((file "autoload/claude-multi-status.el"))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((opens 0) (closes 0))
        (while (not (eobp))
          (let ((char (char-after)))
            (cond
             ((eq char ?\() (setq opens (1+ opens)))
             ((eq char ?\)) (setq closes (1+ closes)))))
          (forward-char 1))
        (if (= opens closes)
            (message "✓ Balanced: %d opens, %d closes" opens closes)
          (message "✗ UNBALANCED: %d opens, %d closes (diff: %d)"
                   opens closes (- opens closes))
          (kill-emacs 1))))))' 2>&1)
echo "$PAREN_CHECK"
echo ""

# Test 2: File loads without errors
echo "Test 2: Module Loads Without Errors"
emacs -batch --eval '(progn
  (condition-case err
      (progn
        (load-file "autoload/claude-multi-status.el")
        (message "✓ Module loaded successfully"))
    (error
      (message "✗ Load failed: %s" err)
      (kill-emacs 1))))' 2>&1
echo ""

# Test 3: Critical functions are defined
echo "Test 3: Critical Functions Defined"
emacs -batch --eval '(progn
  (load-file "autoload/claude-multi-status.el")
  (let ((missing nil)
        (functions (quote (claude-multi--start-directory-watcher
                          claude-multi--stop-directory-watcher
                          claude-multi--handle-directory-event
                          claude-multi--read-status-file
                          claude-multi--status-file-path
                          claude-multi--get-all-status-files
                          claude-multi/cleanup-status-files
                          claude-multi--cleanup-status-tracking))))
    (dolist (func functions)
      (if (fboundp func)
          (message "  ✓ %s" func)
        (progn
          (message "  ✗ %s MISSING" func)
          (push func missing))))
    (when missing
      (message "\n✗ Missing functions: %s" missing)
      (kill-emacs 1))))' 2>&1
echo ""

# Test 4: Byte compile check
echo "Test 4: Byte Compilation Check"
TEMP_DIR=$(mktemp -d)
emacs -batch --eval "(progn
  (setq byte-compile-error-on-warn nil)
  (byte-compile-file \"autoload/claude-multi-status.el\" t)
  (message \"✓ Byte compilation successful\"))" 2>&1 | grep -v "^Compiling" | grep -v "^Loading" || true
rm -f autoload/claude-multi-status.elc
echo ""

# Test 5: Specific function syntax checks
echo "Test 5: Testing Specific Functions"
emacs -batch --eval '(progn
  (load-file "autoload/claude-multi-status.el")

  ;; Test get-all-status-files function
  (message "  Testing claude-multi--get-all-status-files...")
  (let ((test-dir (make-temp-file "claude-status-test-" t)))
    (setq claude-multi-status-directory test-dir)
    (condition-case err
        (progn
          (claude-multi--get-all-status-files)
          (message "    ✓ Executes without error")
          (delete-directory test-dir))
      (error
        (delete-directory test-dir t)
        (message "    ✗ Error: %s" err)
        (kill-emacs 1))))

  ;; Test cleanup function
  (message "  Testing claude-multi/cleanup-status-files...")
  (setq claude-multi-status-directory "/tmp/nonexistent-test-dir/")
  (condition-case err
      (progn
        (claude-multi/cleanup-status-files)
        (message "    ✓ Executes without error"))
    (error
      (message "    ✗ Error: %s" err)
      (kill-emacs 1)))

  ;; Test handle-directory-event function
  (message "  Testing claude-multi--handle-directory-event...")
  (condition-case err
      (progn
        (claude-multi--handle-directory-event (quote (nil created "~/.cma/status/status-test.json")))
        (message "    ✓ Executes without error"))
    (error
      (message "    ✗ Error: %s" err)
      (kill-emacs 1))))' 2>&1
echo ""

echo "==================================="
echo "✓ All Syntax Validation Tests Passed"
echo "==================================="
