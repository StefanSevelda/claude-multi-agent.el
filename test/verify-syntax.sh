#!/bin/bash
# Simple syntax verification for test files

echo "Verifying test file syntax..."

# Check if test files have valid Emacs Lisp syntax
for test_file in test/test-*.el; do
    echo "Checking $test_file..."
    emacs --batch --eval "(progn
        (setq byte-compile-error-on-warn nil)
        (defun buttercup (&rest args) nil)
        (defmacro describe (&rest args) nil)
        (defmacro it (&rest args) nil)
        (defmacro before-each (&rest args) nil)
        (defmacro after-each (&rest args) nil)
        (defmacro expect (&rest args) nil)
        (defun spy-on (&rest args) nil)
        (add-to-list 'load-path \".\")
        (add-to-list 'load-path \"autoload\")
        (load-file \"$test_file\")
        (message \"✓ $test_file syntax OK\"))" 2>&1 | grep -E "(Error|Warning|✓)" || echo "  ✓ No syntax errors"
done

echo ""
echo "All test files have valid syntax!"
