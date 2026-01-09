#!/bin/bash
# Comprehensive syntax validation for all Emacs Lisp files

set -e

cd "$(dirname "$0")/.."

echo "========================================"
echo "Running Comprehensive Syntax Tests"
echo "========================================"
echo ""

# Find all .el files
EL_FILES=$(find . -name "*.el" -not -path "./.git/*" -not -path "./test/*" | sort)

FAILED_FILES=()
TOTAL_FILES=0

for file in $EL_FILES; do
    TOTAL_FILES=$((TOTAL_FILES + 1))
    echo "=== Testing: $file ==="

    # Test 1: Parenthesis balance
    echo -n "  Parenthesis balance... "
    PAREN_RESULT=$(emacs -batch --eval "(progn
      (with-temp-buffer
        (insert-file-contents \"$file\")
        (goto-char (point-min))
        (let ((opens 0) (closes 0))
          (while (not (eobp))
            (let ((char (char-after)))
              (cond
               ((eq char ?\\\() (setq opens (1+ opens)))
               ((eq char ?\\\)) (setq closes (1+ closes)))))
            (forward-char 1))
          (if (= opens closes)
              (message \"OK: %d/%d\" opens closes)
            (progn
              (message \"FAILED: %d opens, %d closes\" opens closes)
              (kill-emacs 1))))))" 2>&1)

    if [ $? -eq 0 ]; then
        echo "✓ $PAREN_RESULT"
    else
        echo "✗ $PAREN_RESULT"
        FAILED_FILES+=("$file (parenthesis)")
        continue
    fi

    # Test 2: File loads
    echo -n "  Load check... "
    LOAD_RESULT=$(emacs -batch --eval "(condition-case err
        (progn
          (load-file \"$file\")
          (message \"OK\"))
      (error
        (message \"FAILED: %s\" err)
        (kill-emacs 1)))" 2>&1 | tail -1)

    if [ $? -eq 0 ]; then
        echo "✓ $LOAD_RESULT"
    else
        echo "✗ $LOAD_RESULT"
        FAILED_FILES+=("$file (load)")
        continue
    fi

    # Test 3: Byte compile
    echo -n "  Byte compile... "
    COMPILE_RESULT=$(emacs -batch --eval "(progn
      (setq byte-compile-error-on-warn nil)
      (if (byte-compile-file \"$file\")
          (message \"OK\")
        (message \"FAILED\")
        (kill-emacs 1)))" 2>&1 | grep -v "^Compiling" | grep -v "^Loading" | tail -1)

    if [ $? -eq 0 ]; then
        echo "✓ $COMPILE_RESULT"
        # Clean up byte-compiled file
        rm -f "${file}c"
    else
        echo "✗ $COMPILE_RESULT"
        FAILED_FILES+=("$file (byte-compile)")
        rm -f "${file}c"
        continue
    fi

    echo ""
done

echo "========================================"
echo "Summary"
echo "========================================"
echo "Total files tested: $TOTAL_FILES"

if [ ${#FAILED_FILES[@]} -eq 0 ]; then
    echo "✓ All tests passed!"
    exit 0
else
    echo "✗ Failed files:"
    for failed in "${FAILED_FILES[@]}"; do
        echo "  - $failed"
    done
    exit 1
fi
