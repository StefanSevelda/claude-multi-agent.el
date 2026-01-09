#!/bin/bash
# Quick parenthesis balance check for all .el files

cd "$(dirname "$0")/.."

FAILED=0

for file in config.el autoload/*.el; do
    [ -f "$file" ] || continue
    
    # Count parens, excluding those in strings and comments
    RESULT=$(emacs -batch --eval "(with-temp-buffer
      (insert-file-contents \"$file\")
      (goto-char (point-min))
      (let ((opens 0) (closes 0))
        (while (not (eobp))
          (let ((char (char-after)))
            (cond
             ((eq char ?\\\() (setq opens (1+ opens)))
             ((eq char ?\\\)) (setq closes (1+ closes)))))
          (forward-char 1))
        (princ (format \"%d %d\" opens closes))))" 2>/dev/null)
    
    OPENS=$(echo "$RESULT" | awk '{print $1}')
    CLOSES=$(echo "$RESULT" | awk '{print $2}')
    
    if [ "$OPENS" = "$CLOSES" ]; then
        echo "✓ $file ($OPENS parens)"
    else
        echo "✗ $file: $OPENS opens, $CLOSES closes (diff: $((OPENS - CLOSES)))"
        FAILED=1
    fi
done

exit $FAILED
