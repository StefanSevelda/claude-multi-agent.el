#!/bin/bash
# emacs-eval.sh - Execute elisp in running Emacs via emacsclient
# Usage: ./emacs-eval.sh '(+ 1 2)'

if [ $# -eq 0 ]; then
    echo "Usage: $0 '<elisp-expression>'"
    echo "Example: $0 '(message \"Hello from Claude\")'"
    exit 1
fi

# Execute elisp and capture result
emacsclient --eval "$1" 2>&1
