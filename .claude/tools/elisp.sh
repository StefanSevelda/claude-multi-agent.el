#!/bin/bash
# Claude Code tool for Elisp operations
# Provides evaluation, inspection, and analysis capabilities for Emacs Lisp code

set -euo pipefail

# Read JSON input from stdin
INPUT=$(cat)

# Parse operation and params using jq
OPERATION=$(echo "$INPUT" | jq -r '.operation')
SYMBOL=$(echo "$INPUT" | jq -r '.symbol // empty')
FILE=$(echo "$INPUT" | jq -r '.file // empty')
EXPR=$(echo "$INPUT" | jq -r '.expression // empty')

# Get project root (Claude provides this via CWD)
PROJECT_ROOT="${PWD}"

# Invoke elisp-eval based on operation
case "$OPERATION" in
    eval)
        if [ -z "$EXPR" ]; then
            echo '{"status":"error","message":"expression parameter required for eval operation"}'
            exit 1
        fi
        echo "$EXPR" | /Users/stefansevelda/bin/elisp-eval eval "$PROJECT_ROOT"
        ;;

    describe)
        if [ -z "$SYMBOL" ]; then
            echo '{"status":"error","message":"symbol parameter required for describe operation"}'
            exit 1
        fi
        # Filter out "Emacs C source dir:" message from find-function
        /Users/stefansevelda/bin/elisp-eval describe "$PROJECT_ROOT" "$SYMBOL" | sed 's/^Emacs C source dir: //'
        ;;

    find-definition)
        if [ -z "$SYMBOL" ]; then
            echo '{"status":"error","message":"symbol parameter required for find-definition operation"}'
            exit 1
        fi
        # Filter out "Emacs C source dir:" message from find-function
        /Users/stefansevelda/bin/elisp-eval find-definition "$PROJECT_ROOT" "$SYMBOL" | sed 's/^Emacs C source dir: //'
        ;;

    lint)
        if [ -z "$FILE" ]; then
            echo '{"status":"error","message":"file parameter required for lint operation"}'
            exit 1
        fi
        /Users/stefansevelda/bin/elisp-eval lint "$PROJECT_ROOT" "$FILE"
        ;;

    list-symbols)
        if [ -z "$FILE" ]; then
            echo '{"status":"error","message":"file parameter required for list-symbols operation"}'
            exit 1
        fi
        /Users/stefansevelda/bin/elisp-eval list-symbols "$PROJECT_ROOT" "$FILE"
        ;;

    *)
        echo '{"status":"error","message":"Unknown operation: '"$OPERATION"'. Valid operations: eval, describe, find-definition, lint, list-symbols"}'
        exit 1
        ;;
esac
