#!/bin/bash
# Claude Hook: PostToolUse
# Runs syntax validation tests after editing claude-multi-status.el
#
# This hook automatically validates syntax when the status module is modified,
# catching parenthesis mismatches and other syntax errors immediately.

set -e

# Parse the tool use event from stdin
TOOL_NAME=$(jq -r '.tool' 2>/dev/null || echo "")
FILE_PATH=$(jq -r '.parameters.file_path // empty' 2>/dev/null || echo "")

# Only run if Edit or Write tool was used on the status module
if [[ "$TOOL_NAME" == "Edit" || "$TOOL_NAME" == "Write" ]]; then
    if [[ "$FILE_PATH" == *"claude-multi-status.el"* ]]; then
        echo ""
        echo "🔍 Running syntax validation tests for claude-multi-status.el..."
        echo ""

        # Run the syntax tests
        if ./test/run-syntax-tests.sh 2>&1 | grep -E "(Test|✓|✗|===)"; then
            echo ""
            echo "✅ Syntax validation passed!"
        else
            echo ""
            echo "❌ Syntax validation failed! Please fix the errors above."
            exit 1
        fi
    fi
fi
