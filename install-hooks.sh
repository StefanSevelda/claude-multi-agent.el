#!/bin/bash
# Installation script for claude-multi-agent hooks
# This script safely installs hooks without overwriting existing Claude configuration

set -e

CLAUDE_DIR="$HOME/.claude"
SETTINGS_FILE="$CLAUDE_DIR/settings.json"

echo "Claude Multi-Agent Hook Installation"
echo "====================================="
echo

# Verify cma binary is available
if ! command -v cma &> /dev/null; then
    echo "❌ ERROR: cma binary not found in PATH"
    echo "   Install cma from: https://github.com/StefanSevelda/cma-agent-framework"
    exit 1
fi

echo "✓ cma binary found: $(which cma)"
echo

# Create .claude directory if it doesn't exist
if [ ! -d "$CLAUDE_DIR" ]; then
    echo "Creating $CLAUDE_DIR directory..."
    mkdir -p "$CLAUDE_DIR"
fi

# Handle settings.json
if [ ! -f "$SETTINGS_FILE" ]; then
    echo "Creating new $SETTINGS_FILE with hook configuration..."
    cat > "$SETTINGS_FILE" << 'EOF'
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit|Bash|Task|AskUserQuestion",
        "hooks": [
          {
            "type": "command",
            "command": "cma hook post-tool-use",
            "timeout": 10
          }
        ]
      }
    ],
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "cma hook stop",
            "timeout": 5
          }
        ]
      }
    ],
    "Notification": [
      {
        "matcher": "",
        "hooks": [
          {
            "type": "command",
            "command": "cma hook notification",
            "timeout": 5
          }
        ]
      }
    ]
  }
}
EOF
    echo "  ✓ settings.json created with hook configuration"
else
    echo "Existing settings.json found. Updating hook configuration..."

    # Backup existing settings
    cp "$SETTINGS_FILE" "$SETTINGS_FILE.bak"
    echo "  - Backup created: $SETTINGS_FILE.bak"

    # Check if Python 3 is available for JSON manipulation
    if ! command -v python3 &> /dev/null; then
        echo
        echo "⚠️  WARNING: python3 not found. Cannot automatically update settings.json"
        echo "   Please manually add the following hooks to your settings.json:"
        echo
        cat << 'EOF'
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit|Bash|Task|AskUserQuestion",
        "hooks": [
          {
            "type": "command",
            "command": "cma hook post-tool-use",
            "timeout": 10
          }
        ]
      }
    ],
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "cma hook stop",
            "timeout": 5
          }
        ]
      }
    ],
    "Notification": [
      {
        "matcher": "",
        "hooks": [
          {
            "type": "command",
            "command": "cma hook notification",
            "timeout": 5
          }
        ]
      }
    ]
  }
EOF
        exit 0
    fi

    # Use Python to merge the settings
    python3 << 'PYTHON_SCRIPT'
import json
import sys
import os

settings_file = os.path.expanduser("~/.claude/settings.json")

# Load existing settings
try:
    with open(settings_file, 'r') as f:
        settings = json.load(f)
except json.JSONDecodeError:
    print(f"  ⚠️  Error: {settings_file} contains invalid JSON")
    print("     Backup created at {}.bak".format(settings_file))
    print("     Please fix the JSON manually and re-run this script")
    sys.exit(1)

# Ensure hooks key exists
if "hooks" not in settings:
    settings["hooks"] = {}

def has_cma_hook(hook_list, command_substring):
    """Check if a cma hook command already exists in the hook list."""
    for hook_config in hook_list:
        if "hooks" in hook_config:
            for hook in hook_config["hooks"]:
                cmd = hook.get("command", "")
                if command_substring in cmd:
                    return True
    return False

def remove_old_python_hooks(hook_list, python_pattern):
    """Remove old Python-based hooks matching the pattern."""
    cleaned = []
    for hook_config in hook_list:
        if "hooks" in hook_config:
            new_hooks = [h for h in hook_config["hooks"] if python_pattern not in h.get("command", "")]
            if new_hooks:
                hook_config["hooks"] = new_hooks
                cleaned.append(hook_config)
        else:
            cleaned.append(hook_config)
    return cleaned

# PostToolUse hook
if "PostToolUse" not in settings["hooks"]:
    settings["hooks"]["PostToolUse"] = []

# Remove old Python hooks if present
settings["hooks"]["PostToolUse"] = remove_old_python_hooks(
    settings["hooks"]["PostToolUse"], "status-summary.py"
)

if not has_cma_hook(settings["hooks"]["PostToolUse"], "cma hook post-tool-use"):
    settings["hooks"]["PostToolUse"].append({
        "matcher": "Write|Edit|Bash|Task|AskUserQuestion",
        "hooks": [
            {
                "type": "command",
                "command": "cma hook post-tool-use",
                "timeout": 10
            }
        ]
    })
    print("  ✓ Added PostToolUse hook (cma hook post-tool-use)")
else:
    print("  - PostToolUse hook already exists")

# Stop hook
if "Stop" not in settings["hooks"]:
    settings["hooks"]["Stop"] = []

settings["hooks"]["Stop"] = remove_old_python_hooks(
    settings["hooks"]["Stop"], "status-stop.py"
)

if not has_cma_hook(settings["hooks"]["Stop"], "cma hook stop"):
    settings["hooks"]["Stop"].append({
        "hooks": [
            {
                "type": "command",
                "command": "cma hook stop",
                "timeout": 5
            }
        ]
    })
    print("  ✓ Added Stop hook (cma hook stop)")
else:
    print("  - Stop hook already exists")

# Notification hook
if "Notification" not in settings["hooks"]:
    settings["hooks"]["Notification"] = []

settings["hooks"]["Notification"] = remove_old_python_hooks(
    settings["hooks"]["Notification"], "status-notification.py"
)

if not has_cma_hook(settings["hooks"]["Notification"], "cma hook notification"):
    settings["hooks"]["Notification"].append({
        "matcher": "",
        "hooks": [
            {
                "type": "command",
                "command": "cma hook notification",
                "timeout": 5
            }
        ]
    })
    print("  ✓ Added Notification hook (cma hook notification)")
else:
    print("  - Notification hook already exists")

# Write updated settings
with open(settings_file, 'w') as f:
    json.dump(settings, f, indent=2)

print(f"  ✓ Settings updated successfully")
PYTHON_SCRIPT
fi

echo
echo "Installation complete!"
echo
echo "The hooks will now track Claude Code agent status in real-time."
echo "Status files are written to: ~/.cma/status/"
echo
echo "To verify the installation:"
echo "  1. Run a Claude Code command that uses Write, Edit, or Bash tools"
echo "  2. Check for status files: ls -l ~/.cma/status/"
echo
