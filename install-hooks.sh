#!/bin/bash
# Installation script for claude-multi-agent hooks
# This script safely installs hooks without overwriting existing Claude configuration

set -e

CLAUDE_DIR="$HOME/.claude"
HOOKS_DIR="$CLAUDE_DIR/hooks"
SETTINGS_FILE="$CLAUDE_DIR/settings.json"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Claude Multi-Agent Hook Installation"
echo "====================================="
echo

# Create .claude directory if it doesn't exist
if [ ! -d "$CLAUDE_DIR" ]; then
    echo "Creating $CLAUDE_DIR directory..."
    mkdir -p "$CLAUDE_DIR"
fi

# Create hooks directory if it doesn't exist
if [ ! -d "$HOOKS_DIR" ]; then
    echo "Creating $HOOKS_DIR directory..."
    mkdir -p "$HOOKS_DIR"
fi

# Copy hook scripts
echo "Installing hook scripts..."
if [ -f "$HOOKS_DIR/status-summary.py" ]; then
    echo "  - status-summary.py already exists, backing up to status-summary.py.bak"
    cp "$HOOKS_DIR/status-summary.py" "$HOOKS_DIR/status-summary.py.bak"
fi
cp "$SCRIPT_DIR/hooks/status-summary.py" "$HOOKS_DIR/status-summary.py"
chmod +x "$HOOKS_DIR/status-summary.py"
echo "  ✓ status-summary.py installed"

if [ -f "$HOOKS_DIR/status-stop.py" ]; then
    echo "  - status-stop.py already exists, backing up to status-stop.py.bak"
    cp "$HOOKS_DIR/status-stop.py" "$HOOKS_DIR/status-stop.py.bak"
fi
cp "$SCRIPT_DIR/hooks/status-stop.py" "$HOOKS_DIR/status-stop.py"
chmod +x "$HOOKS_DIR/status-stop.py"
echo "  ✓ status-stop.py installed"

# Handle settings.json
if [ ! -f "$SETTINGS_FILE" ]; then
    echo
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
            "command": "python3 ~/.claude/hooks/status-summary.py",
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
            "command": "python3 ~/.claude/hooks/status-stop.py",
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
    echo
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
            "command": "python3 ~/.claude/hooks/status-summary.py",
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
            "command": "python3 ~/.claude/hooks/status-stop.py",
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

# Add PostToolUse hook if not present
if "PostToolUse" not in settings["hooks"]:
    settings["hooks"]["PostToolUse"] = []

# Check if our status-summary hook already exists
status_hook_exists = False
for hook_config in settings["hooks"]["PostToolUse"]:
    if "hooks" in hook_config:
        for hook in hook_config["hooks"]:
            if "status-summary.py" in hook.get("command", ""):
                status_hook_exists = True
                break

# Add status-summary hook if not present
if not status_hook_exists:
    settings["hooks"]["PostToolUse"].append({
        "matcher": "Write|Edit|Bash|Task|AskUserQuestion",
        "hooks": [
            {
                "type": "command",
                "command": "python3 ~/.claude/hooks/status-summary.py",
                "timeout": 10
            }
        ]
    })
    print("  ✓ Added PostToolUse hook for status-summary.py")
else:
    print("  - PostToolUse hook for status-summary.py already exists")

# Add Stop hook if not present
if "Stop" not in settings["hooks"]:
    settings["hooks"]["Stop"] = []

# Check if our status-stop hook already exists
stop_hook_exists = False
for hook_config in settings["hooks"]["Stop"]:
    if "hooks" in hook_config:
        for hook in hook_config["hooks"]:
            if "status-stop.py" in hook.get("command", ""):
                stop_hook_exists = True
                break

# Add status-stop hook if not present
if not stop_hook_exists:
    settings["hooks"]["Stop"].append({
        "hooks": [
            {
                "type": "command",
                "command": "python3 ~/.claude/hooks/status-stop.py",
                "timeout": 5
            }
        ]
    })
    print("  ✓ Added Stop hook for status-stop.py")
else:
    print("  - Stop hook for status-stop.py already exists")

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
