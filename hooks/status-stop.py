#!/usr/bin/python3
"""
Claude Stop Hook
Updates session status to 'finished' when Claude completes work.
"""

import json
import sys
from datetime import datetime
from pathlib import Path


def load_hook_input():
    """Read hook input from stdin."""
    try:
        return json.load(sys.stdin)
    except Exception as e:
        print(f"Error reading hook input: {e}", file=sys.stderr)
        sys.exit(0)


def update_status_file(session_id, cwd):
    """Update status file to mark session as finished."""
    status_dir = Path("/tmp/claude-status")
    status_file = status_dir / f"status-{session_id}.json"

    if not status_file.exists():
        # Create minimal status file if it doesn't exist
        status_data = {
            "cwd": str(cwd),
            "session_id": session_id,
            "timestamp": datetime.now().isoformat(),
            "claude_status": "finished",
            "waiting_for_input": False
        }
    else:
        # Read existing and update
        try:
            with open(status_file, 'r') as f:
                status_data = json.load(f)
        except:
            status_data = {}

        status_data["claude_status"] = "finished"
        status_data["timestamp"] = datetime.now().isoformat()
        status_data["waiting_for_input"] = False

    # Write atomically
    try:
        status_dir.mkdir(exist_ok=True)
        temp_file = status_file.with_suffix('.tmp')
        with open(temp_file, 'w') as f:
            json.dump(status_data, f, indent=2)
        temp_file.rename(status_file)
    except Exception as e:
        print(f"Error updating status file: {e}", file=sys.stderr)


def main():
    """Main hook execution."""
    hook_data = load_hook_input()

    session_id = hook_data.get("session_id", "unknown")
    cwd = hook_data.get("cwd", "")

    update_status_file(session_id, cwd)

    sys.exit(0)


if __name__ == "__main__":
    main()
