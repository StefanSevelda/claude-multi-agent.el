# Claude Multi-Agent Hooks

This directory contains Claude Code hooks that enable real-time status tracking for the claude-multi-agent.el Emacs plugin.

## Files

- **status-summary.py** - PostToolUse hook that writes status information to `~/.cma/status/status-{session_id}.json` after each tool use
- **status-stop.py** - Stop hook that marks the session as finished when Claude stops

## Installation

Run the installation script from the project root:

```bash
make install-hooks
# or
./install-hooks.sh
```

The script will:
1. Create `~/.claude/hooks/` directory if it doesn't exist
2. Copy hook scripts to `~/.claude/hooks/`
3. Update `~/.claude/settings.json` to register the hooks (without overwriting existing configuration)
4. Create backups of any existing files before replacing them

## Manual Installation

If you prefer to install manually:

1. Copy the hook scripts:
   ```bash
   cp hooks/status-summary.py ~/.claude/hooks/
   cp hooks/status-stop.py ~/.claude/hooks/
   chmod +x ~/.claude/hooks/*.py
   ```

2. Add the following to your `~/.claude/settings.json`:
   ```json
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
   ```

## Status File Format

The hooks write JSON files to `~/.cma/status/` with the following structure:

```json
{
  "cwd": "/path/to/working/directory",
  "session_id": "uuid",
  "timestamp": "2026-01-08T12:23:22.778699",
  "session_started": "2026-01-08T11:19:00.108073",
  "claude_status": "running",
  "waiting_for_input": false,
  "current_activity": {
    "goal": "Implementing changes",
    "waiting": false
  },
  "context_window": {
    "tokens_used": 150925,
    "tokens_total": 200000,
    "percentage_used": 75.46,
    "percentage_remaining": 24.54,
    "tokens_remaining": 49075
  },
  "git": {
    "branch": "main",
    "repository": "repo-name"
  }
}
```

## How It Works

1. **PostToolUse Hook**: Triggered after each tool use (Write, Edit, Bash, Task, AskUserQuestion)
   - Updates session state (changes, activity, waiting status)
   - Collects context window information
   - Gathers git repository details
   - Writes status to session-specific JSON file

2. **Stop Hook**: Triggered when Claude session ends
   - Marks the session as finished
   - Updates final status

3. **Emacs Integration**: The `claude-multi-status.el` module uses `file-notify` (inotify/kqueue) to watch the status directory and automatically updates agent status in real-time when files change.

## Troubleshooting

### Hooks not running
- Verify hooks are executable: `ls -l ~/.claude/hooks/*.py`
- Check settings.json syntax: `python3 -m json.tool ~/.claude/settings.json`
- Ensure Python 3 is available: `which python3`

### Status files not created
- Check if directory exists: `ls -ld ~/.cma/status/`
- Run hook manually: `echo '{}' | ~/.claude/hooks/status-summary.py`
- Check hook output: The hooks log errors to stderr

### Emacs not showing updates
- Verify file-notify is working: `emacs --batch --eval "(message \"%s\" (fboundp 'file-notify-add-watch))"`
- Check status module is loaded: Open Emacs and run `M-x describe-variable RET claude-multi-status-directory`
- Look for status files: `ls -l ~/.cma/status/`

## Dependencies

- Python 3
- Claude Code with hooks support
- Emacs with file-notify support (inotify on Linux, kqueue on macOS)
