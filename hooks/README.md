# Claude Multi-Agent Hooks

Claude Code hooks that enable real-time status tracking for the claude-multi-agent.el Emacs plugin. All hook logic is handled by the `cma` binary via `cma hook` subcommands.

## Hook Commands

- **`cma hook post-tool-use`** — PostToolUse hook that writes status information to `~/.cma/status/status-{session_id}.json` after each tool use
- **`cma hook stop`** — Stop hook that marks the session as finished when Claude stops
- **`cma hook notification`** — Notification hook that updates status files when Claude needs user attention (idle, permission prompt, question)

## Installation

Run the installation script from the project root:

```bash
make install-hooks
# or
./install-hooks.sh
```

The script will:
1. Verify `cma` binary is available on PATH
2. Update `~/.claude/settings.json` to register the hooks (without overwriting existing configuration)
3. Remove old Python-based hooks if present
4. Create backups of any existing settings before modifying

## Manual Installation

Add the following to your `~/.claude/settings.json`:

```json
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
```

## Status File Format

The hooks write JSON files to `~/.cma/status/` with the following structure:

```json
{
  "cwd": "/path/to/working/directory",
  "session_id": "uuid",
  "timestamp": "2026-01-08T12:23:22+01:00",
  "session_started": "2026-01-08T11:19:00+01:00",
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

1. **PostToolUse Hook** (`cma hook post-tool-use`): Triggered after each tool use (Write, Edit, Bash, Task, AskUserQuestion)
   - Updates session state (changes, activity, waiting status)
   - Collects context window information
   - Gathers git repository details
   - Extracts model/mode info from transcript
   - Writes status to session-specific JSON file

2. **Stop Hook** (`cma hook stop`): Triggered when Claude session ends
   - Marks the session as finished
   - Preserves existing fields (model, mode, agent name)

3. **Notification Hook** (`cma hook notification`): Triggered when Claude needs user attention
   - Handles `idle_prompt` (finished work), `permission_prompt` (needs tool approval), `elicitation_dialog` (asking a question)
   - Updates `notification_type` and `waiting_for_input` fields in the status file
   - Emacs picks up changes via polling and shows color-coded status + desktop alerts

4. **Emacs Integration**: The modeline and table modules poll `cma list --json` to display agent status in real-time.

## Troubleshooting

### Hooks not running
- Verify `cma` is on PATH: `which cma`
- Check settings.json syntax: `python3 -m json.tool ~/.claude/settings.json`
- Test manually: `echo '{"session_id":"test","cwd":"/tmp"}' | cma hook stop`

### Status files not created
- Check if directory exists: `ls -ld ~/.cma/status/`
- Check hook output: Run the hook command manually and check stderr

### Emacs not showing updates
- Verify modeline is enabled: `M-x cma-modeline-mode`
- Check agent list: `M-x cma-table`
- Look for status files: `ls -l ~/.cma/status/`

## Dependencies

- `cma` binary (Go, no Python dependency)
- Claude Code with hooks support
