# Stateless Architecture - User Guide

## Overview

Claude Multi-Agent now uses a **stateless architecture** where all agent information comes from status files written by Claude's Python hooks. This means:

- ✅ **No session restore needed**: Just open the progress buffer to see all running agents
- ✅ **Survives Emacs restarts**: Status files persist, agents auto-discovered
- ✅ **Always accurate**: What you see IS the current reality
- ✅ **Focus works immediately**: Press RET or 'f' on any agent to focus its kitty window

## How It Works

### Status Files (Single Source of Truth)

When Claude runs, its Python hooks write status files to `/tmp/claude-status/`:

```json
{
  "session_id": "8d0a3a4d-...",
  "kitty_window_id": "63",
  "cwd": "/Users/you/project",
  "claude_status": "running",
  "branch": "main",
  ...
}
```

### Auto-Discovery

The progress buffer reads these status files and displays all running agents:

```
* Agents

** 🔵 project (main)
:PROPERTIES:
:SESSION_ID: 8d0a3a4d-c76b-419b-bb84-e8aa6727a8aa
:KITTY_WINDOW: 63
:DIRECTORY: /Users/you/project
:STATUS: RUNNING
:BRANCH: main
:END:
```

## Basic Workflow

### 1. Launch Emacs (Fresh Start)

Even after restarting Emacs:
- Your in-memory agent list is empty
- But status files still exist

### 2. Open Progress Buffer

```
M-x claude-multi/open-progress
```
or
```
SPC c m p
```

**Result**: Progress buffer automatically discovers and displays all running Claude sessions from status files!

### 3. Focus on Any Agent

With cursor on an agent headline:
- Press `RET` or `f` to focus that agent's kitty window
- Works immediately, no restore needed

### 4. View Dashboard

```
M-x claude-multi/dashboard
```
or
```
SPC c m d
```

Shows summary of all running agents discovered from status files.

## Key Features

### Auto-Discovery on Progress Buffer Open

The progress buffer automatically:
1. Reads all files in `/tmp/claude-status/`
2. Extracts session info (window ID, directory, branch, status)
3. Displays them in organized org-mode format
4. Updates in real-time as agents work

### Directory Watcher

The progress buffer watches `/tmp/claude-status/` for changes:
- New status files → New agents appear
- Updated files → Agent info refreshes
- Deleted files → Agents removed

All automatic, no manual refresh needed!

### Focus from Progress Buffer

Position cursor on any agent headline and press:
- `RET` - Focus agent's kitty window
- `f` - Focus agent's kitty window

The KITTY_WINDOW property comes from the status file, so it works even for agents spawned before Emacs started.

## What Changed

### Before (Session-Based)

```elisp
;; Start Emacs
claude-multi--agents => nil (empty!)

;; Must restore manually
M-x claude-multi/restore-session

;; Now see agents
claude-multi--agents => (agent1 agent2)
```

**Problem**: Manual step, can restore wrong/stale session

### Now (Stateless)

```elisp
;; Start Emacs
claude-multi--agents => nil (deprecated, ignore it)

;; Just open progress buffer
M-x claude-multi/open-progress

;; Automatically shows ALL running agents from status files
;; No restore needed!
```

**Benefit**: Always shows current reality, zero configuration

## Technical Details

### Status File Discovery

Function: `claude-multi--get-agents-from-status-files`

Returns list of plists with agent information:
```elisp
((:session-id "..."
  :display-name "project (main)"
  :kitty-window-id "63"
  :working-directory "/path/to/project"
  :status running
  :branch-name "main"
  :timestamp "2026-01-10T13:21:52")
 ...)
```

### In-Memory Agents (Deprecated)

The variable `claude-multi--agents` is now deprecated:
- Kept for backwards compatibility
- Should NOT be used as primary agent source
- May be empty or stale
- Use `claude-multi--get-agents-from-status-files` instead

### Progress Buffer Refresh

Function: `claude-multi--refresh-progress-from-status-files`

Called automatically when opening progress buffer:
1. Reads all status files from `/tmp/claude-status/`
2. Clears and rebuilds agents section
3. Inserts org-mode properties (including KITTY_WINDOW)
4. Updates session statistics

## Troubleshooting

### "No agents shown in progress buffer"

**Check**: Do status files exist?
```bash
ls -la /tmp/claude-status/
```

**Cause**: No Claude sessions running, or hooks not executing

**Solution**: Start a Claude session - it will write a status file

### "Focus doesn't work on an agent"

**Check**: Does the agent have KITTY_WINDOW property?

Position cursor on agent headline and run:
```elisp
M-x org-entry-get RET KITTY_WINDOW
```

**Cause**: Agent started before KITTY_WINDOW_ID fix, or hooks didn't run

**Solution**:
1. Ensure global hooks have latest version: `cp hooks/status-summary.py ~/.claude/hooks/`
2. Start a new Claude session with the fix

### "Dashboard shows wrong count"

**Check**: Compare status files to in-memory agents:
```elisp
(list :status-files (length (claude-multi--get-agents-from-status-files))
      :in-memory (length claude-multi--agents))
```

**Cause**: In-memory list is stale

**Solution**: Ignore `claude-multi--agents`, use dashboard/progress buffer (they use status files)

## Migration from Old System

If you have saved sessions:

1. **Option A: Ignore them**
   - Just use progress buffer auto-discovery
   - Old saved sessions can be deleted

2. **Option B: Clean up**
   ```bash
   rm ~/.emacs.d/.local/cache/claude-multi-sessions/*
   ```

Session save/restore is now optional and rarely needed.

## Summary

The stateless architecture makes Claude Multi-Agent:
- ✅ **Self-healing**: Always shows current reality
- ✅ **Zero-config**: No manual restore after restart
- ✅ **Always accurate**: Can't have stale state
- ✅ **Simple**: Status files are the only source of truth

Just open the progress buffer and everything is there!
