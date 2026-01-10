# Stateless Architecture Design

## Problem
The current system maintains `claude-multi--agents` in memory, which:
- Gets lost on Emacs restart
- Can conflict with actual running Claude sessions
- Requires manual session restore
- Creates complexity with save/restore logic

## Solution: Status-File-Driven Architecture

### Single Source of Truth
- `/tmp/claude-status/*.json` files written by Claude's Python hooks
- Each running Claude session has exactly one status file
- Progress buffer reads directly from status files
- No persistent in-memory state required

### Core Changes

#### 1. Make `claude-multi--agents` Optional/Cached
```elisp
;; Instead of primary storage, make it a cache
(defvar claude-multi--agents-cache nil
  "Cache of agents discovered from status files.
  Rebuilt on demand, not persisted.")
```

#### 2. Agent Discovery from Status Files
```elisp
(defun claude-multi--discover-agents-from-status ()
  "Discover all running agents from /tmp/claude-status/*.json files.
  Returns list of agent-like plists with fields from status files."
  ...)
```

#### 3. Progress Buffer as Primary Interface
- Always shows current reality from status files
- Refreshes automatically via directory watcher
- RET/f to focus works from KITTY_WINDOW property
- No dependency on in-memory agents

#### 4. Spawning New Agents
When spawning:
1. Create kitty window
2. Export KITTY_WINDOW_ID
3. Launch Claude
4. Claude's hooks write status file
5. Progress buffer auto-discovers it

No need to track in `claude-multi--agents`!

### Functions That Need Updates

#### Remove/Update Session Functions
- `claude-multi-session--save` → Deprecate or make optional
- `claude-multi-session--restore` → Remove
- `claude-multi/save-session` → Remove keybinding
- `claude-multi/restore-session` → Remove keybinding

#### Update Agent Selection
Instead of selecting from `claude-multi--agents`, select from status files:
```elisp
(defun claude-multi--select-agent-from-status (prompt)
  "Select an agent from status files."
  (let ((agents (claude-multi--discover-agents-from-status)))
    ...))
```

#### Functions to Update
- `claude-multi/kill-agent` → Use status files to find agents
- `claude-multi/kill-all-agents` → Use status files
- `claude-multi--list-agents` → Return agents from status files
- `claude-multi--get-agent-by-id` → Search status files
- `claude-multi/review-changes` → Use status files
- `claude-multi/worktree-status` → Use status files

### Migration Path

1. **Phase 1: Make status files primary** (this PR)
   - Keep `claude-multi--agents` but don't rely on it
   - All operations read from status files first
   - Session save/restore becomes optional

2. **Phase 2: Remove session persistence** (future)
   - Delete session.el file
   - Remove save/restore commands
   - Clean up keybindings

3. **Phase 3: Make agents cache** (future)
   - Rename to `claude-multi--agents-cache`
   - Only populated for performance
   - Rebuilt from status files on demand

### Benefits

✅ **Auto-discovery**: Open progress buffer, see all running agents
✅ **Survives restarts**: No manual restore needed
✅ **Always accurate**: Can't have stale state
✅ **Simpler code**: No serialization/deserialization
✅ **Focus works immediately**: KITTY_WINDOW in status files
✅ **No conflicts**: Single source of truth

### Backwards Compatibility

- Keep session save/restore as optional feature for now
- Existing keybindings work but are no-ops or warnings
- Migration guide for users with saved sessions
