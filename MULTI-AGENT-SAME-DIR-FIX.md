# Multiple Agents in Same Directory - Status Tracking Fix

## Problem Summary

When multiple agents are launched in the **same working directory**, they all showed the **same status information**. For example:
- Agent-1 and Agent-2 both in `/Users/user/projects/myproject`
- Both showed identical context window, activity, timestamps
- Status updates to one agent appeared on all agents in that directory

## Root Cause

The status matching algorithm used `cl-find-if` which returns the **first** matching agent by working directory:

```elisp
;; OLD CODE (BUGGY)
(cl-find-if
  (lambda (agent)
    (string= (agent-path agent) cwd))  ; Matches first agent only!
  claude-multi--agents)
```

When a new status file appeared:
1. System searched for an agent with matching working directory
2. Found the FIRST agent in that directory
3. Assigned the session to that agent
4. All subsequent status files in the same directory went to the SAME agent
5. Other agents in that directory never got their own sessions

## Solution Implemented

### Key Change: Only Match Unmapped Agents

Modified three functions to check for **already-mapped sessions**:

1. **`claude-multi--find-agent-by-cwd`**: Now only returns agents without a session ID
2. **`claude-multi--rescan-pending-agents`**: Checks if session is already claimed
3. **`claude-multi--register-agent-for-status`**: Skips already-claimed sessions during initial registration

```elisp
;; NEW CODE (FIXED)
(cl-find-if
  (lambda (agent)
    (and (not (claude-agent-session-id agent))  ; Must be unmapped!
         (string= (agent-path agent) cwd)))
  claude-multi--agents)
```

### How It Works Now

1. Agent-1 launches → Pending list → Waits for status file
2. Agent-2 launches → Pending list → Waits for status file
3. Status file A appears for the directory:
   - Finds **unmapped** agent in that directory → Agent-1
   - Maps session A → Agent-1
   - Agent-1 removed from pending
4. Status file B appears for the same directory:
   - Finds **unmapped** agent in that directory → Agent-2 (Agent-1 already mapped!)
   - Maps session B → Agent-2
   - Agent-2 removed from pending
5. Both agents now have unique sessions ✅

## Testing the Fix

### For New Sessions

The fix is automatic. Simply:

1. Reload the package:
   ```elisp
   (unload-feature 'claude-multi-status t)
   (load-file "~/projects/claude-multi-agent.el/config.el")
   ```

2. Start a new session:
   ```elisp
   (claude-multi/start-session)
   ```

3. Launch multiple agents in the same directory
4. Each agent should get its own status

### For Existing Sessions

If you already have agents sharing status, you need to reset the mappings:

```elisp
;; 1. Clear all existing mappings
(clrhash claude-multi--session-to-agent)

;; 2. Clear session IDs from all agents
(dolist (agent claude-multi--agents)
  (setf (claude-agent-session-id agent) nil))

;; 3. Add all agents back to pending list
(setq claude-multi--pending-agents (copy-sequence claude-multi--agents))

;; 4. Force a rescan to remap correctly
(claude-multi--rescan-pending-agents)
```

Or use the utility function:

```elisp
(claude-multi/reset-agent-mappings)
```

## Architecture Insights

### Why One-to-One Mapping is Critical

```
STATUS FILES          AGENTS
─────────────         ──────
session-ABC.json  →   Agent-1 (/dir)
session-DEF.json  →   Agent-2 (/dir)
session-GHI.json  →   Agent-3 (/other)
```

Each Claude Code instance has a unique session ID. The Emacs plugin must maintain a **1:1 mapping** between:
- Session IDs (from status files)
- Agent structs (in Emacs)

Without this, multiple agents "fight" over the same session data.

### The Hash Table Strategy

The fix relies on `claude-multi--session-to-agent` hash table:

```elisp
;; Key: session-id (string)
;; Value: agent (struct)
(gethash "session-ABC" claude-multi--session-to-agent)  → agent-1
(gethash "session-DEF" claude-multi--session-to-agent)  → agent-2
```

Before mapping a new session, we check:
```elisp
(not (gethash session-id claude-multi--session-to-agent))
```

This prevents double-mapping.

### Race Conditions

The fix handles race conditions where multiple status files appear simultaneously:

1. File watcher receives multiple events
2. Each event triggers `claude-multi--process-status-file`
3. Each checks `gethash` before claiming
4. First one wins, subsequent ones skip already-claimed sessions
5. Each pending agent eventually finds an unclaimed session

## Files Modified

### `autoload/claude-multi-status.el`

1. **Line 204-219**: `claude-multi--find-agent-by-cwd`
   - Added check for unmapped agents only
   - Updated docstring

2. **Line 91-110**: `claude-multi--rescan-pending-agents`
   - Added session-already-claimed check
   - Added early exit on match (performance)

3. **Line 240-265**: `claude-multi--register-agent-for-status`
   - Added session-already-claimed check during initial registration
   - Added early exit on match

## Utility Functions

### Diagnostic Commands

```elisp
;; Show which agents are mapped to which sessions
(maphash
  (lambda (session-id agent)
    (message "%s -> %s"
             (substring session-id 0 8)
             (claude-agent-name agent)))
  claude-multi--session-to-agent)

;; Check for duplicate mappings (should be none!)
(let ((dirs (make-hash-table :test 'equal)))
  (dolist (agent claude-multi--agents)
    (let* ((dir (or (claude-agent-worktree-path agent)
                    (claude-agent-working-directory agent)))
           (existing (gethash dir dirs)))
      (puthash dir
               (cons agent existing)
               dirs)))
  ;; Show directories with multiple agents
  (maphash
    (lambda (dir agents)
      (when (> (length agents) 1)
        (message "Dir %s has agents: %s"
                 dir
                 (mapcar #'claude-agent-name agents))))
    dirs))
```

### Reset Mappings

```elisp
(defun claude-multi/reset-agent-mappings ()
  "Reset all session-to-agent mappings and force rematching.
Use this if agents are incorrectly sharing status."
  (interactive)
  (message "Resetting agent-session mappings...")

  ;; Clear mappings
  (clrhash claude-multi--session-to-agent)

  ;; Clear agent session IDs
  (dolist (agent claude-multi--agents)
    (setf (claude-agent-session-id agent) nil))

  ;; Add all back to pending
  (setq claude-multi--pending-agents (copy-sequence claude-multi--agents))

  ;; Force rescan
  (claude-multi--rescan-pending-agents)

  (message "Reset complete. %d agent(s) remapped, %d still pending"
           (hash-table-count claude-multi--session-to-agent)
           (length claude-multi--pending-agents)))
```

## Prevention Strategy

### Best Practice: Use Worktrees

The **recommended approach** is to use git worktrees for agent isolation:

```elisp
(claude-multi/spawn-agent-with-worktree)
```

Benefits:
- Each agent gets a separate directory
- No path conflicts
- Clean file system isolation
- Easier to manage parallel work

### When Same-Directory is Needed

Sometimes you need multiple agents in one directory:
- Testing different approaches
- Parallel research tasks
- Multiple LLM conversations about the same codebase

The fix now supports this use case correctly.

## Testing Checklist

- [x] Single agent in directory works
- [x] Two agents in same directory get unique sessions
- [x] Three+ agents in same directory work
- [x] Agents in different directories still work
- [x] Mix of worktree and non-worktree agents works
- [x] Status updates go to correct agent
- [x] No cross-contamination of status data
- [x] Pending agents eventually get matched
- [x] Old stale files don't interfere

## Future Improvements

Potential enhancements:

1. **Visual feedback**: Show in UI when agents are in the same directory
2. **Automatic worktree suggestion**: Prompt user to use worktrees for isolation
3. **Session ID in agent name**: Display partial session ID in agent name for debugging
4. **Health check command**: Validate 1:1 mapping invariant
5. **Metrics**: Track how many agents share directories (for UX improvement)

## Related Issues

- See `STATUS-TRACKING-FIX.md` for stale session file issues
- See `CLAUDE.md` for general project documentation
