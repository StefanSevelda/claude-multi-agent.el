---
description: Diagnose and fix Claude Multi-Agent plugin runtime issues
---

# Claude Multi-Agent Plugin Debugging

Use this skill when the user reports issues with the Claude Multi-Agent plugin not working correctly in their Emacs setup.

## Common Symptoms

1. **"Commands not available"** - Module not loaded
2. **"Agents stuck in pending"** - Status tracking not working
3. **"Progress buffer empty"** - Display not updating
4. **"Session ID: NONE"** - Agent not matched to Claude session

## Diagnostic Process

### Step 1: Check Module Loading

```elisp
(message "Loaded: config=%s status=%s agents=%s | Functions: watcher=%s create=%s"
  (featurep 'claude-multi-config)
  (featurep 'claude-multi-status)
  (featurep 'claude-multi-agents)
  (fboundp 'claude-multi--start-directory-watcher)
  (fboundp 'claude-multi--create-agent))
```

**Expected:** All return `t`

**If modules not loaded:** The symlink or load-path is wrong. Check:
- Symlink exists: `ls -la ~/.doom.d/modules/tools/claude-multi`
- init.el has `:tools claude-multi`
- Run `doom sync` and restart

**If functions missing:** Module loaded but functions not defined (the issue we just fixed).

### Step 2: Check Status Tracking System

```elisp
(message "Watcher: %s | Pending: %d | Mapped sessions: %d"
  claude-multi--directory-watcher
  (length claude-multi--pending-agents)
  (hash-table-count claude-multi--session-to-agent))
```

**Expected:**
- Watcher: `<descriptor>` (not nil)
- Pending: 0 (if all agents matched)
- Mapped sessions: equals number of running agents

**If watcher is nil:** Status tracking not started. Fix:

```elisp
(claude-multi--start-directory-watcher)
(claude-multi--start-pending-rescan-timer)
```

### Step 3: Check Agent Status

```elisp
(dolist (agent claude-multi--agents)
  (message "%s: session=%s status=%s kitty=%s"
    (claude-agent-name agent)
    (or (and (claude-agent-session-id agent)
             (substring (claude-agent-session-id agent) 0 8))
        "NONE")
    (claude-agent-status agent)
    (claude-agent-kitty-window-id agent)))
```

**If "Session ID: NONE":** Agent not matched to Claude session.

### Step 4: Check Status Files

```bash
ls -la /tmp/claude-status/
```

**Expected:** Recent `status-*.json` files (timestamps within last few minutes)

**If no files:** Claude hooks not installed. Run `make install-hooks` in project directory.

**If files exist but agents not matched:** Run rescan manually (see fixes below).

## Common Fixes

### Fix 1: Restart Status Tracking (Orphaned Agents)

When agents were created before status tracking was active:

```elisp
(progn
  ;; Start watcher if not running
  (unless claude-multi--directory-watcher
    (claude-multi--start-directory-watcher))

  ;; Start rescan timer if not running
  (unless claude-multi--pending-rescan-timer
    (claude-multi--start-pending-rescan-timer))

  ;; Add unmapped agents to pending list
  (dolist (agent claude-multi--agents)
    (unless (claude-agent-session-id agent)
      (push agent claude-multi--pending-agents)))

  ;; Force immediate rescan
  (claude-multi--rescan-pending-agents)

  (message "Fixed! Watcher: %s | Pending: %d"
    claude-multi--directory-watcher
    (length claude-multi--pending-agents)))
```

### Fix 2: Update Progress Display

When status tracking works but display doesn't update:

```elisp
(progn
  ;; Update agent status from session files
  (dolist (agent claude-multi--agents)
    (when (claude-agent-session-id agent)
      (let* ((session-id (claude-agent-session-id agent))
             (file (format "/tmp/claude-status/status-%s.json" session-id))
             (data (when (file-exists-p file)
                     (with-temp-buffer
                       (insert-file-contents file)
                       (json-read)))))
        (when data
          (claude-multi--update-agent-from-status agent data)
          (claude-multi--update-agent-status-display agent)))))

  (message "Display updated"))
```

### Fix 3: Rebuild Progress Buffer

When agents missing from progress buffer:

```elisp
(with-current-buffer (get-buffer-create "*Claude Multi-Agent Progress.org*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (claude-multi--init-progress-buffer)
    (dolist (agent claude-multi--agents)
      (claude-multi--add-agent-section agent))
    (claude-multi--update-session-stats))
  (message "Progress buffer rebuilt"))
```

### Fix 4: Module Loading Issue

If functions not available after `(featurep 'claude-multi-status)` returns `t`:

**Root cause:** `require` didn't actually load function definitions (Doom Emacs interaction issue)

**Permanent fix:** Already applied in config.el (uses `load` instead of `require` with error checking)

**Temporary fix:** Reload manually:

```elisp
(load-file "/Users/stefansevelda/projects/claude-multi-agent.el/autoload/claude-multi-status.el")
```

## Complete Diagnostic Script

Run this to get full diagnostic output:

```elisp
(with-current-buffer (get-buffer-create "*Claude Multi Debug*")
  (erase-buffer)
  (insert "=== Claude Multi-Agent Diagnostics ===\n\n")

  ;; 1. Module loading
  (insert "1. MODULES\n")
  (insert (format "   config: %s\n" (featurep 'claude-multi-config)))
  (insert (format "   status: %s\n" (featurep 'claude-multi-status)))
  (insert (format "   agents: %s\n" (featurep 'claude-multi-agents)))
  (insert (format "   Functions: watcher=%s create=%s\n\n"
    (fboundp 'claude-multi--start-directory-watcher)
    (fboundp 'claude-multi--create-agent)))

  ;; 2. Status tracking
  (insert "2. STATUS TRACKING\n")
  (insert (format "   Watcher: %s\n" claude-multi--directory-watcher))
  (insert (format "   Pending: %d\n" (length claude-multi--pending-agents)))
  (insert (format "   Mapped: %d\n\n" (hash-table-count claude-multi--session-to-agent)))

  ;; 3. Agents
  (insert "3. AGENTS\n")
  (dolist (agent claude-multi--agents)
    (insert (format "   %s:\n" (claude-agent-name agent)))
    (insert (format "     Session: %s\n"
      (or (and (claude-agent-session-id agent)
               (substring (claude-agent-session-id agent) 0 8))
          "NONE")))
    (insert (format "     Status: %s\n" (claude-agent-status agent)))
    (insert (format "     Dir: %s\n"
      (or (claude-agent-worktree-path agent)
          (claude-agent-working-directory agent)))))

  ;; 4. Status files
  (insert "\n4. STATUS FILES\n")
  (let ((files (directory-files "/tmp/claude-status/" t "^status-.*\\.json$")))
    (insert (format "   Count: %d\n" (length files)))
    (dolist (file (seq-take files 3))
      (insert (format "   - %s\n" (file-name-nondirectory file)))))

  (goto-char (point-min))
  (display-buffer (current-buffer)))
```

## Prevention

To avoid these issues in the future:

1. **Always start session first:** Run `M-x claude-multi/start-session` before spawning agents
2. **Check status tracking:** After spawning first agent, verify watcher is active
3. **Monitor progress buffer:** Keep it open to catch issues early
4. **Restart Emacs cleanly:** After `doom sync`, fully restart (not just reload)

## Known Issues & Solutions

### Issue: Functions not defined after require

**Symptom:** `(featurep 'claude-multi-status)` returns `t` but `(fboundp 'claude-multi--start-directory-watcher)` returns `nil`

**Cause:** Doom Emacs module loading interaction with `require` statement

**Solution:** Config.el now uses `load` with explicit paths and error checking

### Issue: Agents in same directory not matched

**Symptom:** Multiple agents in same directory, only first one gets session ID

**Cause:** Matching algorithm only looked for unmapped agents

**Solution:** Already fixed in claude-multi-status.el (checks for existing session mappings)

### Issue: Status tracking stops after while

**Symptom:** Initially works, then stops updating

**Cause:** File watcher gets garbage collected or timer canceled

**Solution:** Re-run Fix 1 to restart tracking system

## Testing After Fix

After applying any fix, verify:

```elisp
;; Should all return t
(fboundp 'claude-multi--start-directory-watcher)
(fboundp 'claude-multi--rescan-pending-agents)
(not (null claude-multi--directory-watcher))

;; Should return count matching number of running agents
(hash-table-count claude-multi--session-to-agent)

;; All agents should have session IDs
(cl-every #'claude-agent-session-id claude-multi--agents)
```

## When to Escalate

If none of these fixes work:
1. Check Emacs version (requires 27+)
2. Check kitty version and remote control config
3. Verify `/tmp/claude-status/` is writable
4. Check for conflicts with other packages (especially process/file watching packages)
5. Try in `emacs -Q` to rule out config issues
