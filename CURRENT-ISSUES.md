# Current Issues - Claude Multi-Agent Plugin

**Date**: January 9, 2026
**Priority**: High
**Status**: Open

This document tracks active issues with the claude-multi-agent Doom Emacs plugin that need resolution.

---

## Issue 1: Plugin Not Loaded on Emacs Start

### Symptom
Plugin requires manual reload on every Emacs start:
- Must run `SPC h r r` (doom/reload) after starting Emacs
- Without reload, commands like `claude-multi/spawn-agent` are not available
- Feature `claude-multi-config` not loaded automatically

### Expected Behavior
Plugin should load automatically when Emacs starts, with all commands and features available immediately.

### Current Workaround
```
SPC h r r  (doom/reload)
```

### Investigation Needed
1. Check if module is in Doom's enabled modules list
2. Verify `init.el` in module directory is correct
3. Check if autoload cookies are properly generated
4. Verify Doom's module loading order
5. Check for errors in `*Messages*` buffer on startup

### Potential Causes
- Module not enabled in `~/.doom.d/init.el`
- Autoload file not generated properly
- Load order issue with dependencies
- Errors during initial load (silently failing)
- `init.el` or `config.el` has syntax/logic errors

### Debug Commands
```elisp
;; Check if module loaded
(featurep 'claude-multi-config)  ; Should return t

;; Check for load errors
(switch-to-buffer "*Messages*")

;; Manually load
(load-file "/Users/stefansevelda/projects/claude-multi-agent.el/config.el")

;; Check if autoload file exists
(file-exists-p (expand-file-name "autoload"
  "/Users/stefansevelda/projects/claude-multi-agent.el"))
```

### Files to Check
- `~/.doom.d/init.el` - Is `:tools claude-multi` enabled?
- `~/.doom.d/modules/tools/claude-multi/init.el` - Proper init?
- `/Users/stefansevelda/projects/claude-multi-agent.el/config.el` - Load errors?
- `~/.config/emacs/.emacs.d/.local/autoloads/@/autoloads.el` - Generated properly?

---

## Issue 2: Agent Status Not Syncing from ~/.cma/status/ ✅ RESOLVED

### Status: RESOLVED (2026-01-09)

**Solution implemented:**
- Fixed file-notify watcher to listen for `'(change created renamed)` events instead of only `'(change)`
- Python hooks use atomic rename which triggers `'renamed'` events
- Improved status display feedback to distinguish between pending and first-update states

### Original Symptom (NOW FIXED)
Agent status information was not being picked up from status files and synced into the session org mode buffer:
- Progress buffer showed "Waiting for status update..."
- Status files existed in `~/.cma/status/` with valid data
- Agent matched to session ID (diagnostic showed this)
- But STATUS drawer in progress buffer never updated with actual status

### Architecture Note: File-Based Status Tracking ONLY

**IMPORTANT**: This issue concerns the **file-based status tracking system** only.

**DO NOT use WebSocket for status updates.** The architecture is:

```
Claude Agent (kitty terminal)
        ↓
Python Hook (status-summary.py)
        ↓
~/.cma/status/status-{session-id}.json  ← Write JSON file
        ↓
Emacs file-notify-add-watch  ← Watch directory
        ↓
Status processing & agent matching
        ↓
Progress buffer STATUS drawer update
```

**Why file-based only:**
- Simpler architecture - no network layer needed
- No connection management/reconnection logic
- Works even if WebSocket fails or is disabled
- Persistent - survives Emacs/agent restarts
- Python hooks already write to `~/.cma/status/`
- Uses standard file-notify (inotify/kqueue)

**WebSocket is for MCP protocol** (tool calls, diffs), NOT for status updates.

### Expected Behavior
After agent launches and status file is created:
1. **Hook writes JSON**: `status-summary.py` writes to `~/.cma/status/status-{session-id}.json`
2. **File-notify detects**: `file-notify-add-watch` triggers on file change
3. **JSON parsed**: Status data extracted from file
4. **Agent matched**: By comparing `cwd` field in JSON to agent's working directory
5. **Cache updated**: Status data stored in `claude-multi--status-cache`
6. **Display updated**: Progress buffer STATUS drawer shows:
   - Model name (e.g., "claude-opus-4")
   - Mode (normal/agentic)
   - Token usage (input/output/cache)
   - Git information (branch, status)
   - Current activity

### Current State
```
*** 🟢 agent-1 | task | 0s :running:
       :STATUS:
       <!-- status-marker-agent-1 -->
    /Waiting for status update.../
       :END:
```

### Investigation Needed

**Focus on file-based tracking pipeline:**

1. ✅ Status files exist in `~/.cma/status/` - VERIFIED
2. ✅ Agent matches to session ID - VERIFIED (diagnostic shows match)
3. ❓ File-notify watcher running? - NEED TO CHECK
4. ❓ File-notify events triggering? - NEED TO CHECK
5. ❓ Status cache populated after file change? - NEED TO CHECK
6. ❓ Display update function called after cache update? - NEED TO CHECK
7. ❓ Status data format matches what display expects? - NEED TO CHECK

**NOT investigating (WebSocket not used for status):**
- ❌ WebSocket connection status
- ❌ WebSocket message handling
- ❌ Network connectivity

### Debug Commands

**Check file-notify system:**
```elisp
;; Enable debug logging
(setq claude-multi-status-debug t)

;; 1. Check file-notify watcher is running
claude-multi--directory-watcher  ; Should return a descriptor, not nil
;; If nil, watcher not started - call:
(claude-multi--start-directory-watcher)

;; 2. Check what directory is being watched
claude-multi-status-directory  ; Should be "~/.cma/status"

;; 3. Verify directory exists and has files
(directory-files (expand-file-name "~/.cma/status") nil "^status-.*\\.json$")
;; Should return list of status files

;; 4. Check if agent matched
(claude-multi/debug-status-matching)
;; Look for: Session ID set, "In Pending: NO"

;; 5. Test file-notify manually by touching a file
(shell-command "touch ~/.cma/status/test.json")
;; Check *Messages* buffer for file-notify event

;; 6. Check status cache
(let ((agent (car claude-multi--agents)))
  (when-let ((session-id (claude-agent-session-id agent)))
    (gethash session-id claude-multi--status-cache)))

;; 7. Check if agent has status data
(let ((agent (car claude-multi--agents)))
  (claude-agent-last-status-data agent))

;; 8. Manually process a status file (bypass file-notify)
(claude-multi--process-status-file
  "~/.cma/status/status-XXXXX.json")

;; 9. Manually trigger display update
(let ((agent (car claude-multi--agents)))
  (claude-multi--update-agent-status-display agent))

;; 10. Check debug log for file-notify events
(switch-to-buffer "*claude-multi-status-debug*")
;; Should see: "File changed: ~/.cma/status/status-XXXXX.json"
```

### Potential Causes

**File-notify pipeline issues:**
1. **Watcher not started**: `claude-multi--directory-watcher` is nil
2. **File-notify events not firing**:
   - macOS kqueue issue
   - Directory permissions
   - Status file written atomically (rename instead of modify)
3. **Event handler not processing files**:
   - `claude-multi--handle-directory-event` has error
   - File path in event doesn't match expected pattern
4. **Status cache not updated**:
   - `claude-multi--process-status-file` not being called
   - JSON parsing error
   - Status data not stored in cache

**Display update issues:**
5. **Display function not called after cache update**:
   - `claude-multi--update-agent-status-display` not triggered
   - Missing call after `claude-multi--update-agent-from-status`
6. **Display update can't find marker**:
   - Progress buffer STATUS marker mismatch
   - Agent ID different from marker ID
7. **Display update fails silently**:
   - Progress buffer in read-only mode
   - Org-mode drawer malformed
   - Status data format incompatible with display logic

**Most likely cause**: File-notify events not triggering display updates even though agent is matched and cache might be populated.

### Files to Check

**File-notify implementation:**
- `autoload/claude-multi-status.el` line 67+ - `claude-multi--start-directory-watcher`
- `autoload/claude-multi-status.el` line 86+ - `claude-multi--handle-directory-event`
- `autoload/claude-multi-status.el` line 97+ - `claude-multi--process-status-file`

**Status processing:**
- `autoload/claude-multi-status.el` line 104+ - `claude-multi--update-agent-from-status`
- `autoload/claude-multi-status.el` line 150+ - Status cache management

**Display update:**
- `autoload/claude-multi-progress.el` line 637+ - `claude-multi--update-agent-status-display`
- Check if this is called from `claude-multi--update-agent-from-status`

### File-Notify Implementation Details

**Current implementation** (in `autoload/claude-multi-status.el`):

```elisp
;; Line 67: Start watcher
(defun claude-multi--start-directory-watcher ()
  (unless claude-multi--directory-watcher
    (setq claude-multi--directory-watcher
          (file-notify-add-watch
           claude-multi-status-directory
           '(change)  ; ← Only watching 'change events
           #'claude-multi--handle-directory-event))))

;; Line 86: Handle events
(defun claude-multi--handle-directory-event (event)
  (let* ((descriptor (nth 0 event))
         (action (nth 1 event))
         (file (nth 2 event)))
    (when (and (stringp file)
               (string-match "status-.*\\.json$" file))
      (claude-multi--process-status-file file))))
```

**Potential issue**: Python hook writes files atomically using `temp_file.rename()`, which triggers a `renamed` event, not `change`. The watcher should listen for `'(change created renamed)`:

```elisp
;; FIX: Watch all relevant events
(file-notify-add-watch
 claude-multi-status-directory
 '(change created renamed)  ; ← Add created and renamed
 #'claude-multi--handle-directory-event)
```

### Hypothesis
The agent **matches** to the status file (confirmed by diagnostic), but:

**Primary hypothesis**: File-notify events not triggering because:
- Watcher only listens for `change` events
- Python hook uses atomic rename (`renamed` event)
- Events are being ignored

**Secondary hypothesis**: Even if events trigger:
1. Status cache gets populated, BUT
2. Display update function (`claude-multi--update-agent-status-display`) not called, OR
3. Display update can't find/modify the drawer

**Testing hypothesis**:
- Add logging to `claude-multi--handle-directory-event`
- Watch *Messages* buffer while agent runs
- Manually touch a status file and see if event fires

---

## Issue 3: Insufficient Logging & Diagnostics

### Symptom
Difficult to troubleshoot Emacs configuration issues due to:
- Limited visibility into module loading process
- No clear indication when things fail silently
- Hard to debug status matching and display update flow
- Debug mode requires manual enable each session
- No persistent logging across sessions

### Expected Behavior
Comprehensive logging system that:
1. **Startup Logging**:
   - Module loading progress
   - Autoload path resolution
   - Dependency loading
   - Configuration variable application
   - Any errors or warnings

2. **Status Tracking Logging**:
   - File-notify events
   - Status file parsing
   - Agent matching attempts (with paths)
   - Cache updates
   - Display updates

3. **Session Logging**:
   - Agent creation and launch
   - Kitty commands sent
   - WebSocket connections/disconnections
   - Agent state transitions

4. **Persistent Logging**:
   - Log to file (not just buffer)
   - Configurable log levels (debug, info, warn, error)
   - Rotation/cleanup of old logs
   - Easy access to view logs

### Proposed Solution

Add comprehensive logging system:

```elisp
;; In config.el
(defcustom claude-multi-debug-level 'info
  "Debug logging level: 'none, 'error, 'warn, 'info, 'debug"
  :type '(choice (const none) (const error) (const warn)
                 (const info) (const debug))
  :group 'claude-multi)

(defcustom claude-multi-log-file
  (expand-file-name "claude-multi.log" user-emacs-directory)
  "File for persistent logging."
  :type 'file
  :group 'claude-multi)

;; Logging function
(defun claude-multi--log (level format-string &rest args)
  "Log message at LEVEL."
  (when (claude-multi--should-log-p level)
    (let* ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
           (message (apply #'format format-string args))
           (log-line (format "[%s] [%s] %s\n" timestamp level message)))
      ;; Log to buffer
      (with-current-buffer (get-buffer-create "*claude-multi-log*")
        (goto-char (point-max))
        (insert log-line))
      ;; Log to file
      (when claude-multi-log-file
        (append-to-file log-line nil claude-multi-log-file)))))

;; Use throughout codebase:
(claude-multi--log 'info "Loading config.el from: %s" load-file-name)
(claude-multi--log 'debug "Autoload dir resolved to: %s" autoload-dir)
(claude-multi--log 'info "Registering agent %s" (claude-agent-name agent))
(claude-multi--log 'debug "Status file matched: %s -> %s" cwd session-id)
(claude-multi--log 'error "Failed to load status file: %s" error-msg)
```

### Interactive Commands Needed

```elisp
;;;###autoload
(defun claude-multi/show-log ()
  "Display the log buffer."
  (interactive)
  (switch-to-buffer "*claude-multi-log*"))

;;;###autoload
(defun claude-multi/clear-log ()
  "Clear the log buffer and file."
  (interactive)
  (when (get-buffer "*claude-multi-log*")
    (with-current-buffer "*claude-multi-log*"
      (erase-buffer)))
  (when (file-exists-p claude-multi-log-file)
    (delete-file claude-multi-log-file)))

;;;###autoload
(defun claude-multi/set-debug-level (level)
  "Set debug logging level."
  (interactive
   (list (intern (completing-read "Debug level: "
                   '("none" "error" "warn" "info" "debug")))))
  (setq claude-multi-debug-level level)
  (message "Debug level set to: %s" level))

;;;###autoload
(defun claude-multi/diagnose ()
  "Run comprehensive diagnostics."
  (interactive)
  (let ((buf (get-buffer-create "*claude-multi-diagnostics*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== Claude Multi-Agent Diagnostics ===\n\n")

      ;; Module loading
      (insert "## Module Loading\n")
      (insert (format "- Feature loaded: %s\n"
                      (featurep 'claude-multi-config)))
      (insert (format "- Autoload path: %s\n"
                      (member "/Users/stefansevelda/projects/claude-multi-agent.el/autoload"
                              load-path)))

      ;; Configuration
      (insert "\n## Configuration\n")
      (insert (format "- Claude command: %s\n" claude-multi-claude-command))
      (insert (format "- Debug level: %s\n" claude-multi-debug-level))

      ;; Status tracking
      (insert "\n## Status Tracking\n")
      (insert (format "- Watcher running: %s\n"
                      (if claude-multi--directory-watcher "YES" "NO")))
      (insert (format "- Agents: %d\n" (length claude-multi--agents)))
      (insert (format "- Pending: %d\n" (length claude-multi--pending-agents)))

      ;; Recent log entries
      (insert "\n## Recent Log (last 20 lines)\n")
      (when (get-buffer "*claude-multi-log*")
        (insert (with-current-buffer "*claude-multi-log*"
                  (buffer-substring-no-properties
                   (max (point-min) (- (point-max) 2000))
                   (point-max)))))

      (goto-char (point-min))
      (special-mode))
    (display-buffer buf)))
```

### Files to Modify
- `config.el` - Add logging system and defcustoms
- All `autoload/*.el` files - Add logging calls throughout
- `config.el` - Add diagnostic commands

---

## Issue 4: Plugin Refresh Requires Too Much Effort

### Symptom
After changing plugin code, refreshing in existing Emacs instances requires:
- Multiple steps (unload, reload multiple files)
- Hard to remember what order to reload files
- Sometimes need full Emacs restart
- No single command to "refresh everything"

### Expected Behavior
Simple, single command to refresh plugin:
```
SPC c m R  (or similar)
```

Should:
1. Unload all plugin features
2. Reload all plugin files in correct order
3. Re-register all agents
4. Restore state where possible
5. Report success/errors

### Proposed Solution

Create a comprehensive reload command:

```elisp
;;;###autoload
(defun claude-multi/reload ()
  "Reload the entire claude-multi plugin.
Unloads all features, reloads files, preserves agent state."
  (interactive)
  (let ((agent-states nil)
        (reload-errors nil))

    (claude-multi--log 'info "Starting plugin reload...")

    ;; 1. Save agent states
    (dolist (agent claude-multi--agents)
      (push (list :name (claude-agent-name agent)
                  :status (claude-agent-status agent)
                  :window-id (claude-agent-kitty-window-id agent))
            agent-states))
    (claude-multi--log 'info "Saved %d agent states" (length agent-states))

    ;; 2. Unload all features
    (mapc (lambda (feature)
            (condition-case err
                (progn
                  (unload-feature feature t)
                  (claude-multi--log 'debug "Unloaded feature: %s" feature))
              (error
               (push (format "Error unloading %s: %s" feature err) reload-errors)
               (claude-multi--log 'error "Failed to unload %s: %s" feature err))))
          '(claude-multi-session
            claude-multi-ediff
            claude-multi-mcp
            claude-multi-websocket
            claude-multi-notifications
            claude-multi-worktree
            claude-multi-progress
            claude-multi-agents
            claude-multi-status
            claude-multi-config))

    ;; 3. Clear variables
    (setq claude-multi--agents nil
          claude-multi--agent-id-counter 0
          claude-multi--directory-watcher nil
          claude-multi--session-to-agent (make-hash-table :test 'equal)
          claude-multi--pending-agents nil
          claude-multi--status-cache (make-hash-table :test 'equal))
    (claude-multi--log 'debug "Cleared plugin state")

    ;; 4. Reload main config (will load all submodules)
    (condition-case err
        (progn
          (load-file "/Users/stefansevelda/projects/claude-multi-agent.el/config.el")
          (claude-multi--log 'info "Reloaded config.el successfully"))
      (error
       (push (format "Error loading config.el: %s" err) reload-errors)
       (claude-multi--log 'error "Failed to reload config.el: %s" err)))

    ;; 5. Verify reload
    (let ((loaded (featurep 'claude-multi-config)))
      (claude-multi--log 'info "Feature claude-multi-config loaded: %s" loaded)
      (if loaded
          (progn
            (message "✓ Plugin reloaded successfully")
            (claude-multi--log 'info "Plugin reload completed"))
        (progn
          (message "✗ Plugin reload failed - check *claude-multi-log*")
          (claude-multi--log 'error "Plugin reload failed"))))

    ;; 6. Report errors if any
    (when reload-errors
      (with-current-buffer (get-buffer-create "*claude-multi-reload-errors*")
        (erase-buffer)
        (insert "=== Plugin Reload Errors ===\n\n")
        (dolist (err reload-errors)
          (insert err "\n"))
        (display-buffer (current-buffer))))

    ;; Return success status
    (null reload-errors)))
```

### Additional Helper Commands

```elisp
;;;###autoload
(defun claude-multi/reload-file ()
  "Reload just the current file (for quick iterations)."
  (interactive)
  (if buffer-file-name
      (progn
        (load-file buffer-file-name)
        (message "✓ Reloaded %s" (file-name-nondirectory buffer-file-name))
        (claude-multi--log 'info "Reloaded file: %s" buffer-file-name))
    (message "Buffer not visiting a file")))

;;;###autoload
(defun claude-multi/eval-and-reload ()
  "Eval current buffer then reload plugin (for development)."
  (interactive)
  (eval-buffer)
  (message "✓ Evaluated buffer")
  (sit-for 0.5)
  (claude-multi/reload))
```

### Keybindings to Add

In `config.el`:

```elisp
(map! :leader
      :prefix ("c m" . "claude-multi")
      :desc "Reload plugin"         "R" #'claude-multi/reload
      :desc "Reload current file"   "r" #'claude-multi/reload-file
      :desc "Eval and reload"       "E" #'claude-multi/eval-and-reload
      :desc "Show log"              "L" #'claude-multi/show-log
      :desc "Clear log"             "C" #'claude-multi/clear-log
      :desc "Set debug level"       "D" #'claude-multi/set-debug-level
      :desc "Run diagnostics"       "?" #'claude-multi/diagnose)
```

### Evil Mode Shortcuts (Quick Reference)

```
SPC c m R     - Reload entire plugin
SPC c m r     - Reload current file only
SPC c m E     - Eval buffer and reload plugin
SPC c m L     - Show log buffer
SPC c m C     - Clear log
SPC c m D     - Set debug level
SPC c m ?     - Run diagnostics
```

### Files to Create/Modify
- `autoload/claude-multi-reload.el` - New file with reload logic
- `config.el` - Add reload commands and keybindings
- `config.el` - Add logging system

---

## Priority & Next Steps

### ✅ Completed
- **Issue 2** - Status not syncing - RESOLVED (file-notify event fix)

### High Priority (Blocking Development)
1. **Issue 1** - Plugin not loading - MUST FIX FIRST
2. **Issue 4** - Create reload system - Makes fixing other issues easier

### Medium Priority (Nice to Have)
3. **Issue 3** - Add logging - Helpful for debugging (debug logging already added)

### Recommended Order
1. Fix Issue 1 (module loading)
2. Implement Issue 4 (reload system) - makes development easier
3. Implement Issue 3 (comprehensive logging system) - optional enhancement

---

## Testing Checklist

After fixing each issue:

### Issue 1 - Module Loading
```elisp
;; Start fresh Emacs
;; Without any commands, check:
(featurep 'claude-multi-config)  ; Should be t
(fboundp 'claude-multi/spawn-agent)  ; Should be t
;; If both are t, issue is fixed
```

### Issue 2 - Status Sync ✅ RESOLVED
```elisp
;; Launch agent
(claude-multi/spawn-agent)
;; Wait 5 seconds
;; Check progress buffer - should show status with model/mode info
;; Status should update automatically via file-notify
;; Check diagnostic:
(claude-multi/debug-status-matching)
;; Should show: Session ID set, status data populated
;; If still broken: (setq claude-multi-status-debug t) and check debug buffer
```

### Issue 3 - Logging
```elisp
;; Should be able to:
(claude-multi/show-log)  ; View logs
(claude-multi/set-debug-level 'debug)  ; Change level
(claude-multi/diagnose)  ; Run diagnostics
;; Log file should exist at: ~/.config/emacs/claude-multi.log
```

### Issue 4 - Reload System
```elisp
;; Edit a file
;; Run: SPC c m R (or M-x claude-multi/reload)
;; Should reload without errors
;; Check: (featurep 'claude-multi-config)  ; Should still be t
;; All commands should still work
```

---

## Related Documentation

- `doom-local-plugin-development.md` - Development workflow and patterns
- `STATUS-FIX-SUMMARY.md` - Previous status tracking fix
- `SESSION-SUMMARY.md` - Development session summary
- `DOOM-LOADING-FIX.md` - Module loading fixes already applied

---

**Note**: This document should be updated as issues are resolved or new issues are discovered.
