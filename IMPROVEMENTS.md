e# Claude Multi-Agent Improvements

## Overview

This document outlines planned improvements to the claude-multi-agent.el system based on user feedback and usability testing.

**Date Created**: 2026-01-12
**Status**: Planning Phase

---

## 1. Fix Cursor Position Jumping

### Problem

When status files change and the progress buffer refreshes, the cursor jumps to the beginning of the buffer. This is disruptive when monitoring agents or after renaming.

**Impact**: Medium - Annoying but not critical
**Frequency**: Every time any agent updates (every few seconds)

### Root Cause

The `claude-multi--refresh-progress-from-status-files` function (in `autoload/claude-multi-progress.el`) calls:
1. `(erase-buffer)` - Clears entire buffer
2. Rebuilds content from scratch
3. Does not restore cursor position

### Solution Options

#### Option A: Preserve Cursor Position (Simple, Recommended)

Save and restore cursor position during refresh:

```elisp
(defun claude-multi--refresh-progress-from-status-files ()
  "Refresh progress buffer from status files, preserving cursor position."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t)
            (saved-point (point))                    ; Save current position
            (saved-window-start (window-start)))     ; Save scroll position

        ;; Existing refresh logic...
        (erase-buffer)
        (insert "* Claude Multi-Agent Session Progress\n\n")
        ;; ... rest of content generation ...

        ;; Restore cursor and scroll position
        (goto-char (min saved-point (point-max)))
        (when (get-buffer-window claude-multi--progress-buffer)
          (set-window-start (get-buffer-window claude-multi--progress-buffer)
                           saved-window-start))))))
```

**Pros**: Simple, works immediately
**Cons**: Cursor may end up in wrong section if agents are added/removed

#### Option B: Smart Position Restoration (Better, More Complex)

Remember which agent headline the cursor was on and restore to that agent:

```elisp
(defun claude-multi--get-current-agent-session-id ()
  "Get the session ID of the agent at point, if any."
  (save-excursion
    (org-back-to-heading t)
    (org-entry-get nil "SESSION_ID")))

(defun claude-multi--goto-agent-session (session-id)
  "Move cursor to agent headline with SESSION_ID."
  (goto-char (point-min))
  (when (re-search-forward (format ":SESSION_ID: %s" session-id) nil t)
    (org-back-to-heading t)))

(defun claude-multi--refresh-progress-from-status-files ()
  "Refresh progress buffer, restoring cursor to same agent."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t)
            (current-session-id (ignore-errors
                                  (claude-multi--get-current-agent-session-id))))

        ;; Refresh content...
        (erase-buffer)
        ;; ... rebuild content ...

        ;; Restore to same agent or stay at top
        (if current-session-id
            (claude-multi--goto-agent-session current-session-id)
          (goto-char (point-min)))))))
```

**Pros**: Cursor stays with the same agent even if order changes
**Cons**: More code, requires org-mode navigation functions

#### Option C: Only Refresh Changed Sections (Advanced)

Instead of erasing the entire buffer, only update the sections that changed:

**Pros**: No cursor jumping, minimal disruption
**Cons**: Complex implementation, harder to maintain

### Recommendation

**Start with Option A** (simple position preservation) for immediate relief, then upgrade to **Option B** (smart agent tracking) for better UX.

### Implementation Plan

1. Add helper functions to `autoload/claude-multi-progress.el`
2. Modify `claude-multi--refresh-progress-from-status-files`
3. Test with multiple agents updating
4. Test with agents being added/removed

---

## 2. Fix Agent Name Overwriting with Null

### Problem

When agents update their status, the `status-summary.py` hook overwrites custom agent names with `null`, losing user customizations made via the rename feature.

**Impact**: High - Breaks the rename feature
**Frequency**: Every time an agent updates (constantly)

### Root Cause

In `hooks/status-summary.py`, the logic reads the existing agent_name from the status file, but if `agent_name` is `null` in the file OR if `CLAUDE_AGENT_NAME` environment variable is not set, it writes `null` back:

```python
# Current problematic code:
env_agent_name = os.environ.get("CLAUDE_AGENT_NAME")
existing_agent_name = existing_data.get("agent_name")  # May be null

# If both are null, writes null back
agent_name = existing_agent_name if existing_agent_name else env_agent_name
```

### Solution

Never overwrite an existing custom name with `null`. Only set agent_name if:
1. It doesn't exist yet (first time), OR
2. The environment variable has a non-null value

```python
def generate_status_json(state, cwd, context_info=None, git_info=None, model_info=None):
    """Generate the status data as JSON."""
    # ... existing code ...

    session_id = state.get("session_id")
    env_agent_name = os.environ.get("CLAUDE_AGENT_NAME")

    # Read existing agent name from status file
    existing_agent_name = None
    if session_id:
        status_dir = Path("/tmp/claude-status")
        status_file = status_dir / f"status-{session_id}.json"
        if status_file.exists():
            try:
                with open(status_file, 'r') as f:
                    existing_data = json.load(f)
                    existing_agent_name = existing_data.get("agent_name")
            except (json.JSONDecodeError, IOError):
                pass

    # IMPROVED: Never overwrite existing name with null
    # Priority:
    # 1. Keep existing custom name (if it exists and is not null)
    # 2. Use environment variable (if set and not null)
    # 3. Otherwise don't set agent_name field at all (or use None)

    if existing_agent_name:
        # Keep existing custom name
        agent_name = existing_agent_name
    elif env_agent_name:
        # Use environment variable if no custom name exists
        agent_name = env_agent_name
    else:
        # Don't set the field, or explicitly set to None
        agent_name = None

    status_data = {
        # ... other fields ...
    }

    # Only include agent_name if it has a value
    if agent_name is not None:
        status_data["agent_name"] = agent_name

    return json.dumps(status_data, indent=2)
```

### Additional Consideration

The environment variable `CLAUDE_AGENT_NAME` may not be set when launching agents. Check where this is set:

```bash
# In autoload/claude-multi-agents.el or wherever agents are spawned
export CLAUDE_AGENT_NAME="claude-agent-1"
```

If it's not being set, we should either:
1. Set it when spawning agents with a default name
2. OR don't rely on it at all for persistence (better)

### Implementation Plan

1. Update `hooks/status-summary.py` with improved logic
2. Test renaming an agent
3. Wait for status update
4. Verify name is preserved
5. Test with new agents (should get auto-generated names)

---

## 3. Add Table View for Agent List

### Problem

The current org-mode hierarchical view is good for detailed information, but a table view would be:
- Easier to scan quickly
- Better for seeing status at a glance
- More compact for many agents

**Impact**: Medium - Nice to have, improves usability
**Frequency**: Constant (affects how users view agents)

### Requirements

1. **Keep existing org-mode view** - Don't remove it
2. **Add new table view** - Parallel alternative
3. **Switch between views** - Via Emacs command
4. **Show key information** in table:
   - Status icon
   - Agent name
   - Directory/Branch
   - Status
   - Model
   - Time/Duration
   - Token usage %

### Proposed Table Format

```
┌──────┬─────────────────────┬──────────────────────┬──────────┬────────┬─────────┬────────┐
│ Icon │ Agent Name          │ Directory            │ Status   │ Model  │ Time    │ Tokens │
├──────┼─────────────────────┼──────────────────────┼──────────┼────────┼─────────┼────────┤
│ 🔵   │ backend-api-dev     │ ~/proj/api (main)    │ RUNNING  │ SONNET │ 12m 34s │ 45.2%  │
│ 🟢   │ frontend-refactor   │ ~/proj/web (feat-ui) │ COMPLETE │ SONNET │ 8m 12s  │ 23.8%  │
│ 🔴   │ database-migration  │ ~/proj/db (migrate)  │ FAILED   │ OPUS   │ 2m 45s  │ 12.1%  │
│ 🟡   │ docs-update         │ ~/proj/docs (main)   │ WAITING  │ HAIKU  │ 45s     │ 5.3%   │
└──────┴─────────────────────┴──────────────────────┴──────────┴────────┴─────────┴────────┘
```

### Implementation Approach

#### Option A: Emacs `tabulated-list-mode` (Recommended)

Use Emacs' built-in tabulated list mode for sortable columns:

```elisp
(define-derived-mode claude-multi-table-mode tabulated-list-mode "Claude-Multi-Table"
  "Major mode for viewing Claude agents in a table format.

Keybindings:
  \\[claude-multi/focus-agent-at-point] - Focus on agent at point
  \\[claude-multi/rename-agent-at-point] - Rename agent at point
  \\[claude-multi/switch-to-org-view] - Switch to org-mode view
  \\[tabulated-list-sort] - Sort by column"

  (setq tabulated-list-format
        [("Icon"   6 nil)
         ("Name"   25 t)
         ("Location" 30 t)
         ("Status" 10 t)
         ("Model"  8 t)
         ("Time"   10 t)
         ("Tokens" 8 t)])

  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Status" nil))
  (add-hook 'tabulated-list-revert-hook 'claude-multi--populate-table-view nil t)
  (tabulated-list-init-header))

(defun claude-multi--populate-table-view ()
  "Populate the table view with agent data from status files."
  (setq tabulated-list-entries
        (mapcar #'claude-multi--agent-to-table-entry
                (claude-multi--read-all-status-files))))

(defun claude-multi--agent-to-table-entry (status-data)
  "Convert status data to table entry format."
  (let* ((session-id (alist-get 'session_id status-data))
         (agent-name (or (alist-get 'agent_name status-data) "unnamed"))
         (status (alist-get 'claude_status status-data))
         (cwd (alist-get 'cwd status-data))
         (model (alist-get 'model_name status-data))
         (context (alist-get 'context_window status-data))
         (tokens-pct (or (alist-get 'percentage_used context) 0))
         (icon (claude-multi--get-status-icon-from-string status)))

    (list session-id
          (vector icon
                  agent-name
                  (file-name-nondirectory cwd)
                  (upcase status)
                  (or model "UNKNOWN")
                  "12m"  ; TODO: Calculate from started_at
                  (format "%.1f%%" tokens-pct)))))
```

**Pros**:
- Built-in Emacs mode, well-tested
- Sortable columns (click headers)
- Clean, native look
- Easy to implement

**Cons**:
- Fixed-width columns (may truncate)
- Less detailed than org-mode

#### Option B: `org-table` (Alternative)

Use org-mode tables for the view:

```elisp
(defun claude-multi--generate-org-table-view ()
  "Generate an org-mode table of agents."
  (let ((agents (claude-multi--read-all-status-files)))
    (with-temp-buffer
      (org-mode)
      (insert "| Icon | Name | Location | Status | Model | Time | Tokens |\n")
      (insert "|------+------+----------+--------+-------+------+--------|\n")
      (dolist (agent agents)
        (insert (format "| %s | %s | %s | %s | %s | %s | %.1f%% |\n"
                       (get-icon agent)
                       (get-name agent)
                       (get-location agent)
                       (get-status agent)
                       (get-model agent)
                       (get-time agent)
                       (get-tokens agent))))
      (org-table-align)
      (buffer-string))))
```

**Pros**:
- Familiar org-mode interface
- Auto-aligning columns
- Easy to export/copy

**Cons**:
- Not sortable by default
- More overhead

### Commands to Add

```elisp
;;;###autoload
(defun claude-multi/switch-to-table-view ()
  "Switch progress buffer to table view."
  (interactive)
  (claude-multi--ensure-progress-buffer)
  (with-current-buffer claude-multi--progress-buffer
    (setq claude-multi--view-mode 'table)
    (claude-multi--refresh-progress-view)))

;;;###autoload
(defun claude-multi/switch-to-org-view ()
  "Switch progress buffer to org-mode view."
  (interactive)
  (claude-multi--ensure-progress-buffer)
  (with-current-buffer claude-multi--progress-buffer
    (setq claude-multi--view-mode 'org)
    (claude-multi--refresh-progress-view)))

;;;###autoload
(defun claude-multi/toggle-view ()
  "Toggle between table and org-mode views."
  (interactive)
  (if (eq claude-multi--view-mode 'table)
      (claude-multi/switch-to-org-view)
    (claude-multi/switch-to-table-view)))
```

### Keybindings

Add to `config.el`:

```elisp
;; Progress buffer view switching
(define-key claude-multi-progress-mode-map (kbd "v") 'claude-multi/toggle-view)
(define-key claude-multi-progress-mode-map (kbd "t") 'claude-multi/switch-to-table-view)
(define-key claude-multi-progress-mode-map (kbd "o") 'claude-multi/switch-to-org-view)

;; Evil mode
(with-eval-after-load 'evil
  (evil-define-key 'normal claude-multi-progress-mode-map
    (kbd "v") 'claude-multi/toggle-view
    (kbd "t") 'claude-multi/switch-to-table-view
    (kbd "o") 'claude-multi/switch-to-org-view))
```

### Implementation Plan

1. Create new file: `autoload/claude-multi-table.el`
2. Implement `claude-multi-table-mode` using `tabulated-list-mode`
3. Add view switching logic to `autoload/claude-multi-progress.el`
4. Add commands to `config.el`
5. Update keybindings
6. Test view switching
7. Test sorting in table view
8. Verify rename/focus work in both views

---

## Implementation Priority

### Phase 1: Critical Fixes (This Week)
1. ✅ **Fix agent name overwriting** - Breaks existing feature
2. ✅ **Fix cursor jumping** - Major UX issue

### Phase 2: Enhancements (Next Week)
3. ⏳ **Add table view** - Nice to have, improves usability

---

## Testing Checklist

### Agent Name Preservation
- [ ] Rename an agent
- [ ] Wait for status update
- [ ] Verify name is preserved
- [ ] Restart Emacs
- [ ] Verify name is still preserved
- [ ] Spawn new agent
- [ ] Verify it gets a default name

### Cursor Position
- [ ] Place cursor on specific agent
- [ ] Wait for status update
- [ ] Verify cursor stays on same agent (or near same position)
- [ ] Test with agent at top of list
- [ ] Test with agent in middle of list
- [ ] Test with agent at bottom of list

### Table View
- [ ] Switch to table view
- [ ] Verify all agents appear
- [ ] Sort by each column
- [ ] Focus agent from table view
- [ ] Rename agent from table view
- [ ] Switch back to org view
- [ ] Verify data is same

---

## Notes

- Keep both views in sync - they should show the same data
- Consider adding a configuration variable for default view preference
- Table view may need its own refresh logic to avoid flickering
- Consider adding filtering/search in future iterations

---

**Last Updated**: 2026-01-12
