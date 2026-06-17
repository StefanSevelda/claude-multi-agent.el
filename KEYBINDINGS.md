# Claude Multi-Agent Keybindings

All keybindings are under the `SPC c m` prefix (Space → c → m).

## Quick Reference

### Session Management
- `SPC c m s` - **Start session** - Initialize a new multi-agent session
- `SPC c m S` - **Save session** - Save current session to disk
- `SPC c m R` - **Restore session** - Load a saved session
- `SPC c m L` - **List sessions** - Show all saved sessions
- `SPC c m D` - **Delete session** - Remove a saved session

### Agent Operations
- `SPC c m a` - **Spawn agent** - Create new agent in current directory
- `SPC c m w` - **Spawn with worktree** - Create agent with git worktree isolation
- `SPC c m f` - **Focus agent** - Jump to specific agent terminal
- `SPC c m k` - **Kill agent** - Terminate a single agent
- `SPC c m K` - **Kill all agents** - Terminate all running agents

### Monitoring & UI
- `SPC c m d` - **Dashboard** - Open agent management dashboard
- `SPC c m p` - **Open progress** - View org-mode progress buffer
- `SPC c m e` - **Export progress** - Export session to markdown

### Utilities
- `SPC c m c` - **Cleanup status files** - Remove stale status JSON files
- `SPC c m l` - **List worktrees** - Show all git worktrees
- `SPC c m ?` - **Debug status matching** - Diagnostic info for status tracking

### Triage Filters (under `SPC c m T`)

These apply a sparse-tree filter to `triage.org`, folding everything except matching entries.

- `SPC c m T w` - **This week** - TODO/INPROGRESS with DEADLINE within current ISO week (Mon–Sun)
- `SPC c m T n` - **No date** - TODO/INPROGRESS entries with no DEADLINE
- `SPC c m T p` - **POSTPONE** - All POSTPONE entries
- `SPC c m T c` - **Clear filter** - Expand all entries (reset view)

### Code Review (under `SPC c m r`)
- `SPC c m r r` - **Review agent changes** - Start reviewing agent's code changes
- `SPC c m r a` - **Accept current diff** - Accept the current diff hunk
- `SPC c m r x` - **Reject current diff** - Reject the current diff hunk
- `SPC c m r n` - **Next diff file** - Move to next file in review

## Alternative Access

All commands are also available via:
- `M-x claude-multi/<command-name>`
- Example: `M-x claude-multi/start-session`

## Setup Verification

To verify keybindings are loaded:

```elisp
;; Check if keymap exists
(lookup-key doom-leader-map (kbd "c m"))
;; Should return a keymap, not nil

;; List all bindings
M-x describe-keymap RET doom-leader-map RET
;; Search for "claude-multi"
```

## Common Workflows

### Starting a New Session
1. `SPC c m s` - Start session
2. `SPC c m a` - Spawn first agent
3. Give agent a task
4. `SPC c m a` - Spawn additional agents as needed
5. `SPC c m p` - Monitor progress

### Working with Worktrees
1. `SPC c m w` - Spawn agent with worktree
2. Agent works in isolated git worktree
3. `SPC c m l` - List all worktrees
4. Worktrees auto-cleanup when agents complete

### Reviewing Changes
1. `SPC c m r r` - Start review
2. `SPC c m r a` - Accept good changes
3. `SPC c m r x` - Reject bad changes
4. `SPC c m r n` - Next file

### Saving/Restoring Work
1. `SPC c m S` - Save current session
2. Close Emacs
3. Restart Emacs later
4. `SPC c m R` - Restore saved session
5. Continue where you left off

## Troubleshooting

If keybindings don't work after Emacs restart:

1. **Check if module loaded:**
   ```elisp
   (featurep 'claude-multi)
   ```

2. **Verify keybindings are registered:**
   ```elisp
   ;; Should return a keymap, not nil
   (lookup-key doom-leader-map (kbd "c m"))
   ```

3. **If keybindings are missing, reload the module:**
   ```elisp
   (load-file "~/.doom.d/modules/tools/claude-multi/config.el")
   ```

4. **Run Doom sync if nothing works:**
   ```bash
   doom sync
   ```
   Then restart Emacs

## Customization

To change keybindings, edit `config.el` and modify the `map!` call inside the `(after! evil ...)` block.

Example - change prefix from `c m` to `C a`:
```elisp
(after! evil
  (map! :leader
        :prefix ("C a" . "claude-multi")  ; Changed from "c m"
        :desc "Start session" "s" #'claude-multi/start-session
        ...))
```

Then run:
```bash
doom sync
```

And restart Emacs.
