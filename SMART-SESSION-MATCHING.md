# Smart Session Matching

## Overview

When you spawn an agent, it needs to match to a Claude Code session running in that directory. If multiple sessions exist in the same directory (e.g., your home directory), the system uses a **smart scoring algorithm** to pick the best match.

## How It Works

### Scoring Algorithm

Each status file is evaluated and assigned a score:

| Session State | Score | Priority |
|--------------|-------|----------|
| **Running** (claude_status: "running") | 10,000,000 | Highest |
| **Recently finished** (<10 minutes old) | 100,000 | High |
| **Older finished** (>10 minutes old) | 100,000 - age_seconds | Medium |
| **No timestamp** | 0 | Lowest |

The agent matches to the session with the **highest score**.

### Example Scenario

You spawn an agent in your home directory (`~/`), which has these status files:

```
/tmp/claude-status/
├── status-abc123.json  # finished 2 hours ago → score: ~28,000
├── status-def456.json  # finished 5 minutes ago → score: 100,000
└── status-ghi789.json  # running now → score: 10,000,000
```

**Result:** Agent matches to `status-ghi789.json` (running session) ✓

## Benefits

### ✅ Spawn Agents Anywhere

You can now reliably spawn agents in:
- **Home directory** (`~/`)
- **Frequently-used project directories**
- **Any directory with multiple sessions**

The system will automatically prefer the active session.

### ✅ Automatic Prioritization

- **Active sessions** always win (score: 1M)
- **Recent sessions** preferred over old ones
- **Stale sessions** naturally deprioritized

### ✅ Multiple Agents, Same Directory

If you have multiple Claude sessions in the same directory, each agent will match to a different session (sessions can't be claimed twice).

## Usage Examples

### Example 1: Home Directory

```elisp
;; Spawn agent in home directory
Task: Test home directory monitoring
Working directory: ~/
```

The agent will match to whichever Claude session is **actively running** in `~/`, not old finished sessions.

### Example 2: Project Directory

```elisp
;; Spawn agent in project
Task: Monitor build process
Working directory: ~/projects/my-app/
```

If you have multiple terminal windows with Claude running in that project, the agent matches to the most recent/active one.

### Example 3: Multiple Agents

```elisp
;; First agent
Task: Monitor frontend build
Working directory: ~/projects/app/
→ Matches to session A (running)

;; Second agent (same directory)
Task: Monitor backend tests
Working directory: ~/projects/app/
→ Matches to session B (running, unclaimed)
```

## Technical Details

### Session States

Claude Code status files have these states:

- `"running"` - Claude is actively working
- `"finished"` - Claude session completed
- `"waiting_for_input"` - Claude needs user input (still active)

### Timestamp Format

Status files use ISO 8601 timestamps:
```json
{
  "timestamp": "2026-01-09T13:28:41.123456",
  "claude_status": "running"
}
```

### Matching Process

1. **Agent created** with working directory
2. **Scan status files** in `/tmp/claude-status/`
3. **Filter by CWD** - only consider matching directories
4. **Score each candidate** - apply scoring algorithm
5. **Select best match** - highest score wins
6. **Register agent** - claim the session
7. **Watch for updates** - file-notify monitors changes

## Troubleshooting

### Agent shows "Waiting for first status update..."

**Cause:** No active Claude session found in that directory.

**Solution:**
1. Verify a Claude session is running: `ls -la /tmp/claude-status/*.json`
2. Check the session's CWD matches agent's working directory
3. Look for status files with recent timestamps

### Agent matched to wrong session

**Cause:** Multiple running sessions in same directory.

**Solution:**
- Sessions are claimed first-come-first-served
- Second agent will match to the next unclaimed session
- Check `claude-multi--session-to-agent` hash table to see which sessions are claimed

### Old sessions not cleaned up

**Cause:** Status files persist after Claude sessions end.

**Solution:**
- The system automatically prefers recent/active sessions
- Old files are harmless (just deprioritized)
- Can manually clean: `rm /tmp/claude-status/status-*.json`

## Implementation

See `autoload/claude-multi-status.el`, function `claude-multi--register-agent-for-status` (line 227).

The scoring logic is in lines 268-276:
```elisp
(score (cond
         ;; Running sessions get highest score
         (is-running 10000000)
         ;; Recent sessions (< 10 minutes old) get medium score
         ((and age-seconds (< age-seconds 600)) 100000)
         ;; Older sessions get score based on recency
         (age-seconds (max 0 (- 100000 age-seconds)))
         ;; No timestamp - lowest score
         (t 0)))
```

## Related Files

- `autoload/claude-multi-status.el` - Session matching implementation
- `autoload/claude-multi-agents.el` - Agent creation and management
- `test/check-parens.py` - Syntax validation for all .el files

## Version

Added in commit: `0021ff6` (2026-01-09)
