# Claude Multi-Agent - Feature Roadmap

This document outlines planned future features for the claude-multi-agent.el plugin. Features are organized by implementation phase, with each phase building upon the previous one.

## Overview

The claude-multi-agent.el plugin enables parallel execution of multiple Claude Code agents with git `worktree` isolation. This roadmap extends the current capabilities to provide deeper integration between Emacs and kitty terminals, enhanced status monitoring, and automated workflow completion.

---

## Phase 1: Enhanced Communication & Monitoring

Core features that improve real-time communication and status visibility between Emacs and Claude agents.

### - [ ] Bidirectional Emacs-Kitty Communication

**Description**: Enable two-way communication between Emacs and Claude terminals in kitty.

**Benefits**:

- Send commands and prompts from Emacs to running agents
- Capture output from kitty terminals back to Emacs
- Enable automated responses to agent questions

**Implementation Notes**:

- Leverage kitty's remote control API for sending commands (`kitty @ send-text`)
- Use `websockets` as event mechanism to sync Emacs and Claude Code
- Implement `websocket` server in Emacs to receive events from Claude terminals
- Create Emacs functions to read agent output in real-time
- Implement message queue for coordinating commands
- Reference implementation: [`monet`](https://github.com/stevemolitor/monet) - Communication framework between Emacs and Claude Code

**Dependencies**: None (foundational feature)

**References**:

- [`monet`](https://github.com/stevemolitor/monet) - Websocket-based communication between Emacs and Claude Code

---

### - [ ] Visual "Waiting for Input" Status Indicator

**Description**: Clear visual indication in Emacs when an agent is waiting for user input.

**Benefits**:

- Immediate awareness of blocked agents
- Reduce context switching between Emacs and kitty windows
- Visual dashboard shows which agents need attention

**Implementation Notes**:

- Enhance existing input detection patterns in `claude-multi-notifications.el`
- Add prominent visual indicator in progress buffer (animated icon, color highlight)
- Update mode-line indicator to show specific agent IDs waiting
- Consider adding desktop notification with agent name

**Dependencies**:

- Builds on existing input detection patterns
- Enhanced by bidirectional communication for more reliable detection

---

### - [ ] Agent "Killed" Status Tracking

**Description**: Mark agents with explicit "Killed" status when manually terminated.

**Benefits**:

- Distinguish between successful completion and forced termination
- Better audit trail of agent lifecycle
- Enable different cleanup behavior for killed vs completed agents

**Implementation Notes**:

- Add `killed` to status `enum` in `claude-agent` struct
- Modify `claude-multi/kill-agent` to set status before cleanup
- Update progress buffer to show ⚫ icon for killed agents
- Preserve killed agent history in session document

**Dependencies**: None

---

### - [ ] Context Window Percentage Display

**Description**: Show current Claude context window usage percentage for each agent.

**Benefits**:

- Monitor when agents approach context limits
- Proactively manage long-running conversations
- Warning indicators when approaching 80%+ usage

**Implementation Notes**:

- Parse Claude output for context window stats (if exposed in CLI)
- Alternative: Track approximate token count based on captured output
- Display percentage in agent status section of progress buffer
- Add visual warning (red) when > 80% full
- Implement periodic polling (every status check cycle)

**Dependencies**: Requires bidirectional communication to capture output

---

### - [ ] Claude Mode Tracking

**Description**: Display current Claude mode (chat, edit, etc.) in session document.

**Benefits**:

- Understand what type of work each agent is performing
- Filter/group agents by mode in dashboard
- Context for interpreting agent behavior

**Implementation Notes**:

- Parse Claude output to detect mode switches
- Add `current-mode` field to `claude-agent` struct
- Display mode badge in progress buffer agent headers
- Track mode history with timestamps
- Support modes: `chat`, `edit`, `code`, `plan`, etc.

**Dependencies**: Requires bidirectional communication to capture mode changes

---

## Phase 2: Developer Experience Enhancements

Features that improve code review workflow and visibility into agent changes.

### - [ ] Session-Based Kitty Window Organization

**Description**: Open a dedicated kitty OS window for each session, with all session agents organized as tabs or splits within that window.

**Benefits**:

- Visual grouping of agents by session
- Easy switching between different sessions (OS window switching)
- Reduced desktop clutter with all session agents in one window
- Clear separation between unrelated work
- Better multi-monitor workflow support

**Implementation Notes**:

- When first agent of a session is created:
  - Launch new kitty OS window (`kitty @ launch --type=os-window`)
  - Store OS window ID in session metadata
  - Tag window with session name/ID
- For subsequent agents in same session:
  - Check if user prefers tabs or splits (configurable)
  - If tabs: Launch as new tab in session window (`kitty @ launch --type=tab --match=id:WINDOW_ID`)
  - If splits: Launch as split within current tab (`kitty @ launch --type=window --match=id:TAB_ID`)
- Track session-to-window mapping in `claude-multi--sessions` hash table
- Add session cleanup to close entire OS window when all agents complete

**Configuration Variables**:

```elisp
claude-multi-session-window-mode    - 'dedicated-window, 'shared, or 'individual (default: 'dedicated-window)
claude-multi-agent-spawn-type       - 'tab or 'split (default: 'tab)
claude-multi-max-splits-per-tab     - Number (default: 4, max splits before new tab)
```

**Dependencies**: None (builds on existing kitty integration)

---

### - [ ] Ediff Integration for Change Review

**Description**: Open Emacs `ediff` tool in a new buffer to review changes made by agents.

**Benefits**:

- Review diffs without leaving Emacs
- Use familiar Emacs `ediff` interface for change review
- Accept/reject changes interactively

**Implementation Notes**:

- Track modified files in agent `worktree` using git status
- Create command `claude-multi/review-agent-changes`
- Generate diffs: `git diff branch-base..agent-branch`
- Open `ediff` buffer comparing base vs agent version
- Support reviewing multiple files sequentially
- Add keybinding (e.g., `SPC c m r`)

**Dependencies**:

- Requires git `worktree` integration (already implemented)
- Enhanced by bidirectional communication for automatic change detection

---

### - [ ] Session Document Links to Diff Buffers

**Description**: Clickable links/shortcuts in session document that open `ediff` buffers.

**Benefits**:

- Quick navigation from agent status to change review
- One-click access to see what each agent modified
- Embedded file change list in session document

**Implementation Notes**:

- Add "Changes" section under each agent in progress buffer
- List modified files as org-mode links: `[[ediff:agent-id:file.el][file.el]]`
- Implement custom link handler for `ediff` protocol
- Link handler opens `ediff` for specific agent + file
- Auto-update links when new changes detected
- Show file count badge (e.g., "3 files changed")

**Dependencies**: Requires `ediff` integration feature

---

### - [ ] Enhanced Agent Status Summaries

**Description**: Add comprehensive status summary for each agent in session document.

**Benefits**:

- At-a-glance understanding of agent progress
- Rich metadata without cluttering main view
- Historical record of agent work

**Implementation Notes**:

- Expand agent section in progress buffer with:
  - **Summary**: Brief description of current task/status
  - **Context Window**: Percentage usage
  - **Mode**: Current Claude mode
  - **Files Changed**: Count and list
  - **Runtime**: Elapsed time since creation
  - **Last Activity**: Timestamp of last output
- Use org-mode properties drawer for structured data
- Implement collapsible sections for detailed info

**Dependencies**:

- Context window tracking
- Claude mode tracking
- File change detection

---

## Phase 3: Workflow Automation & Completion

Advanced features that automate the full cycle from agent work to GitHub PR.

### - [ ] Agent Completion Command with Automated Workflow

**Description**: Single command to finish an agent with full automated cleanup and documentation.

**Command**: `claude-multi/finish-agent` (keybinding: `SPC c m F`)

**Workflow Steps**:

1. **Summarize Changes**
   - Parse `git log` and `git diff --stat` for agent's branch
   - Generate human-readable summary of changes
   - Extract file-level summaries (added, modified, deleted)
   - Compile into markdown format

2. **Add Summary to Session Document**
   - Create new "Completion Summary" section under agent
   - Include:
     - Change summary
     - Files modified (with line counts)
     - Commit messages from agent's branch
     - Link to PR (if created)
   - Timestamp completion

3. **Create GitHub PR**
   - Use GitHub CLI (`gh pr create`) or GitHub API
   - PR title: Agent task description
   - PR body: Generated change summary + link to session document
   - Auto-assign reviewers if configured
   - Add labels: `claude-agent`, `auto-generated`
   - Return PR URL

4. **Link to PR in Session Document**
   - Add clickable PR link to agent's completion summary
   - Update agent status to `completed-pr-created`
   - Show PR number and URL

5. **Cleanup Git Worktree and Directory**
   - Execute `git worktree remove --force`
   - Delete branch if PR merged or no longer needed
   - Remove `worktree` directory
   - Clean up any temporary files
   - Close kitty window if still open

**Implementation Notes**:

- Make workflow steps configurable (enable/disable each step)
- Support dry-run mode to preview actions
- Add confirmation prompt before PR creation
- Handle errors gracefully at each step (continue or abort)
- Log all actions to session document
- Create `autoload/claude-multi-completion.el` for workflow logic

**Benefits**:

- One-command workflow from agent completion to PR
- Consistent documentation across all agent work
- Reduced manual toil in multi-agent workflows
- Audit trail of all agent contributions

**Dependencies**:

- Change summarization (git integration)
- GitHub integration (CLI or API)
- Session document update capability (already implemented)
- Git `worktree` cleanup (already implemented)

**Configuration Variables**:

```elisp
claude-multi-auto-create-pr          - Boolean (default: t)
claude-multi-pr-base-branch          - String (default: "main")
claude-multi-pr-reviewers            - List of GitHub usernames
claude-multi-pr-labels               - List of label strings
claude-multi-completion-confirm      - Boolean (default: t)
claude-multi-cleanup-on-finish       - Boolean (default: t)
```

---

## Implementation Priority

Suggested implementation order based on feature dependencies:

1. **High Priority - Foundation**
   - Bidirectional Emacs-Kitty communication (enables many other features)
   - Agent "Killed" status tracking (quick win, no dependencies)

2. **Medium Priority - Monitoring**
   - Visual "waiting for input" status indicator
   - Context window percentage display
   - Claude mode tracking

3. **Medium Priority - Developer Experience**
   - `ediff` integration for change review
   - Enhanced agent status summaries
   - Session document links to diff buffers

4. **High Priority - Automation**
   - Agent completion command with automated workflow (highest value feature)

---

## Future Considerations

Additional features that may be considered after initial roadmap completion:

- **Agent Templates**: Predefined task templates for common workflows
- **Agent Collaboration**: Coordinate multiple agents working on related tasks
- **Output Filtering**: Customizable filters for agent output in progress buffer
- **Session Persistence**: Save/restore agent sessions across Emacs restarts
- **Remote Agent Support**: Run agents on remote machines via SSH
- **Agent Metrics Dashboard**: Aggregate statistics across all agents/sessions
- **Conflict Resolution UI**: Handle merge conflicts from parallel agents
- **Custom Hooks**: User-defined hooks for agent lifecycle events

---

## Contributing

To propose new features or discuss implementation details, please open an issue on the GitHub repository.
