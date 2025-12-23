# Agent Coordination for Monet Integration

## Status Dashboard

| Agent | Phase | Status | Branch | Started | Completed |
|-------|-------|--------|--------|---------|-----------|
| Agent 1 | WebSocket | 🟢 Completed | feature/monet-integration | 2025-12-22 | 2025-12-22 |
| Agent 2 | MCP | 🟡 Not Started | feature/mcp-protocol | - | - |
| Agent 3 | Ediff | 🟡 Not Started | feature/ediff-integration | - | - |
| Agent 4 | Session | 🟡 Not Started | feature/session-persistence | - | - |

Status: 🟡 Not Started | 🔵 In Progress | 🟢 Completed | 🔴 Blocked

## Interface Contracts

### Agent 1: WebSocket Foundation
**Interface File:** `autoload/claude-multi-websocket.el`

**Exported Functions:**
- `claude-multi-ws--start-server` → port-number
- `claude-multi-ws--stop-server` → void
- `claude-multi-ws--send-message` (agent-id message) → boolean
- `claude-multi-ws--is-connected` (agent-id) → boolean

**Data Structures:**
- `claude-multi--ws-server` (global variable)
- `claude-multi--ws-port` (global variable)
- `claude-multi--ws-connections` (hash table: agent-id → connection)

**Modifications to Existing Files:**
- `autoload/claude-multi-agents.el`: Add fields to `claude-agent` struct
  - `websocket-connection`
  - `communication-backend`
  - `mcp-enabled`
  - `session-id`
  - `mcp-request-counter`
  - `ediff-session`

### Agent 2: MCP Protocol
**Interface File:** `autoload/claude-multi-mcp.el`

**Exported Functions:**
- `claude-multi-mcp--register-tool` (tool-name handler-fn) → void
- `claude-multi-mcp--handle-request` (agent-id request) → response
- `claude-multi-mcp--defer-response` (agent-id request-id callback) → void
- `claude-multi-mcp--complete-deferred-response` (agent-id request-id result) → void

**Built-in MCP Tools:**
- `file/read` - Read file contents
- `file/write` - Write file contents
- `file/list` - List files in directory
- `git/status` - Get git status
- `git/diff` - Get git diff output
- `agent/focus` - Focus agent kitty window
- `agent/status` - Get agent status
- `diff/request` - Request diff review (deferred)
- `session/save` - Save current session
- `session/list` - List available sessions
- `diagnostics/get` - Get Flymake/Flycheck diagnostics
- `selection/get` - Get current Emacs selection

**Dependencies:**
- Requires Agent 1's WebSocket message sending functions
- Uses Agent 1's connection registry

### Agent 3: Ediff Integration
**Interface File:** `autoload/claude-multi-ediff.el`

**Exported Functions:**
- `claude-multi-ediff--create-session` (agent) → ediff-session
- `claude-multi-ediff--show-diff` (agent file-path) → void
- `claude-multi-ediff--accept-changes` (agent) → void
- `claude-multi-ediff--reject-changes` (agent) → void
- `claude-multi-ediff--get-worktree-diff` (agent) → diff-string

**Interactive Commands:**
- `claude-multi/review-agent-changes` - Review changes for selected agent
- `claude-multi/accept-current-diff` - Accept current file changes
- `claude-multi/reject-current-diff` - Reject current file changes
- `claude-multi/next-diff-file` - Jump to next file in review queue

**Dependencies:**
- Requires Agent 2's deferred response pattern
- Uses Agent 2's MCP tool registration

### Agent 4: Session Persistence
**Interface File:** `autoload/claude-multi-session.el`

**Exported Functions:**
- `claude-multi-session--save` → void
- `claude-multi-session--restore` (session-id) → void
- `claude-multi-session--list-sessions` → (list of session-ids)
- `claude-multi-session--delete-session` (session-id) → void
- `claude-multi-session--serialize-agent` (agent) → plist
- `claude-multi-session--deserialize-agent` (plist) → agent

**Interactive Commands:**
- `claude-multi/save-session` - Manually save session
- `claude-multi/restore-session` - Restore session from file
- `claude-multi/list-sessions` - Browse available sessions
- `claude-multi/delete-session` - Delete saved session

**Dependencies:**
- Requires Agent 1's updated agent struct fields

## Integration Points

1. **WebSocket → MCP**: WebSocket routes "mcp-request" messages to MCP handler
2. **MCP → Ediff**: MCP tool "diff/request" creates ediff session
3. **Ediff → MCP**: Ediff completion triggers MCP deferred response
4. **Session → All**: Session serializes agent state including WebSocket/MCP metadata

## PR Order

1. Agent 1 creates PR: `feature/websocket-foundation` → `feature/monet-integration`
2. After Agent 1 PR merged, Agent 2 & 4 create PRs to `feature/monet-integration`
3. After Agent 2 PR merged, Agent 3 creates PR to `feature/monet-integration`
4. After all sub-agent PRs merged, run integration testing on `feature/monet-integration`
5. Create final PR: `feature/monet-integration` → `main`

## Communication Protocol

### When Agent 1 Completes:
1. Commit all changes to `feature/websocket-foundation`
2. Run tests: `make test` (all tests must pass)
3. Push branch: `git push origin feature/websocket-foundation`
4. Create PR to `feature/monet-integration`
5. Update status in this file to 🟢 Completed
6. After PR review and merge, Agents 2 & 4 can start (rebase from `feature/monet-integration`)

### When Agent 2 Completes:
1. Commit all changes to `feature/mcp-protocol`
2. Run tests: `make test` (all tests must pass)
3. Push branch: `git push origin feature/mcp-protocol`
4. Create PR to `feature/monet-integration`
5. Update status in this file to 🟢 Completed
6. After PR review and merge, Agent 3 can start (rebase from `feature/monet-integration`)

### When Agent 3 Completes:
1. Commit all changes to `feature/ediff-integration`
2. Run tests: `make test` (all tests must pass)
3. Push branch: `git push origin feature/ediff-integration`
4. Create PR to `feature/monet-integration`
5. Update status in this file to 🟢 Completed

### When Agent 4 Completes:
1. Commit all changes to `feature/session-persistence`
2. Run tests: `make test` (all tests must pass)
3. Push branch: `git push origin feature/session-persistence`
4. Create PR to `feature/monet-integration`
5. Update status in this file to 🟢 Completed

## Autonomy Rules

**Sub-agents MUST:**
- ✅ Create, modify, and delete files autonomously
- ✅ Run tests and fix failures independently
- ✅ Commit changes with descriptive messages
- ✅ Make implementation decisions independently
- ✅ Push branches and create PRs without asking

**Sub-agents MUST NOT:**
- ❌ Ask for user confirmation during implementation
- ❌ Wait for approval to commit or make changes
- ❌ Stop work due to minor issues (fix them instead)

## Testing Requirements

Each agent must:
1. Write unit tests for all new functions
2. Write integration tests for their feature
3. Ensure all tests pass before creating PR
4. Follow existing test patterns in `test/` directory
5. Use `buttercup` testing framework

## Code Quality Standards

All agents must follow:
1. **File size**: Keep all files between 500-800 LOC
2. **Documentation**: Every function has a docstring
3. **Namespace**: Use `claude-multi-` prefix for public functions
4. **Autoload**: Mark public functions with `;;;###autoload`
5. **Style**: Follow Emacs Lisp conventions (dash-separated names)
6. **Dependencies**: Use `cl-lib`, avoid deprecated `cl.el`

## Branch Hierarchy

```
main (protected)
  └── feature/monet-integration (integration branch)
       ├── feature/websocket-foundation (Agent 1)
       ├── feature/mcp-protocol (Agent 2)
       ├── feature/ediff-integration (Agent 3)
       └── feature/session-persistence (Agent 4)
```

## Worktree Structure

```
claude-multi-agent.el/                    (main repo on feature/monet-integration)
├── autoload/
├── test/
└── ...

../claude-worktrees/
├── monet-integration/                    (Base integration branch)
│   └── feature/monet-integration
├── websocket-foundation/                 (Agent 1 worktree)
│   └── feature/websocket-foundation
├── mcp-protocol/                         (Agent 2 worktree)
│   └── feature/mcp-protocol
├── ediff-integration/                    (Agent 3 worktree)
│   └── feature/ediff-integration
└── session-persistence/                  (Agent 4 worktree)
    └── feature/session-persistence
```

## Notes

- This file is the single source of truth for all agents
- Each agent must read this file before starting work
- Status updates should be committed and pushed regularly
- PRs should include clear descriptions and test evidence
- Integration testing happens on `feature/monet-integration` after all PRs merged
