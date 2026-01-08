# Known Bugs

<!-- vale off -->

## Git Worktree Creation Timing Issue

**Status**: Fixed
**Priority**: High
**Affects**: Agent creation with git worktree isolation

### Description

The git worktree is currently being created before launching the Claude agent in the new kitty tab. This causes issues because the worktree creation happens in the wrong directory context, and the subsequent kitty terminal doesn't automatically navigate into the worktree directory.

### Current (Incorrect) Behavior

1. Create git worktree in Emacs context
2. Launch new kitty tab
3. Start Claude in original directory (not in worktree)

### Expected (Correct) Behavior

The worktree creation should happen **inside** the new kitty tab after it's launched:

1. Spawn new agent with worktree parameters (provide directory and branch name)
2. Open new tab in kitty
3. `cd` into the base directory
4. Ensure branch is up to date from master/main (`git fetch origin main && git rebase origin/main` or similar)
5. Execute `gwt` command with branch name
6. `cd` into the newly created git worktree directory
7. Start Claude

### Impact

- Agents don't run in isolated worktree environments as intended
- File changes may affect the main working directory instead of the isolated worktree
- Parallel agents may conflict with each other

### Technical Notes

- The worktree creation logic needs to be moved from Emacs elisp to commands executed within the kitty terminal
- Commands should be sent via `kitty @ send-text` after tab creation
- Need to wait for worktree creation to complete before starting Claude
- Consider using `gwt` command if available, or fallback to `git worktree add`
- Must ensure the branch is up to date with master/main before creating the worktree to avoid working with stale code
- Update sequence should be: fetch latest changes from remote, then rebase or merge with main branch

### Related Files

- `autoload/claude-multi-agents.el` - Agent creation logic
- `autoload/claude-multi-worktree.el` - Git worktree management
- `config.el` - Configuration and command definitions

### Workaround

None currently. Agents will run in the main working directory until this is fixed.

### Resolution

**Fixed**: The worktree creation has been moved from Emacs context to the kitty terminal.

**Changes made**:

1. **autoload/claude-multi-worktree.el**:
   - Added `claude-multi--get-default-branch()` to detect main/master branch dynamically
   - Added `claude-multi--build-worktree-command()` to construct the full shell command chain
   - Command chain includes: cd to repo root, git fetch, git rebase, worktree creation (gwt or git worktree add), cd to worktree, start Claude

2. **autoload/claude-multi-agents.el**:
   - Modified `claude-multi--launch-agent()` to calculate worktree path without creating it in Emacs
   - Changed kitty launch to start in git repository root (not worktree)
   - Updated command sending logic to build and send the chained worktree command
   - Command uses `&&` operators to ensure each step completes before the next

**How it works now**:

1. Emacs calculates the target worktree path but doesn't create it
2. Kitty terminal launches in the git repository root directory
3. A chained shell command is sent to kitty that:
   - Fetches latest changes from origin
   - Rebases with main/master to ensure up-to-date code
   - Creates worktree using `gwt` (if available) or falls back to `git worktree add`
   - Changes into the worktree directory
   - Starts Claude in the worktree

**Benefits**:

- Worktree is created in correct directory context
- Branch is automatically updated from main/master before work begins
- Uses custom `gwt` function if available, with automatic fallback
- Command chaining ensures proper sequencing and error handling
- Claude starts in the correct worktree directory

<!-- vale on -->
