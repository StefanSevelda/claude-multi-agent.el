# Testing Guide: Worktree Creation Fix

<!-- vale off -->

This guide helps you verify that the worktree creation fix is working correctly.

## Prerequisites

1. Emacs with claude-multi-agent.el loaded
2. kitty terminal with remote control enabled
3. A git repository to test with
4. (Optional) `gwt` command from local-setup repo available in your shell

## Test Scenarios

### Test 1: Basic Worktree Creation with Custom Branch

**Objective**: Verify that worktree is created in the terminal, not in Emacs.

**Steps**:

1. Open Emacs in a git repository
2. Run: `M-x claude-multi/start-session`
3. Run: `M-x claude-multi/spawn-agent-with-worktree`
4. Enter task description: "Test worktree creation"
5. Select a directory for the worktree
6. Enter branch name: "test-worktree-fix"

**Expected Results**:

- Kitty window opens in the repository root directory
- You see command output in the terminal showing:
  - `git fetch origin main` (or master)
  - `git rebase origin/main`
  - Worktree creation (either via gwt or git worktree add)
  - Directory change to worktree
  - Claude starts in the worktree directory

**Verification**:

- In the kitty terminal, run: `pwd`
- Should show the worktree path (e.g., `../claude-worktrees/reponame-agent-1`)
- Run: `git branch` - should show you're on the new branch
- Run: `git log -1` - should show the latest commit from main/master

### Test 2: gwt Command Fallback

**Objective**: Verify fallback to `git worktree add` when gwt is not available.

**Steps**:

1. Temporarily rename or disable `gwt` in your shell
2. Follow Test 1 steps above

**Expected Results**:

- Should see `git worktree add` command execute instead of gwt
- Worktree should be created successfully
- Claude should start in the worktree directory

### Test 3: Multiple Agents in Parallel

**Objective**: Verify multiple agents can create worktrees simultaneously.

**Steps**:

1. Start a session: `M-x claude-multi/start-session`
2. Spawn 3 agents with different branch names:
   - Agent 1: branch "feature-1"
   - Agent 2: branch "feature-2"
   - Agent 3: branch "feature-3"

**Expected Results**:

- Three kitty tabs/windows open
- Each creates its own worktree
- No conflicts or errors
- All agents start in their respective worktrees

**Verification**:

- Check that 3 worktrees exist: `git worktree list`
- Each should be on a different branch
- All should be up-to-date with main/master

### Test 4: Agent Without Worktree

**Objective**: Verify agents work normally without worktree when no branch specified.

**Steps**:

1. Start session: `M-x claude-multi/start-session`
2. Run: `M-x claude-multi/spawn-agent`
3. Enter task description (don't specify branch)

**Expected Results**:

- Agent starts in current directory
- No worktree creation attempted
- Claude starts normally

### Test 5: Main vs Master Branch Detection

**Objective**: Verify default branch detection works for both main and master.

**Test with main branch**:

1. In a repository using `main` as default branch
2. Spawn agent with worktree
3. Check terminal output shows: `git fetch origin main` and `git rebase origin/main`

**Test with master branch**:

1. In a repository using `master` as default branch
2. Spawn agent with worktree
3. Check terminal output shows: `git fetch origin master` and `git rebase origin/master`

### Test 6: Worktree Cleanup

**Objective**: Verify cleanup still works after fix.

**Steps**:

1. Create an agent with worktree
2. Complete the agent's task (close the kitty window)
3. Check if cleanup configuration is respected

**Expected Results**:

- If `claude-multi-auto-cleanup` is `t`: worktree should be automatically removed
- Run `git worktree list` to verify cleanup
- Check that worktree directory is removed from filesystem

**Verification**:

```elisp
;; Check current cleanup setting
claude-multi-auto-cleanup

;; List any remaining worktrees
(claude-multi--list-worktrees)
```

### Test 7: Error Handling

**Objective**: Verify graceful handling when commands fail.

**Test network issue**:

1. Disconnect from network (or block git fetch)
2. Try to spawn agent with worktree
3. Should see error in terminal, but Claude shouldn't start

**Test invalid branch**:

1. Try to spawn with invalid characters in branch name
2. Should see error message
3. Agent should fail gracefully

## Debugging Tips

### View Command Being Sent

Add temporary debug output to see the command:

```elisp
;; In claude-multi-agents.el, around line 183:
(message "DEBUG: Sending command: %s" command)
(claude-multi--send-to-kitty agt command)
```

### Check Worktree Module Output

```elisp
;; Test default branch detection
(claude-multi--get-default-branch)  ; Should return "main" or "master"

;; Test worktree path calculation (requires agent struct)
;; This would be done during agent creation
```

### Monitor kitty Commands

```bash
# In another terminal, watch kitty remote control commands
# (requires kitty debug logging enabled)
tail -f ~/.local/share/kitty/log
```

## Common Issues and Solutions

### Issue: gwt not found error

**Symptom**: Terminal shows "gwt: command not found" and falls back to git worktree add

**Solution**: This is expected behavior if gwt is not in PATH. Either:

- Add gwt to PATH
- Source your alias.sh from local-setup repo
- Rely on the fallback (which should work fine)

### Issue: Worktree already exists

**Symptom**: Error "worktree path already exists"

**Solution**:

```bash
# Clean up old worktrees
git worktree list
git worktree remove <path>
```

Or in Emacs:

```elisp
M-x claude-multi/cleanup-orphaned-worktrees
```

### Issue: Branch not up-to-date

**Symptom**: Working with old code despite fetch/rebase

**Solution**: Check that:

- Network connection is working
- Origin remote is correctly configured: `git remote -v`
- Default branch name is detected correctly

### Issue: Claude doesn't start in worktree

**Symptom**: Claude starts but `pwd` shows wrong directory

**Solution**: This would indicate the command chain broke. Check terminal output for errors in:

- git fetch
- git rebase
- worktree creation
- cd command

## Success Criteria

The fix is working correctly if:

✅ Worktree is created in the terminal (you see the commands execute)
✅ Worktree is created in the correct location
✅ Branch is up-to-date with main/master
✅ Claude starts in the worktree directory
✅ Multiple agents can work in parallel without conflicts
✅ gwt is used when available, git worktree add as fallback
✅ Cleanup still works as expected
✅ Error handling is graceful

## Rollback Procedure

If you need to rollback this fix:

```bash
git revert HEAD  # If you committed the changes
# Or
git checkout HEAD~1 -- autoload/claude-multi-agents.el autoload/claude-multi-worktree.el
```

Then restart Emacs to reload the old code.

<!-- vale on -->
