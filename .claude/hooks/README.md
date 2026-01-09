# Claude Hooks Configuration

This directory contains Claude Code hooks that run automatically during development.

## Active Hooks

### PostToolUse Hook - Syntax Validation

**Trigger**: After any `Edit` or `Write` tool is used

**Action**: Prompts Claude to run syntax validation tests when editing `autoload/claude-multi-status.el`

**Why**: Prevents parenthesis mismatches and syntax errors from being committed.

**Configuration**: Defined in `.claude/settings.json`

## How It Works

When you edit `autoload/claude-multi-status.el`, Claude will:

1. **Automatically detect** the edit via the PostToolUse hook
2. **Run syntax validation**: `./test/run-syntax-tests.sh`
3. **Report results** immediately to you
4. **Fix errors** if tests fail
5. **Re-run tests** to verify the fix

## Example Output

```
===================================
Running Syntax Validation Tests
===================================

Test 1: Parenthesis Balance Check
✓ Balanced: 487 opens, 487 closes

Test 2: Module Loads Without Errors
✓ Module loaded successfully

Test 3: Critical Functions Defined
  ✓ claude-multi--start-directory-watcher
  ✓ claude-multi--rescan-pending-agents
  ✓ claude-multi/cleanup-status-files

✓ All Syntax Validation Tests Passed
```

## Disabling Hooks

If you need to disable the hook temporarily:

1. Edit `.claude/settings.json`
2. Comment out or remove the `PostToolUse` hook
3. Save the file

## Testing the Hook

Make a small edit to `autoload/claude-multi-status.el` and watch for Claude to automatically run the syntax tests.

## Related Files

- `.claude/settings.json` - Hook configuration
- `test/run-syntax-tests.sh` - Syntax validation script
- `git-hooks/pre-commit` - Git pre-commit hook (similar functionality)
- `SYNTAX-TESTS.md` - Full documentation of the testing strategy

## Benefits

✅ **Immediate feedback** - Catch errors as you code
✅ **No manual testing** - Runs automatically after edits
✅ **Fast** - Syntax tests run in ~1 second
✅ **Prevents regressions** - Stops broken code from being committed
✅ **Educational** - Learn from errors in real-time
