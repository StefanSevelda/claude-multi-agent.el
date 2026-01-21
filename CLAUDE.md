# Claude Multi-Agent - Development Guidelines

## Project Overview

Claude Multi-Agent is an Emacs Lisp plugin that enables parallel execution of multiple Claude Code agent instances with git `worktree` isolation. Each agent runs independently in its own kitty terminal window and optional git `worktree`.

### Dependencies

- **kitty**: Terminal emulator with remote control (required)
- **alert**: Cross-platform notifications
- **f**, **s**, **dash**: Utility libraries
- **buttercup**: Testing framework

### Kitty Setup

Enable remote control in `~/.config/kitty/kitty.conf`:

```conf
allow_remote_control yes
listen_on unix:/tmp/kitty-claude
```

Reload kitty: `Ctrl+Shift+F5`

## Code Logic & Architecture

### Core Components

1. **Agent Lifecycle Management** (`autoload/claude-multi-agents.el`)
   - Agent creation, launching, monitoring, and cleanup
   - kitty window management via remote control API
   - Status tracking (pending, running, waiting-input, completed, failed)
   - Lightweight status monitoring (checks every 5s)

2. **Progress Tracking** (`autoload/claude-multi-progress.el`)
   - Centralized markdown-based progress buffer
   - Real-time agent status updates
   - Session statistics and reporting
   - Timestamp-based output logging

3. **Configuration & Commands** (`config.el`)
   - User-facing interactive commands
   - Customizable settings (`worktree` location, cleanup behavior, notifications)
   - Keybindings and mode definitions
   - Global session state management

### Key Design Patterns

- **Struct-based agent representation**: Uses `cl-defstruct` for clean agent modeling
- **Hybrid interaction model**: Emacs is the control center, kitty is where user interacts with Claude
- **Lightweight monitoring**: Polls kitty status every 5s instead of complex output parsing
- **Context buffers**: Each agent has an Emacs buffer for metadata and notes
- **Markdown-based UI**: Progress tracking uses markdown mode for readable, live-updating displays
- **Color-coded agents**: Visual distinction through configurable color palette

### Git `worktree` Integration

- Git `worktree` feature provides filesystem isolation for parallel work
- Location configurable: adjacent directory or internal `.git/worktrees`
- Automatic cleanup based on `claude-multi-auto-cleanup` setting
- Graceful fallback when not in a git repository

## Test Strategy

### Testing Framework

- **Buttercup**: Behavior-Driven Development (BDD) style testing framework for Emacs Lisp
- Test files located in `test/` directory
- Run tests via standard buttercup commands or `make test`

### Automatic Test Execution

#### Git Hook
A **pre-commit hook** is configured to automatically run the full test suite before commits:
- Located at `.git/hooks/pre-commit`
- Runs `make test` with 60-second timeout
- Blocks commits if tests fail
- Bypass with `git commit --no-verify` if needed

#### Claude Hook
A **PostToolUse hook** is configured to automatically run relevant tests when code changes:
- Located at `.claude/hooks/PostToolUse.yaml`
- Triggers on `Edit` or `Write` tools for `*.el` files in `autoload/` or `test/` directories
- Runs corresponding test file (e.g., `autoload/claude-multi-notifications.el` → `test/test-notifications.el`)
- Non-blocking: always exits 0 (won't prevent file edits)
- 30-second timeout per test run

### Running Tests Manually

```bash
# Run all tests
make test

# Run specific test file
buttercup -L . -L autoload -L test \
  -L .test-deps/buttercup \
  -L .test-deps/dash.el \
  -L .test-deps/s.el \
  -L .test-deps/f.el \
  -l buttercup \
  -l test/test-notifications.el \
  -f buttercup-run

# Install test dependencies if needed
make install-test-deps
```

### Test Coverage Status

| Module | Test File | Test Cases | Status |
|--------|-----------|------------|--------|
| Notifications | test-notifications.el | 37 | ✅ Complete |
| Agents | test-session.el | 15 | ⚠️ Partial |
| Progress | test-drawer-core-logic.el | 20 | ⚠️ Partial |
| Kitty Integration | test-kitty-integration.el | 10 | ⚠️ Partial |
| Worktree | test-worktree.el | - | ❌ Missing |
| Table View | test-table-view.el | - | ❌ Missing |
| Rename | test-rename.el | - | ❌ Missing |

**Total**: 230+ test cases across 7 test files
**Target Coverage**: 80%+ of public functions

### Test Coverage Areas

1. **Agent Creation & Lifecycle**
   - Agent structure initialization
   - ID generation and uniqueness
   - Color assignment
   - Status transitions

2. **kitty Window Management**
   - kitty window creation via remote control
   - Status monitoring (window existence checks)
   - Initial command sending
   - Clean shutdown and cleanup

3. **Git `worktree` Operations**
   - Creation in different locations (adjacent vs internal)
   - Path resolution
   - Cleanup on completion/failure
   - Handling non-git repositories

4. **Progress Buffer**
   - Section addition/removal
   - Status updates
   - Output appending
   - Statistics calculation

5. **User Interaction**
   - Agent selection
   - Input handling
   - Dashboard display
   - Error cases

6. **Notifications**
   - Input request detection (12 patterns)
   - Multi-method notifications (popup, modeline, markdown)
   - Waiting agent management
   - Notification cleanup

### Testing Best Practices

- Mock external dependencies (kitty commands, git commands)
- Test both success and failure paths
- Verify window state and cleanup
- Check resource cleanup happens correctly
- Test concurrent agent scenarios
- Use `spy-on` for mocking function calls
- Verify function calls with `expect` assertions

## Code Quality Standards

### File Length Limits

**IMPORTANT**: Keep all files between 500-800 lines maximum to ensure readability and maintainability.

Current file sizes:

- `config.el`: ~206 lines ✓
- `autoload/claude-multi-agents.el`: ~262 lines ✓
- `autoload/claude-multi-progress.el`: ~250 lines ✓
- `autoload/claude-multi-worktree.el`: ~235 lines ✓
- `autoload/claude-multi-notifications.el`: ~308 lines ✓

If a file approaches 800 lines:

1. Extract logical modules into separate files
2. Group related functions into new `autoload` files
3. Consider splitting by responsibility (e.g., separate files for new features)

### Code Organization

- **Namespace prefix**: All functions use `claude-multi-` or `claude-multi--` (internal)
<!-- vale off -->
- **Autoload directives**: Mark public functions with the `;;;###autoload` comment directive
<!-- vale on -->
- **Documentation**: Every function has a documentation string
- **Commentary sections**: Clear headers separating logical sections
- **Lexical binding**: All files use `lexical-binding: t`

### Style Guidelines

- Use `cl-` prefixed functions from `cl-lib` (never deprecated `cl.el`)
- Prefer `when` and `unless` over single-branch `if`
- Use `pcase` for complex conditionals
- Keep functions focused on a single responsibility
- Use descriptive variable names
- Add comments for non-obvious logic
- Follow Emacs Lisp conventions (dash-separated names, etc.)

### Parenthesis Validation

**CRITICAL**: Before editing existing functions, ALWAYS evaluate them first to overcome parenthesis problems:

```bash
# BEFORE editing: Test that the function loads correctly
./emacs-eval.sh '(progn (load-file "path/to/file.el") (message "Loaded successfully"))'

# AFTER editing: Validate the changes load without errors
./emacs-eval.sh '(progn (load-file "path/to/file.el") (message "Changes validated"))'
```

**Why**: Emacs Lisp is extremely sensitive to parenthesis imbalance. A single missing or extra paren will break the entire file. Always:
1. **Test BEFORE editing**: Verify the function loads correctly in its current state
2. **Test AFTER editing**: Verify your changes don't introduce syntax errors
3. **Validate incrementally**: Test after each significant change, not just at the end

**Best Practice**: When editing complex nested forms (like `cond`, nested `let`, or `if` statements):
1. Eval the original function first to establish a working baseline
2. Make your edits carefully, counting opening and closing parens
3. Eval immediately after editing to catch errors early
4. If you get paren errors, compare against the working original

This prevents breaking the codebase with syntax errors and makes debugging much faster.

### Dependencies

Minimal external dependencies:

- `vterm`: Terminal emulator for agent buffers
- `alert`: Cross-platform notifications
- `f`: File utilities
- `s`: String utilities
- `dash`: List utilities
- `buttercup`: Testing only

## Development Workflow

### Emacsclient Integration for Testing

**IMPORTANT**: Claude can now execute elisp directly in your Emacs via emacsclient!

**Setup**: Emacsclient server is enabled in `~/.doom.d/config.el` and starts automatically.

**Usage**: When testing features or debugging, Claude should use:

```bash
./emacs-eval.sh '(elisp-expression-here)'
```

**Examples:**
```bash
# Check if functions are loaded
./emacs-eval.sh '(fboundp '\''claude-multi--start-directory-watcher)'

# Get agent status
./emacs-eval.sh '(dolist (agent claude-multi--agents) (message "%s: %s" (claude-agent-name agent) (claude-agent-status agent)))'

# Run diagnostics
./emacs-eval.sh '(message "Watcher: %s | Pending: %d" claude-multi--directory-watcher (length claude-multi--pending-agents))'
```

**Benefits:**
- No more copy-paste of elisp commands
- Faster debugging and testing cycles
- Automatic verification of changes
- Real-time state inspection

**See**: `EMACS-EVAL-SETUP.md` for detailed setup and usage.

### Standard Development Process

1. **Adding new features**:
   - Create functions in appropriate `autoload` file
   - Add interactive commands to `config.el` if user-facing
   - Update keybindings in `config.el`
   - Add tests for new functionality
   - Keep file size under 800 lines
   - **Test via emacsclient**: Use `./emacs-eval.sh` to verify functions work

2. **Refactoring**:
   - Extract related functions into new files when approaching line limits
   - Maintain clear separation of concerns
   - Update provides/requires as needed
   - Run full test suite
   - **Verify loading**: Use emacsclient to check functions are available after reload

3. **Bug fixes**:
   - Write failing test first
   - Implement fix
   - Verify all tests pass
   - Check for edge cases
   - **Test in live session**: Use emacsclient to verify fix without restart

## Future Considerations

Potential new modules (keep each under 800 lines):

- `autoload/claude-multi-terminal.el` - vterm/term integration enhancements
- `autoload/claude-multi-ui.el` - Dashboard and buffer management improvements

## Notes

- This plugin integrates with Doom Emacs conventions (map! macro, package! declarations)
- Agent lifecycle is asynchronous - use buffer-local variables and process filters for state tracking
- Progress buffer updates must handle read-only mode carefully
- Always test with both git and non-git repositories
- `vterm` requires compilation and may not work on all platforms; ensure proper fallback handling
