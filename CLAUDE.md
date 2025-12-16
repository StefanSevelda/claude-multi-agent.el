# Claude Multi-Agent - Development Guidelines

## Project Overview

Claude Multi-Agent is an Emacs Lisp plugin that enables parallel execution of multiple Claude Code agent instances with git `worktree` isolation. Each agent runs independently in its own `vterm` buffer and optional git `worktree`.

## Code Logic & Architecture

### Core Components

1. **Agent Lifecycle Management** (`autoload/claude-multi-agents.el`)
   - Agent creation, launching, monitoring, and cleanup
   - Process management via `vterm` integration
   - Status tracking (pending, running, waiting-input, completed, failed)
   - Output filtering and event detection

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
- **Buffer-local tracking**: Each `vterm` buffer tracks its associated agent via `claude-multi--current-agent`
- **Process filter monitoring**: Output filtering via `set-process-filter` for real-time event detection
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
- Run tests via standard buttercup commands

### Test Coverage Areas

1. **Agent Creation & Lifecycle**
   - Agent structure initialization
   - ID generation and uniqueness
   - Color assignment
   - Status transitions

2. **Process Management**
   - `vterm` buffer creation
   - Process launching and monitoring
   - Output detection (input requests, completion, errors)
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

### Testing Best Practices

- Mock external dependencies (`vterm`, git commands)
- Test both success and failure paths
- Verify buffer state after operations
- Check cleanup happens correctly
- Test concurrent agent scenarios

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

### Dependencies

Minimal external dependencies:

- `vterm`: Terminal emulator for agent buffers
- `alert`: Cross-platform notifications
- `f`: File utilities
- `s`: String utilities
- `dash`: List utilities
- `buttercup`: Testing only

## Development Workflow

1. **Adding new features**:
   - Create functions in appropriate `autoload` file
   - Add interactive commands to `config.el` if user-facing
   - Update keybindings in `config.el`
   - Add tests for new functionality
   - Keep file size under 800 lines

2. **Refactoring**:
   - Extract related functions into new files when approaching line limits
   - Maintain clear separation of concerns
   - Update provides/requires as needed
   - Run full test suite

3. **Bug fixes**:
   - Write failing test first
   - Implement fix
   - Verify all tests pass
   - Check for edge cases

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
