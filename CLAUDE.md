# Claude Multi-Agent - Development Guidelines

## Project Overview

Claude Multi-Agent is an Emacs Lisp plugin that enables parallel execution of multiple Claude Code agent instances with git worktree isolation. Each agent runs independently in its own kitty terminal window and optional git worktree.

All agent orchestration (spawning, killing, focusing, session management, worktree management) is handled by the **`cma` Go CLI binary**. Elisp is a thin presentation layer that calls `cma` via shell and displays results.

### Dependencies

- **cma**: Go CLI binary for agent orchestration (required, [cma-agent-framework](https://github.com/StefanSevelda/cma-agent-framework))
- **kitty**: Terminal emulator with remote control (required)
- **buttercup**: Testing framework (dev only)

### Kitty Setup

Enable remote control in `~/.config/kitty/kitty.conf`:

```conf
allow_remote_control yes
listen_on unix:/tmp/kitty-claude
```

Reload kitty: `Ctrl+Shift+F5`

## Architecture

### Two-Repo Structure

| Repo | Language | Role |
|------|----------|------|
| `claude-multi-agent.el` (this repo) | Elisp | Presentation layer: keybindings, table view, ediff, MCP tools, layout |
| `cma-agent-framework/cma` | Go | All agent orchestration: kitty, git, status, sessions, worktrees |

### Data Flow

```
User keybinding → config.el → cma-commands.el → cma--call/cma--call-raw (cma-core.el) → cma binary → kitty/git
```

Agents are represented as **JSON alists** from `cma list --json`, not Elisp structs.

### Elisp Modules

| File | Lines | Purpose |
|------|-------|---------|
| `config.el` | ~353 | Customization variables, module loading, keybindings, interactive command wrappers |
| `autoload/cma-core.el` | ~81 | Bridge to `cma` binary: `cma--call` (JSON), `cma--call-raw` (string), `cma--call-async` |
| `autoload/cma-commands.el` | ~281 | Interactive commands: spawn, kill, focus, rename, session, worktree |
| `autoload/cma-table.el` | ~225 | `tabulated-list-mode` view populated from `cma list --json` |
| `autoload/cma-modeline.el` | ~54 | Modeline indicator polling for waiting agents |
| `autoload/claude-multi-ediff.el` | ~232 | Interactive diff review via Emacs ediff |
| `autoload/claude-multi-mcp.el` | ~368 | MCP protocol tools (file, git, agent, diagnostics, selection) |
| `autoload/claude-multi-layout.el` | ~771 | Tiling layout system (agenda, focus, project views) |

### Key Design Patterns

- **CLI-first**: Zero `shell-command-to-string` calls to `git` or `kitty` in Elisp — everything routes through `cma--call`/`cma--call-raw`
- **Alist-based agents**: Agents come from `cma list --json` as alists, accessed via `(alist-get 'field agent)`
- **Thin wrappers**: Each interactive command in `config.el` is a one-liner delegating to `cma-commands.el`
- **Table view**: `cma-table-mode` replaces the old org-mode progress buffer with a `tabulated-list-mode` view
- **Modeline polling**: `cma-modeline.el` polls `cma list --json` every 5s for waiting agents

### cma CLI Subcommands Used by Elisp

| Elisp Call | cma Subcommand | Used In |
|-----------|----------------|---------|
| `cma--call "spawn" ...` | `cma spawn --task --dir --model --json` | cma-commands.el |
| `cma--call "list" "--json"` | `cma list --json` | cma-commands.el, cma-table.el, ediff, mcp |
| `cma--call-raw "kill" ...` | `cma kill SESSION_ID` | cma-commands.el |
| `cma--call-raw "focus" ...` | `cma focus SESSION_ID` | cma-commands.el, mcp |
| `cma--call-raw "rename" ...` | `cma rename SESSION_ID NAME` | cma-commands.el |
| `cma--call "session" ...` | `cma session save/restore/list/delete` | cma-commands.el |
| `cma--call "worktree" ...` | `cma worktree list/create/remove` | cma-commands.el |
| `cma--call "git" "changed-files" ...` | `cma git changed-files --dir --json` | ediff, layout |
| `cma--call-raw "git" "diff" ...` | `cma git diff --dir` | ediff, mcp |
| `cma--call-raw "git" "show" ...` | `cma git show REF --dir --output` | ediff |
| `cma--call-raw "git" "checkout" ...` | `cma git checkout FILE --dir` | ediff |
| `cma--call-raw "git" "status" ...` | `cma git status --dir` | mcp |

## Test Strategy

### Testing Framework

- **Buttercup**: BDD-style testing framework for Emacs Lisp
- Test files in `test/`
- Run via `make test`

### Automatic Test Execution

#### Git Hook
A **pre-commit hook** runs `make test` before commits:
- Blocks commits if tests fail
- Bypass with `git commit --no-verify` only when explicitly requested

#### Claude Hook
A **PostToolUse hook** runs relevant tests when `*.el` files change in `autoload/` or `test/`:
- Non-blocking (always exits 0)
- 30-second timeout per test run

### Running Tests

```bash
make test                # Run all tests (120s timeout)
make install-test-deps   # Install buttercup + utility libs
```

### Test Coverage

| Module | Test File | Tests |
|--------|-----------|-------|
| CMA Core (bridge) | test-simple.el | 9 |
| CMA Commands | test-cma-commands.el | 22 |
| Ediff (cma backend) | test-ediff.el | 26 |

**Total**: 57 test cases across 3 test files

### Testing Best Practices

- Mock `cma--call`/`cma--call-raw` with `spy-on` — never shell out in tests
- Test both success and failure paths
- Use `spy-calls-args-for` to verify correct CLI args are passed
- Use `before-each` to reset global state (`claude-multi--ediff-session`, etc.)

## Code Quality Standards

### File Length Limits

Keep files under 800 lines. Current sizes are healthy:

- `config.el`: ~353 lines
- `autoload/claude-multi-layout.el`: ~771 lines (largest — monitor)
- All others: under 400 lines

### Code Organization

- **Namespace prefix**: `claude-multi-` / `claude-multi--` (internal) for surviving modules; `cma-` / `cma--` for CLI bridge modules
<!-- vale off -->
- **Autoload directives**: Mark public functions with `;;;###autoload`
<!-- vale on -->
- **Documentation**: Every function has a docstring
- **Lexical binding**: All files use `lexical-binding: t`

### Style Guidelines

- Use `cl-` prefixed functions from `cl-lib`
- Prefer `when`/`unless` over single-branch `if`
- Use `pcase` for complex conditionals
- Keep functions focused on a single responsibility
- Follow Emacs Lisp conventions (dash-separated names)

### Parenthesis Validation

Before editing existing functions, validate they load:

```bash
./emacs-eval.sh '(progn (load-file "path/to/file.el") (message "OK"))'
```

Validate again after editing. Emacs Lisp is extremely sensitive to paren imbalance.

## Development Workflow

### Emacsclient Integration

Execute elisp directly in the running Emacs via emacsclient:

```bash
./emacs-eval.sh '(elisp-expression-here)'
```

The script auto-discovers the Emacs daemon socket (works even when `$TMPDIR` is sandboxed, e.g. under Claude Code). Discovery order:
1. `$EMACS_SOCKET` env var — explicit override
2. `/var/folders/*/*/T/emacs<uid>/server` — macOS real TMPDIR (glob)
3. `${TMPDIR}emacs<uid>/server` — standard location
4. `lsof` on the `emacs --daemon` process — fallback
5. `emacsclient` default — when none of the above applies

```bash
# Override socket explicitly if needed
EMACS_SOCKET=/path/to/server ./emacs-eval.sh '(+ 1 2)'
```

Examples:
```bash
# Check if cma bridge is loaded
./emacs-eval.sh '(fboundp '\''cma--call)'

# List agents via cma
./emacs-eval.sh '(message "%S" (cma--call "list" "--json"))'

# Check modeline state
./emacs-eval.sh '(message "Modeline: %s" cma-modeline--text)'
```

### Adding New Features

1. If the feature involves agent orchestration, git, or kitty: implement in the Go CLI first (`cma-agent-framework/cma`), then add a thin Elisp wrapper
2. If the feature is Emacs-only (UI, keybindings, ediff): implement in the appropriate `autoload/` file
3. Add tests that mock `cma--call`/`cma--call-raw`
4. Add keybindings in `config.el` if user-facing

### Git Commit Policy

- Never use `git commit --no-verify` without explicit user permission
- Fix failing tests instead of bypassing hooks

## Notes

- Integrates with Doom Emacs conventions (`map!` macro)
- The `cma` binary must be on PATH — config.el warns at startup if missing
- Agent data flows as JSON alists, not Elisp structs
- Always test with both git and non-git repositories
