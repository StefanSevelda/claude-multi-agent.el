# Elisp-Eval CLI Tool

**Created**: January 8, 2026
**Purpose**: Practical alternative to LSP integration for Emacs Lisp development with Claude Code

## Overview

This tool provides a command-line interface for evaluating, inspecting, and analyzing Emacs Lisp code. It's designed to work with Claude Code and the claude-multi-agent.el project.

### Why This Instead of LSP?

After extensive investigation (documented in `.claude/ELISP-LSP-INVESTIGATION.md` and `.claude/LSP-TEST-RESULTS.md`), we discovered that Claude Code filters community LSP plugins at the platform level. Instead of fighting this limitation, we built a practical alternative that:

1. **Works Today**: No platform restrictions, uses standard bash + Emacs batch mode
2. **Project-Aware**: Automatically loads project files and dependencies
3. **Flexible**: Easy to add new operations
4. **Simple**: Straightforward bash + Emacs architecture
5. **Portable**: Works anywhere Emacs is installed

## Architecture

### Two Components

1. **CLI Tool** (`elisp-eval`)
   - Bash script at `/Users/stefansevelda/bin/elisp-eval`
   - Helper implementation at `/Users/stefansevelda/bin/elisp-eval-helper.el`
   - Invokes Emacs in batch mode with proper load-path setup

2. **Wrapper Script** (`.claude/tools/elisp.sh`)
   - JSON input/output for tool integration
   - Filters and validates parameters
   - Handles all operation types

### Communication Flow

```
User/Claude → elisp.sh wrapper → elisp-eval CLI → Emacs batch mode → Your Project
                                                     ↓
                                                  JSON Response
```

## Operations

### 1. eval - Evaluate Expressions

Evaluate any Emacs Lisp expression and return the result.

**Usage**:
```bash
echo '{"operation":"eval","expression":"(+ 1 2)"}' | ./.claude/tools/elisp.sh
```

**Direct CLI**:
```bash
echo "(* 6 7)" | /Users/stefansevelda/bin/elisp-eval eval .
```

**Output**:
```json
{"status":"success","result":"42","type":"integer"}
```

### 2. describe - Symbol Documentation

Get complete documentation for any symbol (function or variable).

**Usage**:
```bash
echo '{"operation":"describe","symbol":"message"}' | ./.claude/tools/elisp.sh
```

**Direct CLI**:
```bash
/Users/stefansevelda/bin/elisp-eval describe . message
```

**Output**:
```json
{
  "status":"success",
  "symbol":"message",
  "type":"function",
  "documentation":"Display a message at the bottom of the screen...",
  "signature":"(arg1 &rest rest)",
  "file":"/path/to/file.el",
  "line":123
}
```

### 3. find-definition - Locate Symbol Definitions

Find where a symbol is defined (file and line number).

**Usage**:
```bash
echo '{"operation":"find-definition","symbol":"defun"}' | ./.claude/tools/elisp.sh
```

**Direct CLI**:
```bash
/Users/stefansevelda/bin/elisp-eval find-definition . defun
```

**Output**:
```json
{
  "status":"success",
  "symbol":"defun",
  "file":"/opt/homebrew/Cellar/emacs/30.2_2/share/emacs/30.2/lisp/emacs-lisp/byte-run.el.gz",
  "line":402
}
```

### 4. lint - Syntax Checking

Check Emacs Lisp files for syntax errors and style issues.

**Usage**:
```bash
echo '{"operation":"lint","file":"config.el"}' | ./.claude/tools/elisp.sh
```

**Direct CLI**:
```bash
/Users/stefansevelda/bin/elisp-eval lint . config.el
```

**Output**:
```json
{
  "status":"success",
  "file":"config.el",
  "errors":[],
  "warnings":[]
}
```

### 5. list-symbols - Extract All Symbols

List all top-level symbols (functions, variables, constants) in a file.

**Usage**:
```bash
echo '{"operation":"list-symbols","file":"config.el"}' | ./.claude/tools/elisp.sh
```

**Direct CLI**:
```bash
/Users/stefansevelda/bin/elisp-eval list-symbols . config.el
```

**Output**:
```json
{
  "status":"success",
  "file":"config.el",
  "symbols":[
    {
      "name":"claude-multi-worktree-location",
      "kind":"variable",
      "documentation":"Where to create worktrees for agents..."
    },
    {
      "name":"claude-multi-agent-create",
      "kind":"function",
      "signature":"(task-description &optional branch-name use-worktree)",
      "documentation":"Create a new agent..."
    }
  ]
}
```

## Project-Aware Loading

The tool automatically loads the claude-multi-agent.el project:

1. **Load paths**: Adds project root and `autoload/` directory
2. **Dependencies**: Loads all packages from `.packages/` directory
3. **Config**: Loads `config.el` if present
4. **Autoloads**: Loads all `.el` files from `autoload/` directory

This means you can evaluate expressions using project-specific functions:

```bash
echo "(claude-multi-agent-create \"test task\")" | \
  /Users/stefansevelda/bin/elisp-eval eval .
```

## Error Handling

All operations return JSON with consistent error format:

```json
{
  "status":"error",
  "message":"Symbol name required"
}
```

Exit codes:
- `0`: Success
- `1`: Error

## Files Created

### Created
1. **`/Users/stefansevelda/bin/elisp-eval`** - Main CLI script (executable)
2. **`/Users/stefansevelda/bin/elisp-eval-helper.el`** - Implementation (~310 lines)
3. **`.claude/tools/elisp.sh`** - JSON wrapper (executable)

### Modified
4. **`.claude/plugins.json`** - Removed LSP plugin entry

## Files Removed

All LSP-related files have been cleaned up:

### Directories
- `.packages/elisp-lsp-server/`
- `/Users/stefansevelda/.local/lib/elisp-lsp-server/`

### Scripts
- `/Users/stefansevelda/bin/elisp-lsp-server`
- `/Users/stefansevelda/bin/elisp-lsp-server-persistent`

### Test Files
- `test-lsp.el`
- `test_python_lsp.py`
- `test_lsp_comparison.py`
- `test_elisp_lsp_automated.py`
- `Makefile.lsp`

### Temporary Files
- `/tmp/elisp-lsp-spawns.log`
- `/tmp/elisp-lsp-wrapper.log`

## Files Kept (Educational Value)

- `.claude/ELISP-LSP-INVESTIGATION.md` - Complete investigation documentation
- `.claude/LSP-TEST-RESULTS.md` - Test results proving filter theory
- `.claude/LSP-CONFIGURATION-METHODS.md` - Official configuration analysis
- `.claude/plans/lucky-petting-ocean.md` - Implementation plan

## Usage Examples

### From Command Line

```bash
# Evaluate expression
echo "(+ 1 2 3)" | /Users/stefansevelda/bin/elisp-eval eval .

# Get symbol info
/Users/stefansevelda/bin/elisp-eval describe . mapcar

# Find definition
/Users/stefansevelda/bin/elisp-eval find-definition . defun

# Lint file
/Users/stefansevelda/bin/elisp-eval lint . config.el

# List symbols
/Users/stefansevelda/bin/elisp-eval list-symbols . config.el | jq '.symbols[0:5]'
```

### Via JSON Wrapper

```bash
# Evaluate
echo '{"operation":"eval","expression":"(+ 1 2 3)"}' | ./.claude/tools/elisp.sh

# Describe
echo '{"operation":"describe","symbol":"mapcar"}' | ./.claude/tools/elisp.sh

# Find definition
echo '{"operation":"find-definition","symbol":"defun"}' | ./.claude/tools/elisp.sh

# Lint
echo '{"operation":"lint","file":"config.el"}' | ./.claude/tools/elisp.sh

# List symbols
echo '{"operation":"list-symbols","file":"config.el"}' | ./.claude/tools/elisp.sh
```

### From Emacs

You can also invoke the CLI from Emacs:

```elisp
(shell-command-to-string "echo '(+ 1 2)' | /Users/stefansevelda/bin/elisp-eval eval .")
```

## Future Enhancements

Potential additions (keep simple for now):

1. **REPL Mode**: Interactive session (not batch mode)
2. **Formatting**: Auto-format Elisp code
3. **Refactoring**: Rename symbol across files
4. **Testing**: Run tests for specific functions
5. **Package Info**: Show package dependencies
6. **Code Generation**: Generate boilerplate

## Technical Notes

### Why This Works (and LSP Didn't)

1. **Single-shot execution**: Emacs batch mode is perfect for one-off operations
2. **No persistent connection needed**: Each operation is independent
3. **Simple I/O**: stdin/stdout with no protocol complexity
4. **JSON output**: Easy to parse and consume
5. **No platform restrictions**: Standard bash + Emacs, no special permissions needed

### Performance

- Cold start: ~0.3-0.5 seconds (loading project files)
- Warm start: Same (Emacs batch mode always starts fresh)
- Acceptable for interactive use
- Could be optimized with daemon mode in future

### Limitations

1. **No persistent state**: Each invocation starts fresh
2. **No incremental updates**: Full project reload each time
3. **Limited to single operations**: No transaction support
4. **Batch mode restrictions**: Can't use interactive Emacs features

## Integration with Claude Code

While Claude Code's settings.json doesn't support custom tools directly, the tool can be invoked via:

1. **Direct bash**: Claude can run bash commands to invoke the tool
2. **Future MCP integration**: Could add to `claude-multi-mcp.el` as MCP tools
3. **Future plugin**: Could create a proper Claude Code plugin

For now, direct bash invocation is simple and effective.

## Summary

This tool provides practical Emacs Lisp development capabilities without fighting Claude Code's LSP platform limitations. It's:

- ✅ **Working** - All operations tested and functional
- ✅ **Project-aware** - Loads claude-multi-agent.el properly
- ✅ **Simple** - Easy to understand and extend
- ✅ **Portable** - Works anywhere Emacs is installed
- ✅ **Maintainable** - Clean separation of concerns
- ✅ **Documented** - This file + extensive investigation docs

---

**Related Documentation**:
- `.claude/ELISP-LSP-INVESTIGATION.md` - Why LSP doesn't work
- `.claude/LSP-TEST-RESULTS.md` - Filter theory confirmation
- `.claude/LSP-CONFIGURATION-METHODS.md` - Official LSP config analysis
- `.claude/plans/lucky-petting-ocean.md` - Implementation plan
