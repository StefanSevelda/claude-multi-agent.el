# Claude Multi-Agent Documentation Index

## Current Documentation (Up to Date)

### Core Documentation
- **README.org** - Main project documentation, installation, and usage
- **CLAUDE.md** - Project-specific instructions for Claude
- **KEYBINDINGS.md** - Quick reference for all keybindings
- **SYNTAX-TESTS.md** - Testing infrastructure and syntax validation

### Development Guides
- **.claude/doom-local-plugin-development.md** - Doom Emacs plugin development skill
- **.claude/skills/debug-plugin.md** - Plugin debugging guide
- **.claude/hooks/** - Claude Code hooks documentation

### Solution Documentation
- **KEYBINDINGS-SOLUTION.md** - Complete journey of solving keybindings issue
- **EMACS-EVAL-SETUP.md** - Emacsclient integration for testing

### Testing
- **test/README.md** - Test suite documentation
- **test/run-syntax-tests.sh** - Fast syntax validation
- **.github/workflows/syntax-tests.yml** - CI/CD configuration

## Historical Documentation (Reference Only)

These documents capture previous debugging sessions and solutions:

### Debugging Sessions
- **CURRENT-ISSUES.md** - Old issue tracking (resolved)
- **CURRENT-ISSUES-SOLUTIONS.md** - Solutions to old issues
- **FINAL-STATUS.md** - Status from a previous fix
- **SESSION-SUMMARY.md** - Old session summary

### Setup & Configuration
- **DOOM-LOADING-FIX.md** - Old module loading issues (solved)
- **DOOM-SETUP-SUMMARY.md** - Old setup notes
- **DOOM-EMACS-LOCAL-PLUGIN-GUIDE.md** - Moved to .claude/
- **SETUP-COMPLETE.md** - Old setup completion notes

### Feature Fixes
- **STATUS-FIX-SUMMARY.md** - Old status tracking fix
- **STATUS-TRACKING-FIX.md** - Old tracking implementation
- **MULTI-AGENT-SAME-DIR-FIX.md** - Multiple agents in same directory fix

**Note**: Historical docs are kept for reference but may contain outdated information. Always refer to current documentation first.

## Quick Links

### Getting Started
1. Read **README.org** - Project overview
2. Check **KEYBINDINGS.md** - Available commands
3. Run `./test/run-syntax-tests.sh` - Verify installation

### Development
1. Read **.claude/doom-local-plugin-development.md** - Development patterns
2. Check **SYNTAX-TESTS.md** - Testing approach
3. Use **KEYBINDINGS-SOLUTION.md** - If keybindings break

### Testing
1. Run `./test/run-syntax-tests.sh` - Fast syntax check (1 sec)
2. Run `make test` - Full test suite
3. Check GitHub Actions - CI/CD status

## File Organization

```
claude-multi-agent.el/
├── config.el                    # Main configuration
├── packages.el                  # Package declarations
├── autoload/                    # Auto-loaded modules
│   ├── claude-multi-status.el   # Status tracking
│   ├── claude-multi-agents.el   # Agent management
│   └── ...
├── test/                        # Test suite
│   ├── run-syntax-tests.sh      # Syntax validation
│   └── test-status-syntax.el    # Buttercup tests
├── .claude/                     # Claude Code configuration
│   ├── doom-local-plugin-development.md
│   ├── skills/                  # Claude skills
│   └── hooks/                   # Claude hooks
├── git-hooks/                   # Git pre-commit hooks
└── docs/                        # Additional documentation

Current:     17 files (actively maintained)
Historical:  11 files (reference only)
```

## Maintenance

### When to Update
- **Current docs**: Update after any feature change
- **Historical docs**: Don't update, create new docs instead
- **Skill files**: Update when patterns change

### What to Keep
- ✅ Solution documentation (like KEYBINDINGS-SOLUTION.md)
- ✅ Setup guides (like SYNTAX-TESTS.md)
- ✅ Reference guides (like KEYBINDINGS.md)

### What to Archive
- ❌ Temporary debugging files (debug-*.el, fix-*.el)
- ❌ Session summaries for resolved issues
- ❌ Duplicate documentation

## Last Updated

2026-01-09 - After resolving keybindings issue and cleaning up debug files
