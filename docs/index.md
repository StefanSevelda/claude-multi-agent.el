# Claude Multi-Agent for Emacs

Welcome to the documentation for Claude Multi-Agent, an Emacs Lisp plugin that enables parallel execution of multiple Claude Code agent instances with git `worktree` isolation.

## Features

- Run multiple Claude Code agents in parallel
- Automatic git `worktree` isolation for each agent
- Real-time progress tracking in a centralized buffer
- Interactive dashboard for monitoring agent status
- Flexible notification system (popup, markdown, mode-line, sound)
- Automatic cleanup of resources

## Quick Start

1. Install the plugin in your Doom Emacs configuration
2. Start a session: `SPC c m s`
3. Spawn agents: `SPC c m a`
4. Monitor progress: `SPC c m p`

## Key Bindings

All commands are available under the `SPC c m` prefix:

- `SPC c m s` - Start a new multi-agent session
- `SPC c m a` - Spawn a new agent with a task description
- `SPC c m p` - Open the progress tracking buffer
- `SPC c m d` - Show the dashboard with all agents
- `SPC c m i` - Send input to an agent waiting for response
- `SPC c m f` - Focus on a specific agent's shell buffer
- `SPC c m k` - Kill a specific agent
- `SPC c m K` - Kill all agents
- `SPC c m e` - Export progress to a file
- `SPC c m w` - Show agents waiting for input
- `SPC c m t` - List all `worktree` instances
- `SPC c m c` - Cleanup orphaned `worktree` instances

## Documentation

This plugin integrates seamlessly with Doom Emacs and provides a powerful way to parallelize your AI-assisted development workflow. Each agent runs in its own isolated git `worktree`, allowing for true parallel development without conflicts.
