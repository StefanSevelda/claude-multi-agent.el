# Status JSON Format

This document describes the JSON format used by the claude-multi-agent plugin for displaying real-time agent status information.

## Overview

Each Claude agent can generate a `status.json` file in its working directory that is automatically parsed and displayed in the `*Claude Multi-Agent Progress.org*` buffer. The status information is updated in real-time using file watchers.

## JSON Structure

```json
{
  "status": "string",
  "timestamp": "ISO 8601 string",
  "session_started": "ISO 8601 string",
  "waiting_for_input": boolean,
  "current_activity": {
    "goal": "string",
    "waiting": boolean
  },
  "changes": {
    "recent": ["string"],
    "total_count": number
  },
  "business_context": {
    "technical_domains": ["string"],
    "objective": "string",
    "confidence": number,
    "extraction_method": "string",
    "jira_tickets": ["string"]
  },
  "context_window": {
    "tokens_used": number,
    "tokens_total": number,
    "percentage_used": number,
    "tokens_remaining": number
  },
  "git": {
    "branch": "string",
    "repository": "string",
    "changed_files": [
      {
        "file": "string",
        "status": "string"
      }
    ],
    "has_changes": boolean,
    "commits_ahead": number,
    "commits_behind": number
  },
  "question": "string"
}
```

## Field Descriptions

### Top Level Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `status` | string | Yes | Current agent activity or goal |
| `timestamp` | string | Yes | ISO 8601 timestamp of last update |
| `session_started` | string | Yes | ISO 8601 timestamp when session began |
| `waiting_for_input` | boolean | Yes | Whether agent is waiting for user input |
| `question` | string | Conditional | Question text (only when `waiting_for_input` is true) |

### current_activity

Current activity information for the agent.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `goal` | string | Yes | Current goal or task description |
| `waiting` | boolean | Yes | Whether the activity is blocked waiting |

### changes

History of changes made by the agent.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `recent` | array[string] | Yes | List of recent changes (typically last 10) |
| `total_count` | number | Yes | Total number of changes in session |

### business_context

NLP-extracted business context (optional but recommended).

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `technical_domains` | array[string] | No | Technical areas detected (e.g., "nlp", "api", "database") |
| `objective` | string | No | Extracted work objective |
| `confidence` | number | No | Confidence score (0.0 to 1.0) |
| `extraction_method` | string | No | Method used (e.g., "spacy_nlp", "fallback_patterns") |
| `jira_tickets` | array[string] | No | JIRA ticket IDs found in conversation |

### context_window

Claude context window usage information (optional).

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `tokens_used` | number | No | Number of tokens used |
| `tokens_total` | number | No | Total tokens available |
| `percentage_used` | number | No | Percentage of context used (0-100) |
| `tokens_remaining` | number | No | Tokens remaining |

### git

Git repository information (optional).

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `branch` | string | No | Current git branch name |
| `repository` | string | No | Repository name |
| `changed_files` | array[object] | No | List of changed files with status |
| `has_changes` | boolean | No | Whether there are uncommitted changes |
| `commits_ahead` | number | No | Commits ahead of upstream |
| `commits_behind` | number | No | Commits behind upstream |

#### changed_files Object

| Field | Type | Description |
|-------|------|-------------|
| `file` | string | File path relative to repository root |
| `status` | string | Git status code (see below) |

**Git Status Codes:**
- `M` - Modified
- `A` - Added (new file)
- `D` - Deleted
- `R` - Renamed
- `MM` - Modified in both working directory and index
- `??` - Untracked

## Example: Working Status

```json
{
  "status": "Implementing changes in hooks",
  "timestamp": "2025-12-18T18:35:02.928709",
  "session_started": "2025-12-18T17:34:07.541938",
  "waiting_for_input": false,
  "current_activity": {
    "goal": "Implementing changes in hooks",
    "waiting": false
  },
  "changes": {
    "recent": [
      "Modified hooks/status-summary",
      "Check if spaCy is now working",
      "Modified hooks/status-summary",
      "View enhanced business context"
    ],
    "total_count": 50
  },
  "business_context": {
    "technical_domains": ["nlp", "hooks"],
    "objective": "implementing feature",
    "confidence": 0.7,
    "extraction_method": "spacy_nlp",
    "jira_tickets": []
  },
  "context_window": {
    "tokens_used": 69488,
    "tokens_total": 200000,
    "percentage_used": 34.74,
    "tokens_remaining": 130512
  },
  "git": {
    "branch": "main",
    "repository": "my-project",
    "changed_files": [
      {
        "file": ".claude/hooks/status-summary.py",
        "status": "M"
      }
    ],
    "has_changes": true,
    "commits_ahead": 0,
    "commits_behind": 0
  }
}
```

## Example: Waiting for Input

```json
{
  "status": "Working on implementation",
  "timestamp": "2025-12-18T16:50:45.987654",
  "session_started": "2025-12-18T16:30:15.789012",
  "waiting_for_input": true,
  "current_activity": {
    "goal": "Working on implementation",
    "waiting": true
  },
  "changes": {
    "recent": [
      "Edited `app.py`",
      "Ran: `python test.py`",
      "Asked user for input"
    ],
    "total_count": 15
  },
  "context_window": {
    "tokens_used": 67890,
    "tokens_total": 200000,
    "percentage_used": 33.95,
    "tokens_remaining": 132110
  },
  "git": {
    "branch": "main",
    "repository": "web-app",
    "changed_files": [
      {
        "file": "app.py",
        "status": "M"
      }
    ],
    "has_changes": true,
    "commits_ahead": 0,
    "commits_behind": 2
  },
  "question": "Which authentication method should we use for the API endpoints?"
}
```

## Display in Emacs

The JSON data is parsed and displayed in org-mode format with the following sections:

### Status Indicator
- Normal: `- Status :: {status}`
- Waiting: Prominent warning box with "⏸ WAITING FOR INPUT"

### Business Context
- **Domains**: Technical areas (e.g., `=nlp=, =hooks=`)
- **Objective**: Work objective
- **JIRA**: Clickable org links to tickets
- **Confidence**: Shown only if below 80%

### Context Window
- Usage statistics with formatted numbers
- Visual progress bar using █ (filled) and ░ (empty) characters
- Example: `[█████████████░░░░░░░░░░░░░░░░░░░░░░░░]`

### Git Status
- Repository and branch information
- Commits ahead (↑) and behind (↓) indicators
- List of changed files with status icons:
  - `[M]` - Modified
  - `[+]` - Added
  - `[-]` - Deleted
  - `[→]` - Renamed
  - `[?]` - Untracked

### Current Activity
- Goal and waiting status

### Recent Changes
- Bulleted list of recent changes
- Total change count if more than shown

### Session Footer
- Session start time and last update timestamp
- Centered formatting

## Integration

The plugin watches for changes to `status.json` in each agent's working directory (typically the git worktree). When the file is modified:

1. File watcher detects the change
2. JSON is parsed using Emacs `json-read-file`
3. Content is formatted as org-mode
4. Agent's Status section in progress buffer is updated

The integration includes:
- Recursion guard to prevent infinite update loops
- Event filtering to only process status.json changes
- Graceful error handling for malformed JSON
- Automatic cleanup when agents are terminated

## File Location

The `status.json` file should be placed in the agent's working directory:
- For agents with worktrees: `{worktree-path}/status.json`
- For agents without worktrees: `{default-directory}/status.json`

The file is typically generated by a hook script (e.g., `status-summary.py`) that runs after tool use in Claude Code.
