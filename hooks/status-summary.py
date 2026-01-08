#!/usr/bin/python3
"""
Claude Agent Status Summary Hook
Writes session-specific status JSON to /tmp/claude-status/ for Emacs integration.
"""

import json
import sys
import os
from datetime import datetime
from pathlib import Path
import subprocess


def load_hook_input():
    """Read hook input from stdin."""
    try:
        return json.load(sys.stdin)
    except Exception as e:
        print(f"Error reading hook input: {e}", file=sys.stderr)
        sys.exit(0)


def get_state_file(session_id):
    """Get path to session state file."""
    state_dir = Path("/tmp/claude-status-hooks")
    state_dir.mkdir(exist_ok=True)
    return state_dir / f"session-{session_id}.json"


def load_state(session_id):
    """Load session state from file."""
    state_file = get_state_file(session_id)
    if state_file.exists():
        try:
            with open(state_file, 'r') as f:
                return json.load(f)
        except:
            pass
    return {
        "session_id": session_id,
        "changes": [],
        "current_goal": "Working on task",
        "started_at": datetime.now().isoformat(),
        "waiting_for_input": False,
        "question_asked": None,
        "claude_status": "running"
    }


def save_state(session_id, state):
    """Save session state to file."""
    state_file = get_state_file(session_id)
    try:
        with open(state_file, 'w') as f:
            json.dump(state, f, indent=2)
    except Exception as e:
        print(f"Error saving state: {e}", file=sys.stderr)


def get_git_info(cwd):
    """Get git repository information if available."""
    try:
        # Check if we're in a git repo
        result = subprocess.run(
            ["git", "rev-parse", "--is-inside-work-tree"],
            cwd=cwd,
            capture_output=True,
            text=True,
            timeout=2
        )
        if result.returncode != 0:
            return None

        git_info = {}

        # Get current branch
        result = subprocess.run(
            ["git", "branch", "--show-current"],
            cwd=cwd,
            capture_output=True,
            text=True,
            timeout=2
        )
        if result.returncode == 0:
            git_info["branch"] = result.stdout.strip()

        # Get repository name from remote URL
        result = subprocess.run(
            ["git", "config", "--get", "remote.origin.url"],
            cwd=cwd,
            capture_output=True,
            text=True,
            timeout=2
        )
        if result.returncode == 0:
            url = result.stdout.strip()
            if url:
                repo_name = url.rstrip('/').split('/')[-1].replace('.git', '')
                git_info["repository"] = repo_name

        # Get changed files
        result = subprocess.run(
            ["git", "status", "--porcelain"],
            cwd=cwd,
            capture_output=True,
            text=True,
            timeout=2
        )
        if result.returncode == 0:
            lines = result.stdout.strip().split('\n')
            changed_files = []
            for line in lines:
                if line.strip():
                    status = line[:2]
                    filename = line[3:].strip()
                    changed_files.append({
                        "file": filename,
                        "status": status.strip()
                    })
            git_info["changed_files"] = changed_files
            git_info["has_changes"] = len(changed_files) > 0

        # Get commit count ahead/behind
        result = subprocess.run(
            ["git", "rev-list", "--left-right", "--count", "HEAD...@{upstream}"],
            cwd=cwd,
            capture_output=True,
            text=True,
            timeout=2
        )
        if result.returncode == 0:
            counts = result.stdout.strip().split()
            if len(counts) == 2:
                git_info["commits_ahead"] = int(counts[0])
                git_info["commits_behind"] = int(counts[1])

        return git_info
    except Exception as e:
        print(f"Error getting git info: {e}", file=sys.stderr)
        return None


def format_change(tool_name, tool_input, tool_response):
    """Format a change entry with semantic context."""
    if tool_name in ["Write", "Edit"]:
        file_path = tool_input.get("file_path", "unknown")
        path = Path(file_path)
        parts = path.parts
        if len(parts) >= 2:
            component = parts[-2]
            file_name = path.stem
            action = "Created" if tool_name == "Write" else "Modified"
            return f"{action} {component}/{file_name}"
        else:
            action = "Created" if tool_name == "Write" else "Edited"
            return f"{action} `{path.name}`"

    elif tool_name == "Bash":
        description = tool_input.get("description", "")
        if description:
            return description
        command = tool_input.get("command", "")
        if len(command) > 50:
            command = command[:47] + "..."
        return f"Ran: `{command}`"

    elif tool_name == "Task":
        description = tool_input.get("description", "task")
        return f"Completed: {description}"

    elif tool_name in ["Read", "Grep", "Glob"]:
        if tool_name == "Read":
            file_path = tool_input.get("file_path", "")
            if file_path:
                return f"Reading {Path(file_path).name}"
        elif tool_name == "Grep":
            pattern = tool_input.get("pattern", "")
            return f"Searching for: {pattern[:30]}"
        elif tool_name == "Glob":
            pattern = tool_input.get("pattern", "")
            return f"Finding files: {pattern}"
        return f"{tool_name} action"

    elif tool_name == "AskUserQuestion":
        return "Asked user for input"

    else:
        return f"{tool_name} action"


def infer_goal_from_changes(changes):
    """Infer current goal from recent changes with pattern detection."""
    if not changes:
        return "Starting work"

    recent = changes[-10:]

    investigation_terms = ["Reading", "Searching", "Finding", "Exploring"]
    implementation_terms = ["Modified", "Created", "Edited"]
    testing_terms = ["Running", "test", "build"]

    investigation_count = sum(1 for c in recent if any(term in c for term in investigation_terms))
    implementation_count = sum(1 for c in recent if any(term in c for term in implementation_terms))
    testing_count = sum(1 for c in recent if any(term in c for term in testing_terms))

    # Extract work area from file paths
    work_areas = []
    for change in recent:
        if "/" in change:
            parts = change.split("/")
            if len(parts) >= 2:
                work_areas.append(parts[0].split()[-1])

    area_context = ""
    if work_areas:
        from collections import Counter
        most_common = Counter(work_areas).most_common(1)
        if most_common:
            area_context = f" in {most_common[0][0]}"

    if testing_count > 0 and implementation_count > 0:
        return f"Testing changes{area_context}"
    elif investigation_count > implementation_count * 2:
        return f"Investigating code{area_context}"
    elif implementation_count > 0:
        return f"Implementing changes{area_context}"
    elif investigation_count > 0:
        return f"Exploring codebase{area_context}"
    else:
        return "Working on task"


def generate_status_json(state, cwd, context_info=None, git_info=None):
    """Generate the status data as JSON."""
    changes = state.get("changes", [])
    goal = state.get("current_goal", "Working on task")
    waiting_for_input = state.get("waiting_for_input", False)
    question_asked = state.get("question_asked")
    started_at = state.get("started_at")
    claude_status = state.get("claude_status", "running")

    # Auto-update goal based on recent activity
    if len(changes) > 0:
        goal = infer_goal_from_changes(changes)

    # Determine claude_status based on waiting state
    if waiting_for_input:
        claude_status = "waiting-for-user"
    elif claude_status != "finished":
        claude_status = "running"

    status_data = {
        "cwd": str(cwd),
        "session_id": state.get("session_id"),
        "timestamp": datetime.now().isoformat(),
        "session_started": started_at,
        "claude_status": claude_status,
        "waiting_for_input": waiting_for_input,
        "current_activity": {
            "goal": goal,
            "waiting": waiting_for_input
        }
    }

    # Add context window information
    if context_info:
        status_data["context_window"] = context_info

    # Add git repository information
    if git_info:
        status_data["git"] = git_info

    # Add question details if waiting for input
    if waiting_for_input and question_asked:
        status_data["question"] = question_asked

    return status_data


def write_status_file(session_id, cwd, content):
    """Write session-specific status JSON to /tmp directory."""
    status_dir = Path("/tmp/claude-status")
    status_dir.mkdir(exist_ok=True)
    status_file = status_dir / f"status-{session_id}.json"

    # Add current working directory to the content
    content["cwd"] = str(cwd)

    try:
        # Write atomically using temp file + rename
        temp_file = status_file.with_suffix('.tmp')
        with open(temp_file, 'w') as f:
            json.dump(content, f, indent=2)
        temp_file.rename(status_file)
    except Exception as e:
        print(f"Error writing status-{session_id}.json: {e}", file=sys.stderr)
        sys.exit(0)


def main():
    """Main hook execution."""
    hook_data = load_hook_input()

    session_id = hook_data.get("session_id", "unknown")
    tool_name = hook_data.get("tool_name", "")
    tool_input = hook_data.get("tool_input", {})
    tool_response = hook_data.get("tool_response", {})
    cwd = hook_data.get("cwd", os.getcwd())

    # Load session state
    state = load_state(session_id)

    # Handle AskUserQuestion tool to set waiting state
    if tool_name == "AskUserQuestion":
        state["waiting_for_input"] = True
        questions = tool_input.get("questions", [])
        if questions and len(questions) > 0:
            state["question_asked"] = questions[0].get("question", "Question details not available")
        else:
            state["question_asked"] = "Agent is waiting for your input"
    else:
        # Clear waiting state on any other tool use (user has responded)
        state["waiting_for_input"] = False
        state["question_asked"] = None

    # Add new change
    change_text = format_change(tool_name, tool_input, tool_response)

    # Avoid duplicate consecutive entries
    if not state["changes"] or state["changes"][-1] != change_text:
        state["changes"].append(change_text)

    # Keep only last 50 changes to avoid file bloat
    if len(state["changes"]) > 50:
        state["changes"] = state["changes"][-50:]

    # Save updated state
    save_state(session_id, state)

    # Get context window information from hook input
    context_info = None
    context_window_data = hook_data.get('context_window')

    if context_window_data:
        context_window_size = context_window_data.get('context_window_size', 200000)
        current_usage = context_window_data.get('current_usage', {})

        if current_usage:
            input_tokens = current_usage.get('input_tokens', 0)
            cache_creation = current_usage.get('cache_creation_input_tokens', 0)
            cache_read = current_usage.get('cache_read_input_tokens', 0)
            output_tokens = current_usage.get('output_tokens', 0)

            total_input = input_tokens + cache_creation + cache_read
            tokens_used = total_input + output_tokens
            active_tokens = (input_tokens + cache_creation + output_tokens)

            percentage = (tokens_used / context_window_size) * 100 if context_window_size > 0 else 0
            active_percentage = (active_tokens / context_window_size) * 100 if context_window_size > 0 else 0

            context_info = {
                "tokens_used": tokens_used,
                "tokens_total": context_window_size,
                "percentage_used": round(percentage, 2),
                "percentage_remaining": round(100 - percentage, 2),
                "tokens_remaining": context_window_size - tokens_used,
                "input_tokens": total_input,
                "output_tokens": output_tokens,
                "cache_read_tokens": cache_read,
                "active_tokens": active_tokens,
                "active_percentage": round(active_percentage, 2),
                "compaction_warning": percentage >= 95.0
            }

    # Fallback: Try to extract from transcript if context_window not in hook data
    transcript_path = hook_data.get("transcript_path")
    if not context_info and transcript_path and os.path.exists(transcript_path):
        try:
            with open(transcript_path, 'r') as f:
                lines = f.readlines()

            for line in reversed(lines[-50:]):
                line = line.strip()
                if line:
                    try:
                        entry = json.loads(line)
                        if entry.get('type') == 'assistant' and 'message' in entry:
                            usage = entry['message'].get('usage', {})
                            if usage:
                                input_tokens = usage.get('input_tokens', 0)
                                cache_creation = usage.get('cache_creation_input_tokens', 0)
                                cache_read = usage.get('cache_read_input_tokens', 0)
                                output_tokens = usage.get('output_tokens', 0)

                                total_input = input_tokens + cache_creation + cache_read
                                tokens_used = total_input + output_tokens
                                active_tokens = (input_tokens + cache_creation + output_tokens)

                                tokens_total = int(os.environ.get('CLAUDE_CONTEXT_WINDOW', 200000))

                                percentage = (tokens_used / tokens_total) * 100 if tokens_total > 0 else 0
                                active_percentage = (active_tokens / tokens_total) * 100 if tokens_total > 0 else 0

                                context_info = {
                                    "tokens_used": tokens_used,
                                    "tokens_total": tokens_total,
                                    "percentage_used": round(percentage, 2),
                                    "percentage_remaining": round(100 - percentage, 2),
                                    "tokens_remaining": tokens_total - tokens_used,
                                    "input_tokens": total_input,
                                    "output_tokens": output_tokens,
                                    "cache_read_tokens": cache_read,
                                    "active_tokens": active_tokens,
                                    "active_percentage": round(active_percentage, 2),
                                    "compaction_warning": percentage >= 95.0,
                                    "source": "transcript_fallback"
                                }
                                break
                    except json.JSONDecodeError:
                        continue
        except Exception as e:
            print(f"Error extracting token info: {e}", file=sys.stderr)

    # Get git repository information
    git_info = get_git_info(cwd)

    # Generate and write status JSON
    status_content = generate_status_json(state, cwd, context_info, git_info)
    write_status_file(session_id, cwd, status_content)

    sys.exit(0)


if __name__ == "__main__":
    main()
