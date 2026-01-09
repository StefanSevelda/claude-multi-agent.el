# Quick Guide: Using elisp-eval with claude-multi-agent.el

Real-world examples using the actual plugin code.

## Quick Reference

```bash
# Via JSON wrapper (recommended)
echo '{"operation":"OPERATION","param":"value"}' | ./.claude/tools/elisp.sh

# Direct CLI
/Users/stefansevelda/bin/elisp-eval OPERATION . [args...]
```

## Real Examples from This Project

### 1. Discover Functions

**Find all agent-related functions:**
```bash
echo '{"operation":"list-symbols","file":"autoload/claude-multi-agents.el"}' | \
  ./.claude/tools/elisp.sh 2>/dev/null | \
  jq -r '.symbols[] | select(.kind == "function" and (.name | contains("agent"))) | .name'
```

**Output:**
```
claude-multi--create-agent
claude-multi--get-agent-color-scheme
claude-multi--launch-agent
claude-multi--handle-agent-completion
claude-multi--kill-agent
claude-multi--list-agents
...
```

### 2. Find Definitions

**Locate where color assignment happens:**
```bash
echo '{"operation":"find-definition","symbol":"claude-multi--assign-color"}' | \
  ./.claude/tools/elisp.sh 2>/dev/null | jq .
```

**Output:**
```json
{
  "status": "success",
  "symbol": "claude-multi--assign-color",
  "file": "/Users/stefansevelda/projects/claude-multi-agent.el/autoload/claude-multi-agents.el",
  "line": 89
}
```

### 3. Evaluate with Project Data

**Count how many agent colors are configured:**
```bash
echo '{"operation":"eval","expression":"(length claude-multi-agent-colors)"}' | \
  ./.claude/tools/elisp.sh 2>/dev/null | jq -r '.result'
```

**Output:**
```
10
```

**Transform the color list:**
```bash
echo '{"operation":"eval","expression":"(mapcar (lambda (c) (upcase c)) claude-multi-agent-colors)"}' | \
  ./.claude/tools/elisp.sh 2>/dev/null | jq -r '.result'
```

**Output:**
```
("RED" "GREEN" "YELLOW" "BLUE" "MAGENTA" "CYAN" "ORANGE" "PURPLE" "PINK" "LIME")
```

### 4. Analyze Module Structure

**See what's in the progress module:**
```bash
echo '{"operation":"list-symbols","file":"autoload/claude-multi-progress.el"}' | \
  ./.claude/tools/elisp.sh 2>/dev/null | \
  jq '.symbols[0:5] | .[] | {name, kind}'
```

**Output:**
```json
{"name": "claude-multi--progress-buffer", "kind": "variable"}
{"name": "claude-multi--session-start-time", "kind": "variable"}
{"name": "claude-multi--current-session-window-id", "kind": "variable"}
{"name": "claude-multi--progress-init", "kind": "function"}
{"name": "claude-multi--progress-get-or-create-buffer", "kind": "function"}
```

### 5. Find All MCP Tools

**List all registered MCP tool functions:**
```bash
echo '{"operation":"list-symbols","file":"autoload/claude-multi-mcp.el"}' | \
  ./.claude/tools/elisp.sh 2>/dev/null | \
  jq -r '.symbols[] | select(.kind == "function" and (.name | contains("tool"))) | .name'
```

**Output:**
```
claude-multi-mcp--register-tool
claude-multi-mcp--tool-file-read
claude-multi-mcp--tool-file-write
claude-multi-mcp--tool-file-list
claude-multi-mcp--tool-git-status
claude-multi-mcp--tool-git-diff
claude-multi-mcp--tool-agent-focus
claude-multi-mcp--tool-agent-status
...
```

### 6. Get Configuration Variables

**Find all customizable variables:**
```bash
echo '{"operation":"list-symbols","file":"config.el"}' | \
  ./.claude/tools/elisp.sh 2>/dev/null | \
  jq '.symbols[] | select(.kind == "variable") | {name, docs: (.documentation | split("\n") | .[0])}'
```

**Output:**
```json
{
  "name": "claude-multi-worktree-location",
  "docs": "Where to create worktrees for agents."
}
{
  "name": "claude-multi-claude-command",
  "docs": "Command to run Claude Code CLI."
}
{
  "name": "claude-multi-kitty-listen-address",
  "docs": "Kitty listen address for remote control."
}
...
```

### 7. Check Syntax

**Lint a file for errors:**
```bash
echo '{"operation":"lint","file":"autoload/claude-multi-agents.el"}' | \
  ./.claude/tools/elisp.sh 2>/dev/null | jq .
```

**Output:**
```json
{
  "status": "success",
  "file": "autoload/claude-multi-agents.el",
  "errors": [],
  "warnings": []
}
```

## Advanced Examples

### Count Functions by Module

```bash
for file in autoload/*.el; do
  count=$(echo "{\"operation\":\"list-symbols\",\"file\":\"$file\"}" | \
    ./.claude/tools/elisp.sh 2>/dev/null | \
    jq '.symbols[] | select(.kind == "function")' | wc -l)
  echo "$(basename $file): $count functions"
done
```

### Find All Public Commands

```bash
echo '{"operation":"list-symbols","file":"config.el"}' | \
  ./.claude/tools/elisp.sh 2>/dev/null | \
  jq -r '.symbols[] | select(.kind == "function" and (.name | startswith("claude-multi-") and (startswith("claude-multi--") | not))) | .name'
```

### Evaluate Complex Expressions

**Get agent struct definition:**
```bash
echo '{"operation":"eval","expression":"(cl-struct-slot-info (quote claude-multi-agent))"}' | \
  ./.claude/tools/elisp.sh 2>/dev/null | jq .
```

**Calculate project statistics:**
```bash
echo '{"operation":"eval","expression":"(cons (length claude-multi-agent-colors) (length claude-multi-agent-color-schemes))"}' | \
  ./.claude/tools/elisp.sh 2>/dev/null | jq -r '"Colors: \(.result | split(" ")[0]), Schemes: \(.result | split(" ")[1])"'
```

## Debugging Workflow

### 1. Find the function
```bash
echo '{"operation":"list-symbols","file":"autoload/claude-multi-agents.el"}' | \
  ./.claude/tools/elisp.sh 2>/dev/null | \
  jq -r '.symbols[] | select(.kind == "function") | .name' | grep launch
```

### 2. Get its location
```bash
echo '{"operation":"find-definition","symbol":"claude-multi--launch-agent"}' | \
  ./.claude/tools/elisp.sh 2>/dev/null | jq -r '"\(.file):\(.line)"'
```

### 3. Understand its signature
```bash
echo '{"operation":"list-symbols","file":"autoload/claude-multi-agents.el"}' | \
  ./.claude/tools/elisp.sh 2>/dev/null | \
  jq '.symbols[] | select(.name == "claude-multi--launch-agent")'
```

## Common Patterns

### Search for patterns in multiple files

```bash
for file in autoload/*.el; do
  echo "=== $file ==="
  echo "{\"operation\":\"list-symbols\",\"file\":\"$file\"}" | \
    ./.claude/tools/elisp.sh 2>/dev/null | \
    jq -r '.symbols[] | select(.name | contains("status")) | .name'
done
```

### Get full module overview

```bash
echo '{"operation":"list-symbols","file":"autoload/claude-multi-mcp.el"}' | \
  ./.claude/tools/elisp.sh 2>/dev/null | \
  jq '{
    variables: [.symbols[] | select(.kind == "variable") | .name],
    functions: [.symbols[] | select(.kind == "function") | .name],
    total: (.symbols | length)
  }'
```

## Tips

1. **Always suppress stderr** with `2>/dev/null` to avoid loading messages
2. **Use jq for formatting** - JSON output is designed for programmatic use
3. **Pipe through head** for large results: `| head -20`
4. **Use select filters** to find specific symbols: `select(.name | contains("text"))`
5. **Check exit codes** - 0 for success, 1 for errors

## From Within Emacs

You can also use this from Emacs itself:

```elisp
;; Evaluate and parse JSON result
(let* ((json-str (shell-command-to-string
                  "echo '(+ 1 2)' | /Users/stefansevelda/bin/elisp-eval eval . 2>/dev/null"))
       (result (json-parse-string json-str :object-type 'alist)))
  (message "Result: %s" (alist-get 'result result)))
```

## Next Steps

- See `.claude/ELISP-EVAL-TOOL.md` for complete API documentation
- See `.claude/ELISP-LSP-INVESTIGATION.md` for why we built this instead of LSP
- Add your own operations to `/Users/stefansevelda/bin/elisp-eval-helper.el`
