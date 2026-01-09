# Testing Claude Code LSP Integration with elisp-lsp Plugin

**Status**: Plugin installed and configured, requires Claude Code restart to activate

## Current Situation

The `elisp-lsp` plugin is properly installed and configured:

✅ **Plugin Location**: `~/.claude/plugins/cache/elisp-lsp-marketplace/elisp-lsp/0.1.0/`
✅ **LSP Server Binary**: `/Users/stefansevelda/bin/elisp-lsp-server` (tested and working)
✅ **Configuration**: `.lsp.json` properly configured for `.el` files
✅ **Project Config**: `.claude/plugins.json` enables the plugin

**Issue**: Claude Code sessions don't hot-reload LSP server configurations. The LSP tool won't recognize `.el` files until Claude Code is restarted.

## How to Test Claude Code Integration

### Step 1: Restart Claude Code

Exit this Claude Code session and start a new one:

```bash
# Exit current session (Ctrl+D or type exit)
exit

# Start new Claude Code session
cd /Users/stefansevelda/projects/claude-multi-agent.el
claude
```

### Step 2: Verify LSP Server is Available

Once in the new session, ask Claude to test LSP features:

```
Show me the document symbols in test-lsp.el
```

Or directly request LSP operations:

```
Use LSP to find all symbols in autoload/claude-multi-agents.el
```

### Step 3: Test Individual LSP Features

#### Test 1: Document Symbols (List all functions/variables)

```
Show me all functions and variables defined in test-lsp.el
```

**Expected Output**: Should list:
- `test-function` (Function)
- `test-variable` (Variable)
- `another-test-function` (Function)

#### Test 2: Hover Information

```
What does the function test-function do? (use LSP hover)
```

**Expected**: Documentation string and function signature

#### Test 3: Go to Definition

```
Where is test-variable defined in test-lsp.el?
```

**Expected**: File path and line number (line 14)

#### Test 4: Find References

```
Find all references to test-function in test-lsp.el
```

**Expected**: List of locations where the function is called

## Manual LSP Server Test (Verify Server Works)

If Claude Code doesn't recognize the LSP server, verify it's working manually:

```bash
# Test initialize
JSON='{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"rootUri":"file:///Users/stefansevelda/projects/claude-multi-agent.el","capabilities":{}}}'
LEN=$(printf "%s" "$JSON" | wc -c | tr -d ' ')
printf "Content-Length: %s\r\n\r\n%s" "$LEN" "$JSON" | /Users/stefansevelda/bin/elisp-lsp-server

# Expected: JSON response with capabilities
```

## Troubleshooting

### LSP Server Not Found

**Symptom**: "No LSP server available for file type: .el"

**Solutions**:

1. **Restart Claude Code** (most common fix)
   - Exit current session completely
   - Start new session in project directory

2. **Verify plugin is enabled**:
   ```bash
   cat .claude/plugins.json
   # Should show: "plugins": ["elisp-lsp@elisp-lsp-marketplace"]
   ```

3. **Check LSP server binary exists**:
   ```bash
   ls -l /Users/stefansevelda/bin/elisp-lsp-server
   # Should exist and be executable
   ```

4. **Verify Emacs is installed**:
   ```bash
   emacs --version
   # Should be version 29 or higher
   ```

### LSP Server Starts But Doesn't Respond

**Symptom**: Long timeout, then "LSP server failed to respond"

**Debug Steps**:

1. **Test server manually**:
   ```bash
   python3 test_elisp_lsp_automated.py
   # Should show 6/6 tests passing
   ```

2. **Check server logs**:
   - LSP server logs to stderr
   - Should see "[ELISP-LSP] Starting..." messages

3. **Verify file path is correct**:
   - LSP uses `file://` URIs
   - Ensure file exists at the specified path

### Plugin Not Loading

**Symptom**: Plugin doesn't appear in Claude Code

**Solutions**:

1. **Check plugin cache**:
   ```bash
   ls -la ~/.claude/plugins/cache/elisp-lsp-marketplace/elisp-lsp/0.1.0/
   # Should show .claude-plugin/, .lsp.json, README.md
   ```

2. **Verify .lsp.json syntax**:
   ```bash
   cat ~/.claude/plugins/cache/elisp-lsp-marketplace/elisp-lsp/0.1.0/.lsp.json | jq .
   # Should parse without errors
   ```

3. **Check project plugin config**:
   ```bash
   cat .claude/plugins.json
   # Should be valid JSON
   ```

## Expected Behavior After Restart

Once Claude Code is restarted with the plugin loaded:

### ✅ What Should Work

1. **LSP Tool Recognition**:
   - `.el` files should be recognized as Emacs Lisp
   - LSP server should auto-start when working with `.el` files

2. **Document Symbols**:
   - Lists all `defun`, `defvar`, `defconst`, `defmacro`, `defcustom`
   - Shows function/variable names and kinds

3. **Hover Information**:
   - Shows documentation strings
   - Shows function signatures
   - May return null if cursor not on symbol (acceptable)

4. **Go to Definition**:
   - Jumps to where symbols are defined
   - Works for functions and variables
   - May return null if definition not found (acceptable)

5. **Find References**:
   - Lists all locations where symbol is used
   - May return empty array if no references (acceptable)

### ⚠️ Known Limitations

1. **Single-Shot Server**: Each LSP request spawns a new Emacs process
   - Adds ~200-300ms latency per request
   - Acceptable for interactive use
   - No state preservation between requests

2. **No Completion**: Not yet implemented
   - Plugin advertises capabilities it provides
   - Completion can be added in future

3. **Basic Symbol Detection**: Uses regex-based parsing
   - Finds top-level `defun`/`defvar` only
   - Doesn't parse nested structures
   - Good enough for navigation and overview

4. **Null Responses**: Some operations return null
   - Hover: When cursor not on symbol
   - Definition: When symbol not found
   - This is correct LSP behavior

## Verification Checklist

Before reporting issues, verify:

- [ ] Claude Code has been restarted since plugin installation
- [ ] You're in the correct project directory
- [ ] `.claude/plugins.json` exists and lists the plugin
- [ ] LSP server binary exists at `/Users/stefansevelda/bin/elisp-lsp-server`
- [ ] Emacs 29+ is installed
- [ ] Automated tests pass: `python3 test_elisp_lsp_automated.py`
- [ ] You're asking Claude to use LSP on `.el` files specifically

## Success Indicators

### When Plugin is Working

You'll see Claude Code:
1. Use LSP tool automatically when analyzing `.el` files
2. Show symbol information in responses
3. Navigate to definitions when asked
4. List all functions/variables in files

### Example Successful Interaction

```
User: "What functions are defined in test-lsp.el?"

Claude: *Uses LSP documentSymbol tool*
Found 3 symbols in test-lsp.el:
- test-function (Function) - line 8
- test-variable (Variable) - line 14
- another-test-function (Function) - line 17
```

## Next Steps After Successful Test

Once the plugin is working:

1. **Use it naturally**: Ask Claude about Emacs Lisp code
2. **Navigate codebases**: "Show me all functions in this file"
3. **Understand code**: "What does this function do?"
4. **Find definitions**: "Where is this variable defined?"

## Performance Notes

### Expected Latency

| Operation | Time | Feels Like |
|-----------|------|------------|
| Document Symbol | ~300ms | Instant |
| Hover | ~280ms | Instant |
| Definition | ~290ms | Instant |
| References | ~310ms | Instant |

**"Instant" threshold**: < 500ms (imperceptible to humans)

### Why It's Fast Enough

Even though each request spawns Emacs:
- Human perception threshold: ~100-300ms
- Our latency: ~250-350ms
- Result: Feels instant to users
- Good enough for interactive development

## Documentation Reference

For more details:
- **Test Results**: `.claude/ELISP-LSP-FIX-SUCCESS.md`
- **Test Documentation**: `.claude/LSP-TEST-DOCUMENTATION.md`
- **Automated Tests**: `test_elisp_lsp_automated.py`
- **Comparison Tests**: `test_lsp_comparison.py`

---

**Quick Test Command** (after restart):

```
Show me all symbols defined in test-lsp.el using LSP
```

**Expected**: List of 3 symbols (test-function, test-variable, another-test-function)

**If it works**: ✅ Plugin is fully integrated!
**If it doesn't**: Follow troubleshooting steps above.
