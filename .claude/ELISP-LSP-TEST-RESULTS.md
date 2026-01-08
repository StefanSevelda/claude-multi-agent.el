# Emacs Lisp LSP Plugin Test Results

**Date**: 2025-01-08
**Tester**: Claude Code
**Plugin Version**: 0.1.0
**Status**: ⚠️ **Server Communication Issue Detected**

## Executive Summary

The elisp-lsp plugin infrastructure is correctly installed and configured:
- ✅ Plugin installed at `~/.claude/plugins/cache/elisp-lsp-marketplace/elisp-lsp/0.1.0/`
- ✅ LSP server binary installed at `/Users/stefansevelda/bin/elisp-lsp-server`
- ✅ Configuration files (`plugin.json`, `.lsp.json`) are correct
- ✅ Project configured to use the plugin via `.claude/plugins.json`
- ❌ **LSP server not responding to JSON-RPC requests**

## Test Environment

```
Platform: macOS (Darwin 24.6.0)
Emacs Version: 30.2
Claude Code: Latest
LSP Server: elisp-lsp-server 0.1.0
Test File: test-lsp.el (created for testing)
```

## Installation Verification

### Plugin Structure ✅

```bash
~/.claude/plugins/cache/elisp-lsp-marketplace/elisp-lsp/0.1.0/
├── .claude-plugin/
│   ├── plugin.json       # ✅ Correct metadata
│   └── marketplace.json
├── .lsp.json             # ✅ Correct LSP configuration
├── README.md
└── LICENSE
```

### LSP Server Binary ✅

```bash
$ which elisp-lsp-server
/Users/stefansevelda/bin/elisp-lsp-server

$ cat /Users/stefansevelda/bin/elisp-lsp-server
# ✅ Wrapper script exists and finds LSP server at:
# /Users/stefansevelda/.local/lib/elisp-lsp-server/elisp-lsp-server.el
```

### Configuration Files ✅

**`.claude/plugins.json`**:
```json
{
  "plugins": ["elisp-lsp@elisp-lsp-marketplace"]
}
```

**`.lsp.json`**:
```json
{
  "elisp": {
    "command": "/Users/stefansevelda/bin/elisp-lsp-server",
    "args": [],
    "extensionToLanguage": {".el": "elisp"},
    "transport": "stdio"
  }
}
```

## Test Execution

### Test 1: Server Startup ✅

```bash
$ /Users/stefansevelda/bin/elisp-lsp-server
[ELISP-LSP] Starting Emacs Lisp LSP Server...
# Server starts and waits for input
```

**Result**: Server binary executes successfully.

### Test 2: JSON-RPC Initialize Request ❌

**Test Command**:
```python
# Using Python LSP client to send properly formatted request
send_request(1, "initialize", {
    "rootUri": "file:///Users/stefansevelda/projects/claude-multi-agent.el",
    "capabilities": {}
})
```

**Expected**: LSP initialize response with capabilities
**Actual**: No response received (timeout after 2 seconds)

**Server stderr output**:
```
[ELISP-LSP] Starting Emacs Lisp LSP Server...
# No additional output - server not processing requests
```

### Test 3: Stdin Reading in Emacs Batch Mode ❌

**Diagnostic Test**:
```bash
$ emacs --batch --eval '(progn (message "Testing stdin...") (let ((char (read-char))) (message "Read: %c" char)))' <<< "A"
Testing stdin...
# No character read - read-char blocked indefinitely
```

**Result**: `read-char` in Emacs batch mode doesn't work with piped stdin.

## Root Cause Analysis

### Issue: `read-char` in Batch Mode

The LSP server implementation uses `read-char` to read from stdin:

```elisp
(defun elisp-lsp--read-line ()
  "Read a single line from stdin."
  (let ((line "")
        (char nil))
    (while (and (setq char (ignore-errors (read-char)))  ; ← PROBLEM HERE
                (not (= char ?\n)))
      (unless (= char ?\r)
        (setq line (concat line (char-to-string char)))))
    line))
```

**Problem**: In Emacs batch mode with piped stdin, `read-char` doesn't work properly. It:
1. Doesn't read from the pipe
2. Blocks indefinitely
3. Never returns data

### Why This Happens

Emacs's `read-char` function is designed for interactive use (reading from terminal). In batch mode:
- Terminal input is disabled
- Standard input is available but `read-char` doesn't connect to it
- Need to use different input methods for batch mode

## Recommendations

### Short-term Solutions

#### Option 1: Use `read-from-minibuffer` Alternative
Replace `read-char` with a batch-mode compatible input method:

```elisp
(defun elisp-lsp--read-char ()
  "Read a character from stdin in batch mode."
  (when-let ((byte (ignore-errors (read-byte 0))))  ; Read from stdin (descriptor 0)
    (byte-to-string byte)))
```

#### Option 2: Use Process-Based Communication
Instead of reading directly, use `make-process` with a pipe:

```elisp
(let ((process (make-process
                :name "lsp-stdin"
                :command '("cat")
                :filter #'elisp-lsp--process-filter
                :sentinel #'elisp-lsp--process-sentinel)))
  (process-send-string process input))
```

#### Option 3: Use File Descriptors Directly
Use `insert-file-contents-literally` with `/dev/stdin`:

```elisp
(defun elisp-lsp--read-message ()
  "Read LSP message from stdin."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally "/dev/stdin" nil 0 1024)  ; Read up to 1024 bytes
    (buffer-string)))
```

### Long-term Solution

Consider using an existing Emacs LSP server implementation or framework:
1. **elsp** - Experimental but designed for this use case
2. **lsp-mode** - Could be adapted to run as standalone server
3. **eglot** - Lighter weight, could be wrapped

### Alternative Approach

Since Emacs already has excellent LSP client support, consider:
1. Running Emacs as an LSP *client* in batch mode
2. Using external LSP servers for better performance
3. Creating an MCP server instead of LSP server for Claude Code integration

## Verification Steps for Fix

Once the stdin reading issue is fixed, verify with:

```bash
# Test 1: Initialize
printf "Content-Length: 119\r\n\r\n{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"rootUri\":\"file:///test\",\"capabilities\":{}}}" | \
  /Users/stefansevelda/bin/elisp-lsp-server

# Expected: JSON response with capabilities

# Test 2: Use Python client
python3 /tmp/test_lsp_client.py

# Expected: All tests pass with ✓
```

## Current Status

**Plugin Infrastructure**: ✅ Ready
**LSP Server Communication**: ❌ Blocked
**Claude Code Integration**: ⏸️ Waiting for server fix

## Files Created for Testing

- `test-lsp.el` - Test Emacs Lisp file with functions and variables
- `/tmp/test_lsp_client.py` - Python LSP client for testing
- `/tmp/test-lsp.sh` - Bash test script

## Next Steps

1. Fix `read-char` issue in `elisp-lsp-server.el`
2. Test server communication with Python client
3. Verify LSP features (hover, definition, references, symbols)
4. Test integration with Claude Code LSP tool
5. Clean up test files

## Additional Notes

The concept is sound and the infrastructure is correctly set up. The only blocker is the stdin communication in Emacs batch mode. Once fixed, the server should provide:

- ✨ Hover information with function signatures
- 🔍 Go to definition
- 📚 Find references
- 📋 Document symbols

All using Emacs's powerful native semantic analysis.

## References

- Emacs Manual: [Batch Mode](https://www.gnu.org/software/emacs/manual/html_node/elisp/Batch-Mode.html)
- LSP Specification: [JSON-RPC Protocol](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
- Similar Issues: [emacs-lsp/lsp-mode#3472](https://github.com/emacs-lsp/lsp-mode/issues/3472)
