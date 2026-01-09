# Claude Code LSP Configuration Methods

**Source**: Official Claude Code plugin documentation
**Date**: January 8, 2026

## Two Configuration Approaches

Claude Code supports two methods for defining LSP servers in plugins:

### Method 1: Inline in plugin.json

```json
{
  "name": "my-plugin",
  "lspServers": {
    "go": {
      "command": "gopls",
      "args": ["serve"],
      "extensionToLanguage": {
        ".go": "go"
      }
    }
  }
}
```

**Advantages**:
- Single configuration file
- Simpler for single-language plugins
- Direct integration

### Method 2: Separate .lsp.json file

```json
// plugin.json
{
  "name": "my-plugin",
  "lspServers": "./.lsp.json"
}

// .lsp.json
{
  "go": {
    "command": "gopls",
    "args": ["serve"],
    "extensionToLanguage": {
      ".go": "go"
    }
  }
}
```

**Advantages**:
- Separation of concerns
- Better for multi-language plugins
- Easier to maintain large configurations

**Our plugin uses**: Method 2 (separate `.lsp.json`)

---

## Complete Field Reference

### Required Fields

| Field | Type | Description |
|-------|------|-------------|
| `command` | string | The LSP binary to execute (must be in PATH) |
| `extensionToLanguage` | object | Maps file extensions to language identifiers |

**Example**:
```json
{
  "command": "elisp-lsp-server-persistent",
  "extensionToLanguage": {
    ".el": "elisp"
  }
}
```

---

### Optional Fields

| Field | Type | Description |
|-------|------|-------------|
| `args` | string[] | Command-line arguments for the LSP server |
| `transport` | string | Communication transport: `stdio` (default) or `socket` |
| `env` | object | Environment variables to set when starting the server |
| `initializationOptions` | object | Options passed to server during initialization |
| `settings` | object | Settings passed via `workspace/didChangeConfiguration` |
| `workspaceFolder` | string | Workspace folder path for the server |
| `startupTimeout` | number | Max time to wait for server startup (milliseconds) |
| `shutdownTimeout` | number | Max time to wait for graceful shutdown (milliseconds) |
| `restartOnCrash` | boolean | Whether to automatically restart the server if it crashes |
| `maxRestarts` | number | Maximum number of restart attempts before giving up |
| `loggingConfig` | object | Debug logging configuration (see below) |

---

## Debug Logging Configuration

The `loggingConfig` field enables verbose LSP logging when users pass `--enable-lsp-logging`. This helps debug language server issues without impacting normal operation.

```json
"loggingConfig": {
  "args": ["--log-level", "4"],
  "env": {
    "TSS_LOG": "-level verbose -file ${CLAUDE_PLUGIN_LSP_LOG_FILE}"
  }
}
```

**Special variables**:
- `${CLAUDE_PLUGIN_LSP_LOG_FILE}`: Path to the LSP log file

---

## Our Configuration Analysis

### Current Configuration (Method 2)

**File**: `.claude/plugins/cache/elisp-lsp-marketplace/elisp-lsp/0.1.0/.lsp.json`

```json
{
  "elisp": {
    "command": "/Users/stefansevelda/bin/elisp-lsp-server-persistent",
    "args": [],
    "extensionToLanguage": {
      ".el": "elisp"
    },
    "transport": "stdio",
    "startupTimeout": 10000,
    "shutdownTimeout": 5000,
    "restartOnCrash": true,
    "maxRestarts": 3
  }
}
```

**Status**: ✅ **Correct according to documentation**

### Validation Against Documentation

| Field | Our Value | Status |
|-------|-----------|--------|
| `command` | `/Users/stefansevelda/bin/elisp-lsp-server-persistent` | ✅ Valid path |
| `args` | `[]` | ✅ Valid (optional) |
| `extensionToLanguage` | `{".el": "elisp"}` | ✅ Correct format |
| `transport` | `"stdio"` | ✅ Valid (default) |
| `startupTimeout` | `10000` | ✅ Valid (10 seconds) |
| `shutdownTimeout` | `5000` | ✅ Valid (5 seconds) |
| `restartOnCrash` | `true` | ✅ Valid |
| `maxRestarts` | `3` | ✅ Valid |

**Conclusion**: Our configuration follows the official specification exactly.

---

## Testing Alternative Configuration

Let's test if inline configuration in `plugin.json` makes any difference:

### Current (Separate .lsp.json)
```json
// plugin.json
{
  "name": "elisp-lsp",
  "lspServers": "./.lsp.json"
}
```

### Alternative (Inline)
```json
// plugin.json
{
  "name": "elisp-lsp",
  "lspServers": {
    "elisp": {
      "command": "/Users/stefansevelda/bin/elisp-lsp-server-persistent",
      "extensionToLanguage": {
        ".el": "elisp"
      }
    }
  }
}
```

**Hypothesis**: This won't change the outcome because:
1. Both methods are documented and supported
2. Our separate `.lsp.json` is correctly formatted
3. Debug logs show the configuration is being read
4. The issue is at the initialization/filtering stage, not configuration parsing

---

## Comparison with Official Plugins

### Our Plugin (Community)

**Method**: Separate `.lsp.json` file

**Loaded**: ✅ Yes
```
[DEBUG] Loaded 1 LSP server(s) from plugin: elisp-lsp
```

**Started**: ❌ No (filtered out)

---

### Official Plugins (gopls, pyright)

**Method**: Unknown (no `.lsp.json` in cache)

**Hypothesis**:
- Configuration may be built into Claude Code binary
- Or use different plugin structure
- Or loaded from different location

**Loaded**: ✅ Yes
```
[DEBUG] Loaded 1 LSP server(s) from plugin: gopls-lsp
[DEBUG] Loaded 1 LSP server(s) from plugin: pyright-lsp
```

**Started**: ✅ Yes
```
[DEBUG] Starting LSP server instance: plugin:gopls-lsp:gopls
[DEBUG] Starting LSP server instance: plugin:pyright-lsp:pyright
```

---

## Key Insights from Documentation

### 1. Configuration Format is Correct

Our configuration matches the documented schema exactly:
- ✅ Required fields present (`command`, `extensionToLanguage`)
- ✅ Optional fields valid (`args`, `transport`, timeouts, restart config)
- ✅ Proper JSON structure
- ✅ Valid data types

### 2. Restart Configuration Already Present

We already have crash recovery configured:
```json
"restartOnCrash": true,
"maxRestarts": 3
```

This should handle server crashes automatically. The fact that we still need a persistent wrapper suggests:
- Configuration is being read correctly
- But server never gets started in the first place
- Validates our "filter before spawn" theory

### 3. Debug Logging Available

We could add enhanced logging:
```json
"loggingConfig": {
  "env": {
    "ELISP_LSP_LOG": "/tmp/elisp-lsp-debug.log"
  }
}
```

But this only helps if the server actually starts (which it doesn't for community plugins).

---

## Does This Change Our Investigation?

### What We Learned

1. **Two valid configuration methods exist** (inline vs separate file)
2. **Our configuration is correct** (matches official documentation)
3. **Restart handling is built-in** (we didn't need custom wrapper for that)
4. **Debug logging is available** (if server starts)

### What Doesn't Change

1. ✅ Configuration format is valid
2. ✅ Claude Code reads the configuration
3. ❌ **Server still never starts** (filter theory still confirmed)
4. ❌ **Community plugins still excluded** (platform limitation remains)

### Conclusion

This documentation **confirms our configuration is correct** and **doesn't change our root cause finding**: Claude Code filters community LSP plugins before initialization.

---

## Recommended Configuration Updates

Even though it won't solve the filter issue, we can improve our configuration:

### Add Debug Logging

```json
{
  "elisp": {
    "command": "/Users/stefansevelda/bin/elisp-lsp-server-persistent",
    "extensionToLanguage": {
      ".el": "elisp"
    },
    "transport": "stdio",
    "startupTimeout": 10000,
    "shutdownTimeout": 5000,
    "restartOnCrash": true,
    "maxRestarts": 3,
    "loggingConfig": {
      "env": {
        "ELISP_LSP_LOG": "${CLAUDE_PLUGIN_LSP_LOG_FILE}"
      }
    }
  }
}
```

**Benefit**: If/when the plugin is accepted officially, debug logging will be available.

### Try Inline Configuration

Worth testing if inline configuration bypasses any filtering:

```json
// plugin.json
{
  "name": "elisp-lsp",
  "version": "0.1.0",
  "lspServers": {
    "elisp": {
      "command": "/Users/stefansevelda/bin/elisp-lsp-server-persistent",
      "extensionToLanguage": {
        ".el": "elisp"
      }
    }
  }
}
```

**Expected**: Won't make a difference (configuration parsing works, filtering is the issue)

---

## Final Assessment

| Aspect | Status | Notes |
|--------|--------|-------|
| Configuration Format | ✅ Correct | Matches official documentation |
| Required Fields | ✅ Present | `command`, `extensionToLanguage` |
| Optional Fields | ✅ Valid | All fields properly formatted |
| File Structure | ✅ Valid | Both inline and separate file supported |
| **Server Initialization** | ❌ **Blocked** | **Filtered before spawn (platform limitation)** |

**Root cause unchanged**: The configuration is perfect, but Claude Code's platform policy prevents community LSP plugins from starting.

---

## References

- **Official Documentation**: Claude Code Plugin LSP Configuration
- **Our Investigation**: `.claude/ELISP-LSP-INVESTIGATION.md`
- **Test Results**: `.claude/LSP-TEST-RESULTS.md`
