# Doom Emacs Loading Fix

**Date**: January 9, 2026
**Issue**: Module loading failures after doom sync and restart
**Status**: ✅ FIXED

## Problem

After modifying claude-multi-agent.el files, changes weren't reflected after `doom sync` and Emacs restart. Multiple issues:

1. **Duplicate loading mechanisms**: Both module symlink AND package definition
2. **Symlink path resolution**: `load-file-name` returned symlink path, not real path
3. **Setting timing**: Variables set before package loaded, getting overwritten

## Errors Seen

```
void-function claude-multi--read-status-file
Autoloading file /Users/stefansevelda/.doom.d/modules/tools/claude-$
Failed to launch agent
```

## Root Causes

### Issue 1: Conflicting Load Methods

**Before**: Had BOTH configured
```elisp
;; In ~/.doom.d/packages.el
(package! claude-multi-agent
  :recipe (:local-repo "/Users/stefansevelda/projects/claude-multi-agent.el"))

;; AND symlink
~/.doom.d/modules/tools/claude-multi -> /Users/stefansevelda/projects/claude-multi-agent.el
```

These conflicted, causing Doom to try loading through both mechanisms.

### Issue 2: Symlink Path Resolution

**Before**: Used `load-file-name` directly
```elisp
(let ((autoload-dir (expand-file-name "autoload"
                                      (file-name-directory load-file-name))))
  (add-to-list 'load-path autoload-dir))
```

When loaded through symlink: `/Users/stefansevelda/.doom.d/modules/tools/claude-multi/config.el`
This looked for: `/Users/stefansevelda/.doom.d/modules/tools/claude-multi/autoload/` ❌ WRONG!

**After**: Resolve symlink first
```elisp
(let ((autoload-dir (expand-file-name "autoload"
                                      (file-name-directory (file-truename load-file-name)))))
  (add-to-list 'load-path autoload-dir))
```

Now resolves to: `/Users/stefansevelda/projects/claude-multi-agent.el/autoload/` ✅ CORRECT!

### Issue 3: Variable Setting Timing

**Before**: Set immediately
```elisp
;; In ~/.doom.d/config.el
(setq claude-multi-claude-command "claude26")
```

Problem: `defcustom` in config.el runs AFTER this, overwriting with default "claude".

**After**: Use `after!` block
```elisp
;; In ~/.doom.d/config.el
(after! claude-multi-agent
  (setq claude-multi-claude-command "claude26"))
```

Now sets AFTER package loads, preserving the value.

## Solutions Applied

### 1. Remove Duplicate Package Definition

**File**: `~/.doom.d/packages.el`

```elisp
;; REMOVED:
;; (package! claude-multi-agent
;;   :recipe (:local-repo "/Users/stefansevelda/projects/claude-multi-agent.el"))

;; ADDED comment:
;; Claude Multi-Agent - loaded via module symlink in modules/tools/claude-multi
;; (package definition not needed - using module approach)
```

**Why**: Using ONLY the module symlink approach for direct development workflow.

### 2. Fix Symlink Path Resolution

**File**: `/Users/stefansevelda/projects/claude-multi-agent.el/config.el` (lines 215-224)

```elisp
(let ((autoload-dir (expand-file-name "autoload"
                                      (or (and load-file-name
                                               ;; Resolve symlinks to get the real path
                                               (file-name-directory (file-truename load-file-name)))
                                          ...))))
  (message ">>> CLAUDE-MULTI: Adding autoload directory to load-path: %s" autoload-dir)
  (add-to-list 'load-path autoload-dir)
  (require 'claude-multi-status)
  ...)
```

**Why**: `file-truename` resolves the symlink to the actual file location.

### 3. Fix Variable Setting Timing

**File**: `~/.doom.d/config.el` (lines 67-72)

```elisp
;; BEFORE:
;; (setq claude-multi-claude-command "claude26")

;; AFTER:
(after! claude-multi-agent
  (setq claude-multi-claude-command "claude26")
  (setq claude-multi-output-throttle-delay 0.5)
  (setq claude-multi-kitty-window-type 'tab))
```

**Why**: `after!` ensures settings apply AFTER the package defines its variables.

### 4. Update Module Init File

**File**: `~/.doom.d/modules/tools/claude-multi/init.el`

```elisp
(when (file-exists-p (expand-file-name "autoload" (file-name-directory load-file-name)))
  (add-to-load-path! "autoload"))
```

**Why**: Ensure autoload directory is in load-path early (belt-and-suspenders).

## Verification

After applying fixes:

```bash
# 1. Run doom sync
~/.config/emacs/.emacs.d/bin/doom sync

# 2. Restart Emacs
```

Then in Emacs:

```elisp
;; Verify modules loaded
(featurep 'claude-multi-status)  ; Should return t
(fboundp 'claude-multi--read-status-file)  ; Should return t

;; Verify settings applied
claude-multi-claude-command  ; Should return "claude26"

;; Verify autoload path
(member "/Users/stefansevelda/projects/claude-multi-agent.el/autoload" load-path)  ; Should return t
```

## Workflow Going Forward

### Making Changes

1. **Edit files** in `/Users/stefansevelda/projects/claude-multi-agent.el/`
2. **Run doom sync**: `~/.config/emacs/.emacs.d/bin/doom sync`
3. **Restart Emacs** or run `M-x doom/reload`
4. **Changes are live** ✅

### Why This Works Now

- **Module symlink**: Provides direct access to project files
- **file-truename**: Resolves symlink to find autoload directory
- **after! block**: Ensures settings apply after package loads
- **No conflicts**: Single loading mechanism (module approach)

## Files Modified

### Project Files
- ✅ `config.el` - Fixed symlink resolution in autoload path

### Doom Config Files
- ✅ `~/.doom.d/packages.el` - Removed duplicate package definition
- ✅ `~/.doom.d/config.el` - Use `after!` block for settings
- ✅ `~/.doom.d/modules/tools/claude-multi/init.el` - Add autoload to load-path

## Benefits

1. ✅ **Immediate reflection**: Changes appear after doom sync + restart
2. ✅ **No errors**: All modules load correctly
3. ✅ **Correct settings**: claude26 command used
4. ✅ **Clean approach**: Single loading mechanism
5. ✅ **Development friendly**: Direct symlink to project

## Testing Done

```elisp
;; 1. Verify loading
(featurep 'claude-multi-status)  ; → t ✅

;; 2. Verify functions available
(fboundp 'claude-multi--read-status-file)  ; → t ✅
(fboundp 'claude-multi/debug-status-matching)  ; → t ✅

;; 3. Verify settings
claude-multi-claude-command  ; → "claude26" ✅

;; 4. Launch agent
(claude-multi/spawn-agent)  ; → Uses claude26 ✅

;; 5. Check diagnostic
(claude-multi/debug-status-matching)  ; → Shows all agents ✅
```

## Related Issues

This fix also resolves:
- Status tracking not working (modules weren't loaded)
- Wrong claude command (settings overwritten)
- Autoload failures (wrong path)

## Documentation

- Main fix: This document
- Status fix: `STATUS-FIX-SUMMARY.md`
- Session summary: `SESSION-SUMMARY.md`
- Elisp skill: `.claude/elisp-verification.md`

## Key Learnings

1. **Symlinks need resolution**: Always use `file-truename` for symlinked files
2. **Load order matters**: Use `after!` for settings that override defcustom
3. **Single loading method**: Choose ONE mechanism (module OR package)
4. **Debug messages help**: Added logging to show resolved paths

## Commit

```
commit 3f72ffb
Author: Stefan Sevelda
Date:   Thu Jan 9 2026

    Fix module loading with symlink path resolution

    When loading through Doom module symlink, use file-truename to resolve
    the real path before looking for autoload directory.
```
