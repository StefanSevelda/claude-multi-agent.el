# Solving Current claude-multi-agent Setup Issues

## Your Current Setup Issues

Based on your context, you're experiencing these specific problems:

1. Settings in `after!` block need correct feature name
2. Module loading requires `file-truename` to resolve symlinks
3. Changes require `doom sync` and restart to take effect
4. Using module symlink: `~/.doom.d/modules/tools/claude-multi -> /path/to/project`
5. Conflicts when trying both module symlink AND `:local-repo` package

## Root Cause Analysis

### Issue 1: `after!` Feature Name

**Problem**: You're using `after!` with the wrong feature name.

```elisp
;; ❌ Wrong - this doesn't match what's provided
(after! claude-multi-config
  (setq claude-multi-worktree-location 'internal))

;; ❌ Also wrong - this is not a feature
(after! claude-multi
  ...)
```

**Root Cause**: Doom's `after!` expects a **feature symbol** (from `provide`), not a module name or file name.

**Solution**: Use the feature name from your autoload files:

```elisp
;; ✅ Correct - matches (provide 'claude-multi-agents)
(after! claude-multi-agents
  (setq claude-multi-worktree-location 'internal))
```

**Better Solution**: Don't use `after!` for your own module's variables. Since your module's `config.el` loads first, define variables there directly:

```elisp
;; In ~/.doom.d/modules/tools/claude-multi/config.el

;; Define variables directly (no after! needed)
(defcustom claude-multi-worktree-location 'adjacent
  "Where to create worktrees."
  :type 'symbol
  :group 'claude-multi)

;; Users can customize via:
;; M-x customize-group RET claude-multi RET
;; Or in their ~/.doom.d/config.el:
(setq claude-multi-worktree-location 'internal)  ; No after! needed!
```

### Issue 2: Variables Not Accessible

**Problem**: `defcustom` in autoload files aren't accessible when module loads.

**Root Cause**: Load order issue. When `config.el` loads and calls `(require 'claude-multi-agents)`, variables defined in that file may not be fully processed yet.

**Solution**: Move ALL `defcustom` definitions to `config.el` BEFORE loading autoload files:

```elisp
;;; config.el
;; Step 1: Define defgroup
(defgroup claude-multi nil
  "Manage multiple Claude Code agents."
  :group 'tools
  :prefix "claude-multi-")

;; Step 2: Define ALL defcustom variables
(defcustom claude-multi-worktree-location 'adjacent ...)
(defcustom claude-multi-claude-command "claude" ...)
(defcustom claude-multi-kitty-listen-address nil ...)
;; ... ALL other defcustoms ...

;; Step 3: Define defvar variables
(defvar claude-multi--agents nil ...)
(defvar claude-multi--agent-id-counter 0 ...)

;; Step 4: NOW load autoload files (they can reference the variables)
(let ((autoload-dir ...))
  (require 'claude-multi-agents)    ; Can now use the variables
  (require 'claude-multi-progress))
```

### Issue 3: Symlink Path Resolution

**Problem**: When module is symlinked, `load-file-name` points to the symlink, not the real directory.

**Root Cause**: Emacs returns the symlink path, not the resolved path, breaking relative path resolution.

**Your Current Solution** (Correct):
```elisp
(let ((autoload-dir (expand-file-name
                      "autoload"
                      (file-name-directory
                       (file-truename load-file-name)))))  ; <-- file-truename is key
  (add-to-list 'load-path autoload-dir))
```

This is the **correct approach** for symlinked modules. Keep using it!

### Issue 4: Need `doom sync` After Every Change

**Problem**: Code changes don't take effect without `doom sync` and restart.

**Root Cause**: This suggests you might be using `:local-repo` somewhere, or byte-compiled files are stale.

**Solutions**:

1. **Remove `:local-repo` if present**:
```elisp
;; In ~/.doom.d/packages.el
;; DELETE or comment out:
; (package! claude-multi-agent :recipe (:local-repo "..."))
```

2. **Disable byte-compilation during development**:
```elisp
;; Add to top of each .el file:
; -*- no-byte-compile: t; -*-
```

3. **Delete existing .elc files**:
```bash
find ~/.doom.d/modules/tools/claude-multi -name "*.elc" -delete
find /path/to/your/project -name "*.elc" -delete
```

4. **Reload functions without restart**:
```elisp
;; After changing a function, put cursor after it and press:
;; C-x C-e   (eval-last-sexp)

;; Or eval entire buffer:
;; SPC c e   (or M-x +eval/buffer-or-region)
```

### Issue 5: Module Symlink vs `:local-repo` Conflict

**Problem**: Tried both module symlink AND `:local-repo` package, causing conflicts.

**Root Cause**: These are two different approaches that conflict:
- **Module symlink**: Module system loads it automatically
- **`:local-repo`**: Package manager tries to install it

**Solution**: Choose ONE approach. For development, use module symlink:

```bash
# 1. Remove from ~/.doom.d/packages.el
# Delete any (package! claude-multi-agent ...) lines

# 2. Keep only module in init.el
# In ~/.doom.d/init.el:
(doom! :tools
       claude-multi    ; <-- Only this
       ...)

# 3. Remove symlink if needed and recreate
rm ~/.doom.d/modules/tools/claude-multi
ln -s /path/to/claude-multi-agent.el ~/.doom.d/modules/tools/claude-multi

# 4. Run doom sync once
doom sync

# 5. Restart Emacs once
doom reload
```

## Complete Fix: Step-by-Step

### Step 1: Clean Up Conflicts

```bash
# Remove any stale byte-compiled files
find ~/.doom.d/modules/tools/claude-multi -name "*.elc" -delete 2>/dev/null
find /path/to/claude-multi-agent.el -name "*.elc" -delete 2>/dev/null

# Check for conflicts
grep -r "claude-multi" ~/.doom.d/packages.el
# If found, delete those lines
```

### Step 2: Fix Project Structure

```bash
# Your project should look like this:
/path/to/claude-multi-agent.el/
├── config.el          # Main config
├── packages.el        # External deps only
└── autoload/
    ├── claude-multi-agents.el
    ├── claude-multi-progress.el
    ├── claude-multi-worktree.el
    └── claude-multi-notifications.el
```

### Step 3: Fix config.el Order

```elisp
;;; config.el -*- lexical-binding: t; -*-
; -*- no-byte-compile: t; -*-  ; <-- Add this!

;;; Commentary:
;; Configuration for Claude Multi-Agent Plugin

;;; Code:

;; STEP 1: defgroup
(defgroup claude-multi nil
  "Manage multiple Claude Code agents in parallel."
  :group 'tools
  :prefix "claude-multi-")

;; STEP 2: ALL defcustom variables (move from autoload files if needed)
(defcustom claude-multi-worktree-location 'adjacent
  "Where to create worktrees."
  :type '(choice (const :tag "Adjacent directory" adjacent)
                 (const :tag "Internal .git/worktrees" internal))
  :group 'claude-multi)

(defcustom claude-multi-claude-command "claude"
  "Command to run Claude Code CLI."
  :type 'string
  :group 'claude-multi)

;; ... ALL other defcustom variables ...

;; STEP 3: defvar global variables
(defvar claude-multi--agents nil
  "List of all active agents.")

(defvar claude-multi--agent-id-counter 0
  "Counter for generating unique agent IDs.")

;; STEP 4: Load autoload modules (with file-truename for symlinks)
(let ((autoload-dir (expand-file-name
                      "autoload"
                      (file-name-directory
                       (file-truename load-file-name)))))  ; <-- file-truename!
  (add-to-list 'load-path autoload-dir)
  (require 'claude-multi-status)        ; Load in dependency order
  (require 'claude-multi-agents)
  (require 'claude-multi-progress)
  (require 'claude-multi-worktree)
  (require 'claude-multi-notifications))

;; STEP 5: Configure external packages
(use-package! alert
  :defer t
  :config
  (setq alert-default-style 'osx-notifier))

;; STEP 6: Keybindings
(map! :leader
      :prefix ("c m" . "claude-multi")
      :desc "Start session" "s" #'claude-multi/start-session
      :desc "Spawn agent"   "a" #'claude-multi/spawn-agent)

(provide 'claude-multi-config)
;;; config.el ends here
```

### Step 4: Ensure Symlink is Correct

```bash
# Check symlink
ls -la ~/.doom.d/modules/tools/claude-multi

# Should show:
# claude-multi -> /path/to/claude-multi-agent.el

# If wrong, fix it:
rm ~/.doom.d/modules/tools/claude-multi
ln -s /path/to/claude-multi-agent.el ~/.doom.d/modules/tools/claude-multi
```

### Step 5: Update init.el

```elisp
;;; ~/.doom.d/init.el
(doom! :tools
       claude-multi    ; <-- Enable module
       magit
       ;; ... other modules
       )
```

### Step 6: Clean User Config

In `~/.doom.d/config.el`, you should NOT need `after!` for your module:

```elisp
;;; ~/.doom.d/config.el

;; ❌ Don't do this for your own module:
; (after! claude-multi-config
;   (setq claude-multi-worktree-location 'internal))

;; ✅ Do this instead (module already loaded):
(setq claude-multi-worktree-location 'internal)
(setq claude-multi-claude-command "claude")

;; Or better yet, use customize interface:
;; M-x customize-group RET claude-multi RET
```

### Step 7: One-Time Sync

```bash
# Run doom sync once
doom sync

# Restart Emacs once
doom reload
# Or restart manually
```

### Step 8: Verify Setup

```elisp
;; In Emacs, check if module loaded:
(featurep 'claude-multi-agents)    ; Should return: t
(featurep 'claude-multi-progress)  ; Should return: t

;; Check variables are defined:
(describe-variable 'claude-multi-worktree-location)
(describe-variable 'claude-multi--agents)

;; Check functions are available:
(describe-function 'claude-multi/start-session)
(describe-function 'claude-multi/spawn-agent)

;; Check load-path includes autoload directory:
(member (expand-file-name "autoload"
                          (file-name-directory
                           (file-truename
                            (locate-library "claude-multi-config"))))
        load-path)  ; Should return: that path
```

## Testing Your Fixes

### Test 1: Variable Access
```elisp
;; Should work immediately:
(setq claude-multi-worktree-location 'internal)
claude-multi-worktree-location  ; Should return: internal
```

### Test 2: Function Changes Without Restart
```elisp
;; 1. Edit a function in autoload/claude-multi-agents.el
;; 2. Put cursor after function definition
;; 3. Press: C-x C-e
;; 4. Call function immediately - should use new code
```

### Test 3: Add New Command Without Sync
```elisp
;; 1. Add to autoload/claude-multi-agents.el:
;;;###autoload
(defun claude-multi/test-command ()
  "Test command."
  (interactive)
  (message "Test command works!"))

;; 2. Eval function with C-x C-e
;; 3. Call immediately:
(claude-multi/test-command)  ; Should work!

;; 4. NO doom sync needed!
```

### Test 4: Change Keybinding Without Restart
```elisp
;; 1. Edit keybinding in config.el
;; 2. Eval the map! form with C-x C-e
;; 3. Keybinding should work immediately
```

## Daily Development Workflow (Post-Fix)

```bash
# Morning: Start Emacs once
emacs

# Edit code in /path/to/claude-multi-agent.el/autoload/...

# Test changes:
# - Put cursor after function
# - Press: C-x C-e
# - Test immediately

# Edit config in config.el

# Test changes:
# - Put cursor after config form
# - Press: C-x C-e
# - Test immediately

# NO doom sync needed!
# NO restart needed!

# End of day: Just close Emacs
```

## When You MUST Run doom sync

Only in these cases:

1. Changed `packages.el` (added/removed external dependencies)
2. Changed `~/.doom.d/init.el` (enabled/disabled modules)
3. Updated Doom itself (`doom upgrade`)

**NOT needed for**:
- Code changes in `config.el` or `autoload/*.el`
- Variable changes
- Keybinding changes
- New functions (with `;;;###autoload`)

## Quick Reference: Common Tasks

### Reload Entire Module
```elisp
(progn
  (unload-feature 'claude-multi-agents t)
  (unload-feature 'claude-multi-progress t)
  (unload-feature 'claude-multi-worktree t)
  (unload-feature 'claude-multi-notifications t)
  (load (expand-file-name "config.el"
                          (file-name-directory
                           (file-truename
                            (locate-library "claude-multi-config"))))))
```

### Test Variable Definition
```elisp
;; Should all return t:
(boundp 'claude-multi-worktree-location)
(boundp 'claude-multi--agents)
(get 'claude-multi-worktree-location 'custom-type)  ; Should return type spec
```

### Debug Load Path
```elisp
(let ((module-dir (file-name-directory
                   (file-truename
                    (locate-library "claude-multi-config")))))
  (list
   :module-dir module-dir
   :autoload-dir (expand-file-name "autoload" module-dir)
   :in-load-path (member (expand-file-name "autoload" module-dir)
                         load-path)))
```

## Summary of Fixes

| Issue | Root Cause | Solution |
|-------|------------|----------|
| Wrong `after!` feature name | Using module name instead of provided feature | Use feature names from `(provide 'name)` or don't use `after!` at all for your module |
| Variables not accessible | Defined in autoload files, not in config.el | Move ALL `defcustom` to config.el BEFORE requires |
| Symlink paths break | `load-file-name` returns symlink, not real path | Use `file-truename` to resolve symlinks |
| Need doom sync for changes | Byte-compiled files or `:local-repo` usage | Disable byte-compilation, remove `:local-repo`, eval functions directly |
| Module vs package conflict | Using both approaches | Choose module approach only, remove `:local-repo` |

## Next Steps

1. Apply fixes from this document
2. Run `doom sync` once
3. Restart Emacs once
4. Test that changes work without sync/restart
5. Enjoy smooth development workflow!

For complete reference, see `DOOM-EMACS-LOCAL-PLUGIN-GUIDE.md`.
