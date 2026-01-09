# Doom Emacs Local Plugin Development Skill

**Purpose**: Guide Claude in properly developing and maintaining local Emacs Lisp plugins within Doom Emacs

**When to use**: When working on any Emacs Lisp files in this project, configuring Doom Emacs settings, or troubleshooting module loading issues

## Project Setup

### Current Configuration

This project is loaded as a **Doom custom module** via symlink:

```
~/.doom.d/modules/tools/claude-multi -> /Users/stefansevelda/projects/claude-multi-agent.el
```

**Key files:**
- **Project**: `/Users/stefansevelda/projects/claude-multi-agent.el/`
  - `config.el` - Main configuration (provides `claude-multi-config`)
  - `autoload/*.el` - Autoloaded modules
  - `init.el` - Minimal module init

- **Doom config**: `~/.doom.d/`
  - `config.el` - User settings (uses `after!` blocks)
  - `packages.el` - External package declarations (NOT this package)
  - `modules/tools/claude-multi/` - Symlink to project

## Critical Rules

### 1. File Structure & Load Order

The config.el file MUST follow this order:

```elisp
;;; config.el
;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; 1. FIRST: defgroup
(defgroup claude-multi nil
  "Documentation"
  :group 'tools
  :prefix "claude-multi-")

;; 2. SECOND: ALL defcustom declarations
(defcustom claude-multi-claude-command "claude"
  "Command to run Claude Code CLI."
  :type 'string
  :group 'claude-multi)

(defcustom claude-multi-worktree-location 'adjacent
  "Where to create worktrees."
  :type '(choice (const adjacent) (const internal))
  :group 'claude-multi)

;; 3. THIRD: defvar declarations
(defvar claude-multi--agents nil
  "List of all active agents.")

;; 4. FOURTH: Load autoload modules with file-truename
(let ((autoload-dir (expand-file-name "autoload"
                                      (file-name-directory
                                       (file-truename load-file-name)))))
  (add-to-list 'load-path autoload-dir)
  (require 'claude-multi-status)
  (require 'claude-multi-agents)
  (require 'claude-multi-progress))

;; 5. FIFTH: Interactive commands and other code

;; 6. LAST: Provide feature
(provide 'claude-multi-config)
```

**Why this order matters:**
- `defcustom` must come BEFORE `require` statements
- Required modules may reference the customization variables
- `file-truename` resolves symlinks to find the real autoload directory

### 2. Symlink Path Resolution

**ALWAYS use `file-truename` when resolving paths from `load-file-name`:**

```elisp
;; ✅ CORRECT - Resolves symlinks
(file-name-directory (file-truename load-file-name))

;; ❌ WRONG - Returns symlink path
(file-name-directory load-file-name)
```

**Why**: When Doom loads the module via symlink, `load-file-name` points to:
- Symlink: `~/.doom.d/modules/tools/claude-multi/config.el`
- Real file: `/Users/stefansevelda/projects/claude-multi-agent.el/config.el`

Without `file-truename`, looking for `autoload/` directory fails.

### 3. Feature Names & after! Blocks

**Feature name comes from the `(provide ...)` statement:**

```elisp
;; In config.el
(provide 'claude-multi-config)  ; <-- This is the feature name

;; In ~/.doom.d/config.el
(after! claude-multi-config     ; <-- Use this, not 'claude-multi-agent'
  (setq claude-multi-claude-command "claude26"))
```

**Alternative - No after! needed for your own module:**

Since the variables are already defined when your module loads, you can set them directly in `~/.doom.d/config.el`:

```elisp
;; This works because module is loaded via doom's module system
(setq claude-multi-claude-command "claude26")
(setq claude-multi-output-throttle-delay 0.5)
```

### 4. Byte Compilation During Development

**Disable byte compilation during active development:**

```elisp
;; Add to first line of files being actively developed
;; -*- lexical-binding: t; no-byte-compile: t; -*-
```

**Why**: Byte-compiled files (`.elc`) are loaded instead of source files (`.el`), causing changes to not be reflected until recompilation.

**When to enable**: Only for production/stable code.

### 5. Module vs Package Approach

**DO NOT mix both approaches:**

```elisp
;; ❌ WRONG - Don't have both:
;; In packages.el:
(package! claude-multi-agent
  :recipe (:local-repo "/path/to/project"))

;; AND symlink:
~/.doom.d/modules/tools/claude-multi -> /path/to/project
```

**✅ CORRECT - Use ONLY module symlink:**
- Remove any `package!` declarations for this project
- Keep only the symlink in `modules/tools/`
- Let Doom's module system handle loading

### 6. Keybinding Configuration

**CRITICAL: Use `doom-after-modules-config-hook` for custom module keybindings.**

For custom modules loaded via symlink, use Doom's module hook:

```elisp
;; In config.el
(add-hook 'doom-after-modules-config-hook
  (defun claude-multi--setup-keybindings ()
    "Set up keybindings after all Doom modules are configured."
    (map! :leader
          :prefix ("c m" . "claude-multi")
          :desc "Start session" "s" #'claude-multi/start-session
          :desc "Spawn agent"   "a" #'claude-multi/spawn-agent)))
```

**Why this works:**
- The hook runs AFTER all Doom modules are fully configured
- This ensures evil and the leader key are completely set up
- This is the proper way for custom modules to register keybindings

**Why `use-package! evil :config` doesn't work:**
- Evil is already loaded before our module's config.el runs
- The `:config` block only executes when the package is first configured
- Since evil is already configured, our :config block never runs

**Common mistakes (DON'T do these):**

❌ **WRONG - Top-level `after!` doesn't work reliably:**
```elisp
;; This may not execute at the right time during startup
(after! evil
  (map! :leader ...))
```

❌ **WRONG - `+bindings.el` is NOT a Doom convention:**
```elisp
;; +bindings.el  <-- This file is not automatically loaded by Doom
(map! :leader ...)
```

❌ **WRONG - Conditional loading with `featurep`:**
```elisp
;; Complex and unreliable
(if (featurep 'doom-keybinds)
    (setup-keybindings)
  (with-eval-after-load ...))
```

**✅ CORRECT - Use `doom-after-modules-config-hook`:**
```elisp
(add-hook 'doom-after-modules-config-hook
  (defun my-module--setup-keybindings ()
    (map! :leader
          :prefix ("c m" . "my-module")
          :desc "Command" "c" #'my-command)))
```

**Key insights:**
1. Custom modules (via symlink) load differently than built-in Doom modules
2. Built-in modules can use `use-package!` :config because they control package loading
3. Custom modules must use hooks because packages are already loaded
4. `doom-after-modules-config-hook` is specifically for this use case

**Verification:**
```elisp
;; Check if keybindings are registered
(lookup-key doom-leader-map (kbd "c m"))
;; Should return a keymap, not nil

;; Manually reload if needed
(load-file "~/.doom.d/modules/tools/claude-multi/config.el")
```

## Development Workflow

### One-Time Setup

```bash
# 1. Create module symlink (already done)
ln -s /Users/stefansevelda/projects/claude-multi-agent.el \
      ~/.doom.d/modules/tools/claude-multi

# 2. Sync Doom
doom sync

# 3. Restart Emacs
```

### Daily Development (Post-Setup)

**Making changes:**

1. **Edit code** in project files
2. **Eval immediately** - No restart needed!
   ```elisp
   ;; Eval a function (Evil mode):
   (defun my-function () ...)  ; Place cursor after closing paren
   SPC c e  ; Doom + Evil: evaluates current form
   ;; Or: g r  ; Evil: evaluate last sexp (gr in normal mode)

   ;; Eval entire buffer (Evil mode):
   SPC c b  ; Doom: eval-buffer
   ;; Or: M-x eval-buffer

   ;; Or reload specific file:
   (load-file "/path/to/file.el")
   ```

3. **Test changes** immediately
4. **Commit** when working

**Evil Mode Key Bindings (Doom Emacs):**
- `SPC c e` - Evaluate expression under cursor
- `SPC c b` - Evaluate entire buffer
- `SPC c r` - Evaluate region
- `g r` - Evaluate last sexp (normal mode)
- `SPC h r r` - Reload Doom configuration (doom/reload)

**When you DO need doom sync:**
- Added/removed files from `packages.el`
- Modified module flags in `init.el`
- Added new modules

**When you DON'T need doom sync:**
- Editing existing `.el` files
- Changing function implementations
- Adding new functions to existing files
- Modifying variables

### Reloading Changes

**Best practice workflow:**

```elisp
;; Option 1: Eval specific changes
;; After editing a function, eval it:
C-x C-e  ; at end of defun

;; Option 2: Reload entire file
(load-file "/Users/stefansevelda/projects/claude-multi-agent.el/config.el")

;; Option 3: Reload via module system
(unload-feature 'claude-multi-config t)
(load-file "/Users/stefansevelda/projects/claude-multi-agent.el/config.el")

;; Option 4: Full reload (slower)
M-x doom/reload
```

## Common Issues & Solutions

### Issue: "void-function" errors after changes

**Cause**: Module not loaded or function not evaled

**Solution:**
```elisp
;; Check if module loaded
(featurep 'claude-multi-config)  ; Should return t

;; If nil, load it:
(load-file "/Users/stefansevelda/projects/claude-multi-agent.el/config.el")

;; Verify function exists:
(fboundp 'claude-multi--read-status-file)  ; Should return t
```

### Issue: Settings not applied (wrong command, wrong values)

**Cause**: Using wrong feature name in `after!` block

**Solution:**
```elisp
;; In ~/.doom.d/config.el
;; ❌ WRONG:
(after! claude-multi-agent  ; Package name
  (setq claude-multi-claude-command "claude26"))

;; ✅ CORRECT:
(after! claude-multi-config  ; Feature name from (provide ...)
  (setq claude-multi-claude-command "claude26"))

;; ✅ BETTER: No after! needed for own module
(setq claude-multi-claude-command "claude26")
```

### Issue: Changes not reflected after edit

**Cause**: Byte-compiled file or not evaled

**Solution:**
```elisp
;; 1. Remove byte-compiled files
(shell-command "rm /Users/stefansevelda/projects/claude-multi-agent.el/**/*.elc")

;; 2. Disable byte-compilation
;; Add to file headers:
;; -*- no-byte-compile: t; -*-

;; 3. Reload the file
(load-file "/path/to/modified-file.el")
```

### Issue: "Autoloading file" errors on startup

**Cause**: Doom trying to autoload before module configured

**Solution:**
Keep `init.el` minimal:

```elisp
;;; init.el
;; Nothing needed here - config.el handles everything
(provide 'init)
```

### Issue: Can't find autoload directory

**Cause**: Not using `file-truename` to resolve symlink

**Solution:**
Already fixed in config.el - uses `file-truename`:

```elisp
(let ((autoload-dir (expand-file-name "autoload"
                                      (file-name-directory
                                       (file-truename load-file-name)))))
  (add-to-list 'load-path autoload-dir))
```

## Testing & Verification

### After Making Changes

```elisp
;; 1. Verify module loaded
(featurep 'claude-multi-config)  ; → t

;; 2. Verify functions available
(fboundp 'claude-multi--read-status-file)  ; → t
(fboundp 'claude-multi/spawn-agent)  ; → t

;; 3. Verify settings applied
claude-multi-claude-command  ; → "claude26"

;; 4. Check load-path includes autoload
(member "/Users/stefansevelda/projects/claude-multi-agent.el/autoload"
        load-path)  ; → t

;; 5. Test functionality
(claude-multi/spawn-agent)
;; Should launch with claude26, not claude
```

### Verifying Elisp Files

Use the elisp-eval tool (from elisp-verification.md skill):

```bash
# Lint file
/Users/stefansevelda/bin/elisp-eval lint . config.el

# Test loading
echo "(load-file \"config.el\")" | /Users/stefansevelda/bin/elisp-eval eval .

# Test specific function
echo "(claude-multi--normalize-path \"/test/\")" | \
  /Users/stefansevelda/bin/elisp-eval eval .
```

## Best Practices

### 1. Variable Declaration Order

Always declare variables BEFORE they're used:

```elisp
;; ✅ CORRECT
(defcustom claude-multi-claude-command "claude" ...)
(defvar claude-multi--agents nil ...)

(require 'claude-multi-agents)  ; Can now reference these variables

;; ❌ WRONG
(require 'claude-multi-agents)  ; Tries to use undefined variables

(defcustom claude-multi-claude-command "claude" ...)
```

### 2. Use Autoload Cookies

Mark public functions with autoload cookies:

```elisp
;;;###autoload
(defun claude-multi/spawn-agent ()
  "Public command - automatically available"
  ...)

(defun claude-multi--internal-helper ()
  "Private function - no autoload"
  ...)
```

### 3. Namespace Conventions

```elisp
;; Public API: claude-multi/command-name
(defun claude-multi/spawn-agent () ...)

;; Internal functions: claude-multi--private-name
(defun claude-multi--launch-agent () ...)

;; Variables: claude-multi-setting-name
(defcustom claude-multi-claude-command "claude" ...)

;; Internal variables: claude-multi--internal-var
(defvar claude-multi--agents nil ...)
```

### 4. Documentation

Every function and variable needs documentation:

```elisp
(defun claude-multi--normalize-path (path)
  "Normalize PATH for status file matching.
Returns canonical absolute path with symlinks resolved, no trailing slash."
  ...)

(defvar claude-multi--agents nil
  "List of all active agents.")
```

### 5. File Headers

All files should have proper headers:

```elisp
;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; autoload/claude-multi-status.el --- Status tracking

;;; Commentary:
;; File-based status tracking for agents

;;; Code:

;; ... code here ...

(provide 'claude-multi-status)
;;; claude-multi-status.el ends here
```

## When to Use This Skill

Use this skill when:

1. **Modifying config.el** - Ensure proper load order
2. **Adding new autoload files** - Check they're required in correct order
3. **User reports settings not working** - Check `after!` uses correct feature name
4. **Module won't load** - Verify `file-truename` path resolution
5. **Changes not reflected** - Check for byte-compilation, eval changes
6. **Adding new customization variables** - Add before `require` statements

## Integration with Other Skills

This skill works with:

- **elisp-verification.md** - Use elisp-eval tool to verify code
- **doom-local-plugin-development.md** - This skill (self-reference)

## Quick Reference

**Evil Mode Commands (Doom Emacs):**
```
SPC c e       - Evaluate expression under cursor
SPC c b       - Evaluate entire buffer
SPC c r       - Evaluate region
g r           - Evaluate last sexp (normal mode)
SPC h r r     - Reload Doom (doom/reload)
SPC m ? (or SPC c m ?)  - Debug status matching (claude-multi)
```

**Elisp Evaluation:**
```elisp
;; Check module status
(featurep 'claude-multi-config)

;; Reload module
(load-file "/Users/stefansevelda/projects/claude-multi-agent.el/config.el")

;; Verify settings
claude-multi-claude-command  ; Should be "claude26"

;; Check load path
load-path  ; Should include autoload directory

;; Force eval a function (Evil mode: SPC c e)
;; Or traditional: C-x C-e after closing paren

;; Reload buffer (Evil mode: SPC c b)
;; Or: M-x eval-buffer

;; Doom reload (Evil mode: SPC h r r)
;; Or: M-x doom/reload
```

## Documentation References

- Project docs: `DOOM-EMACS-LOCAL-PLUGIN-GUIDE.md`
- Quick reference: `DOOM-SETUP-SUMMARY.md`
- Issue solutions: `CURRENT-ISSUES-SOLUTIONS.md`
- Status fix: `STATUS-FIX-SUMMARY.md`
- Elisp testing: `.claude/elisp-verification.md`

## Summary

**Key Takeaways:**

1. ✅ Use module symlink approach (not `:local-repo` package)
2. ✅ Declare all `defcustom` BEFORE `require` statements
3. ✅ Use `file-truename` to resolve symlink paths
4. ✅ Use correct feature name in `after!` blocks (from `provide` statement)
5. ✅ Disable byte-compilation during development
6. ✅ Eval changes directly - no need for doom sync on code changes
7. ✅ Keep `init.el` minimal
8. ✅ Use elisp-eval tool to verify changes

**The Golden Rule**:
After editing code, eval it immediately with `C-x C-e` or `M-x eval-buffer`. No need to restart Emacs or run doom sync for code changes!
