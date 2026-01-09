# Doom Emacs Local Plugin Development: Complete Guide

## Executive Summary

For local plugin development in Doom Emacs, **creating a custom module in `~/.doom.d/modules/` is the recommended approach** for active development. This provides:

- **Immediate integration** with Doom's module system
- **Proper autoload support** without needing `doom sync` for code changes
- **Clear separation** between your config and the plugin itself
- **Flexible configuration** using standard Doom patterns

**Key Insight**: The `:local-repo` package approach is better for *consuming* local packages, while custom modules are better for *developing* them. For active development where you're constantly changing code, a custom module provides the smoothest workflow.

## Official Documentation References

### Primary Sources

1. **Getting Started Guide**: https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org
   - Section: "Writing your own modules"
   - Section: "Using/loading local packages"
   - Section: "Configuring packages"

2. **Module Load Order Table** (from official docs):
   ```
   | File                                        | Interactive | Batch | CLI |
   |---------------------------------------------+-------------+-------+-----|
   | $DOOMDIR/init.el                            | yes         | yes   | yes |
   | {~/.emacs.d,$DOOMDIR}/modules/*/*/init.el   | yes         | yes   | yes |
   | {~/.emacs.d,$DOOMDIR}/modules/*/*/config.el | yes         | no    | no  |
   | $DOOMDIR/config.el                          | yes         | no    | no  |
   ```

3. **Key Quote from Docs**:
   > "To create your own module you need only create a directory for it in
   > `~/.doom.d/modules/abc/xyz`, then add `:abc xyz` to your `doom!` block in
   > `~/.doom.d/init.el` to enable it."

## Best Practices

### 1. **Use Custom Modules for Active Development**

✅ **DO**: Create a module in `~/.doom.d/modules/tools/claude-multi/`

```
~/.doom.d/modules/tools/claude-multi/
├── config.el          # Main configuration and autoload requires
├── packages.el        # Package declarations (if any external deps)
└── autoload/          # Your package's actual code
    ├── claude-multi-agents.el
    ├── claude-multi-progress.el
    └── ...
```

❌ **DON'T**: Use `:local-repo` for packages you're actively developing
- `:local-repo` is for *consuming* stable local packages
- It still requires `doom sync` for recipe changes
- Better suited for vendored dependencies

### 2. **Module File Purposes**

#### `packages.el`
- Declare external dependencies your module needs
- **Don't** configure packages here
- **Don't** declare your own package (it's already part of the module)

```elisp
;; ~/.doom.d/modules/tools/claude-multi/packages.el
;; -*- no-byte-compile: t; -*-

;; Only list EXTERNAL dependencies
(package! alert)
(package! websocket)
```

#### `config.el`
- Load your autoload files
- Configure external packages
- Define user-facing commands
- Set up keybindings

```elisp
;; ~/.doom.d/modules/tools/claude-multi/config.el
;;; tools/claude-multi/config.el -*- lexical-binding: t; -*-

;; Define customization group FIRST
(defgroup claude-multi nil
  "Manage multiple Claude Code agents."
  :group 'tools
  :prefix "claude-multi-")

;; Define all defcustom variables BEFORE loading autoload files
(defcustom claude-multi-worktree-location 'adjacent
  "Where to create worktrees."
  :type 'symbol
  :group 'claude-multi)

;; Load autoload modules
(let ((autoload-dir (expand-file-name "autoload"
                                      (file-name-directory load-file-name))))
  (add-to-list 'load-path autoload-dir)
  (require 'claude-multi-agents)
  (require 'claude-multi-progress))

;; Configure external packages
(use-package! alert
  :defer t
  :config
  (setq alert-default-style 'osx-notifier))

;; Set up keybindings
(map! :leader
      :prefix ("c m" . "claude-multi")
      :desc "Start session" "s" #'claude-multi/start-session
      :desc "Spawn agent"   "a" #'claude-multi/spawn-agent)
```

#### `autoload/` directory
- Contains your actual package implementation
- Files here are autoloaded (no explicit require needed for `;;;###autoload` functions)
- Use `;;;###autoload` cookies for user-facing commands

```elisp
;; ~/.doom.d/modules/tools/claude-multi/autoload/claude-multi-agents.el
;;; autoload/claude-multi-agents.el -*- lexical-binding: t; -*-

;;;###autoload
(defun claude-multi/start-session ()
  "Start a new multi-agent session."
  (interactive)
  ...)

(defun claude-multi--internal-helper ()
  "Internal helper (not autoloaded)."
  ...)
```

#### `init.el` (optional)
- Loaded very early, before `config.el`
- Use for early setup that must happen before other modules load
- Use `use-package-hook!` to override Doom's defaults
- **Don't** use `use-package!` or `after!` here
- Keep it minimal (errors here can break Doom)

```elisp
;; ~/.doom.d/modules/tools/claude-multi/init.el
;;; tools/claude-multi/init.el -*- lexical-binding: t; -*-

;; Only use if you need to override Doom's built-in config
(use-package-hook! some-package
  :pre-config
  (setq some-var custom-value)
  t)  ; Must return non-nil!
```

### 3. **Feature Naming and `after!`**

**Critical Understanding**:
- `after!` expects a **feature symbol**, not a package name
- Feature symbol = what's in `(provide 'feature-name)`
- Module path doesn't automatically become a feature

```elisp
;; In your autoload/claude-multi-agents.el
(provide 'claude-multi-agents)  ; <-- This is the feature name

;; In config.el or user's config
(after! claude-multi-agents     ; <-- Use the feature name
  (setq claude-multi-some-var 'custom-value))
```

**Your module structure means**:
- `config.el` is loaded automatically by Doom
- Each autoload file provides its own feature
- Users configure via `after!` with those feature names

### 4. **Load Path Resolution for Symlinks**

If you want to **symlink** your module (not required, but useful):

```bash
# Option A: Direct development in ~/.doom.d/modules/
cd /path/to/your/project
# Edit files directly in ~/.doom.d/modules/tools/claude-multi/

# Option B: Symlink from external project
ln -s /path/to/claude-multi-agent.el ~/.doom.d/modules/tools/claude-multi
```

If using symlinks, use `file-truename` to resolve paths:

```elisp
;; In config.el
(let ((autoload-dir (expand-file-name
                      "autoload"
                      (file-name-directory (file-truename load-file-name)))))
  (add-to-list 'load-path autoload-dir))
```

### 5. **Avoid `doom sync` During Development**

**When you MUST run `doom sync`:**
- Changed `$DOOMDIR/init.el` (enabled/disabled modules)
- Modified `packages.el` (added/removed dependencies)
- Changed module flags
- Updated Doom itself

**When you DON'T need `doom sync`:**
- Changed code in `config.el` or `autoload/*.el`
- Changed `defcustom` variables
- Modified keybindings
- Added new functions with `;;;###autoload`

**Fast reload during development:**
```elisp
;; Evaluate this to reload your module without restart:
(progn
  (unload-feature 'claude-multi-agents t)
  (unload-feature 'claude-multi-progress t)
  (load "~/.doom.d/modules/tools/claude-multi/config.el"))

;; Or just evaluate changed functions with:
;; C-x C-e (after sexp)
;; gr (Evil operator)
;; SPC c e (eval buffer/region)
```

### 6. **Configuration Variables: Timing Matters**

**Problem**: `defcustom` in autoload files may not be accessible when config.el loads.

**Solution**: Define ALL `defcustom` variables in `config.el` BEFORE loading autoload files:

```elisp
;; config.el
;;; Define ALL customization variables FIRST
(defgroup claude-multi nil ...)
(defcustom claude-multi-worktree-location 'adjacent ...)
(defcustom claude-multi-claude-command "claude" ...)

;; NOW load autoload files that reference these variables
(require 'claude-multi-agents)  ; Can safely reference the defcustoms
```

### 7. **Use `use-package!` not `use-package`**

Always use Doom's wrapped macros in modules:

```elisp
;; ✅ DO
(use-package! alert
  :defer t
  :config ...)

(after! magit
  ...)

;; ❌ DON'T
(use-package alert  ; Missing the !
  :ensure t         ; Never use :ensure in Doom
  ...)
```

### 8. **Byte-Compilation During Development**

**Recommendation**: Disable byte-compilation during active development:

```elisp
;; Add to packages.el to disable byte-compilation
(package! my-package
  :recipe (:local-repo "/path/to/my/package"
           :build (:not compile)))
```

Or add `; -*- no-byte-compile: t; -*-` to individual files.

**Why**: Byte-compiled code won't reflect changes until recompiled. During development, interpret-only is faster.

## Example Setup: Complete Module Structure

### Directory Layout

```
~/.doom.d/
├── init.el                                    # Enable your module here
├── config.el                                  # User's personal config
├── packages.el                                # User's personal packages
└── modules/
    └── tools/
        └── claude-multi/
            ├── packages.el                    # External dependencies only
            ├── config.el                      # Main config & autoload loading
            └── autoload/
                ├── claude-multi-agents.el
                ├── claude-multi-progress.el
                ├── claude-multi-worktree.el
                └── claude-multi-notifications.el
```

### File: `~/.doom.d/init.el`

```elisp
;;; init.el -*- lexical-binding: t; -*-

(doom! :tools
       claude-multi    ; Enable your module
       magit

       ;; ... other modules
       )
```

### File: `~/.doom.d/modules/tools/claude-multi/packages.el`

```elisp
;; -*- no-byte-compile: t; -*-
;;; tools/claude-multi/packages.el

;; Only declare EXTERNAL dependencies
(package! alert)
(package! websocket)
(package! f)
(package! s)
(package! dash)
```

### File: `~/.doom.d/modules/tools/claude-multi/config.el`

```elisp
;;; tools/claude-multi/config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Claude Multi-Agent Plugin

;;; Code:

;; Define customization group FIRST
(defgroup claude-multi nil
  "Manage multiple Claude Code agents in parallel."
  :group 'tools
  :prefix "claude-multi-")

;; Define ALL defcustom variables BEFORE loading autoload files
(defcustom claude-multi-worktree-location 'adjacent
  "Where to create worktrees."
  :type '(choice (const :tag "Adjacent directory" adjacent)
                 (const :tag "Internal .git/worktrees" internal))
  :group 'claude-multi)

(defcustom claude-multi-claude-command "claude"
  "Command to run Claude Code CLI."
  :type 'string
  :group 'claude-multi)

;; Global variables
(defvar claude-multi--agents nil
  "List of all active agents.")

;; Load autoload modules
(let ((autoload-dir (expand-file-name "autoload"
                                      (file-name-directory
                                       (file-truename load-file-name)))))
  (add-to-list 'load-path autoload-dir)
  (require 'claude-multi-agents)
  (require 'claude-multi-progress)
  (require 'claude-multi-worktree)
  (require 'claude-multi-notifications))

;; Configure external packages
(use-package! alert
  :defer t
  :config
  (setq alert-default-style 'osx-notifier))

;; Set up keybindings (loaded after doom-keybinds)
(map! :leader
      :prefix ("c m" . "claude-multi")
      :desc "Start session"     "s" #'claude-multi/start-session
      :desc "Spawn agent"       "a" #'claude-multi/spawn-agent
      :desc "Open progress"     "p" #'claude-multi/open-progress
      :desc "Kill all"          "K" #'claude-multi/kill-all-agents)

(provide 'claude-multi-config)
;;; config.el ends here
```

### File: `~/.doom.d/modules/tools/claude-multi/autoload/claude-multi-agents.el`

```elisp
;;; autoload/claude-multi-agents.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Agent lifecycle management

;;; Code:

;; Define struct
(cl-defstruct claude-agent
  id name color status created-at)

;;;###autoload
(defun claude-multi/start-session ()
  "Initialize a new multi-agent session."
  (interactive)
  (setq claude-multi--agents nil)
  (message "Claude Multi-Agent session started."))

;;;###autoload
(defun claude-multi/spawn-agent ()
  "Spawn a new Claude agent."
  (interactive)
  (let* ((task (read-string "Task description: "))
         (agent (make-claude-agent
                 :name task
                 :status 'pending
                 :created-at (current-time))))
    (push agent claude-multi--agents)
    (message "Spawned agent: %s" task)))

;; Internal helpers (not autoloaded)
(defun claude-multi--launch-agent (agent)
  "Launch AGENT in kitty terminal."
  ...)

(provide 'claude-multi-agents)
;;; claude-multi-agents.el ends here
```

## Common Pitfalls

### Pitfall 1: Using `:local-repo` for Active Development

**Problem**: You created a package with `:local-repo` and need to run `doom sync` after every change.

**Solution**: Use a custom module instead. `:local-repo` is for *consuming* local packages, not developing them.

### Pitfall 2: Wrong Feature Name in `after!`

**Problem**:
```elisp
(after! claude-multi-config  ; Wrong! This is the provide, not what's loaded
  ...)
```

**Solution**: Use the feature name from the `provide` statement:
```elisp
(after! claude-multi-agents  ; Correct! This matches (provide 'claude-multi-agents)
  (setq ...))
```

### Pitfall 3: Configuring in Wrong File

**Problem**: Using `use-package!` or `after!` in `init.el` or `packages.el`.

**Solution**:
- `init.el`: Only `use-package-hook!` for overriding defaults
- `packages.el`: Only `package!` declarations
- `config.el`: All `use-package!` and `after!` blocks

### Pitfall 4: Not Resolving Symlinks

**Problem**: Module symlinked from external location, autoload paths break.

**Solution**: Use `file-truename` to resolve symlinks:
```elisp
(file-name-directory (file-truename load-file-name))
```

### Pitfall 5: Eager Loading Everything

**Problem**: Loading all dependencies immediately, slowing startup.

**Solution**: Use `:defer t`, `:after`, `:commands`, `:hook` for lazy loading:
```elisp
(use-package! websocket
  :defer t  ; Don't load until needed
  :commands (websocket-server-start))
```

### Pitfall 6: Module vs Package Confusion

**Problem**: Trying to use both a custom module AND `:local-repo` package for the same code.

**Solution**: Choose one:
- **Module**: For code you're developing/maintaining
- **Package**: For external dependencies or stable vendored code

### Pitfall 7: Byte-Compiled Stale Code

**Problem**: Code changes don't take effect because old .elc files exist.

**Solution**: During development, disable byte-compilation:
```elisp
;; Add to top of file:
; -*- no-byte-compile: t; -*-
```

## Development Workflow

### Initial Setup

```bash
# 1. Create module directory
mkdir -p ~/.doom.d/modules/tools/claude-multi/autoload

# 2. Create files (see example structure above)
touch ~/.doom.d/modules/tools/claude-multi/config.el
touch ~/.doom.d/modules/tools/claude-multi/packages.el
touch ~/.doom.d/modules/tools/claude-multi/autoload/claude-multi-agents.el

# 3. Enable module in init.el
# Add :tools claude-multi to your doom! block

# 4. Install dependencies
doom sync

# 5. Restart Emacs
doom sync && doom reload
```

### Daily Development Cycle

```bash
# 1. Edit code in ~/.doom.d/modules/tools/claude-multi/

# 2. Reload changed functions (NO restart needed):
#    In Emacs:
#    - C-x C-e after a function to eval it
#    - SPC c e to eval buffer/region
#    - gr (Evil) to eval with operator

# 3. Only run doom sync if you changed packages.el
doom sync  # Only if packages.el changed!

# 4. Test immediately in same session
```

### Full Reload Without Restart

```elisp
;; Create a helper function to reload your module
(defun reload-claude-multi ()
  "Reload the claude-multi module."
  (interactive)
  (unload-feature 'claude-multi-agents t)
  (unload-feature 'claude-multi-progress t)
  (unload-feature 'claude-multi-worktree t)
  (unload-feature 'claude-multi-notifications t)
  (load "~/.doom.d/modules/tools/claude-multi/config.el")
  (message "Claude Multi-Agent module reloaded!"))

;; Call it with: M-x reload-claude-multi
```

### Testing Changes

```elisp
;; Method 1: Evaluate individual functions
(defun my-function ()
  "New function.")

;; Put cursor after closing paren, press: C-x C-e

;; Method 2: Evaluate entire buffer
;; Open buffer, press: SPC c e

;; Method 3: Evaluate region
;; Select code, press: SPC c e

;; Method 4: Use IELM REPL
;; Press: SPC o r
;; Type and eval code interactively
```

### Debugging

```elisp
;; Enable debugging
(setq debug-on-error t)

;; Check if module loaded
(featurep 'claude-multi-agents)  ; Should return t

;; Check load-path
(member "~/.doom.d/modules/tools/claude-multi/autoload" load-path)

;; Describe function to see if it's loaded
(describe-function 'claude-multi/start-session)

;; Check variable values
(describe-variable 'claude-multi--agents)
```

## References

### Official Documentation
- **Doom Emacs Getting Started**: https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org
- **Module README**: https://github.com/doomemacs/doomemacs/blob/master/modules/README.org
- **Doom Emacs FAQ**: https://github.com/doomemacs/doomemacs/blob/master/docs/faq.org

### Example Modules
- **Built-in Magit**: https://github.com/doomemacs/doomemacs/tree/master/modules/tools/magit
- **Built-in LSP**: https://github.com/doomemacs/doomemacs/tree/master/modules/tools/lsp
- **Built-in Company**: https://github.com/doomemacs/doomemacs/tree/master/modules/completion/company

### Community Resources
- **Doom Emacs Discord**: https://discord.gg/qvGgnVx
- **Doom Emacs Discourse**: https://discourse.doomemacs.org/
- **GitHub Discussions**: https://github.com/doomemacs/doomemacs/discussions

### Key Concepts
- **straight.el** (Doom's package manager): https://github.com/raxod502/straight.el
- **use-package**: https://github.com/jwiegley/use-package
- **Emacs Autoloads**: https://www.gnu.org/software/emacs/manual/html_node/elisp/Autoload.html

## Migration Path: From `:local-repo` to Custom Module

If you already have a `:local-repo` setup and want to migrate:

```bash
# 1. Remove from ~/.doom.d/packages.el
# Delete or comment:
# (package! claude-multi-agent :recipe (:local-repo "..."))

# 2. Create module structure
mkdir -p ~/.doom.d/modules/tools/claude-multi

# 3. Move files from your project to module
cp /path/to/project/config.el ~/.doom.d/modules/tools/claude-multi/
cp -r /path/to/project/autoload ~/.doom.d/modules/tools/claude-multi/

# 4. Create packages.el for dependencies
cat > ~/.doom.d/modules/tools/claude-multi/packages.el << 'EOF'
;; -*- no-byte-compile: t; -*-
(package! alert)
(package! websocket)
EOF

# 5. Update ~/.doom.d/init.el
# Add :tools claude-multi to doom! block

# 6. Remove old after! blocks from ~/.doom.d/config.el
# Configuration now lives in the module's config.el

# 7. Sync and restart
doom sync
doom reload
```

## Alternative: Keeping External Project + Module Symlink

If you want to keep developing in an external project directory:

```bash
# Keep your project separate
cd /path/to/claude-multi-agent.el

# Structure it as a module
mkdir -p autoload
# config.el in root
# autoload/*.el files

# Symlink into Doom
ln -s /path/to/claude-multi-agent.el ~/.doom.d/modules/tools/claude-multi

# Enable in init.el
# Add :tools claude-multi

# Sync once
doom sync

# Now edit files in /path/to/claude-multi-agent.el
# Changes are immediately reflected (no doom sync needed)
```

## Conclusion

For local plugin development in Doom Emacs:

1. **Use custom modules** in `~/.doom.d/modules/` for active development
2. **Define all `defcustom` variables in `config.el`** before loading autoload files
3. **Use `file-truename`** if symlinking modules
4. **Avoid `doom sync`** except for package changes
5. **Use `;;;###autoload`** for user-facing commands
6. **Reload individual functions** with `C-x C-e` or `SPC c e`
7. **Use `after!` with feature names** from `provide` statements

This approach provides the smoothest development experience with minimal friction and maximum integration with Doom's module system.
