# Doom Emacs Setup Summary for claude-multi-agent

## Quick Reference: The Right Way

### Module Structure (Recommended)

```
~/.doom.d/
├── init.el                    # Add: :tools claude-multi
└── modules/
    └── tools/
        └── claude-multi/
            ├── packages.el    # External deps only: alert, websocket, etc.
            ├── config.el      # Main config, loads autoload files
            └── autoload/
                ├── claude-multi-agents.el
                ├── claude-multi-progress.el
                ├── claude-multi-worktree.el
                └── claude-multi-notifications.el
```

### Key Files

#### `~/.doom.d/init.el`
```elisp
(doom! :tools
       claude-multi    ; Enable your module
       ;; ... other modules
       )
```

#### `~/.doom.d/modules/tools/claude-multi/packages.el`
```elisp
;; -*- no-byte-compile: t; -*-
;;; tools/claude-multi/packages.el

;; Only EXTERNAL dependencies
(package! alert)
(package! websocket)
(package! f)
(package! s)
(package! dash)
```

#### `~/.doom.d/modules/tools/claude-multi/config.el`
```elisp
;;; tools/claude-multi/config.el -*- lexical-binding: t; -*-

;; 1. Define customization group
(defgroup claude-multi nil
  "Manage multiple Claude Code agents."
  :group 'tools
  :prefix "claude-multi-")

;; 2. Define ALL defcustom variables FIRST
(defcustom claude-multi-worktree-location 'adjacent
  "Where to create worktrees."
  :type 'symbol
  :group 'claude-multi)

;; 3. Define global variables
(defvar claude-multi--agents nil
  "List of all active agents.")

;; 4. Load autoload files
(let ((autoload-dir (expand-file-name
                      "autoload"
                      (file-name-directory
                       (file-truename load-file-name)))))
  (add-to-list 'load-path autoload-dir)
  (require 'claude-multi-agents)
  (require 'claude-multi-progress))

;; 5. Configure external packages
(use-package! alert
  :defer t
  :config
  (setq alert-default-style 'osx-notifier))

;; 6. Set up keybindings
(map! :leader
      :prefix ("c m" . "claude-multi")
      :desc "Start session" "s" #'claude-multi/start-session
      :desc "Spawn agent"   "a" #'claude-multi/spawn-agent)

(provide 'claude-multi-config)
```

#### `~/.doom.d/modules/tools/claude-multi/autoload/claude-multi-agents.el`
```elisp
;;; autoload/claude-multi-agents.el -*- lexical-binding: t; -*-

;;;###autoload
(defun claude-multi/start-session ()
  "Start a new multi-agent session."
  (interactive)
  ...)

(defun claude-multi--internal-helper ()
  "Internal helper (not autoloaded)."
  ...)

(provide 'claude-multi-agents)
```

## Common Commands

### Initial Setup
```bash
# Create module structure
mkdir -p ~/.doom.d/modules/tools/claude-multi/autoload

# Add :tools claude-multi to ~/.doom.d/init.el

# Install dependencies
doom sync

# Restart Emacs
```

### Daily Development
```bash
# Edit code - NO doom sync needed for code changes!

# Only run doom sync if packages.el changed:
doom sync  # Only when dependencies change!

# Reload individual functions in Emacs:
# C-x C-e    - Eval last sexp
# SPC c e    - Eval buffer/region
# gr         - Evil eval operator
```

### Full Module Reload (without restart)
```elisp
;; In Emacs:
(progn
  (unload-feature 'claude-multi-agents t)
  (unload-feature 'claude-multi-progress t)
  (load "~/.doom.d/modules/tools/claude-multi/config.el"))
```

## Critical Rules

### ✅ DO
1. Define all `defcustom` in `config.el` BEFORE loading autoload files
2. Use `file-truename` if symlinking modules
3. Use `;;;###autoload` for interactive commands
4. Use `use-package!` (with `!`) for external packages
5. Use `after!` with feature names from `(provide 'name)`
6. Put actual code in `autoload/*.el` files
7. Use `:defer t` for lazy loading

### ❌ DON'T
1. Use `:local-repo` for packages you're actively developing
2. Configure packages in `packages.el` (that's for `config.el`)
3. Use `use-package` without `!` in Doom modules
4. Use `:ensure t` in Doom (straight.el manages packages)
5. Put heavy code in `init.el` (use `config.el`)
6. Forget `file-truename` when using symlinks
7. Expect `after! claude-multi` to work (use feature names)

## Troubleshooting

### Module not loading?
```elisp
;; Check if feature loaded
(featurep 'claude-multi-agents)  ; Should return t

;; Check load-path
load-path  ; Should include autoload directory

;; Reload module
(load "~/.doom.d/modules/tools/claude-multi/config.el")
```

### Variables not defined?
- Make sure `defcustom` is in `config.el` BEFORE `require` statements
- Check that you're not trying to use module variables in `packages.el`

### Keybindings not working?
- Make sure `map!` is in `config.el`, not `init.el`
- Check that commands have `;;;###autoload`

### Changes not taking effect?
- Delete `.elc` files: `find ~/.doom.d/modules/tools/claude-multi -name "*.elc" -delete`
- Reload functions with `C-x C-e` or `SPC c e`
- Only need `doom sync` if `packages.el` changed

## Feature Names vs Package Names

**Critical**: `after!` uses **feature names**, not module paths!

```elisp
;; In autoload/claude-multi-agents.el
(provide 'claude-multi-agents)  ; <-- Feature name

;; In user's config or your config.el
(after! claude-multi-agents     ; <-- Use feature name
  (setq claude-multi-some-var 'custom))

;; NOT this:
(after! claude-multi-config     ; ❌ Wrong!
  ...)
```

## Migration from Current Setup

If you have a symlinked module with issues:

1. **Current Issue**: Using `after!` with wrong feature name
2. **Current Issue**: Variables not accessible at config time
3. **Current Issue**: Need `doom sync` after every change

**Fix**:
```elisp
;; In config.el, define ALL defcustom FIRST:
(defgroup claude-multi nil ...)
(defcustom claude-multi-worktree-location ...)
(defcustom claude-multi-claude-command ...)
;; ... all other defcustoms ...

;; THEN load autoload files:
(require 'claude-multi-agents)
(require 'claude-multi-progress)

;; User configuration uses feature names:
(after! claude-multi-agents  ; Match (provide 'claude-multi-agents)
  (setq ...))
```

## Performance Tips

### Lazy Loading
```elisp
;; Good - only loads when needed
(use-package! websocket
  :defer t
  :commands (websocket-server-start))

;; Bad - loads immediately
(require 'websocket)
```

### Disable Byte-Compilation During Development
```elisp
;; Add to top of files:
; -*- no-byte-compile: t; -*-

;; Or in packages.el:
(package! my-package
  :recipe (:build (:not compile)))
```

## External Project + Symlink Approach

If you want to keep code in external project:

```bash
# Your project structure
/path/to/claude-multi-agent.el/
├── config.el
└── autoload/
    ├── claude-multi-agents.el
    └── ...

# Symlink into Doom
ln -s /path/to/claude-multi-agent.el ~/.doom.d/modules/tools/claude-multi

# Add to init.el
# :tools claude-multi

# Run once
doom sync

# Edit in /path/to/claude-multi-agent.el
# Changes are immediate (no doom sync)
```

## Key Insight

**Module approach is better than `:local-repo` for development because**:

1. No `doom sync` needed for code changes
2. Autoloads work automatically with `;;;###autoload`
3. Proper integration with Doom's module system
4. Can still keep code in external project via symlink
5. User configuration is cleaner (module is already "installed")

`:local-repo` is for *consuming* stable local packages, not *developing* them.

## Reference
See `DOOM-EMACS-LOCAL-PLUGIN-GUIDE.md` for complete documentation.
