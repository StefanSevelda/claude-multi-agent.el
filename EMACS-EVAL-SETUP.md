# Emacs Direct Eval Setup

This allows Claude to execute elisp commands directly in your running Emacs without you needing to copy-paste.

## Setup (One-time)

### 1. Start Emacs Server

Add this to your `~/.doom.d/config.el`:

```elisp
;; Start server if not already running
(unless (server-running-p)
  (server-start))
```

Then restart Emacs or run: `M-x server-start`

### 2. Verify It Works

In your terminal, run:

```bash
emacsclient --eval '(+ 1 2)'
```

Should output: `3`

## Usage

Once setup, Claude can execute commands like this:

```bash
./emacs-eval.sh '(message "Hello from Claude")'
```

Or directly:

```bash
emacsclient --eval '(fboundp '\''claude-multi--start-directory-watcher)'
```

## For Debugging Sessions

When Claude provides elisp to diagnose issues, instead of:

**❌ Old way (manual copy-paste):**
```
Run this in Emacs:
M-: (message "Debug info") RET
```

**✅ New way (automatic):**
```bash
./emacs-eval.sh '(message "Debug info")'
```

Claude can run diagnostics and see results immediately!

## Advanced: Make It Available Everywhere

Add to your `~/.zshrc` or `~/.bashrc`:

```bash
alias emacs-eval='emacsclient --eval'
```

Then from anywhere:

```bash
emacs-eval '(claude-multi--start-directory-watcher)'
```

## Troubleshooting

### "can't find socket"

**Cause:** Emacs server not running

**Fix:** In Emacs run: `M-x server-start`

Or add to config.el as shown above.

### "wrong type argument"

**Cause:** Quoting issues with complex elisp

**Fix:** Use the shell script which handles quoting:

```bash
./emacs-eval.sh '(progn (message "Line 1") (message "Line 2"))'
```

### Want to see what Claude would execute?

```bash
# Dry run mode - shows command without executing
echo './emacs-eval.sh' \"$CLAUDE_ELISP\"
```

## Security Note

This gives Claude ability to execute code in your Emacs. Only use when:
- You trust the code being executed
- You review commands before running
- You're in a safe development environment

Consider adding confirmation:

```bash
#!/bin/bash
# emacs-eval-confirm.sh
echo "Execute: $1"
read -p "Confirm? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    emacsclient --eval "$1"
fi
```
