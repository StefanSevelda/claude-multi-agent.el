;;; quick-diagnose.el --- Quick diagnostic for claude-multi-agent issues

;; Run this with: emacs -Q -l quick-diagnose.el
;; Or in your Emacs: M-x eval-buffer, then check *Messages* buffer

(defun quick-diagnose ()
  "Quick diagnostic check for claude-multi-agent."
  (message "=== Claude Multi-Agent Quick Diagnostic ===\n")

  ;; 1. Check if module is loaded
  (message "1. Module loaded: %s"
           (if (featurep 'claude-multi-config) "YES" "NO"))

  ;; 2. Check if commands are available
  (message "2. Commands available:")
  (message "   - claude-multi/start-session: %s"
           (if (fboundp 'claude-multi/start-session) "YES" "NO"))
  (message "   - claude-multi/spawn-agent: %s"
           (if (fboundp 'claude-multi/spawn-agent) "YES" "NO"))

  ;; 3. Check critical variables
  (message "3. Variables defined:")
  (message "   - claude-multi--agents: %s"
           (if (boundp 'claude-multi--agents) "YES" "NO"))
  (message "   - claude-multi-worktree-location: %s"
           (if (boundp 'claude-multi-worktree-location)
               (format "YES (%s)" claude-multi-worktree-location)
             "NO"))

  ;; 4. Check dependencies
  (message "4. Dependencies:")
  (message "   - kitty available: %s"
           (if (executable-find "kitty") "YES" "NO"))
  (message "   - alert package: %s"
           (if (require 'alert nil t) "YES" "NO"))
  (message "   - org-mode: %s"
           (if (require 'org nil t) "YES" "NO"))

  ;; 5. Check environment
  (message "5. Environment:")
  (message "   - KITTY_LISTEN_ON: %s"
           (or (getenv "KITTY_LISTEN_ON") "NOT SET"))

  ;; 6. Try to load if not loaded
  (unless (featurep 'claude-multi-config)
    (message "\n⚠ Module not loaded. Trying to load from current directory...")
    (let ((config-file (expand-file-name "config.el" default-directory)))
      (if (file-exists-p config-file)
          (progn
            (message "   Found: %s" config-file)
            (message "   Loading...")
            (condition-case err
                (progn
                  (load-file config-file)
                  (message "   ✓ Loaded successfully!"))
              (error (message "   ✗ Load failed: %S" err))))
        (message "   ✗ config.el not found in current directory"))))

  (message "\n=== End Diagnostic ==="))

;; Auto-run when evaluated
(quick-diagnose)

(provide 'quick-diagnose)
