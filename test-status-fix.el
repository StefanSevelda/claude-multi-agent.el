;;; test-status-fix.el --- Test script for status tracking fix

;; This script tests the agent status tracking fix
;; Run this in your Emacs with: M-x eval-buffer

(defun test-status-fix/reload-and-check ()
  "Reload the status tracking module and check current state."
  (interactive)
  (message "=== Testing Status Fix ===")

  ;; 1. Reload the modified file
  (message "1. Reloading claude-multi-status.el...")
  (load-file "/Users/stefansevelda/projects/claude-multi-agent.el/autoload/claude-multi-status.el")
  (message "   ✓ Reloaded successfully")

  ;; 2. Enable debug mode
  (message "2. Enabling debug mode...")
  (setq claude-multi-status-debug t)
  (message "   ✓ Debug mode enabled")

  ;; 3. Show current state
  (message "3. Current state:")
  (message "   - Watcher running: %s"
           (if (boundp 'claude-multi--directory-watcher)
               (if claude-multi--directory-watcher "YES" "NO")
             "NOT BOUND"))
  (message "   - Total agents: %d"
           (if (boundp 'claude-multi--agents)
               (length claude-multi--agents)
             0))
  (message "   - Pending agents: %d"
           (if (boundp 'claude-multi--pending-agents)
               (length claude-multi--pending-agents)
             0))

  ;; 4. Show status files
  (let ((status-dir "/tmp/claude-status/"))
    (if (file-exists-p status-dir)
        (let ((files (directory-files status-dir nil "^status-.*\\.json$")))
          (message "   - Status files: %d" (length files))
          (dolist (file files)
            (message "     • %s" file)))
      (message "   - Status directory doesn't exist: %s" status-dir)))

  (message "\n4. Opening diagnostic buffer...")
  (claude-multi/debug-status-matching)
  (message "   ✓ Check the *Claude Status Diagnostics* buffer")

  (message "\n=== Next Steps ===")
  (message "1. Check *Claude Status Diagnostics* buffer for details")
  (message "2. If agents are still pending, run: (test-status-fix/force-rematch)")
  (message "3. Check *claude-multi-status-debug* buffer for matching logs"))

(defun test-status-fix/force-rematch ()
  "Force all agents to re-register and attempt matching."
  (interactive)
  (message "=== Force Re-matching Agents ===")

  (when (boundp 'claude-multi--agents)
    (message "Re-registering %d agent(s)..." (length claude-multi--agents))

    ;; Clear pending list first to avoid duplicates
    (when (boundp 'claude-multi--pending-agents)
      (setq claude-multi--pending-agents nil))

    ;; Re-register each agent
    (dolist (agent claude-multi--agents)
      (message "  Re-registering: %s" (claude-agent-name agent))
      (claude-multi--register-agent-for-status agent))

    (message "✓ Re-registration complete")
    (message "\nWait 1-2 seconds, then run: (test-status-fix/check-results)")))

(defun test-status-fix/check-results ()
  "Check if agents successfully matched to status files."
  (interactive)
  (message "=== Checking Match Results ===")

  (let ((matched 0)
        (pending 0)
        (total 0))

    (when (boundp 'claude-multi--agents)
      (setq total (length claude-multi--agents))
      (dolist (agent claude-multi--agents)
        (if (claude-agent-session-id agent)
            (progn
              (setq matched (1+ matched))
              (message "✓ %s - MATCHED (session: %s)"
                       (claude-agent-name agent)
                       (claude-agent-session-id agent)))
          (setq pending (1+ pending))
          (message "✗ %s - PENDING"
                   (claude-agent-name agent)))))

    (message "\n=== Summary ===")
    (message "Total agents: %d" total)
    (message "Matched: %d" matched)
    (message "Pending: %d" pending)

    (if (> pending 0)
        (progn
          (message "\n⚠ Some agents still pending!")
          (message "Check *claude-multi-status-debug* buffer for details")
          (message "Diagnostic buffer will show path comparison details"))
      (message "\n✓ All agents matched successfully!"))

    ;; Show diagnostic buffer
    (claude-multi/debug-status-matching)))

(defun test-status-fix/show-debug-log ()
  "Display the debug log buffer."
  (interactive)
  (if (get-buffer "*claude-multi-status-debug*")
      (switch-to-buffer "*claude-multi-status-debug*")
    (message "No debug log buffer - make sure debug mode is enabled: (setq claude-multi-status-debug t)")))

;; Main entry point
(defun test-status-fix ()
  "Main test function - run this to test the status fix."
  (interactive)
  (test-status-fix/reload-and-check))

(message "Test script loaded!")
(message "Run: M-x test-status-fix")
(message "Or: (test-status-fix)")

(provide 'test-status-fix)
