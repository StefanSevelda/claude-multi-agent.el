;;; debug-status-tracking.el --- Debug script for status tracking issues

;; Enable status debug logging
(setq claude-multi-status-debug t)

;; Display current agent states
(defun claude-multi/debug-agents ()
  "Display detailed debug information about all agents and their status tracking."
  (interactive)
  (with-output-to-temp-buffer "*claude-multi-debug*"
    (princ "=== CLAUDE MULTI-AGENT DEBUG INFO ===\n\n")

    ;; Check if variables are bound
    (if (not (boundp 'claude-multi--agents))
        (princ "ERROR: claude-multi--agents is not bound. Is the package loaded?\n")

      (princ (format "Total agents: %d\n\n" (length claude-multi--agents)))

      (dolist (agent claude-multi--agents)
        (princ (format "--- Agent: %s ---\n" (claude-agent-name agent)))
        (princ (format "  ID: %s\n" (claude-agent-id agent)))
        (princ (format "  Session ID: %s\n" (or (claude-agent-session-id agent) "NOT SET")))
        (princ (format "  Status: %s\n" (claude-agent-status agent)))
        (princ (format "  Worktree: %s\n" (or (claude-agent-worktree-path agent) "none")))
        (princ (format "  Working Dir: %s\n" (or (claude-agent-working-directory agent) "none")))
        (princ (format "  Normalized Path: %s\n"
                       (claude-multi--normalize-path
                        (or (claude-agent-worktree-path agent)
                            (claude-agent-working-directory agent)
                            default-directory))))
        (princ (format "  Is Pending: %s\n" (if (claude-multi--agent-is-pending-p agent) "YES" "NO")))
        (princ (format "  Has Cached Status: %s\n"
                       (if (claude-multi--get-cached-status agent) "YES" "NO")))
        (princ "\n"))

      (princ "\n=== PENDING AGENTS ===\n")
      (princ (format "Count: %d\n" (length claude-multi--pending-agents)))
      (dolist (agent claude-multi--pending-agents)
        (princ (format "  - %s\n" (claude-agent-name agent))))

      (princ "\n=== SESSION TO AGENT MAPPING ===\n")
      (maphash
       (lambda (session-id agent)
         (princ (format "  %s -> %s\n" session-id (claude-agent-name agent))))
       claude-multi--session-to-agent)

      (princ "\n=== STATUS FILES IN /tmp/claude-status/ ===\n")
      (dolist (file (directory-files claude-multi-status-directory t "^status-.*\\.json$"))
        (let ((status-data (claude-multi--read-status-file file)))
          (when status-data
            (princ (format "\nFile: %s\n" (file-name-nondirectory file)))
            (princ (format "  Session ID: %s\n" (alist-get 'session_id status-data)))
            (princ (format "  CWD: %s\n" (alist-get 'cwd status-data)))
            (princ (format "  Normalized CWD: %s\n"
                           (claude-multi--normalize-path (alist-get 'cwd status-data))))
            (princ (format "  Status: %s\n" (alist-get 'claude_status status-data)))))))))

;; Check status debug buffer
(defun claude-multi/show-status-debug-log ()
  "Show the status debug log buffer."
  (interactive)
  (if (get-buffer "*claude-multi-status-debug*")
      (switch-to-buffer "*claude-multi-status-debug*")
    (message "No status debug log buffer found. Set claude-multi-status-debug to t first.")))

;; Manually trigger rescan
(defun claude-multi/force-rescan-pending ()
  "Force a rescan of pending agents."
  (interactive)
  (claude-multi--rescan-pending-agents)
  (message "Forced rescan of %d pending agent(s)" (length claude-multi--pending-agents)))

(provide 'debug-status-tracking)
;;; debug-status-tracking.el ends here
