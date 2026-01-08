;; -*- lexical-binding: t; -*-
;;; diagnose.el --- Diagnostic tool for claude-multi-agent

;;; Commentary:
;; Run this to diagnose issues with progress buffer sync
;; Usage: M-x eval-buffer in this file, then M-x claude-multi-diagnose

;;; Code:

(defun claude-multi-diagnose ()
  "Diagnose claude-multi-agent configuration and state."
  (interactive)
  (let ((buf (get-buffer-create "*Claude Multi-Agent Diagnostics*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== Claude Multi-Agent Diagnostics ===\n\n")

      ;; Check if modules are loaded
      (insert "** Module Status **\n")
      (insert (format "- config loaded: %s\n"
                     (if (featurep 'claude-multi-config) "✓ YES" "✗ NO")))
      (insert (format "- agents loaded: %s\n"
                     (if (featurep 'claude-multi-agents) "✓ YES" "✗ NO")))
      (insert (format "- progress loaded: %s\n"
                     (if (featurep 'claude-multi-progress) "✓ YES" "✗ NO")))
      (insert "\n")

      ;; Check critical variables
      (insert "** Variable Status **\n")
      (insert (format "- claude-multi-use-org-tags: %s\n"
                     (if (boundp 'claude-multi-use-org-tags)
                         (format "✓ %s" claude-multi-use-org-tags)
                       "✗ NOT DEFINED")))
      (insert (format "- claude-multi--progress-buffer: %s\n"
                     (if (and (boundp 'claude-multi--progress-buffer)
                              claude-multi--progress-buffer
                              (buffer-live-p claude-multi--progress-buffer))
                         (format "✓ %s" (buffer-name claude-multi--progress-buffer))
                       "✗ NO BUFFER")))
      (insert (format "- claude-multi--session-start-time: %s\n"
                     (if (and (boundp 'claude-multi--session-start-time)
                              claude-multi--session-start-time)
                         "✓ SET"
                       "✗ NOT SET")))
      (insert (format "- claude-multi--agents: %s\n"
                     (if (boundp 'claude-multi--agents)
                         (format "✓ %d agents" (length claude-multi--agents))
                       "✗ NOT DEFINED")))
      (insert "\n")

      ;; Check functions
      (insert "** Function Status **\n")
      (insert (format "- claude-multi--add-agent-section: %s\n"
                     (if (fboundp 'claude-multi--add-agent-section) "✓ YES" "✗ NO")))
      (insert (format "- claude-multi--update-session-stats: %s\n"
                     (if (fboundp 'claude-multi--update-session-stats) "✓ YES" "✗ NO")))
      (insert (format "- claude-multi--init-progress-buffer: %s\n"
                     (if (fboundp 'claude-multi--init-progress-buffer) "✓ YES" "✗ NO")))
      (insert "\n")

      ;; Check progress buffer content if it exists
      (when (and (boundp 'claude-multi--progress-buffer)
                 claude-multi--progress-buffer
                 (buffer-live-p claude-multi--progress-buffer))
        (insert "** Progress Buffer Content **\n")
        (insert (format "Buffer: %s\n" (buffer-name claude-multi--progress-buffer)))
        (insert (format "Size: %d bytes\n"
                       (with-current-buffer claude-multi--progress-buffer
                         (buffer-size))))
        (insert "First 500 chars:\n")
        (insert "```\n")
        (insert (with-current-buffer claude-multi--progress-buffer
                  (substring (buffer-string) 0 (min 500 (buffer-size)))))
        (insert "\n```\n\n"))

      ;; List agents if any
      (when (and (boundp 'claude-multi--agents) claude-multi--agents)
        (insert "** Active Agents **\n")
        (dolist (agent claude-multi--agents)
          (insert (format "- %s: status=%s, task=%s\n"
                         (claude-agent-id agent)
                         (claude-agent-status agent)
                         (claude-agent-task-description agent)))))

      (insert "\n** Recommendations **\n")
      (cond
       ((not (featurep 'claude-multi-config))
        (insert "⚠ Config not loaded. Run: M-x load-file RET config.el RET\n"))
       ((not (boundp 'claude-multi-use-org-tags))
        (insert "⚠ claude-multi-use-org-tags not defined. Old version loaded?\n")
        (insert "  Run: M-x load-file RET config.el RET\n"))
       ((not (and (boundp 'claude-multi--progress-buffer)
                  claude-multi--progress-buffer
                  (buffer-live-p claude-multi--progress-buffer)))
        (insert "⚠ Progress buffer not initialized.\n")
        (insert "  Run: M-x claude-multi/start-session\n"))
       (t
        (insert "✓ Everything looks good!\n")
        (insert "  Try: M-x claude-multi/spawn-agent\n")))

      (goto-char (point-min))
      (display-buffer buf))))

(provide 'diagnose)
;;; diagnose.el ends here
