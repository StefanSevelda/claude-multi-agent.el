;;; fix-agent-2.el --- Quick fix for agent-2 status issue

;; This script will force a rescan of all pending agents
;; and attempt to match them to status files

(defun claude-multi/fix-agent-2 ()
  "Force rescan of pending agents and update status displays."
  (interactive)
  (message "Forcing rescan of pending agents...")

  ;; Try to rescan pending agents
  (when (fboundp 'claude-multi--rescan-pending-agents)
    (claude-multi--rescan-pending-agents))

  ;; Force update all agent status displays
  (when (boundp 'claude-multi--agents)
    (dolist (agent claude-multi--agents)
      (when (fboundp 'claude-multi--update-agent-status-display)
        (claude-multi--update-agent-status-display agent))))

  (message "Rescan complete. Check if agent-2 status is now populated."))

;; Also provide a function to manually register an agent
(defun claude-multi/manually-match-agent-to-status (agent-name session-id)
  "Manually match AGENT-NAME to SESSION-ID.
This is useful when automatic matching fails."
  (interactive
   (list
    (completing-read "Agent name: "
                     (mapcar #'claude-agent-name claude-multi--agents))
    (completing-read "Session ID: "
                     (directory-files claude-multi-status-directory nil "^status-.*\\.json$"))))

  ;; Extract session ID from filename if needed
  (when (string-match "status-\\(.+\\)\\.json" session-id)
    (setq session-id (match-string 1 session-id)))

  ;; Find the agent
  (let ((agent (cl-find-if
                (lambda (a) (string= (claude-agent-name a) agent-name))
                claude-multi--agents)))
    (if (not agent)
        (message "Agent not found: %s" agent-name)
      ;; Set the session ID
      (setf (claude-agent-session-id agent) session-id)
      ;; Register in mapping
      (puthash session-id agent claude-multi--session-to-agent)
      ;; Remove from pending
      (setq claude-multi--pending-agents
            (delq agent claude-multi--pending-agents))
      ;; Read and update from status file
      (let* ((status-file (expand-file-name
                           (format "status-%s.json" session-id)
                           claude-multi-status-directory))
             (status-data (claude-multi--read-status-file status-file)))
        (when status-data
          (claude-multi--update-agent-from-status agent status-data)))
      (message "Successfully matched %s to session %s" agent-name session-id))))

(provide 'fix-agent-2)
;;; fix-agent-2.el ends here
