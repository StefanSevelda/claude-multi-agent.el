;;; fix-stale-sessions.el --- Fix for stale session ID mapping

(defun claude-multi/remap-agents-to-fresh-sessions ()
  "Remap all agents to the most recent matching status files.
This fixes the issue where agents are stuck with stale session IDs."
  (interactive)
  (let ((remapped-count 0))
    (message "Remapping agents to fresh status files...")

    (dolist (agent claude-multi--agents)
      (let* ((agent-path (or (claude-agent-worktree-path agent)
                             (claude-agent-working-directory agent)
                             default-directory))
             (normalized-agent-path (claude-multi--normalize-path agent-path))
             (old-session-id (claude-agent-session-id agent))
             (best-match nil)
             (best-match-time 0))

        ;; Find the most recent status file matching this agent's path
        (dolist (file (directory-files claude-multi-status-directory t "^status-.*\\.json$"))
          (let ((status-data (claude-multi--read-status-file file)))
            (when status-data
              (let ((cwd (alist-get 'cwd status-data))
                    (file-mtime (float-time (nth 5 (file-attributes file)))))
                (when (and cwd
                           (string= (claude-multi--normalize-path cwd)
                                    normalized-agent-path)
                           (> file-mtime best-match-time))
                  (setq best-match file
                        best-match-time file-mtime))))))

        ;; If we found a newer match, remap the agent
        (when best-match
          (let* ((status-data (claude-multi--read-status-file best-match))
                 (new-session-id (alist-get 'session_id status-data)))
            (when (and new-session-id (not (string= old-session-id new-session-id)))
              ;; Remove old mapping
              (when old-session-id
                (remhash old-session-id claude-multi--session-to-agent))

              ;; Set new session ID
              (setf (claude-agent-session-id agent) new-session-id)

              ;; Add new mapping
              (puthash new-session-id agent claude-multi--session-to-agent)

              ;; Update cache
              (puthash new-session-id status-data claude-multi--status-cache)

              ;; Update agent from new status
              (claude-multi--update-agent-from-status agent status-data)

              ;; Update display
              (when (fboundp 'claude-multi--update-agent-status-display)
                (claude-multi--update-agent-status-display agent))

              (message "Remapped %s: %s -> %s"
                       (claude-agent-name agent)
                       (if old-session-id (substring old-session-id 0 8) "none")
                       (substring new-session-id 0 8))
              (cl-incf remapped-count))))))

    (if (> remapped-count 0)
        (message "Successfully remapped %d agent(s) to fresh sessions" remapped-count)
      (message "No agents needed remapping"))))

(defun claude-multi/clean-stale-status-files (&optional max-age-hours)
  "Clean status files older than MAX-AGE-HOURS (default: 1 hour).
This removes stale status files that might interfere with agent matching."
  (interactive "P")
  (let ((max-age-hours (or max-age-hours 1))
        (now (float-time))
        (cleaned-count 0))

    (dolist (file (directory-files claude-multi-status-directory t "^status-.*\\.json$"))
      (let* ((file-mtime (float-time (nth 5 (file-attributes file))))
             (age-hours (/ (- now file-mtime) 3600.0)))
        (when (> age-hours max-age-hours)
          (delete-file file)
          (cl-incf cleaned-count)
          (message "Deleted stale status file: %s (%.1f hours old)"
                   (file-name-nondirectory file) age-hours))))

    (message "Cleaned up %d stale status file%s"
             cleaned-count
             (if (= cleaned-count 1) "" "s"))))

(defun claude-multi/force-refresh-agent-status (agent-name)
  "Force refresh status display for a specific agent."
  (interactive
   (list (completing-read "Agent name: "
                          (mapcar #'claude-agent-name claude-multi--agents))))

  (let ((agent (cl-find-if
                (lambda (a) (string= (claude-agent-name a) agent-name))
                claude-multi--agents)))
    (if (not agent)
        (message "Agent not found: %s" agent-name)
      (let* ((session-id (claude-agent-session-id agent))
             (status-file (when session-id
                           (expand-file-name
                            (format "status-%s.json" session-id)
                            claude-multi-status-directory))))
        (if (not status-file)
            (message "No session ID for agent: %s" agent-name)
          (if (not (file-exists-p status-file))
              (message "Status file doesn't exist: %s" status-file)
            (let ((status-data (claude-multi--read-status-file status-file)))
              (when status-data
                ;; Update cache
                (puthash session-id status-data claude-multi--status-cache)
                ;; Update agent
                (claude-multi--update-agent-from-status agent status-data)
                ;; Update display
                (when (fboundp 'claude-multi--update-agent-status-display)
                  (claude-multi--update-agent-status-display agent))
                (message "Refreshed status for %s" agent-name)))))))))

(provide 'fix-stale-sessions)
;;; fix-stale-sessions.el ends here
