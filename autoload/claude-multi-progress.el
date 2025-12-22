;; -*- lexical-binding: t; -*-
;;; autoload/progress.el --- Org-mode progress tracking for Claude Multi-Agent

;;; Commentary:
;; Centralized org-mode-based progress tracking for all agents

;;; Code:

(require 's)
(require 'f)
(require 'org)

;;; Throttling variables

(defvar claude-multi--last-update-time nil
  "Hash table tracking last update time for each agent.")

(defvar claude-multi--pending-updates nil
  "Hash table of pending output updates for each agent.")

(defvar claude-multi--update-timer nil
  "Timer for processing pending updates.")

;;; Progress buffer initialization

;;;###autoload
(defun claude-multi--init-progress-buffer ()
  "Initialize the progress buffer with session header."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: Claude Multi-Agent Session\n"))
        (insert (format "#+AUTHOR: %s\n" (user-full-name)))
        (insert (format "#+DATE: %s\n\n"
                       (format-time-string "%Y-%m-%d %H:%M:%S"
                                         claude-multi--session-start-time)))
        (insert (format "* Session Info\n\n"))
        (insert (format "- Started :: %s\n"
                       (format-time-string "%Y-%m-%d %H:%M:%S"
                                         claude-multi--session-start-time)))
        (insert (format "- Working Directory :: =%s=\n" default-directory))
        (insert (format "- Session Window ID :: %s\n"
                       (or claude-multi--current-session-window-id "Not created yet")))
        (insert (format "- Stats :: %d total | %d running | %d waiting | %d completed | %d failed\n\n"
                       0 0 0 0 0))
        (insert (format "* Agents\n\n"))))))

;;; Agent section management

;;;###autoload
(defun claude-multi--add-agent-section (agent)
  "Add a new section for AGENT to the progress buffer with ultra-compact format."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let* ((inhibit-read-only t)
             (task-desc (claude-agent-task-description agent))
             (truncated-task (if (> (length task-desc) 40)
                                 (concat (substring task-desc 0 37) "...")
                               task-desc))
             (tags (when claude-multi-use-org-tags
                     (claude-multi--agent-tags-from-status (claude-agent-status agent))))
             (business-context (claude-multi--extract-business-context agent))
             (agent-dir (or (claude-agent-worktree-path agent)
                           (claude-agent-working-directory agent)
                           default-directory))
             (status-file (expand-file-name "status.json" agent-dir))
             (ctx-pct nil)
             (file-count nil)
             (duration nil))

        ;; Try to extract info from status.json if available
        (when (file-exists-p status-file)
          (condition-case nil
              (let* ((json-object-type 'plist)
                     (json-array-type 'list)
                     (json-key-type 'keyword)
                     (data (json-read-file status-file))
                     (context (plist-get data :context_window))
                     (git-info (plist-get data :git)))
                (when context
                  (setq ctx-pct (plist-get context :percentage_used)))
                (when git-info
                  (let ((changed-files (plist-get git-info :changed_files)))
                    (when changed-files
                      (setq file-count (length changed-files))))))
            (error nil)))

        ;; Calculate duration
        (setq duration (claude-multi--format-duration (claude-agent-created-at agent)
                                                      (claude-agent-completed-at agent)))

        (goto-char (point-max))

        ;; Ultra-compact headline format:
        ;; ** EMOJI AGENT-ID | DOMAIN | TASK | DURATION | CTX% | FILES | :tags:
        (insert (format "\n** %s "
                       (claude-multi--get-status-icon (claude-agent-status agent))))

        ;; Insert agent ID with color matching kitty tab
        (let ((agent-id-start (point)))
          (insert (claude-agent-id agent))
          (add-text-properties agent-id-start (point)
                               `(face (:foreground ,(claude-agent-color agent) :weight bold))))

        ;; Add business domains if available (extract from business-context)
        (when business-context
          ;; Extract just the domain part (before the pipe)
          (let ((domain-part (if (string-match "💼 \\([^|]+\\)" business-context)
                                 (match-string 1 business-context)
                               nil)))
            (when domain-part
              (insert (format " | %s" (string-trim domain-part))))))

        ;; Add task description
        (insert (format " | %s" truncated-task))

        ;; Add duration
        (insert (format " | %s" duration))

        ;; Add context percentage if available
        (when ctx-pct
          (insert (format " | %.0f%% ctx" ctx-pct)))

        ;; Add file count if available
        (when file-count
          (insert (format " | %d files" file-count)))

        ;; Add org tags if enabled
        (when tags
          (insert (format " :%s:" (mapconcat #'identity tags ":"))))

        (insert "\n")

        ;; STATUS drawer - contains all details (will be shown by default)
        (insert "   :STATUS:\n")
        (insert (format "   <!-- status-marker-%s -->\n" (claude-agent-id agent)))
        (insert "   /Waiting for status update.../\n")
        (insert "   :END:\n\n")

        ;; Show the STATUS drawer by default (org-mode collapses drawers by default)
        (save-excursion
          (forward-line -5)  ; Go back to the ** headline
          (claude-multi--show-subtree-safe))))))

(defun claude-multi--show-subtree-safe ()
  "Safely show org subtree, with fallback if org-mode not available."
  (when (fboundp 'org-show-subtree)
    (condition-case nil
        (org-show-subtree)
      (error nil))))

;;; Output appending

;;;###autoload
(defun claude-multi--append-agent-output (agent output)
  "Append OUTPUT from AGENT to its section in the progress buffer.
Uses throttling to reduce flashing based on `claude-multi-output-throttle-delay'."
  (unless claude-multi--last-update-time
    (setq claude-multi--last-update-time (make-hash-table :test 'equal)))
  (unless claude-multi--pending-updates
    (setq claude-multi--pending-updates (make-hash-table :test 'equal)))

  (let* ((agent-id (claude-agent-id agent))
         (now (float-time))
         (last-update (gethash agent-id claude-multi--last-update-time 0))
         (delay claude-multi-output-throttle-delay))

    ;; Store the pending output
    (puthash agent-id output claude-multi--pending-updates)

    ;; If throttling is disabled or enough time has passed, update immediately
    (when (or (= delay 0) (> (- now last-update) delay))
      (claude-multi--do-append-agent-output agent output)
      (puthash agent-id now claude-multi--last-update-time)
      (remhash agent-id claude-multi--pending-updates))

    ;; Otherwise, schedule a delayed update if not already scheduled
    (when (and (> delay 0)
               (<= (- now last-update) delay)
               (not claude-multi--update-timer))
      (setq claude-multi--update-timer
            (run-with-timer delay nil
                           #'claude-multi--process-pending-updates)))))

(defun claude-multi--do-append-agent-output (agent output)
  "Actually append OUTPUT from AGENT to progress buffer without throttling."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t)
            (marker-pos (claude-multi--find-agent-marker agent)))
        (when marker-pos
          (goto-char marker-pos)
          ;; Insert before the marker
          (forward-line -1)
          (end-of-line)
          (insert "\n")
          (insert (format "- [%s] %s"
                         (format-time-string "%H:%M:%S")
                         (string-trim output)))
          ;; Color code based on content
          (when (string-match-p "error\\|fail" (downcase output))
            (add-text-properties (line-beginning-position) (line-end-position)
                                '(face (:foreground "red"))))
          (when (string-match-p "success\\|complete" (downcase output))
            (add-text-properties (line-beginning-position) (line-end-position)
                                '(face (:foreground "green"))))
          (when (claude-multi--detect-input-request output)
            (add-text-properties (line-beginning-position) (line-end-position)
                                '(face (:foreground "yellow" :weight bold)))))))))

(defun claude-multi--process-pending-updates ()
  "Process any pending output updates for all agents."
  (setq claude-multi--update-timer nil)
  (when claude-multi--pending-updates
    (maphash
     (lambda (agent-id output)
       (let ((agent (claude-multi--get-agent-by-id agent-id)))
         (when agent
           (claude-multi--do-append-agent-output agent output)
           (puthash agent-id (float-time) claude-multi--last-update-time))))
     claude-multi--pending-updates)
    (clrhash claude-multi--pending-updates)))

;;;###autoload
(defun claude-multi--update-agent-status (agent)
  "Update the status of AGENT in the progress buffer with ultra-compact format."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let* ((inhibit-read-only t)
             (task-desc (claude-agent-task-description agent))
             (truncated-task (if (> (length task-desc) 40)
                                 (concat (substring task-desc 0 37) "...")
                               task-desc))
             (tags (when claude-multi-use-org-tags
                     (claude-multi--agent-tags-from-status (claude-agent-status agent))))
             (business-context (claude-multi--extract-business-context agent))
             (agent-dir (or (claude-agent-worktree-path agent)
                           (claude-agent-working-directory agent)
                           default-directory))
             (status-file (expand-file-name "status.json" agent-dir))
             (ctx-pct nil)
             (file-count nil)
             (duration nil))

        ;; Try to extract info from status.json if available
        (when (file-exists-p status-file)
          (condition-case nil
              (let* ((json-object-type 'plist)
                     (json-array-type 'list)
                     (json-key-type 'keyword)
                     (data (json-read-file status-file))
                     (context (plist-get data :context_window))
                     (git-info (plist-get data :git)))
                (when context
                  (setq ctx-pct (plist-get context :percentage_used)))
                (when git-info
                  (let ((changed-files (plist-get git-info :changed_files)))
                    (when changed-files
                      (setq file-count (length changed-files))))))
            (error nil)))

        ;; Calculate duration
        (setq duration (claude-multi--format-duration (claude-agent-created-at agent)
                                                      (claude-agent-completed-at agent)))

        (save-excursion
          (goto-char (point-min))
          ;; Find the agent section by searching for the agent ID
          (when (re-search-forward
                 (format "^\\*\\* .* %s" (regexp-quote (claude-agent-id agent)))
                 nil t)
            (beginning-of-line)
            ;; Update the entire headline with all compact info
            (kill-line)

            ;; Ultra-compact headline format
            (insert (format "** %s "
                           (claude-multi--get-status-icon (claude-agent-status agent))))

            ;; Insert agent ID with color matching kitty tab
            (let ((agent-id-start (point)))
              (insert (claude-agent-id agent))
              (add-text-properties agent-id-start (point)
                                   `(face (:foreground ,(claude-agent-color agent) :weight bold))))

            ;; Add business domains if available
            (when business-context
              (let ((domain-part (if (string-match "💼 \\([^|]+\\)" business-context)
                                     (match-string 1 business-context)
                                   nil)))
                (when domain-part
                  (insert (format " | %s" (string-trim domain-part))))))

            ;; Add task description
            (insert (format " | %s" truncated-task))

            ;; Add duration
            (insert (format " | %s" duration))

            ;; Add context percentage if available
            (when ctx-pct
              (insert (format " | %.0f%% ctx" ctx-pct)))

            ;; Add file count if available
            (when file-count
              (insert (format " | %d files" file-count)))

            ;; Add org tags if enabled
            (when tags
              (insert (format " :%s:" (mapconcat #'identity tags ":"))))))

        ;; Update session stats
        (claude-multi--update-session-stats)))))

;;;###autoload
(defun claude-multi--remove-agent-section (agent)
  "Remove AGENT's section from the progress buffer."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward
                 (format "^\\*\\* .* %s$" (regexp-quote (claude-agent-name agent)))
                 nil t)
            (let ((section-start (line-beginning-position)))
              ;; Find the end of this section (next ** or end of buffer)
              (forward-line 1)
              (if (re-search-forward "^\\*\\* " nil t)
                  (beginning-of-line)
                (goto-char (point-max)))
              (delete-region section-start (point)))))
        (claude-multi--update-session-stats)))))

;;; Session stats management

(defun claude-multi--update-session-stats ()
  "Update the session statistics in the progress buffer."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t)
            (total 0)
            (running 0)
            (waiting 0)
            (completed 0)
            (failed 0))
        ;; Count agents by status
        (dolist (agent claude-multi--agents)
          (cl-incf total)
          (pcase (claude-agent-status agent)
            ('running (cl-incf running))
            ('waiting-input (cl-incf waiting))
            ('completed (cl-incf completed))
            ('failed (cl-incf failed))))

        ;; Update the stats line
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^- Stats :: " nil t)
            (beginning-of-line)
            (kill-line)
            (insert (format "- Stats :: %d total | %d running | %d waiting | %d completed | %d failed"
                           total running waiting completed failed))))))))

;;; Helper functions

;;;###autoload
(defun claude-multi--get-status-icon (status)
  "Get an emoji icon for the given STATUS."
  (pcase status
    ('running "🟢")
    ('waiting-input "🟡")
    ('completed "🔵")
    ('failed "🔴")
    ('pending "⚪")
    (_ "❓")))

(defun claude-multi--agent-tags-from-status (status)
  "Convert agent STATUS to org-mode tag strings.
Returns a list of tag strings appropriate for the status."
  (pcase status
    ('running '("running"))
    ('waiting-input '("waiting" "input"))
    ('completed '("completed"))
    ('failed '("failed"))
    ('pending '("pending"))
    (_ '())))

(defun claude-multi--extract-business-context (agent)
  "Extract business context from AGENT's status.json for display in headline.
Returns a string like '💼 api | fixing issue' or nil if not available."
  (let* ((agent-dir (or (claude-agent-worktree-path agent)
                       (claude-agent-working-directory agent)
                       default-directory))
         (status-file (expand-file-name "status.json" agent-dir)))
    (when (file-exists-p status-file)
      (condition-case nil
          (let* ((json-object-type 'plist)
                 (json-array-type 'list)
                 (json-key-type 'keyword)
                 (data (json-read-file status-file))
                 (business-ctx (plist-get data :business_context)))
            (when business-ctx
              (let ((domains (plist-get business-ctx :technical_domains))
                    (objective (plist-get business-ctx :objective)))
                (when (or domains objective)
                  (concat "💼 "
                         (when domains
                           (mapconcat #'identity domains ", "))
                         (when (and domains objective) " | ")
                         (when objective
                           (if (> (length objective) 40)
                               (concat (substring objective 0 37) "...")
                             objective)))))))
        (error nil)))))

(defun claude-multi--insert-agent-marker (agent)
  "Insert a marker at the end of AGENT's progress section.
This marker is used to find where to append new output."
  (let ((marker-text (format "<!-- agent-marker-%s -->" (claude-agent-id agent))))
    (insert marker-text)))

(defun claude-multi--find-agent-marker (agent)
  "Find the position of AGENT's marker in the progress buffer.
Returns the position or nil if not found."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward
               (format "<!-- agent-marker-%s -->" (regexp-quote (claude-agent-id agent)))
               nil t)
          (point))))))

;;;###autoload
(defun claude-multi--highlight-input-requests ()
  "Highlight all input request lines in the progress buffer."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "🟡\\|WAITING FOR INPUT" nil t)
            (add-text-properties (line-beginning-position) (line-end-position)
                                '(face (:foreground "yellow" :weight bold)))))))))

;;; Auto-scroll functionality

(defun claude-multi--auto-scroll-progress ()
  "Auto-scroll the progress buffer to show latest updates."
  (when (buffer-live-p claude-multi--progress-buffer)
    (let ((windows (get-buffer-window-list claude-multi--progress-buffer nil t)))
      (dolist (window windows)
        (with-selected-window window
          (goto-char (point-max))
          (recenter -3))))))

;;; Export functionality

;;;###autoload
(defun claude-multi/export-progress ()
  "Export the progress buffer to an org file."
  (interactive)
  (when (buffer-live-p claude-multi--progress-buffer)
    (let ((filename (read-file-name "Export to: "
                                    nil
                                    (format "claude-session-%s.org"
                                           (format-time-string "%Y%m%d-%H%M%S")))))
      (with-current-buffer claude-multi--progress-buffer
        (write-region (point-min) (point-max) filename))
      (message "Progress exported to %s" filename))))

;;; Per-agent status summary integration

(require 'json)

(defvar claude-multi--status-file-watches nil
  "Hash table mapping agent IDs to file watch descriptors.")

(defvar claude-multi--status-watch-upgraded nil
  "Hash table tracking which watches have been upgraded from directory to file.")

(defvar claude-multi--status-update-in-progress nil
  "Guard to prevent recursive status updates.")

(defun claude-multi--parse-status-json (json-file)
  "Parse status.json file and return formatted org-mode content."
  (when (file-exists-p json-file)
    (condition-case err
        (let* ((json-object-type 'plist)
               (json-array-type 'list)
               (json-key-type 'keyword)
               (data (json-read-file json-file))
               (status (plist-get data :status))
               (timestamp (plist-get data :timestamp))
               (session-started (plist-get data :session_started))
               (waiting (plist-get data :waiting_for_input))
               (activity (plist-get data :current_activity))
               (changes (plist-get data :changes))
               (question (plist-get data :question))
               (context (plist-get data :context_window))
               (git-info (plist-get data :git))
               (business-ctx (plist-get data :business_context)))

          ;; Build formatted output
          (with-temp-buffer
            ;; Status indicator
            (if waiting
                (insert "#+BEGIN_WARNING\n⏸ *WAITING FOR INPUT*\n#+END_WARNING\n\n")
              (insert (format "- Status :: %s\n\n" (or status "Working..."))))

            ;; Show question prominently if waiting for input
            (when (and waiting question)
              (insert "*Question from Agent:*\n\n")
              (insert "#+BEGIN_QUOTE\n")
              (insert (format "%s\n" question))
              (insert "#+END_QUOTE\n\n"))

            ;; Business context (technical domains, objective, JIRA tickets)
            (when business-ctx
              (let ((domains (plist-get business-ctx :technical_domains))
                    (objective (plist-get business-ctx :objective))
                    (confidence (plist-get business-ctx :confidence))
                    (method (plist-get business-ctx :extraction_method))
                    (jira (plist-get business-ctx :jira_tickets)))
                (insert "*Business Context*\n\n")
                (when domains
                  (insert "- Domains :: ")
                  (insert (mapconcat (lambda (d) (format "=%s=" d)) domains ", "))
                  (insert "\n"))
                (when objective
                  (insert (format "- Objective :: %s\n" objective)))
                (when (and jira (> (length jira) 0))
                  (insert "- JIRA :: ")
                  (insert (mapconcat (lambda (ticket) (format "[[%s]]" ticket)) jira ", "))
                  (insert "\n"))
                (when (and confidence (< confidence 0.8))
                  (insert (format "- Confidence :: %.0f%% /%s/\n" (* confidence 100) method)))
                (insert "\n")))

            ;; Context window usage
            (when context
              (let* ((used (plist-get context :tokens_used))
                     (total (plist-get context :tokens_total))
                     (pct (plist-get context :percentage_used))
                     (remaining (plist-get context :tokens_remaining)))
                (insert "*Context Window*\n\n")
                (insert (format "- Usage :: %d / %d tokens (%.1f%%)\n"
                               used total pct))
                (insert (format "- Remaining :: %s tokens\n"
                               (claude-multi--format-number remaining)))
                ;; Visual progress bar
                (let ((bar-width 40)
                      (filled (round (* bar-width (/ pct 100.0)))))
                  (insert "- Progress :: [")
                  (insert (make-string filled ?█))
                  (insert (make-string (- bar-width filled) ?░))
                  (insert "]\n"))
                (insert "\n")))

            ;; Git information
            (when git-info
              (let ((branch (plist-get git-info :branch))
                    (repo (plist-get git-info :repository))
                    (changed-files (plist-get git-info :changed_files))
                    (has-changes (plist-get git-info :has_changes))
                    (ahead (plist-get git-info :commits_ahead))
                    (behind (plist-get git-info :commits_behind)))
                (insert "*Git Status*\n\n")
                (when repo
                  (insert (format "- Repository :: %s\n" repo)))
                (when branch
                  (insert (format "- Branch :: =%s=" branch))
                  (when (or (and ahead (> ahead 0)) (and behind (> behind 0)))
                    (insert " (")
                    (when (and ahead (> ahead 0))
                      (insert (format "↑%d" ahead)))
                    (when (and ahead (> ahead 0) behind (> behind 0))
                      (insert " "))
                    (when (and behind (> behind 0))
                      (insert (format "↓%d" behind)))
                    (insert ")"))
                  (insert "\n"))
                (when (and has-changes changed-files)
                  (insert (format "- Changed Files :: %d\n" (length changed-files)))
                  (insert "\n")
                  (dolist (file-info changed-files)
                    (let ((file (plist-get file-info :file))
                          (status (plist-get file-info :status)))
                      (insert (format "  - =%s= ~%s~\n"
                                    (claude-multi--git-status-icon status)
                                    file)))))
                (insert "\n")))

            ;; Current activity section
            (when activity
              (let ((goal (plist-get activity :goal))
                    (waiting-activity (plist-get activity :waiting)))
                (insert "*Current Activity*\n\n")
                (when goal
                  (insert (format "- Goal :: %s\n" goal)))
                (when waiting-activity
                  (insert "- Status :: Waiting\n"))
                (insert "\n")))

            ;; Recent changes section
            (when changes
              (let ((recent (plist-get changes :recent))
                    (total (plist-get changes :total_count)))
                (when recent
                  (insert "*Recent Changes*\n\n")
                  (dolist (change recent)
                    (insert (format "- %s\n" change)))
                  (insert "\n")
                  (when (and total (> total (length recent)))
                    (insert (format "#+BEGIN_CENTER\n/%d total changes this session/\n#+END_CENTER\n\n" total))))))

            ;; Session info footer
            (insert "#+BEGIN_CENTER\n")
            (when session-started
              (insert (format "Session: %s  |  "
                            (claude-multi--format-timestamp session-started))))
            (insert (format "Updated: %s\n"
                          (if timestamp
                              (claude-multi--format-timestamp timestamp)
                            "unknown")))
            (insert "#+END_CENTER\n")

            (buffer-string)))
      (error
       (format "#+BEGIN_EXAMPLE\nError parsing status.json: %s\n#+END_EXAMPLE\n"
               (error-message-string err))))))

(defun claude-multi--git-status-icon (status)
  "Return a descriptive icon/text for git STATUS code."
  (pcase status
    ("M" "[M]") ; Modified
    ("A" "[+]") ; Added
    ("D" "[-]") ; Deleted
    ("R" "[→]") ; Renamed
    ("MM" "[M*]") ; Modified in both
    ("??" "[?]") ; Untracked
    (_ (format "[%s]" status))))

(defun claude-multi--format-number (num)
  "Format NUM with thousand separators."
  (let ((str (number-to-string num))
        (result ""))
    (while (> (length str) 3)
      (setq result (concat "," (substring str -3) result))
      (setq str (substring str 0 -3)))
    (concat str result)))

(defun claude-multi--format-timestamp (ts)
  "Format ISO 8601 TIMESTAMP TS to a readable format."
  (if (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" ts)
      (format "%s-%s-%s %s:%s:%s"
              (match-string 1 ts)
              (match-string 2 ts)
              (match-string 3 ts)
              (match-string 4 ts)
              (match-string 5 ts)
              (match-string 6 ts))
    ts))

;;;###autoload
(defun claude-multi--update-agent-status-display (agent)
  "Update AGENT's :STATUS: drawer with latest status.json content.
The STATUS drawer is collapsible in org-mode - use TAB to fold/unfold."
  ;; Guard against recursive calls
  (when (and (not claude-multi--status-update-in-progress)
             (buffer-live-p claude-multi--progress-buffer))
    (setq claude-multi--status-update-in-progress t)
    (unwind-protect
        (with-current-buffer claude-multi--progress-buffer
          (let* ((inhibit-read-only t)
                 (agent-dir (or (claude-agent-worktree-path agent)
                               (claude-agent-working-directory agent)
                               default-directory))
                 (status-file (expand-file-name "status.json" agent-dir))
                 (content (claude-multi--parse-status-json status-file))
                 (headline-pos nil))
            ;; Find the agent's status marker inside the STATUS drawer
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward (format "<!-- status-marker-%s -->"
                                              (regexp-quote (claude-agent-id agent))) nil t)
                ;; Move past the marker line to start of content area
                (beginning-of-line)
                (forward-line 1)
                ;; Save position for insertion
                (let ((insert-pos (point)))
                  ;; Find the :END: tag of the STATUS drawer
                  (when (re-search-forward "^   :END:" nil t)
                    (beginning-of-line)
                    ;; Delete old content between marker and :END:
                    (delete-region insert-pos (point))
                    ;; Go back to insertion point and insert new content
                    (goto-char insert-pos)
                    (if content
                        (insert content "\n")
                      (insert "/Status file not found or empty/\n"))

                    ;; Find the headline position for this agent
                    (goto-char (point-min))
                    (when (re-search-forward
                           (format "^\\*\\* .* %s" (regexp-quote (claude-agent-id agent)))
                           nil t)
                      (setq headline-pos (line-beginning-position)))))))

            ;; Auto-expand drawer if agent is waiting for input
            (when (and headline-pos content (string-match-p "WAITING FOR INPUT" content))
              (save-excursion
                (goto-char headline-pos)
                (claude-multi--show-subtree-safe)))))
      (setq claude-multi--status-update-in-progress nil))))

;;;###autoload
(defun claude-multi--watch-agent-status-file (agent)
  "Watch AGENT's status.json file for changes and auto-update.
Watches the agent's directory initially, then switches to watching
the status.json file once it's created."
  (unless claude-multi--status-file-watches
    (setq claude-multi--status-file-watches (make-hash-table :test 'equal)))
  (unless claude-multi--status-watch-upgraded
    (setq claude-multi--status-watch-upgraded (make-hash-table :test 'equal)))

  (let* ((agent-id (claude-agent-id agent))
         (agent-dir (or (claude-agent-worktree-path agent)
                       (claude-agent-working-directory agent)
                       default-directory))
         (status-file (expand-file-name "status.json" agent-dir)))
    ;; If status.json already exists, watch it directly
    (if (file-exists-p status-file)
        (progn
          (puthash agent-id t claude-multi--status-watch-upgraded)
          (puthash agent-id
                   (file-notify-add-watch
                    status-file
                    '(change)
                    (lambda (event)
                      ;; Only process if the event is about status.json
                      (when (string-match-p "status\\.json" (format "%s" event))
                        (claude-multi--update-agent-status-display agent))))
                   claude-multi--status-file-watches))
      ;; Watch directory and upgrade to file watch when created
      (puthash agent-id
               (file-notify-add-watch
                agent-dir
                '(change)
                (lambda (event)
                  ;; Only process events related to status.json
                  (when (and (string-match-p "status\\.json" (format "%s" event))
                            (not (gethash agent-id claude-multi--status-watch-upgraded)))
                    (if (file-exists-p status-file)
                        (progn
                          ;; Upgrade to watching the file directly
                          (puthash agent-id t claude-multi--status-watch-upgraded)
                          (let ((old-watch (gethash agent-id claude-multi--status-file-watches)))
                            (when old-watch
                              (file-notify-rm-watch old-watch))
                            (puthash agent-id
                                    (file-notify-add-watch
                                     status-file
                                     '(change)
                                     (lambda (event)
                                       (claude-multi--update-agent-status-display agent)))
                                    claude-multi--status-file-watches))
                          (claude-multi--update-agent-status-display agent))
                      ;; File doesn't exist yet, just update display
                      (claude-multi--update-agent-status-display agent)))))
               claude-multi--status-file-watches))
    ;; Initial update
    (claude-multi--update-agent-status-display agent)))

;;;###autoload
(defun claude-multi--stop-watching-agent-status (agent)
  "Stop watching AGENT's status.json file."
  (when claude-multi--status-file-watches
    (let* ((agent-id (claude-agent-id agent))
           (watch-desc (gethash agent-id claude-multi--status-file-watches)))
      (when watch-desc
        (file-notify-rm-watch watch-desc)
        (remhash agent-id claude-multi--status-file-watches)
        (when claude-multi--status-watch-upgraded
          (remhash agent-id claude-multi--status-watch-upgraded))))))

;;;###autoload
(defun claude-multi--stop-all-status-watches ()
  "Stop all status file watches."
  (when claude-multi--status-file-watches
    (maphash (lambda (agent-id watch-desc)
              (file-notify-rm-watch watch-desc))
            claude-multi--status-file-watches)
    (clrhash claude-multi--status-file-watches))
  (when claude-multi--status-watch-upgraded
    (clrhash claude-multi--status-watch-upgraded)))

(provide 'claude-multi-progress)
;;; progress.el ends here
