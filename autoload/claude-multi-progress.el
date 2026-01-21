;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; autoload/progress.el --- Org-mode progress tracking for Claude Multi-Agent

;;; Commentary:
;; Centralized org-mode-based progress tracking for all agents

;;; Code:

(require 's)
(require 'f)
(require 'org)

;; Forward declarations for filenotify functions
(declare-function file-notify-add-watch "filenotify")
(declare-function file-notify-rm-watch "filenotify")

;; Forward declarations for functions in other modules
(declare-function claude-multi--get-agent-by-id "claude-multi-agents")
(declare-function claude-multi--detect-input-request "claude-multi-notifications")
(declare-function claude-multi--format-duration "claude-multi-agents")
(declare-function claude-multi--get-cached-status "claude-multi-status")
(declare-function claude-multi--read-agent-mapping "claude-multi-status")
(declare-function claude-multi--get-all-agent-mappings "claude-multi-status")
(declare-function claude-agent-id "claude-multi-agents")
(declare-function claude-agent-status "claude-multi-agents")
(declare-function claude-agent-color "claude-multi-agents")
(declare-function claude-agent-task-description "claude-multi-agents")
(declare-function claude-agent-worktree-path "claude-multi-agents")
(declare-function claude-agent-working-directory "claude-multi-agents")
(declare-function claude-agent-created-at "claude-multi-agents")
(declare-function claude-agent-completed-at "claude-multi-agents")

;; Forward declarations for variables defined in config.el
(defvar claude-multi--progress-buffer)
(defvar claude-multi--session-start-time)
(defvar claude-multi--current-session-window-id)
(defvar claude-multi-use-org-tags)
(defvar claude-multi-output-throttle-delay)
(defvar claude-multi--agents)

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

;;;###autoload
(defun claude-multi--make-properties-editable ()
  "Make org property drawers editable in read-only progress buffer.
Specifically makes AGENT_NAME property editable so users can rename agents."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^:PROPERTIES:$" nil t)
            (let ((start (line-beginning-position)))
              (when (re-search-forward "^:END:$" nil t)
                (let ((end (line-end-position)))
                  ;; Make the entire drawer editable by setting inhibit-read-only
                  (put-text-property start end 'read-only nil)
                  (put-text-property start end 'inhibit-read-only t)
                  ;; Also make AGENT_NAME line specifically editable and visually distinct
                  (save-excursion
                    (goto-char start)
                    (when (re-search-forward "^:AGENT_NAME: " end t)
                      (let ((name-start (match-end 0))
                            (name-end (line-end-position)))
                        ;; Make the value part editable and highlighted
                        (put-text-property name-start name-end 'read-only nil)
                        (put-text-property name-start name-end 'inhibit-read-only t)
                        (put-text-property name-start name-end 'face 'font-lock-variable-name-face)
                        (put-text-property name-start name-end 'rear-nonsticky t)))))))))))))

;;;###autoload
(defun claude-multi--refresh-progress-from-status-files ()
  "Refresh progress buffer by reading all status files from /tmp/claude-status/.
Dispatches to appropriate refresh based on current view mode (org or table)."
  ;; Check current view mode and dispatch appropriately
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (if (eq claude-multi--view-mode 'table)
          ;; Refresh table view
          (when (and (fboundp 'claude-multi-table/refresh)
                     (derived-mode-p 'claude-multi-table-mode))
            (claude-multi-table/refresh))
        ;; Refresh org view (default)
        (claude-multi--refresh-org-view-from-status-files)))))

;;;###autoload
(defun claude-multi--refresh-org-view-from-status-files ()
  "Refresh org-mode view by reading all status files from /tmp/claude-status/."
  ;; Ensure helper functions are defined (workaround for loading issues)
  (unless (fboundp 'claude-multi--get-status-icon-from-string)
    (eval '(defun claude-multi--get-status-icon-from-string (status-str)
             (pcase status-str
               ("running" "🔵")
               ("waiting-for-user" "🟡")
               ("finished" "🟢")
               ("error" "🔴")
               (_ "⚪")))))
  (unless (fboundp 'claude-multi--update-session-stats-from-files)
    (eval '(defun claude-multi--update-session-stats-from-files (status-files)
             (let ((total (length status-files))
                   (running 0) (waiting 0) (completed 0) (failed 0))
               (dolist (entry status-files)
                 (let* ((data (cdr entry))
                        (status (alist-get 'claude_status data))
                        (waiting-input (alist-get 'waiting_for_input data)))
                   (cond
                    ((string= status "finished") (cl-incf completed))
                    ((string= status "error") (cl-incf failed))
                    (waiting-input (cl-incf waiting))
                    ((string= status "running") (cl-incf running)))))
               (save-excursion
                 (goto-char (point-min))
                 (when (re-search-forward "^- Stats :: " nil t)
                   (delete-region (point) (line-end-position))
                   (insert (format "%d total | %d running | %d waiting | %d completed | %d failed"
                                  total running waiting completed failed))))))))
  (when (buffer-live-p claude-multi--progress-buffer)
    (let ((status-files (claude-multi--get-all-status-files)))
      (with-current-buffer claude-multi--progress-buffer
        (let ((inhibit-read-only t))
          ;; Clear agents section and rebuild
          (save-excursion
            (goto-char (point-min))
            ;; If "* Agents" section doesn't exist, initialize buffer structure first
            (unless (re-search-forward "^\\* Agents" nil t)
              (claude-multi--init-progress-buffer)
              (goto-char (point-min)))
            ;; Now find the Agents section (it must exist after init)
            (when (re-search-forward "^\\* Agents" nil t)
              (let ((agents-start (line-beginning-position)))
                ;; Delete from "* Agents" to end of buffer
                (delete-region agents-start (point-max))
                (goto-char agents-start)
                (insert "* Agents\n\n")

                ;; Insert each session from status files
                (dolist (entry status-files)
                  (let* ((data (cdr entry))
                         (session-id (alist-get 'session_id data))
                         ;; Agent name: ONLY from mapping file (ignores status.json agent_name)
                         (mapped-name (when (fboundp 'claude-multi--read-rename-mapping)
                                       (claude-multi--read-rename-mapping session-id)))
                         ;; All other metadata: from status.json
                         (kitty-id (alist-get 'kitty_window_id data))
                         (cwd (alist-get 'cwd data))
                         (status (alist-get 'claude_status data))
                         (timestamp (alist-get 'timestamp data))
                         (model (alist-get 'model_name data))
                         (mode (alist-get 'claude_mode data))
                         (context (alist-get 'context_window data))
                         (git (alist-get 'git data))
                         ;; Build descriptive name from directory and branch for auto-generated names
                         (dir-name (when cwd (file-name-nondirectory (directory-file-name cwd))))
                         (branch (when git (alist-get 'branch git)))
                         (auto-name (cond
                                     ;; Use directory + branch if available
                                     ((and dir-name branch) (format "%s (%s)" dir-name branch))
                                     ;; Use directory only
                                     (dir-name dir-name)
                                     ;; Fallback to session ID
                                     (t (format "Session %s" (substring session-id 0 8)))))
                         ;; Final display name: mapping file wins, fallback to auto-generated
                         (display-name (or mapped-name auto-name))
                         ;; For properties drawer: only store if explicitly mapped by user
                         (agent-name mapped-name))

                    ;; Insert session header
                    (insert (format "** %s %s\n"
                                   (claude-multi--get-status-icon-from-string status)
                                   display-name))

                    ;; Insert properties drawer
                    (insert ":PROPERTIES:\n")
                    (insert (format ":SESSION_ID: %s\n" session-id))
                    (when kitty-id
                      (insert (format ":KITTY_WINDOW: %s\n" kitty-id)))
                    (when agent-name
                      (insert (format ":AGENT_NAME: %s\n" agent-name)))
                    (insert (format ":DIRECTORY: %s\n" cwd))
                    (insert (format ":STATUS: %s\n" (upcase status)))
                    (when model
                      (insert (format ":MODEL: %s\n" (upcase model))))
                    (when mode
                      (insert (format ":MODE: %s\n" (upcase mode))))
                    (when context
                      (insert (format ":TOKENS: %d/%d (%.1f%%)\n"
                                     (or (alist-get 'tokens_used context) 0)
                                     (or (alist-get 'tokens_total context) 200000)
                                     (or (alist-get 'percentage_used context) 0))))
                    (when git
                      (when-let ((branch (alist-get 'branch git)))
                        (insert (format ":BRANCH: %s\n" branch))))
                    (insert (format ":UPDATED: %s\n" (or timestamp "unknown")))
                    (insert ":END:\n\n"))))

              ;; Update stats
              (claude-multi--update-session-stats-from-files status-files)

              ;; Make property drawers editable
              (claude-multi--make-properties-editable)))))))

;;;###autoload
(defun claude-multi--get-status-icon-from-string (status-str)
  "Return icon emoji for STATUS-STR."
  (pcase status-str
    ("running" "🔵")
    ("waiting-for-user" "🟡")
    ("finished" "🟢")
    ("error" "🔴")
    (_ "⚪")))

;;;###autoload
(defun claude-multi--update-session-stats-from-files (status-files)
  "Update session statistics line from STATUS-FILES."
  (let ((total (length status-files))
        (running 0)
        (waiting 0)
        (completed 0)
        (failed 0))
    (dolist (entry status-files)
      (let* ((data (cdr entry))
             (status (alist-get 'claude_status data))
             (waiting-input (alist-get 'waiting_for_input data)))
        (cond
         ((string= status "finished") (cl-incf completed))
         ((string= status "error") (cl-incf failed))
         (waiting-input (cl-incf waiting))
         ((string= status "running") (cl-incf running)))))

    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^- Stats :: " nil t)
        (delete-region (point) (line-end-position))
        (insert (format "%d total | %d running | %d waiting | %d completed | %d failed"
                       total running waiting completed failed)))))))

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
             (duration nil)
             (model-name nil)
             (claude-mode nil))

        ;; Try to extract info from cached status first
        (let ((cached-status (when (fboundp 'claude-multi--get-cached-status)
                               (claude-multi--get-cached-status agent))))
          (when cached-status
            (setq model-name (alist-get 'model_name cached-status))
            (setq claude-mode (alist-get 'claude_mode cached-status))
            (let ((context (alist-get 'context_window cached-status))
                  (git-info (alist-get 'git cached-status)))
              (when context
                (setq ctx-pct (alist-get 'percentage_used context)))
              (when git-info
                (let ((changed-files (alist-get 'changed_files git-info)))
                  (when changed-files
                    (setq file-count (length changed-files))))))))

        ;; Fallback: try to extract info from status.json file if no cached data
        (when (and (not model-name) (file-exists-p status-file))
          (condition-case nil
              (let* ((_json-object-type 'plist)
                     (_json-array-type 'list)
                     (_json-key-type 'keyword)
                     (data (json-read-file status-file))
                     (context (plist-get data :context_window))
                     (git-info (plist-get data :git)))
                (setq model-name (plist-get data :model_name))
                (setq claude-mode (plist-get data :claude_mode))
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
        ;; ** EMOJI AGENT-ID [MODEL/MODE] | DOMAIN | TASK | DURATION | CTX% | FILES | :tags:
        (insert (format "\n** %s "
                       (claude-multi--get-status-icon (claude-agent-status agent))))

        ;; Insert agent ID with color matching kitty tab
        (let ((agent-id-start (point)))
          (insert (claude-agent-id agent))
          (add-text-properties agent-id-start (point)
                               `(face (:foreground ,(claude-agent-color agent) :weight bold))))

        ;; Add model/mode badge after agent ID
        (when (or model-name claude-mode)
          (insert " [")
          (when model-name
            (insert (upcase model-name)))
          (when (and model-name claude-mode (not (string= claude-mode "normal")))
            (insert "/"))
          (when (and claude-mode (not (string= claude-mode "normal")))
            (insert (upcase claude-mode)))
          (insert "]"))

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
          (claude-multi--show-subtree-safe))

        ;; Try to populate STATUS drawer immediately if status data is available
        (claude-multi--update-agent-status-display agent)))))

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
Uses throttling to reduce flashing based on
`claude-multi-output-throttle-delay'."
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
              (let* ((_json-object-type 'plist)
                     (_json-array-type 'list)
                     (_json-key-type 'keyword)
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
                 ;; Tell byte compiler these are used dynamically by json-read-file
                 (_ (ignore json-object-type json-array-type json-key-type))
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
;;
;; Note: Status tracking is now handled by claude-multi-status.el
;; which uses file-notify to watch /tmp/claude-status/*.json files.
;; The functions below are stubs for backward compatibility.

(require 'json)

(defun claude-multi--format-status-data (status-data)
  "Format STATUS-DATA (an alist from claude-multi-status) into org-mode content."
  (when status-data
    (condition-case err
        (let* ((claude-status (alist-get 'claude_status status-data))
               (timestamp (alist-get 'timestamp status-data))
               (session-started (alist-get 'session_started status-data))
               (waiting (alist-get 'waiting_for_input status-data))
               (is-busy (alist-get 'is_busy status-data))
               (model-name (alist-get 'model_name status-data))
               (claude-mode (alist-get 'claude_mode status-data))
               (activity (alist-get 'current_activity status-data))
               (context (alist-get 'context_window status-data))
               (git-info (alist-get 'git status-data)))

          ;; Build formatted output
          (with-temp-buffer
            ;; Status indicator with busy state
            (if waiting
                (insert "#+BEGIN_WARNING\n⏸ *WAITING FOR INPUT*\n#+END_WARNING\n\n")
              (let ((status-str (or claude-status "Working...")))
                (when is-busy
                  (setq status-str (concat "🔄 " status-str " (BUSY)")))
                (insert (format "- Status :: %s\n\n" status-str))))

            ;; Agent Configuration (Model and Mode)
            (when (or model-name claude-mode)
              (insert "*Agent Configuration*\n\n")
              (when model-name
                (insert (format "- Model :: %s\n" (upcase model-name))))
              (when claude-mode
                (insert (format "- Mode :: %s\n" (upcase claude-mode))))
              (insert "\n"))

            ;; Context window usage
            (when context
              (let* ((used (alist-get 'tokens_used context))
                     (total (alist-get 'tokens_total context))
                     (pct (alist-get 'percentage_used context)))
                (insert "*Context Window*\n\n")
                (insert (format "- Usage :: %d / %d tokens (%.1f%%)\n"
                               (or used 0) (or total 200000) (or pct 0.0)))
                ;; Visual progress bar
                (let* ((bar-width 40)
                       (filled (round (* bar-width (/ (or pct 0.0) 100.0)))))
                  (insert "- Progress :: [")
                  (insert (make-string filled ?█))
                  (insert (make-string (- bar-width filled) ?░))
                  (insert "]\n"))
                (insert "\n")))

            ;; Git information
            (when git-info
              (let ((branch (alist-get 'branch git-info)))
                (insert "*Git Status*\n\n")
                (when branch
                  (insert (format "- Branch :: =%s=\n" branch)))
                (insert "\n")))

            ;; Current activity section
            (when activity
              (let ((goal (alist-get 'goal activity)))
                (insert "*Current Activity*\n\n")
                (when goal
                  (insert (format "- Goal :: %s\n" goal)))
                (insert "\n")))

            ;; Session info footer
            (insert "#+BEGIN_CENTER\n")
            (insert (format "Updated: %s\n"
                          (if timestamp
                              (claude-multi--format-timestamp (format "%s" timestamp))
                            "unknown")))
            (insert "#+END_CENTER\n")

            (buffer-string)))
      (error
       (format "#+BEGIN_EXAMPLE\nError formatting status: %s\n#+END_EXAMPLE\n"
               (error-message-string err))))))

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
  "Update AGENT's :STATUS: drawer with latest status from claude-multi-status module.
The STATUS drawer is collapsible in org-mode - use TAB to fold/unfold."
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let* ((inhibit-read-only t)
             ;; Get cached status from the status module
             (status-data (when (fboundp 'claude-multi--get-cached-status)
                           (claude-multi--get-cached-status agent)))
             (content (when status-data
                       (claude-multi--format-status-data status-data)))
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
                  (let ((pending-p (and (fboundp 'claude-multi--agent-is-pending-p)
                                        (claude-multi--agent-is-pending-p agent))))
                    (if pending-p
                        (insert "/Waiting for status file (agent in pending state).../\n")
                      (insert "/Waiting for first status update.../\n"))))

                ;; Find the headline position for this agent
                (goto-char (point-min))
                (when (re-search-forward
                       (format "^\\*\\* .* %s" (regexp-quote (claude-agent-id agent)))
                       nil t)
                  (setq headline-pos (line-beginning-position))))))

        ;; Auto-expand drawer if agent is waiting for input
        (when (and headline-pos content (string-match-p "WAITING FOR INPUT" content))
          (save-excursion
            (goto-char headline-pos)
            (claude-multi--show-subtree-safe))))))))

;;;###autoload
(defun claude-multi--watch-agent-status-file (_agent)
  "Stub for backward compatibility.
Status watching is now handled by claude-multi-status.el."
  nil)

;;;###autoload
(defun claude-multi--stop-watching-agent-status (_agent)
  "Stub for backward compatibility.
Status watching is now handled by claude-multi-status.el."
  nil)

;;;###autoload
(defun claude-multi--stop-all-status-watches ()
  "Stub for backward compatibility.
Status watching is now handled by claude-multi-status.el."
  nil)

;;; Focus agent at point

;;;###autoload
(defun claude-multi--get-agent-info-at-point ()
  "Get agent information from org headline at point.
Returns plist with :session-id, :kitty-window, :agent-name, :directory, :display-name."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      ;; Move to the headline
      (org-back-to-heading t)
      (let* ((session-id (org-entry-get nil "SESSION_ID"))
             (kitty-window (org-entry-get nil "KITTY_WINDOW"))
             (directory (org-entry-get nil "DIRECTORY"))
             (agent-name (org-entry-get nil "AGENT_NAME"))
             ;; Extract display name from headline
             (headline (org-get-heading t t t t))
             (display-name (when headline
                            ;; Remove status icon and extract name
                            (if (string-match "^[^[:alnum:]]*\\([^[]+\\)" headline)
                                (string-trim (match-string 1 headline))
                              headline))))
        (when (or session-id agent-name)
          (list :session-id session-id
                :kitty-window kitty-window
                :agent-name agent-name
                :directory directory
                :display-name (or display-name agent-name directory session-id)))))))

;;;###autoload
(defun claude-multi/focus-agent-at-point ()
  "Focus on the agent at point in the progress buffer.
Works in both org view and table view."
  (interactive)
  (cond
   ;; Table view mode
   ((derived-mode-p 'claude-multi-table-mode)
    (let ((session-id (tabulated-list-get-id)))
      (if (not session-id)
          (message "No agent at point")
        (when (fboundp 'claude-multi-table/focus-agent)
          (claude-multi-table/focus-agent)))))

   ;; Org view mode
   ((derived-mode-p 'org-mode)
    (let ((agent-info (claude-multi--get-agent-info-at-point)))
      (if (not agent-info)
          (message "No agent found at point. Place cursor on an agent headline.")
        (let* ((window-id (plist-get agent-info :kitty-window))
               (display-name (plist-get agent-info :display-name))
               (listen-addr (or (and (boundp 'claude-multi-kitty-listen-address)
                                    claude-multi-kitty-listen-address)
                               (getenv "KITTY_LISTEN_ON")
                               "unix:/tmp/kitty-claude")))
          (if (not window-id)
              (message "No kitty window ID found for %s. Agent may not have started running Claude commands yet (window ID is written to status.json after first Claude hook execution)."
                      display-name)
            (condition-case err
                (progn
                  (call-process-shell-command
                   (format "kitty @ --to=%s focus-window --match=id:%s"
                          listen-addr window-id)
                   nil 0)
                  (message "Focused on %s (window %s)" display-name window-id))
              (error
               (message "Failed to focus window %s: %s" window-id (error-message-string err)))))))))

   (t (message "Focus agent only works in org or table view"))))

;;;###autoload
(defun claude-multi/kill-agent-at-point ()
  "Kill the agent at point in the progress buffer.
Works in both org view and table view.
Closes kitty window, removes status file, and cleans up mapping file.
If agent doesn't exist, still cleans up all files."
  (interactive)
  (cond
   ;; Table view mode
   ((derived-mode-p 'claude-multi-table-mode)
    (let ((session-id (tabulated-list-get-id)))
      (if (not session-id)
          (message "No agent at point")
        (when (y-or-n-p (format "Really kill agent %s? " (substring session-id 0 8)))
          (require 'claude-multi-status)
          (claude-multi--kill-agent-by-session-id session-id)
          (claude-multi-table/refresh)
          (message "Cleaned up agent")))))

   ;; Org view mode
   ((derived-mode-p 'org-mode)
    (save-excursion
      (org-back-to-heading t)
      (let* ((session-id (org-entry-get nil "SESSION_ID"))
             (agent-info (claude-multi--get-agent-info-at-point))
             (display-name (if agent-info
                              (plist-get agent-info :display-name)
                            (or (org-entry-get nil "AGENT_NAME")
                                (format "Session %s" (substring session-id 0 8))))))
        (if (not session-id)
            (message "No session ID found. Cannot kill agent.")
          (when (y-or-n-p (format "Really kill agent %s? " display-name))
            (require 'claude-multi-status)
            ;; Always clean up files, even if agent doesn't exist
            (claude-multi--delete-status-file session-id)
            (when (fboundp 'claude-multi--delete-rename-mapping)
              (claude-multi--delete-rename-mapping session-id))
            ;; Try to close kitty window if it exists
            (when-let ((kitty-id (org-entry-get nil "KITTY_WINDOW")))
              (condition-case nil
                  (let ((listen-addr (or (getenv "KITTY_LISTEN_ON")
                                        "unix:/tmp/kitty-claude")))
                    (call-process-shell-command
                     (format "kitty @ --to=%s close-window --match=id:%s 2>/dev/null"
                            listen-addr kitty-id)
                     nil 0))
                (error nil)))
            ;; Refresh display
            (when (fboundp 'claude-multi--refresh-progress-from-status-files)
              (claude-multi--refresh-progress-from-status-files))
            (message "Cleaned up agent: %s" display-name))))))

   (t (message "Kill agent only works in org or table view"))))

;;; View switching

;;;###autoload
(defun claude-multi/switch-to-table-view ()
  "Switch progress buffer to table view."
  (interactive)
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      ;; Load table module first
      (require 'claude-multi-table)
      ;; Switch to table mode (this will set the keymap)
      (claude-multi-table-mode)
      ;; Set view mode
      (setq-local claude-multi--view-mode 'table)
      ;; Update evil keymaps if evil is active
      (when (and (fboundp 'evil-normalize-keymaps)
                 (fboundp 'evil-state))
        (evil-normalize-keymaps))
      ;; Populate and display
      (claude-multi--populate-table-view)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (message "Switched to table view. Press 'f' to focus, 'o' to return to org view."))))

;;;###autoload
(defun claude-multi/switch-to-org-view ()
  "Switch progress buffer to org-mode view."
  (interactive)
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (let ((inhibit-read-only t))
        ;; Switch to org mode
        (org-mode)
        ;; Set view mode
        (setq-local claude-multi--view-mode 'org)
        ;; Refresh from status files
        (claude-multi--refresh-progress-from-status-files)
        (message "Switched to org view. Press 't' to switch to table view.")))))

;;;###autoload
(defun claude-multi/toggle-view ()
  "Toggle between table and org-mode views."
  (interactive)
  (when (buffer-live-p claude-multi--progress-buffer)
    (with-current-buffer claude-multi--progress-buffer
      (if (eq claude-multi--view-mode 'table)
          (claude-multi/switch-to-org-view)
        (claude-multi/switch-to-table-view)))))

(provide 'claude-multi-progress)
;;; progress.el ends here
