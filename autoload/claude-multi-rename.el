;; -*- lexical-binding: t; -*-
;;; autoload/claude-multi-rename.el --- Agent renaming with status.json persistence

;;; Commentary:
;; Provides agent renaming by watching org property changes in the progress buffer.
;; When user edits :AGENT_NAME: property, automatically syncs to:
;; 1. /tmp/claude-status/status-<SESSION_ID>.json (persistence)
;; 2. Kitty window title (live UI update)

;;; Code:

(require 'json)
(require 'org)

;; Forward declarations
(declare-function claude-multi--status-file-path "claude-multi-status")
(declare-function claude-multi--read-status-file "claude-multi-status")

(defvar claude-multi-kitty-listen-address)

;;; Property change detection

(defvar claude-multi--rename-timer nil
  "Timer for debouncing rename detection.")

(defvar claude-multi--last-seen-names nil
  "Hash table tracking last seen agent names by session ID.
Used to detect actual changes vs. buffer refreshes.")

;;;###autoload
(defun claude-multi/rename-agent-at-point ()
  "Rename the agent at point in the progress buffer.
Prompts for new name and updates status.json and kitty window title."
  (interactive)
  (let ((agent-info (claude-multi--get-agent-info-at-point)))
    (if (not agent-info)
        (message "No agent found at point. Place cursor on an agent headline.")
      (let* ((session-id (plist-get agent-info :session-id))
             (kitty-window (plist-get agent-info :kitty-window))
             (current-name (or (plist-get agent-info :agent-name)
                             (plist-get agent-info :display-name)))
             (new-name (read-string (format "Rename '%s' to: " current-name) current-name)))
        (if (string-empty-p new-name)
            (message "Agent name cannot be empty")
          ;; Update status file
          (claude-multi--update-status-agent-name session-id new-name)
          ;; Update kitty window title
          (when kitty-window
            (claude-multi--update-kitty-window-title kitty-window new-name session-id))
          ;; Refresh progress buffer to show new name
          (when (fboundp 'claude-multi--refresh-progress-from-status-files)
            (claude-multi--refresh-progress-from-status-files))
          (message "Agent renamed to: %s" new-name))))))

;;;###autoload
(defun claude-multi--setup-rename-hooks ()
  "Setup hooks for detecting AGENT_NAME property changes in progress buffer."
  (when (derived-mode-p 'claude-multi-progress-mode)
    ;; Initialize tracking hash table
    (unless claude-multi--last-seen-names
      (setq claude-multi--last-seen-names (make-hash-table :test 'equal)))
    ;; Hook into buffer changes with debouncing
    (add-hook 'after-change-functions #'claude-multi--after-change-detect-rename nil t)))

(defun claude-multi--after-change-detect-rename (beg end len)
  "Detect property changes after buffer edits (debounced).
BEG, END, and LEN are standard after-change-functions parameters."
  (when (and (derived-mode-p 'claude-multi-progress-mode)
             ;; Only trigger if change is in properties region
             (save-excursion
               (goto-char beg)
               (or (org-in-block-p '("PROPERTIES"))
                   (looking-at "^:[A-Z_]+:"))))
    (when claude-multi--rename-timer
      (cancel-timer claude-multi--rename-timer))
    (setq claude-multi--rename-timer
          (run-with-idle-timer 0.5 nil #'claude-multi--check-all-agent-names))))

;;;###autoload
(defun claude-multi--check-all-agent-names ()
  "Check all agent sections for name changes and sync to status files."
  (when (buffer-live-p (get-buffer "*Claude Multi-Agent Progress.org*"))
    (with-current-buffer "*Claude Multi-Agent Progress.org*"
      (save-excursion
        (goto-char (point-min))
        ;; Find the Agents section
        (when (re-search-forward "^\\* Agents" nil t)
          (while (re-search-forward "^\\*\\* " nil t)
            (claude-multi--check-agent-name-at-point)))))))

(defun claude-multi--check-agent-name-at-point ()
  "Check if agent name changed at current headline and sync if needed."
  (when-let* ((session-id (org-entry-get nil "SESSION_ID"))
              (new-name (org-entry-get nil "AGENT_NAME"))
              (kitty-window (org-entry-get nil "KITTY_WINDOW")))
    ;; Check if name actually changed
    (let ((last-seen (gethash session-id claude-multi--last-seen-names)))
      (unless (equal new-name last-seen)
        ;; Name changed - update both status file and kitty window
        (claude-multi--update-status-agent-name session-id new-name)
        (claude-multi--update-kitty-window-title kitty-window new-name session-id)
        ;; Remember this name
        (puthash session-id new-name claude-multi--last-seen-names)))))

;;; Status file updates

;;;###autoload
(defun claude-multi--update-status-agent-name (session-id new-name)
  "Update agent_name in status.json for SESSION-ID to NEW-NAME."
  (require 'claude-multi-status)
  (when-let* ((status-file (claude-multi--status-file-path session-id))
              ((file-exists-p status-file)))
    (condition-case err
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'symbol)
               (json-false nil)
               (data (with-temp-buffer
                      (insert-file-contents status-file)
                      (goto-char (point-min))
                      (json-read))))
          ;; Update agent_name field
          (setf (alist-get 'agent_name data) new-name)
          ;; Write back to file
          (with-temp-file status-file
            (insert (json-encode data)))
          (message "Updated agent name to: %s" new-name))
      (error
       (message "Failed to update status file for %s: %s"
                session-id (error-message-string err))))))

;;; Kitty window title updates

;;;###autoload
(defun claude-multi--update-kitty-window-title (window-id new-name session-id)
  "Update kitty window WINDOW-ID title to show NEW-NAME.
SESSION-ID is used for the window title prefix."
  (when (and window-id new-name)
    (let* ((listen-addr (or (and (boundp 'claude-multi-kitty-listen-address)
                                claude-multi-kitty-listen-address)
                          (getenv "KITTY_LISTEN_ON")
                          "unix:/tmp/kitty-claude"))
           (short-id (substring session-id 0 8))
           (title (format "[%s] %s" short-id new-name)))
      (condition-case err
          (progn
            (call-process-shell-command
             (format "kitty @ --to=%s set-window-title --match=id:%s '%s' 2>/dev/null"
                    listen-addr window-id (shell-quote-argument title))
             nil 0)
            (message "Updated kitty window title to: %s" title))
        (error
         (message "Failed to update kitty window title: %s"
                 (error-message-string err)))))))

;;; Manual rename command (for non-property-based workflow)

;;;###autoload
(defun claude-multi/rename-agent-at-point (new-name)
  "Rename the agent at point to NEW-NAME.
This is a manual alternative to editing the :AGENT_NAME: property directly.
Updates both status.json and kitty window title."
  (interactive
   (let ((agent-info (when (fboundp 'claude-multi--get-agent-info-at-point)
                       (claude-multi--get-agent-info-at-point))))
     (if (not agent-info)
         (error "No agent found at point")
       (list (read-string "New agent name: "
                         (plist-get agent-info :agent-name))))))
  (save-excursion
    (org-back-to-heading t)
    (when-let* ((session-id (org-entry-get nil "SESSION_ID"))
                (kitty-window (org-entry-get nil "KITTY_WINDOW")))
      ;; Update the property in the buffer
      (org-entry-put nil "AGENT_NAME" new-name)
      ;; Force sync
      (claude-multi--update-status-agent-name session-id new-name)
      (claude-multi--update-kitty-window-title kitty-window new-name session-id))))

(provide 'claude-multi-rename)
;;; claude-multi-rename.el ends here
