;; -*- lexical-binding: t; -*-
;;; autoload/claude-multi-rename.el --- Agent renaming with status.json persistence

;;; Commentary:
;; Provides agent renaming in table view progress buffer.
;; Updates:
;; 1. /tmp/claude-status/status-<SESSION_ID>.json (persistence)
;; 2. Kitty window title (live UI update)

;;; Code:

(require 'json)

;; Forward declarations
(declare-function claude-multi--status-file-path "claude-multi-status")
(declare-function claude-multi--read-status-file "claude-multi-status")

(defvar claude-multi-kitty-listen-address)

;;; Rename mapping directory

(defvar claude-multi-rename-directory "/tmp/claude-multi-renames"
  "Directory for persisting user-defined agent renames.
Renames are stored separately from status files to survive status updates.")

;;; Property change detection

(defvar claude-multi--rename-timer nil
  "Timer for debouncing rename detection.")

(defvar claude-multi--last-seen-names nil
  "Hash table tracking last seen agent names by session ID.
Used to detect actual changes vs. buffer refreshes.")

;;;###autoload
(defun claude-multi/rename-agent-at-point ()
  "Rename the agent at point in the progress buffer (table view).
Prompts for new name and updates status.json and kitty window title."
  (interactive)
  ;; Use table view rename function
  (if (and (derived-mode-p 'claude-multi-table-mode)
           (fboundp 'claude-multi-table/rename-agent))
      (claude-multi-table/rename-agent)
    (message "Rename only works in table view")))


;;; Rename mapping file functions

;;;###autoload
(defun claude-multi--rename-mapping-file (session-id)
  "Return path to rename mapping file for SESSION-ID."
  (expand-file-name session-id claude-multi-rename-directory))

;;;###autoload
(defun claude-multi--write-rename-mapping (session-id new-name)
  "Write NEW-NAME to rename mapping file for SESSION-ID."
  (let ((mapping-file (claude-multi--rename-mapping-file session-id)))
    ;; Ensure directory exists
    (unless (file-exists-p claude-multi-rename-directory)
      (make-directory claude-multi-rename-directory t))
    ;; Write rename to file
    (with-temp-file mapping-file
      (insert new-name))
    (message "Wrote rename mapping: %s -> %s" session-id new-name)))

;;;###autoload
(defun claude-multi--read-rename-mapping (session-id)
  "Read rename from mapping file for SESSION-ID.
Returns the renamed name or nil if no mapping exists."
  (let ((mapping-file (claude-multi--rename-mapping-file session-id)))
    (when (file-exists-p mapping-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents mapping-file)
            (string-trim (buffer-string)))
        (error nil)))))

;;;###autoload
(defun claude-multi--delete-rename-mapping (session-id)
  "Delete rename mapping file for SESSION-ID."
  (let ((mapping-file (claude-multi--rename-mapping-file session-id)))
    (when (file-exists-p mapping-file)
      (delete-file mapping-file)
      (message "Deleted rename mapping for: %s" session-id))))

;;; Status file updates

;;;###autoload
(defun claude-multi--update-status-agent-name (session-id new-name)
  "Persist agent rename to mapping file for SESSION-ID with NEW-NAME.
Also updates status.json for reference, but mapping file is the authoritative source.
Display code reads ONLY from mapping file, ignoring status.json agent_name."
  (require 'claude-multi-status)
  ;; Write to the authoritative rename mapping file
  (claude-multi--write-rename-mapping session-id new-name)
  (message "Agent renamed to: %s" new-name))

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
