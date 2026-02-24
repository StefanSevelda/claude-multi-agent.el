;; -*- lexical-binding: t; -*-
;;; autoload/claude-multi-ediff.el --- Interactive diff review for Claude Multi-Agent

;;; Commentary:
;; Provides interactive code review functionality using Emacs ediff.
;; Agents are represented as alists from `cma list --json` (not cl-defstruct).
;; All git operations delegate to the `cma git` subcommands.

;;; Code:

(require 'cl-lib)
(require 'ediff)
(require 'cma-core)

(defvar claude-multi--current-ediff-agent nil
  "Agent alist currently being reviewed in ediff session.")

(defvar claude-multi--ediff-control-buffer nil
  "Current ediff control buffer for agent review.")

(defvar claude-multi--ediff-session nil
  "Plist tracking the current review session state.")

;;; Core ediff session management

;;;###autoload
(defun claude-multi-ediff--create-session (agent &optional files)
  "Create ediff review session for AGENT alist.
If FILES is provided, use that list instead of detecting changes."
  (let* ((dir (alist-get 'cwd agent))
         (changed-files (or files (claude-multi-ediff--get-changed-files dir)))
         (session (list :current-file nil
                       :files-to-review changed-files
                       :agent-id (alist-get 'session_id agent)
                       :accepted-files nil
                       :rejected-files nil
                       :working-directory dir
                       :mcp-request-id nil)))
    (setq claude-multi--ediff-session session)
    (message "Created ediff session for %s with %d file(s)"
             (alist-get 'name agent) (length changed-files))
    session))

;;;###autoload
(defun claude-multi-ediff--get-changed-files (dir)
  "Get list of changed files in DIR via cma git."
  (let ((result (cma--call "git" "changed-files" "--dir" dir "--json")))
    (when result
      (mapcar (lambda (file) (expand-file-name file dir)) result))))

;;;###autoload
(defun claude-multi-ediff--get-worktree-diff (dir)
  "Get unified diff of all changes in DIR via cma git."
  (cma--call-raw "git" "diff" "--dir" dir))

;;;###autoload
(defun claude-multi-ediff--show-diff (file-path)
  "Show ediff for FILE-PATH comparing worktree vs main branch."
  (let* ((session claude-multi--ediff-session)
         (dir (plist-get session :working-directory))
         (base-file (claude-multi-ediff--get-main-version file-path dir)))
    (plist-put session :current-file file-path)
    (ediff-buffers
     (find-file-noselect base-file)
     (find-file-noselect file-path)
     '(claude-multi-ediff--setup-hook)
     'ediff-revision)
    (plist-put session :ediff-buffer ediff-control-buffer)
    (setq claude-multi--ediff-control-buffer ediff-control-buffer)
    (message "Reviewing %s (press 'a' to accept, 'r' to reject)"
             (file-name-nondirectory file-path))))

(defun claude-multi-ediff--get-main-version (file dir)
  "Get main branch version of FILE in DIR via cma git show."
  (let* ((relative-file (file-relative-name file dir))
         (temp-file (make-temp-file "claude-main-" nil
                                    (concat "." (or (file-name-extension file) "txt"))))
         (ref-path (format "HEAD:%s" relative-file)))
    (cma--call-raw "git" "show" ref-path "--dir" dir "--output" temp-file)
    temp-file))

(defun claude-multi-ediff--setup-hook ()
  "Setup ediff session with custom keybindings."
  (when claude-multi--ediff-session
    (local-set-key (kbd "a") 'claude-multi-ediff--accept-current)
    (local-set-key (kbd "r") 'claude-multi-ediff--reject-current)
    (local-set-key (kbd "n") 'claude-multi-ediff--next-file)
    (message "Ediff review keys: [a]ccept [r]eject [n]ext [q]uit")))

;;;###autoload
(defun claude-multi-ediff--accept-changes ()
  "Accept current file changes."
  (let* ((session claude-multi--ediff-session)
         (current-file (plist-get session :current-file)))
    (when current-file
      (plist-put session :accepted-files
                 (cons current-file (plist-get session :accepted-files)))
      (message "Accepted changes to %s" (file-name-nondirectory current-file))
      (claude-multi-ediff--next-or-complete))))

;;;###autoload
(defun claude-multi-ediff--reject-changes ()
  "Reject current file changes (revert to main)."
  (let* ((session claude-multi--ediff-session)
         (current-file (plist-get session :current-file)))
    (when current-file
      (claude-multi-ediff--revert-file current-file)
      (plist-put session :rejected-files
                 (cons current-file (plist-get session :rejected-files)))
      (message "Rejected changes to %s" (file-name-nondirectory current-file))
      (claude-multi-ediff--next-or-complete))))

(defun claude-multi-ediff--revert-file (file)
  "Revert FILE to main branch version via cma git checkout."
  (let* ((session claude-multi--ediff-session)
         (dir (plist-get session :working-directory))
         (relative-file (file-relative-name file dir)))
    (cma--call-raw "git" "checkout" relative-file "--dir" dir)
    (let ((buf (get-file-buffer file)))
      (when buf
        (with-current-buffer buf
          (revert-buffer t t t))))))

(defun claude-multi-ediff--next-or-complete ()
  "Move to next file or complete review."
  (let* ((session claude-multi--ediff-session)
         (files-remaining (plist-get session :files-to-review)))
    (when (and ediff-control-buffer (buffer-live-p ediff-control-buffer))
      (with-current-buffer ediff-control-buffer
        (ediff-quit t)))
    (if files-remaining
        (let ((next-file (car files-remaining)))
          (plist-put session :files-to-review (cdr files-remaining))
          (claude-multi-ediff--show-diff next-file))
      (claude-multi-ediff--complete-review))))

(defun claude-multi-ediff--complete-review ()
  "Complete diff review and send MCP response."
  (let* ((session claude-multi--ediff-session)
         (request-id (plist-get session :mcp-request-id))
         (agent-id (plist-get session :agent-id))
         (accepted (plist-get session :accepted-files))
         (rejected (plist-get session :rejected-files))
         (result `((accepted . ,(apply 'vector (nreverse accepted)))
                   (rejected . ,(apply 'vector (nreverse rejected)))
                   (status . "completed"))))
    (when (and request-id (fboundp 'claude-multi-mcp--complete-deferred-response))
      (claude-multi-mcp--complete-deferred-response agent-id request-id result))
    (setq claude-multi--ediff-session nil)
    (setq claude-multi--current-ediff-agent nil)
    (setq claude-multi--ediff-control-buffer nil)
    (message "Review completed: %d accepted, %d rejected"
             (length accepted) (length rejected))))

;;; Interactive commands

;;;###autoload
(defun claude-multi/review-agent-changes ()
  "Start interactive review of changes for selected agent."
  (interactive)
  (let* ((agents (cma--call "list" "--json"))
         (names (mapcar (lambda (a)
                          (cons (format "%s [%s] - %s"
                                        (alist-get 'name a)
                                        (upcase (or (alist-get 'status a) "unknown"))
                                        (or (alist-get 'cwd a) ""))
                                a))
                        agents))
         (choice (completing-read "Review changes for: " names nil t))
         (agent (cdr (assoc choice names))))
    (when agent
      (setq claude-multi--current-ediff-agent agent)
      (let* ((dir (alist-get 'cwd agent))
             (changed-files (claude-multi-ediff--get-changed-files dir)))
        (if (null changed-files)
            (message "No changes to review for %s" (alist-get 'name agent))
          (claude-multi-ediff--create-session agent changed-files)
          (let* ((session claude-multi--ediff-session)
                 (first-file (car changed-files)))
            (plist-put session :files-to-review (cdr changed-files))
            (claude-multi-ediff--show-diff first-file)))))))

;;;###autoload
(defun claude-multi/accept-current-diff ()
  "Accept current file changes in active ediff review."
  (interactive)
  (if claude-multi--ediff-session
      (claude-multi-ediff--accept-changes)
    (message "No active ediff review session")))

;;;###autoload
(defun claude-multi/reject-current-diff ()
  "Reject current file changes in active ediff review."
  (interactive)
  (if claude-multi--ediff-session
      (claude-multi-ediff--reject-changes)
    (message "No active ediff review session")))

;;;###autoload
(defun claude-multi/next-diff-file ()
  "Skip to next file in active ediff review."
  (interactive)
  (if claude-multi--ediff-session
      (claude-multi-ediff--next-or-complete)
    (message "No active ediff review session")))

(defun claude-multi-ediff--accept-current ()
  "Internal: accept current file."
  (interactive)
  (when claude-multi--ediff-session
    (claude-multi-ediff--accept-changes)))

(defun claude-multi-ediff--reject-current ()
  "Internal: reject current file."
  (interactive)
  (when claude-multi--ediff-session
    (claude-multi-ediff--reject-changes)))

(defun claude-multi-ediff--next-file ()
  "Internal: move to next file."
  (interactive)
  (when claude-multi--ediff-session
    (claude-multi-ediff--next-or-complete)))

;;;###autoload
(defun claude-multi-ediff--set-mcp-request-id (request-id)
  "Set MCP REQUEST-ID for the current ediff session."
  (when claude-multi--ediff-session
    (plist-put claude-multi--ediff-session :mcp-request-id request-id)))

(provide 'claude-multi-ediff)
;;; claude-multi-ediff.el ends here
