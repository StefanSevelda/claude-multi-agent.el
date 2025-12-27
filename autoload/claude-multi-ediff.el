;; -*- lexical-binding: t; -*-
;;; autoload/claude-multi-ediff.el --- Interactive diff review for Claude Multi-Agent

;;; Commentary:
;; Provides interactive code review functionality using Emacs ediff.
;; Integrates with MCP deferred response pattern so Claude agents can request
;; human review of their changes.  Users can review changes file-by-file,
;; accepting or rejecting modifications.

;;; Code:

(require 'cl-lib)
(require 'ediff)

;; Forward declarations
(declare-function claude-multi--get-agent-by-id "claude-multi-agents")
(declare-function claude-agent-id "claude-multi-agents")
(declare-function claude-agent-name "claude-multi-agents")
(declare-function claude-agent-working-directory "claude-multi-agents")
(declare-function claude-agent-ediff-session "claude-multi-agents")
(declare-function claude-multi--select-agent "claude-multi-agents")
(declare-function claude-multi-mcp--complete-deferred-response "claude-multi-mcp")

(eval-when-compile
  (defvar claude-multi--agents))

(defvar claude-multi--current-ediff-agent nil
  "Agent currently being reviewed in ediff session.")

(defvar claude-multi--ediff-control-buffer nil
  "Current ediff control buffer for agent review.")

;;; Core ediff session management

;;;###autoload
(defun claude-multi-ediff--create-session (agent &optional files)
  "Create ediff review session for AGENT.
If FILES is provided, use that list instead of detecting changes."
  (let* ((changed-files (or files (claude-multi-ediff--get-changed-files agent)))
         (session (list :current-file nil
                       :files-to-review changed-files
                       :agent-id (claude-agent-id agent)
                       :accepted-files nil
                       :rejected-files nil
                       :mcp-request-id nil)))
    (setf (claude-agent-ediff-session agent) session)
    (message "Created ediff session for %s with %d file(s)"
             (claude-agent-name agent) (length changed-files))
    session))

;;;###autoload
(defun claude-multi-ediff--get-changed-files (agent)
  "Get list of changed files in AGENT's worktree."
  (let* ((default-directory (or (claude-agent-working-directory agent)
                                default-directory))
         (output (string-trim (shell-command-to-string "git diff --name-only HEAD"))))
    (if (string-empty-p output)
        nil
      (mapcar (lambda (file) (expand-file-name file default-directory))
              (split-string output "\n" t)))))

;;;###autoload
(defun claude-multi-ediff--get-worktree-diff (agent)
  "Get unified diff of all changes in AGENT's worktree."
  (let ((default-directory (or (claude-agent-working-directory agent)
                               default-directory)))
    (shell-command-to-string "git diff HEAD")))

;;;###autoload
(defun claude-multi-ediff--show-diff (agent file-path)
  "Show ediff for FILE-PATH comparing worktree vs main branch."
  (let* ((session (claude-agent-ediff-session agent))
         (worktree-file file-path)
         (base-file (claude-multi-ediff--get-main-version file-path agent)))
    (plist-put session :current-file file-path)
    (setq claude-multi--current-ediff-agent agent)
    (ediff-buffers
     (find-file-noselect base-file)
     (find-file-noselect worktree-file)
     '(claude-multi-ediff--setup-hook)
     'ediff-revision)
    (plist-put session :ediff-buffer ediff-control-buffer)
    (setq claude-multi--ediff-control-buffer ediff-control-buffer)
    (message "Reviewing %s (press 'a' to accept, 'r' to reject)"
             (file-name-nondirectory file-path))))

(defun claude-multi-ediff--get-main-version (file agent)
  "Get main branch version of FILE for AGENT."
  (let* ((default-directory (or (claude-agent-working-directory agent)
                                default-directory))
         (relative-file (file-relative-name file default-directory))
         (temp-file (make-temp-file "claude-main-" nil
                                    (concat "." (or (file-name-extension file) "txt"))))
         (cmd (format "git show HEAD:%s > %s 2>/dev/null"
                     (shell-quote-argument relative-file)
                     (shell-quote-argument temp-file))))
    (if (= 0 (shell-command cmd))
        temp-file
      (with-temp-file temp-file (insert ""))
      temp-file)))

(defun claude-multi-ediff--setup-hook ()
  "Setup ediff session with custom keybindings."
  (when claude-multi--current-ediff-agent
    (local-set-key (kbd "a") 'claude-multi-ediff--accept-current)
    (local-set-key (kbd "r") 'claude-multi-ediff--reject-current)
    (local-set-key (kbd "n") 'claude-multi-ediff--next-file)
    (message "Ediff review keys: [a]ccept [r]eject [n]ext [q]uit")))

;;;###autoload
(defun claude-multi-ediff--accept-changes (agent)
  "Accept current file changes for AGENT."
  (let* ((session (claude-agent-ediff-session agent))
         (current-file (plist-get session :current-file)))
    (when current-file
      (plist-put session :accepted-files
                 (cons current-file (plist-get session :accepted-files)))
      (message "Accepted changes to %s" (file-name-nondirectory current-file))
      (claude-multi-ediff--next-or-complete agent))))

;;;###autoload
(defun claude-multi-ediff--reject-changes (agent)
  "Reject current file changes for AGENT (revert to main)."
  (let* ((session (claude-agent-ediff-session agent))
         (current-file (plist-get session :current-file)))
    (when current-file
      (claude-multi-ediff--revert-file current-file agent)
      (plist-put session :rejected-files
                 (cons current-file (plist-get session :rejected-files)))
      (message "Rejected changes to %s" (file-name-nondirectory current-file))
      (claude-multi-ediff--next-or-complete agent))))

(defun claude-multi-ediff--revert-file (file agent)
  "Revert FILE to main branch version."
  (let ((default-directory (or (claude-agent-working-directory agent)
                               default-directory)))
    (shell-command (format "git checkout HEAD -- %s"
                          (shell-quote-argument (file-relative-name file default-directory))))
    (let ((buf (get-file-buffer file)))
      (when buf
        (with-current-buffer buf
          (revert-buffer t t t))))))

(defun claude-multi-ediff--next-or-complete (agent)
  "Move to next file or complete review."
  (let* ((session (claude-agent-ediff-session agent))
         (files-remaining (plist-get session :files-to-review)))
    (when (and ediff-control-buffer (buffer-live-p ediff-control-buffer))
      (with-current-buffer ediff-control-buffer
        (ediff-quit t)))
    (if files-remaining
        (let ((next-file (car files-remaining)))
          (plist-put session :files-to-review (cdr files-remaining))
          (claude-multi-ediff--show-diff agent next-file))
      (claude-multi-ediff--complete-review agent))))

(defun claude-multi-ediff--complete-review (agent)
  "Complete diff review and send MCP response."
  (let* ((session (claude-agent-ediff-session agent))
         (request-id (plist-get session :mcp-request-id))
         (accepted (plist-get session :accepted-files))
         (rejected (plist-get session :rejected-files))
         (result `((accepted . ,(apply 'vector (nreverse accepted)))
                   (rejected . ,(apply 'vector (nreverse rejected)))
                   (status . "completed"))))
    (when (and request-id (fboundp 'claude-multi-mcp--complete-deferred-response))
      (claude-multi-mcp--complete-deferred-response
       (claude-agent-id agent) request-id result))
    (setf (claude-agent-ediff-session agent) nil)
    (setq claude-multi--current-ediff-agent nil)
    (setq claude-multi--ediff-control-buffer nil)
    (message "Review completed: %d accepted, %d rejected"
             (length accepted) (length rejected))))

;;; Interactive commands

;;;###autoload
(defun claude-multi/review-agent-changes ()
  "Start interactive review of changes for selected agent."
  (interactive)
  (if (null claude-multi--agents)
      (message "No active agents to review")
    (let ((agent (claude-multi--select-agent claude-multi--agents "Review changes for: ")))
      (when agent
        (let ((changed-files (claude-multi-ediff--get-changed-files agent)))
          (if (null changed-files)
              (message "No changes to review for %s" (claude-agent-name agent))
            (claude-multi-ediff--create-session agent changed-files)
            (let* ((session (claude-agent-ediff-session agent))
                   (first-file (car changed-files)))
              (plist-put session :files-to-review (cdr changed-files))
              (claude-multi-ediff--show-diff agent first-file))))))))

;;;###autoload
(defun claude-multi/accept-current-diff ()
  "Accept current file changes in active ediff review."
  (interactive)
  (if claude-multi--current-ediff-agent
      (claude-multi-ediff--accept-changes claude-multi--current-ediff-agent)
    (message "No active ediff review session")))

;;;###autoload
(defun claude-multi/reject-current-diff ()
  "Reject current file changes in active ediff review."
  (interactive)
  (if claude-multi--current-ediff-agent
      (claude-multi-ediff--reject-changes claude-multi--current-ediff-agent)
    (message "No active ediff review session")))

;;;###autoload
(defun claude-multi/next-diff-file ()
  "Skip to next file in active ediff review."
  (interactive)
  (if claude-multi--current-ediff-agent
      (claude-multi-ediff--next-or-complete claude-multi--current-ediff-agent)
    (message "No active ediff review session")))

(defun claude-multi-ediff--accept-current ()
  "Internal: accept current file."
  (interactive)
  (when claude-multi--current-ediff-agent
    (claude-multi-ediff--accept-changes claude-multi--current-ediff-agent)))

(defun claude-multi-ediff--reject-current ()
  "Internal: reject current file."
  (interactive)
  (when claude-multi--current-ediff-agent
    (claude-multi-ediff--reject-changes claude-multi--current-ediff-agent)))

(defun claude-multi-ediff--next-file ()
  "Internal: move to next file."
  (interactive)
  (when claude-multi--current-ediff-agent
    (claude-multi-ediff--next-or-complete claude-multi--current-ediff-agent)))

;;;###autoload
(defun claude-multi-ediff--set-mcp-request-id (agent request-id)
  "Set MCP REQUEST-ID for AGENT's ediff session."
  (let ((session (claude-agent-ediff-session agent)))
    (when session
      (plist-put session :mcp-request-id request-id))))

(provide 'claude-multi-ediff)
;;; claude-multi-ediff.el ends here
