;; -*- lexical-binding: t; -*-
;;; autoload/worktree.el --- Git worktree management for Claude Multi-Agent

;;; Commentary:
;; Handles creation and deletion of git worktrees for agent isolation
;; Each agent gets its own worktree to work in without conflicts

;;; Code:

(require 'f)
(require 's)

;;; Worktree detection

;;;###autoload
(defun claude-multi--in-git-repo-p ()
  "Return non-nil if the current directory is inside a git repository."
  (let ((default-directory (expand-file-name default-directory)))
    (= 0 (call-process "git" nil nil nil "rev-parse" "--git-dir"))))

;;;###autoload
(defun claude-multi--get-git-root ()
  "Return the root directory of the current git repository."
  (when (claude-multi--in-git-repo-p)
    (let ((root (string-trim
                 (shell-command-to-string "git rev-parse --show-toplevel"))))
      (when (and root (not (string-empty-p root)))
        root))))

(defun claude-multi--get-current-branch ()
  "Return the name of the current git branch."
  (string-trim
   (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))

(defun claude-multi--get-default-branch ()
  "Return the default branch name (main or master) of the repository."
  (when (claude-multi--in-git-repo-p)
    (let ((default-directory (claude-multi--get-git-root)))
      ;; Try to get default branch from origin/HEAD
      (let ((remote-head (string-trim
                         (shell-command-to-string
                          "git symbolic-ref refs/remotes/origin/HEAD 2>/dev/null"))))
        (if (and remote-head (not (string-empty-p remote-head)))
            ;; Extract branch name from refs/remotes/origin/HEAD
            (replace-regexp-in-string "^refs/remotes/origin/" "" remote-head)
          ;; Fallback: check if main or master exists
          (cond
           ((= 0 (call-process "git" nil nil nil "show-ref" "--verify" "--quiet" "refs/heads/main"))
            "main")
           ((= 0 (call-process "git" nil nil nil "show-ref" "--verify" "--quiet" "refs/heads/master"))
            "master")
           ;; Default to main if neither exists
           (t "main")))))))

;;; Worktree creation

;;;###autoload
(defun claude-multi--build-worktree-command (agent repo-root worktree-path branch-name)
  "Build shell command to create worktree for AGENT in kitty terminal.
REPO-ROOT is the git repository root directory.
WORKTREE-PATH is the target path for the worktree.
BRANCH-NAME is the branch name for the worktree.
Returns a shell command string that creates the worktree and starts Claude."
  (let* ((default-directory repo-root)  ; Set context for git commands
         (default-branch (claude-multi--get-default-branch))
         (claude-cmd (or claude-multi-claude-command "claude"))
         ;; Build the gwt fallback command
         ;; Check if branch already exists
         (branch-exists-p (= 0 (call-process "git" nil nil nil
                                            "rev-parse" "--verify"
                                            "--quiet" branch-name)))
         (git-worktree-cmd (if branch-exists-p
                              (format "git worktree add '%s' '%s'"
                                     worktree-path branch-name)
                            (format "git worktree add '%s' -b '%s' HEAD"
                                   worktree-path branch-name)))
         ;; Get parent directory to ensure it exists
         (worktree-parent (file-name-directory worktree-path)))
    ;; Build the complete command chain
    ;; Note: gwt likely creates parent directories, but git worktree add doesn't
    (format "cd '%s' && git fetch origin %s && git rebase origin/%s && (gwt '%s' || (mkdir -p '%s' && %s)) && cd '%s' && %s"
           repo-root
           default-branch
           default-branch
           branch-name
           worktree-parent
           git-worktree-cmd
           worktree-path
           claude-cmd)))

;;;###autoload
(defun claude-multi--create-worktree (agent)
  "Create a git worktree for AGENT.
Returns the path to the worktree, or nil if creation failed."
  (condition-case err
      (let* ((repo-root (claude-multi--get-git-root))
             (repo-name (file-name-nondirectory repo-root))
             (agent-id (claude-agent-id agent))
             (branch-name (or (claude-agent-branch-name agent)
                             (format "claude/%s" agent-id)))
             (worktree-path (or (claude-agent-worktree-path agent)
                               (claude-multi--determine-worktree-path repo-root repo-name branch-name))))

        ;; Ensure the worktree parent directory exists
        (let ((worktree-parent (file-name-directory worktree-path)))
          (unless (file-exists-p worktree-parent)
            (make-directory worktree-parent t)))

        ;; Check if worktree already exists
        (when (file-exists-p worktree-path)
          (error "Worktree path already exists: %s" worktree-path))

        ;; Create the worktree with a new branch or checkout existing branch
        (let ((default-directory repo-root))
          (with-temp-buffer
            (let* ((branch-exists (= 0 (call-process "git" nil nil nil
                                                     "rev-parse" "--verify"
                                                     branch-name)))
                   (exit-code (if branch-exists
                                  ;; Branch exists, check it out
                                  (call-process "git" nil t nil
                                               "worktree" "add"
                                               worktree-path
                                               branch-name)
                                ;; Branch doesn't exist, create new one
                                (call-process "git" nil t nil
                                             "worktree" "add"
                                             "-b" branch-name
                                             worktree-path
                                             "HEAD"))))
              (unless (= 0 exit-code)
                (error "Git worktree add failed: %s" (buffer-string))))))

        (message "Created worktree for %s at %s (branch: %s)"
                (claude-agent-name agent) worktree-path branch-name)
        worktree-path)

    (error
     (message "Failed to create worktree for %s: %s"
              (claude-agent-name agent)
              (error-message-string err))
     ;; Return nil to indicate failure - agent will use current directory
     nil)))

(defun claude-multi--determine-worktree-path (repo-root repo-name branch-name)
  "Determine the worktree path based on configuration.
REPO-ROOT is the git repository root.
REPO-NAME is the name of the repository.
BRANCH-NAME is the branch name for the worktree.

When using 'adjacent mode, this mimics gwt behavior:
worktrees are created as siblings to the main repo at ../<repo-name>-<branch-name>"
  (pcase claude-multi-worktree-location
    ('adjacent
     ;; Create worktrees as siblings to repo (same as gwt does)
     ;; e.g., /Users/user/projects/repo-name-branch-name
     (expand-file-name (format "../%s-%s" repo-name branch-name) repo-root))
    ('internal
     ;; Create worktrees in .git/worktrees/
     (expand-file-name (format ".git/worktrees/%s" branch-name) repo-root))
    (_
     ;; Default to adjacent (gwt-style)
     (expand-file-name (format "../%s-%s" repo-name branch-name) repo-root))))

;;; Worktree deletion

;;;###autoload
(defun claude-multi--delete-worktree (agent)
  "Delete the worktree associated with AGENT."
  (when-let ((worktree-path (claude-agent-worktree-path agent)))
    (condition-case err
        (progn
          ;; First, remove the worktree using git
          (let ((default-directory (claude-multi--get-git-root)))
            (with-temp-buffer
              (let ((exit-code (call-process "git" nil t nil
                                            "worktree" "remove"
                                            "--force"
                                            worktree-path)))
                (unless (= 0 exit-code)
                  (error "Git worktree remove failed: %s" (buffer-string))))))

          ;; Delete the branch
          (let ((branch-name (format "claude/%s" (claude-agent-id agent)))
                (default-directory (claude-multi--get-git-root)))
            (call-process "git" nil nil nil
                         "branch" "-D" branch-name))

          (message "Deleted worktree for %s" (claude-agent-name agent)))

      (error
       (message "Failed to delete worktree for %s: %s"
                (claude-agent-name agent)
                (error-message-string err))
       ;; Try to clean up the directory manually
       (when (file-exists-p worktree-path)
         (ignore-errors
           (delete-directory worktree-path t)))))))

;;; Worktree listing

;;;###autoload
(defun claude-multi--list-worktrees ()
  "Return a list of all git worktrees in the current repository."
  (when (claude-multi--in-git-repo-p)
    (let ((default-directory (claude-multi--get-git-root)))
      (with-temp-buffer
        (when (= 0 (call-process "git" nil t nil "worktree" "list" "--porcelain"))
          (goto-char (point-min))
          (let (worktrees)
            (while (re-search-forward "^worktree \\(.+\\)$" nil t)
              (push (match-string 1) worktrees))
            (nreverse worktrees)))))))

;;;###autoload
(defun claude-multi/list-worktrees ()
  "Interactively display all git worktrees."
  (interactive)
  (if-let ((worktrees (claude-multi--list-worktrees)))
      (let ((buf (get-buffer-create "*Claude Worktrees.org*")))
        (with-current-buffer buf
          (read-only-mode -1)
          (erase-buffer)
          (insert "#+TITLE: Claude Multi-Agent Worktrees\n\n")
          (dolist (worktree worktrees)
            (insert (format "- %s\n" worktree)))
          (org-mode)
          (goto-char (point-min))
          (read-only-mode 1))
        (display-buffer buf))
    (message "No git worktrees found or not in a git repository")))

;;; Cleanup utilities

;;;###autoload
(defun claude-multi/cleanup-orphaned-worktrees ()
  "Clean up worktrees that don't have corresponding active agents."
  (interactive)
  (when (claude-multi--in-git-repo-p)
    (let* ((all-worktrees (claude-multi--list-worktrees))
           (agent-worktrees (delq nil (mapcar #'claude-agent-worktree-path
                                             claude-multi--agents)))
           (orphaned (cl-set-difference all-worktrees agent-worktrees
                                       :test #'string=)))
      ;; Filter to only Claude agent worktrees
      (setq orphaned (cl-remove-if-not
                     (lambda (path) (string-match-p "claude" path))
                     orphaned))

      (if orphaned
          (when (y-or-n-p (format "Found %d orphaned worktree(s). Clean them up? "
                                 (length orphaned)))
            (dolist (worktree orphaned)
              (condition-case err
                  (progn
                    (let ((default-directory (claude-multi--get-git-root)))
                      (call-process "git" nil nil nil
                                   "worktree" "remove" "--force" worktree))
                    (message "Removed orphaned worktree: %s" worktree))
                (error
                 (message "Failed to remove worktree %s: %s"
                         worktree (error-message-string err))))))
        (message "No orphaned worktrees found")))))

;;; Validation utilities

;;;###autoload
(defun claude-multi--validate-git-repo ()
  "Validate that the current directory is a git repository.
Display helpful error messages if not."
  (unless (executable-find "git")
    (user-error "Git executable not found. Please install git"))
  (unless (claude-multi--in-git-repo-p)
    (user-error "Not in a git repository. Please run this from a git repository")))

(defun claude-multi--check-uncommitted-changes ()
  "Return non-nil if there are uncommitted changes in the repository."
  (when (claude-multi--in-git-repo-p)
    (let ((default-directory (claude-multi--get-git-root)))
      (with-temp-buffer
        (call-process "git" nil t nil "status" "--porcelain")
        (> (buffer-size) 0)))))

;;;###autoload
(defun claude-multi--warn-uncommitted-changes ()
  "Warn the user if there are uncommitted changes.
Returns t to continue, nil to abort."
  (if (claude-multi--check-uncommitted-changes)
      (yes-or-no-p "You have uncommitted changes. Continue creating worktrees? ")
    t))

;;; Worktree status

;;;###autoload
(defun claude-multi/worktree-status (agent)
  "Show the git status of AGENT's worktree."
  (interactive
   (list (claude-multi--select-agent claude-multi--agents "Show worktree status for: ")))
  (if-let ((worktree-path (claude-agent-worktree-path agent)))
      (let ((default-directory worktree-path))
        (magit-status))
    (message "Agent %s has no worktree" (claude-agent-name agent))))

(provide 'claude-multi-worktree)
;;; worktree.el ends here
