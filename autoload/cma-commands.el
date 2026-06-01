;; -*- lexical-binding: t; -*-
;;; autoload/cma-commands.el --- Interactive commands backed by cma CLI

;;; Commentary:
;; Thin interactive wrappers that shell out to `cma` for agent orchestration.
;; Function names match the existing claude-multi/ namespace so keybindings work.

;;; Code:

(require 'cma-core)

(declare-function cma-table/refresh "cma-table")

(defvar claude-multi-default-model "sonnet"
  "Default model for spawning agents.  Overridden by config.el defcustom.")

(defvar claude-multi-worktree-location 'adjacent
  "Where to create worktrees.  Overridden by config.el defcustom.")

;;;###autoload
(defun cma/spawn-agent ()
  "Spawn a new Claude agent via cma CLI.
Prompts for task description, working directory, agent name, and model."
  (interactive)
  (let* ((task (string-trim (read-string "Task description: ")))
         (dir (read-directory-name "Working directory: " default-directory nil t))
         (name (string-trim (read-string "Agent name (empty for auto): ")))
         (model (completing-read "Model: " '("sonnet" "opus" "opusplan" "haiku")
                                 nil nil nil nil claude-multi-default-model))
         (args (list "spawn" "--task" task "--dir" (expand-file-name dir) "--json"))
         (args (if (not (string-empty-p name))
                   (append args (list "--name" name))
                 args))
         (args (if (and model (not (string-empty-p model)))
                   (append args (list "--model" model))
                 args))
         (result (apply #'cma--call args)))
    (if result
        (let* ((agent (alist-get 'agent result))
               (name (alist-get 'id agent))
               (pane-id (alist-get 'pane_id agent)))
          (message "Spawned agent %s (pane %s, model %s)" name pane-id model)
          (when (fboundp 'cma-table/refresh)
            (cma-table/refresh)))
      (message "Spawn failed: %s" (or cma--last-error "unknown error")))))

;;;###autoload
(defun cma/spawn-agent-with-worktree ()
  "Spawn a Claude agent with git worktree isolation via cma CLI.
Prompts for task, directory, branch/worktree name, agent name, and model.
When `claude-multi-worktree-location' is \\='claude, the prompt asks for a
worktree name (passed as --branch to cma, which routes it to --worktree)."
  (interactive)
  (let* ((task (string-trim (read-string "Task description: ")))
         (dir (read-directory-name "Working directory: " default-directory nil t))
         (claude-mode (eq claude-multi-worktree-location 'claude))
         (branch (string-trim (read-string (if claude-mode "Worktree name: " "Branch name: "))))
         (name (read-string "Agent name (empty for auto): "))
         (model (completing-read "Model: " '("sonnet" "opus" "opusplan" "haiku")
                                 nil nil nil nil claude-multi-default-model))
         (args (list "spawn" "--task" task "--dir" (expand-file-name dir) "--json"))
         (args (if (and branch (not (string-empty-p branch)))
                   (append args (list "--branch" branch))
                 args))
         (args (if (not (string-empty-p name))
                   (append args (list "--name" name))
                 args))
         (args (if (and model (not (string-empty-p model)))
                   (append args (list "--model" model))
                 args))
         (result (apply #'cma--call args)))
    (if result
        (let* ((agent (alist-get 'agent result))
               (name (alist-get 'name agent)))
          (message "Spawned agent %s with worktree (%s: %s, model: %s)"
                   name
                   (if claude-mode "worktree" "branch")
                   branch model)
          (when (fboundp 'cma-table/refresh)
            (cma-table/refresh)))
      (message "Spawn failed: %s" (or cma--last-error "unknown error")))))

;;; Worktree cleanup helpers

(defun cma--maybe-cleanup-worktree (branch)
  "Prompt to remove worktree for BRANCH if one exists.
Checks `cma worktree list --json` and offers removal with optional branch deletion."
  (when (and branch (not (string-empty-p branch)))
    (let ((worktrees (cma--call "worktree" "list" "--json")))
      (when (and worktrees
                 (cl-find-if (lambda (wt)
                               (string= (alist-get 'branch wt) branch))
                             worktrees))
        (when (y-or-n-p (format "Remove worktree for branch '%s'? " branch))
          (let ((delete-branch (y-or-n-p (format "Also delete branch '%s'? " branch))))
            (if delete-branch
                (cma--call-raw "worktree" "remove" branch "--delete-branch")
              (cma--call-raw "worktree" "remove" branch))
            (message "Removed worktree for %s%s" branch
                     (if delete-branch " (branch deleted)" ""))))))))

(defun cma--maybe-cleanup-worktrees-batch (branches)
  "Prompt to clean up worktrees for a list of BRANCHES.
Skips nil and empty branch names."
  (let ((valid-branches (cl-remove-if-not
                         (lambda (b) (and b (not (string-empty-p b))))
                         branches)))
    (when valid-branches
      (dolist (branch valid-branches)
        (cma--maybe-cleanup-worktree branch)))))

;;;###autoload
(defun cma/kill-agent ()
  "Kill a specific Claude agent via cma CLI.
After killing, offers to clean up associated worktree."
  (interactive)
  (let* ((agents (cma--call "list" "--json"))
         ;; Store full alist so we can extract git_branch after kill
         (names (mapcar (lambda (a)
                          (cons (format "%s [%s] - %s"
                                        (alist-get 'name a)
                                        (upcase (alist-get 'status a))
                                        (or (alist-get 'cwd a) ""))
                                a))
                        agents))
         (choice (completing-read "Kill agent: " names nil t))
         (agent-alist (cdr (assoc choice names))))
    (when agent-alist
      (let ((agent-id (alist-get 'agent_id agent-alist))
            (branch (alist-get 'git_branch agent-alist))
            (agent-name (car (split-string choice " "))))
        (when (y-or-n-p (format "Really kill agent %s? " agent-name))
          (cma--call-raw "kill" agent-id)
          (message "Killed agent %s" agent-name)
          (cma--maybe-cleanup-worktree branch)
          (when (fboundp 'cma-table/refresh)
            (cma-table/refresh)))))))

;;;###autoload
(defun cma/kill-all-agents ()
  "Kill all Claude agents via cma CLI.
After killing, offers to clean up associated worktrees."
  (interactive)
  (let ((agents (cma--call "list" "--json")))
    (when (and agents
               (y-or-n-p (format "Really kill all %d agents? " (length agents))))
      ;; Collect branches BEFORE killing (kill deletes status files)
      (let ((branches (mapcar (lambda (a) (alist-get 'git_branch a)) agents)))
        (cma--call-raw "kill" "--all")
        (message "All agents killed")
        (cma--maybe-cleanup-worktrees-batch branches)
        (when (fboundp 'cma-table/refresh)
          (cma-table/refresh))))))

;;;###autoload
(defun cma/focus-agent ()
  "Focus on a specific agent's terminal window via cma CLI."
  (interactive)
  (let* ((agents (cma--call "list" "--json"))
         (names (mapcar (lambda (a)
                          (cons (format "%s [%s] - %s"
                                        (alist-get 'name a)
                                        (upcase (alist-get 'status a))
                                        (or (alist-get 'cwd a) ""))
                                (alist-get 'agent_id a)))
                        agents))
         (choice (completing-read "Focus agent: " names nil t))
         (agent-id (cdr (assoc choice names))))
    (when agent-id
      (cma--call-raw "focus" agent-id)
      (message "Focused on %s" (car (split-string choice " "))))))

;;;###autoload
(defun cma/rename-agent ()
  "Rename an agent via cma CLI."
  (interactive)
  (let* ((agents (cma--call "list" "--json"))
         (names (mapcar (lambda (a)
                          (cons (format "%s [%s]"
                                        (alist-get 'name a)
                                        (alist-get 'agent_id a))
                                (alist-get 'agent_id a)))
                        agents))
         (choice (completing-read "Rename agent: " names nil t))
         (agent-id (cdr (assoc choice names)))
         (new-name (read-string "New name: ")))
    (when (and agent-id (not (string-empty-p new-name)))
      (cma--call-raw "rename" agent-id new-name)
      (message "Renamed to %s" new-name)
      (when (fboundp 'cma-table/refresh)
        (cma-table/refresh)))))

;;;###autoload
(defun cma/save-session ()
  "Save current agent session via cma CLI."
  (interactive)
  (let ((output (cma--call-raw "session" "save")))
    (message "%s" (or output "Session saved"))))

;;;###autoload
(defun cma/restore-session ()
  "Restore an agent session via cma CLI."
  (interactive)
  (let* ((sessions (cma--call "session" "list" "--json"))
         (choices (mapcar (lambda (s)
                           (cons (format "%s (%d agents)"
                                         (alist-get 'filename s)
                                         (alist-get 'agent_count s))
                                 (alist-get 'filename s)))
                         sessions)))
    (if (null choices)
        (message "No saved sessions found")
      (let* ((choice (completing-read "Restore session: " choices nil t))
             (filename (cdr (assoc choice choices))))
        (when filename
          (let ((output (cma--call-raw "session" "restore" filename)))
            (message "%s" (or output "Session restored"))))))))

;;;###autoload
(defun cma/list-sessions ()
  "List saved sessions via cma CLI."
  (interactive)
  (let ((output (cma--call-raw "session" "list")))
    (if output
        (with-current-buffer (get-buffer-create "*CMA Sessions*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert output))
          (special-mode)
          (display-buffer (current-buffer)))
      (message "No saved sessions"))))

;;;###autoload
(defun cma/delete-session ()
  "Delete a saved session via cma CLI."
  (interactive)
  (let* ((sessions (cma--call "session" "list" "--json"))
         (choices (mapcar (lambda (s)
                           (cons (format "%s (%d agents)"
                                         (alist-get 'filename s)
                                         (alist-get 'agent_count s))
                                 (alist-get 'filename s)))
                         sessions)))
    (if (null choices)
        (message "No saved sessions")
      (let* ((choice (completing-read "Delete session: " choices nil t))
             (filename (cdr (assoc choice choices))))
        (when (and filename (y-or-n-p (format "Delete %s? " filename)))
          (cma--call-raw "session" "delete" filename)
          (message "Deleted %s" filename))))))

;;;###autoload
(defun cma/list-worktrees ()
  "List git worktrees via cma CLI."
  (interactive)
  (let ((output (cma--call-raw "worktree" "list")))
    (if output
        (with-current-buffer (get-buffer-create "*CMA Worktrees*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert output))
          (special-mode)
          (display-buffer (current-buffer)))
      (message "No worktrees found or not in a git repository"))))

;;;###autoload
(defun cma/worktree-create ()
  "Create a new git worktree via cma CLI.
Prompts for a branch name."
  (interactive)
  (let* ((branch (string-trim (read-string "Branch name for worktree: ")))
         (result (when (not (string-empty-p branch))
                   (cma--call "worktree" "create" branch "--json"))))
    (if result
        (message "Created worktree for branch '%s'" branch)
      (message "Worktree creation failed: %s"
               (or cma--last-error "unknown error or empty branch")))))

;;;###autoload
(defun cma/worktree-remove ()
  "Remove a git worktree via cma CLI.
Offers selection from existing worktrees and optional branch deletion."
  (interactive)
  (let ((worktrees (cma--call "worktree" "list" "--json")))
    (if (or (null worktrees) (eq worktrees :null))
        (message "No worktrees found")
      (let* ((choices (mapcar (lambda (wt)
                                (let ((branch (or (alist-get 'branch wt) ""))
                                      (path (or (alist-get 'path wt) "")))
                                  (cons (format "%s (%s)" branch path)
                                        branch)))
                              worktrees))
             (choice (completing-read "Remove worktree: " choices nil t))
             (branch (cdr (assoc choice choices))))
        (when (and branch (not (string-empty-p branch)))
          (when (y-or-n-p (format "Remove worktree for branch '%s'? " branch))
            (let ((delete-branch (y-or-n-p (format "Also delete branch '%s'? " branch))))
              (if delete-branch
                  (cma--call-raw "worktree" "remove" branch "--delete-branch")
                (cma--call-raw "worktree" "remove" branch))
              (message "Removed worktree for %s%s" branch
                       (if delete-branch " (branch deleted)" "")))))))))

;;;###autoload
(defun cma/worktree-prune ()
  "Prune merged/gone worktrees via cma CLI.
Claude-managed worktrees are excluded unless --include-claude is passed."
  (interactive)
  (let ((output (cma--call-raw "worktree" "prune")))
    (message "%s" (or output "No worktrees to prune"))))

;;;###autoload
(defun cma/worktree-clean ()
  "Remove all Claude-managed worktrees (.claude/worktrees/) via cma CLI."
  (interactive)
  (when (y-or-n-p "Remove all Claude-managed worktrees? ")
    (let ((output (cma--call-raw "worktree" "clean" "--force")))
      (message "%s" (or output "No Claude worktrees to clean")))))

(provide 'cma-commands)
;;; cma-commands.el ends here
