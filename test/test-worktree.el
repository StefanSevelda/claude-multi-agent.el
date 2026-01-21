;; -*- lexical-binding: t; -*-
;;; test-worktree.el --- Tests for claude-multi-worktree module

;;; Commentary:
;; Comprehensive tests for git worktree management including:
;; - Git repository detection
;; - Branch operations (current, default)
;; - Worktree path determination (adjacent, internal)
;; - Worktree creation and listing
;; - Validation and error handling

;;; Code:

(require 'buttercup)
(load-file "test/test-helper.el")
(require 'claude-multi-worktree)
(require 'claude-multi-agents)

;;; Test variables - define variables that are normally in config.el

(defvar claude-multi-claude-command "claude"
  "Command to launch Claude (for testing).")

(defvar claude-multi-worktree-location 'adjacent
  "Worktree location mode (for testing).")

;;; Test fixtures

(defvar test-worktree--temp-git-repo nil
  "Temporary git repository for testing.")

(defun test-worktree--setup-temp-repo ()
  "Create a temporary git repository for testing."
  (let ((temp-dir (make-temp-file "claude-worktree-test-" t)))
    (setq test-worktree--temp-git-repo temp-dir)
    (let ((default-directory temp-dir))
      ;; Initialize git repo
      (call-process "git" nil nil nil "init")
      (call-process "git" nil nil nil "config" "user.name" "Test User")
      (call-process "git" nil nil nil "config" "user.email" "test@example.com")
      ;; Create initial commit
      (write-region "test" nil "README.md")
      (call-process "git" nil nil nil "add" "README.md")
      (call-process "git" nil nil nil "commit" "-m" "Initial commit"))
    temp-dir))

(defun test-worktree--cleanup-temp-repo ()
  "Clean up temporary git repository."
  (when (and test-worktree--temp-git-repo
             (file-exists-p test-worktree--temp-git-repo))
    (delete-directory test-worktree--temp-git-repo t)
    (setq test-worktree--temp-git-repo nil)))

;;; Git Detection Tests

(describe "Git Repository Detection"

  (describe "claude-multi--in-git-repo-p"

    (it "detects when in a git repository"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let ((default-directory temp-repo))
              (expect (claude-multi--in-git-repo-p) :to-be-truthy))
          (test-worktree--cleanup-temp-repo))))

    (it "returns nil when not in a git repository"
      (let ((temp-dir (make-temp-file "not-a-repo-" t)))
        (unwind-protect
            (let ((default-directory temp-dir))
              (expect (claude-multi--in-git-repo-p) :not :to-be-truthy))
          (delete-directory temp-dir t)))))

  (describe "claude-multi--get-git-root"

    (it "returns the repository root directory"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let ((default-directory temp-repo))
              ;; Use file-truename to resolve symlinks (macOS /var -> /private/var)
              (expect (file-truename (claude-multi--get-git-root))
                      :to-equal (file-truename temp-repo)))
          (test-worktree--cleanup-temp-repo))))

    (it "returns nil when not in a git repository"
      (let ((temp-dir (make-temp-file "not-a-repo-" t)))
        (unwind-protect
            (let ((default-directory temp-dir))
              (expect (claude-multi--get-git-root) :to-be nil))
          (delete-directory temp-dir t))))

    (it "returns root even when in a subdirectory"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let* ((subdir (expand-file-name "subdir" temp-repo)))
              (make-directory subdir)
              (let ((default-directory subdir))
                ;; Use file-truename to resolve symlinks
                (expect (file-truename (claude-multi--get-git-root))
                        :to-equal (file-truename temp-repo))))
          (test-worktree--cleanup-temp-repo))))))

;;; Branch Operations Tests

(describe "Branch Operations"

  (describe "claude-multi--get-current-branch"

    (it "returns the current branch name"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let ((default-directory temp-repo))
              ;; Git init creates 'main' or 'master' branch
              (let ((branch (claude-multi--get-current-branch)))
                (expect (or (equal branch "main")
                           (equal branch "master"))
                        :to-be-truthy)))
          (test-worktree--cleanup-temp-repo))))

    (it "returns new branch name after switching"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let ((default-directory temp-repo))
              (call-process "git" nil nil nil "checkout" "-b" "feature-branch")
              (expect (claude-multi--get-current-branch) :to-equal "feature-branch"))
          (test-worktree--cleanup-temp-repo)))))

  (describe "claude-multi--get-default-branch"

    (it "returns main when it exists"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let ((default-directory temp-repo))
              ;; Ensure we're on main/master
              (call-process "git" nil nil nil "branch" "-M" "main")
              (expect (claude-multi--get-default-branch) :to-equal "main"))
          (test-worktree--cleanup-temp-repo))))

    (it "returns master when main doesn't exist"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let ((default-directory temp-repo))
              ;; Rename to master
              (call-process "git" nil nil nil "branch" "-M" "master")
              (expect (claude-multi--get-default-branch) :to-equal "master"))
          (test-worktree--cleanup-temp-repo))))))

;;; Worktree Path Determination Tests

(describe "Worktree Path Determination"

  (describe "claude-multi--determine-worktree-path"

    (it "uses adjacent mode by default"
      (let ((claude-multi-worktree-location 'adjacent)
            (repo-root "/home/user/projects/my-repo")
            (repo-name "my-repo")
            (branch-name "feature/test"))
        (let ((path (claude-multi--determine-worktree-path repo-root repo-name branch-name)))
          (expect path :to-equal "/home/user/projects/my-repo-feature/test"))))

    (it "creates adjacent worktrees as siblings"
      (let ((claude-multi-worktree-location 'adjacent)
            (repo-root "/home/user/projects/my-repo")
            (repo-name "my-repo")
            (branch-name "claude/agent-1"))
        (let ((path (claude-multi--determine-worktree-path repo-root repo-name branch-name)))
          (expect path :to-equal "/home/user/projects/my-repo-claude/agent-1"))))

    (it "uses internal mode when configured"
      (let ((claude-multi-worktree-location 'internal)
            (repo-root "/home/user/projects/my-repo")
            (repo-name "my-repo")
            (branch-name "feature/test"))
        (let ((path (claude-multi--determine-worktree-path repo-root repo-name branch-name)))
          (expect path :to-equal "/home/user/projects/my-repo/.git/worktrees/feature/test"))))

    (it "defaults to adjacent for unknown modes"
      (let ((claude-multi-worktree-location 'unknown)
            (repo-root "/home/user/projects/my-repo")
            (repo-name "my-repo")
            (branch-name "test"))
        (let ((path (claude-multi--determine-worktree-path repo-root repo-name branch-name)))
          (expect path :to-equal "/home/user/projects/my-repo-test"))))))

;;; Worktree Command Building Tests

(describe "Worktree Command Building"

  (describe "claude-multi--build-worktree-command"

    (before-each
      (setq claude-multi-claude-command "claude"))

    (it "builds command with default branch rebase"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let ((default-directory temp-repo))
              (call-process "git" nil nil nil "branch" "-M" "main")
              (let* ((agent (make-claude-agent :id "test-1"))
                     (worktree-path "/tmp/worktree-path")
                     (branch-name "feature/test")
                     (window-id "123")
                     (cmd (claude-multi--build-worktree-command
                           agent temp-repo worktree-path branch-name window-id)))
                (expect cmd :to-match "git fetch origin main")
                (expect cmd :to-match "git rebase origin/main")
                (expect cmd :to-match "gwt 'feature/test'")
                (expect cmd :to-match "KITTY_WINDOW_ID=123")))
          (test-worktree--cleanup-temp-repo))))

    (it "includes fallback to git worktree add"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let ((default-directory temp-repo))
              (let* ((agent (make-claude-agent :id "test-1"))
                     (worktree-path "/tmp/worktree-path")
                     (branch-name "feature/test")
                     (window-id "123")
                     (cmd (claude-multi--build-worktree-command
                           agent temp-repo worktree-path branch-name window-id)))
                (expect cmd :to-match "git worktree add")))
          (test-worktree--cleanup-temp-repo))))

    (it "uses custom claude command when configured"
      (let ((claude-multi-claude-command "custom-claude")
            (temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let ((default-directory temp-repo))
              (let* ((agent (make-claude-agent :id "test-1"))
                     (cmd (claude-multi--build-worktree-command
                           agent temp-repo "/tmp/path" "branch" "123")))
                (expect cmd :to-match "custom-claude")))
          (test-worktree--cleanup-temp-repo))))))

;;; Worktree Creation Tests

(describe "Worktree Creation"

  (describe "claude-multi--create-worktree"

    (it "creates worktree for new branch"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let* ((default-directory temp-repo)
                   (agent (make-claude-agent
                          :id "test-1"
                          :name "Test Agent"
                          :branch-name "test-branch"))
                   (worktree-path (claude-multi--create-worktree agent)))
              (expect worktree-path :to-be-truthy)
              (expect (file-exists-p worktree-path) :to-be-truthy))
          (test-worktree--cleanup-temp-repo))))

    (it "returns nil when worktree creation fails"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let* ((default-directory temp-repo)
                   (agent (make-claude-agent
                          :id "test-1"
                          :name "Test Agent"
                          :worktree-path "/invalid/path/that/cannot/be/created"
                          :branch-name "test-branch")))
              ;; Should return nil on failure
              (expect (claude-multi--create-worktree agent) :to-be nil))
          (test-worktree--cleanup-temp-repo))))

    (it "fails when worktree path already exists"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let* ((default-directory temp-repo)
                   (existing-path (expand-file-name "existing" temp-repo))
                   (agent (make-claude-agent
                          :id "test-1"
                          :name "Test Agent"
                          :worktree-path existing-path
                          :branch-name "test-branch")))
              ;; Create the path first
              (make-directory existing-path)
              ;; Should return nil when path exists
              (expect (claude-multi--create-worktree agent) :to-be nil))
          (test-worktree--cleanup-temp-repo))))

    (it "handles existing branch checkout"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let ((default-directory temp-repo))
              ;; Create a branch first
              (call-process "git" nil nil nil "checkout" "-b" "existing-branch")
              (call-process "git" nil nil nil "checkout" "main")
              ;; Now create worktree for existing branch
              (let* ((agent (make-claude-agent
                            :id "test-1"
                            :name "Test Agent"
                            :branch-name "existing-branch"))
                     (worktree-path (claude-multi--create-worktree agent)))
                (expect worktree-path :to-be-truthy)
                (expect (file-exists-p worktree-path) :to-be-truthy)))
          (test-worktree--cleanup-temp-repo))))))

;;; Worktree Listing Tests

(describe "Worktree Listing"

  (describe "claude-multi--list-worktrees"

    (it "lists all worktrees in repository"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let ((default-directory temp-repo))
              (let ((worktrees (claude-multi--list-worktrees)))
                ;; Should at least have the main worktree
                (expect worktrees :to-be-truthy)
                (expect (length worktrees) :to-be-greater-than 0)
                (expect (member temp-repo worktrees) :to-be-truthy)))
          (test-worktree--cleanup-temp-repo))))

    (it "returns nil when not in git repository"
      (let ((temp-dir (make-temp-file "not-a-repo-" t)))
        (unwind-protect
            (let ((default-directory temp-dir))
              (expect (claude-multi--list-worktrees) :to-be nil))
          (delete-directory temp-dir t))))

    (it "includes newly created worktrees"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let* ((default-directory temp-repo)
                   (agent (make-claude-agent
                          :id "test-1"
                          :name "Test Agent"
                          :branch-name "new-worktree"))
                   (worktree-path (claude-multi--create-worktree agent)))
              (let ((worktrees (claude-multi--list-worktrees)))
                (expect (member worktree-path worktrees) :to-be-truthy)))
          (test-worktree--cleanup-temp-repo))))))

;;; Validation Tests

(describe "Validation Functions"

  (describe "claude-multi--validate-git-repo"

    (it "passes when git is available and in repo"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let ((default-directory temp-repo))
              (expect (claude-multi--validate-git-repo) :not :to-throw))
          (test-worktree--cleanup-temp-repo))))

    (it "throws error when not in git repo"
      (let ((temp-dir (make-temp-file "not-a-repo-" t)))
        (unwind-protect
            (let ((default-directory temp-dir))
              (expect (claude-multi--validate-git-repo) :to-throw 'user-error))
          (delete-directory temp-dir t)))))

  (describe "claude-multi--check-uncommitted-changes"

    (it "returns nil when no uncommitted changes"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let ((default-directory temp-repo))
              (expect (claude-multi--check-uncommitted-changes) :not :to-be-truthy))
          (test-worktree--cleanup-temp-repo))))

    (it "returns t when there are uncommitted changes"
      (let ((temp-repo (test-worktree--setup-temp-repo)))
        (unwind-protect
            (let ((default-directory temp-repo))
              ;; Create uncommitted change
              (write-region "modified" nil "README.md")
              (expect (claude-multi--check-uncommitted-changes) :to-be-truthy))
          (test-worktree--cleanup-temp-repo))))

    (it "returns nil when not in git repository"
      (let ((temp-dir (make-temp-file "not-a-repo-" t)))
        (unwind-protect
            (let ((default-directory temp-dir))
              (expect (claude-multi--check-uncommitted-changes) :not :to-be-truthy))
          (delete-directory temp-dir t))))))

(provide 'test-worktree)
;;; test-worktree.el ends here
