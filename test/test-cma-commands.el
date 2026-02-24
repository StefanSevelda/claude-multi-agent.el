;; -*- lexical-binding: t; -*-
;;; test-cma-commands.el --- Tests for cma-commands.el

;;; Commentary:
;; Tests for model selection on spawn, worktree cleanup on kill,
;; and interactive worktree create/remove commands.

;;; Code:

(require 'buttercup)
(load-file "test/test-helper.el")

;; Provide stubs for variables referenced by cma-commands
(defvar claude-multi-default-model "sonnet")

(require 'cma-core)
(require 'cma-commands)

;;; ─────────────────────────────────────────────────
;;; Model selection in spawn
;;; ─────────────────────────────────────────────────

(describe "Model selection on spawn"

  (describe "cma/spawn-agent"

    (it "passes --model flag to cma CLI"
      (spy-on 'read-string :and-return-value "test task")
      (spy-on 'read-directory-name :and-return-value "/tmp/")
      (spy-on 'completing-read :and-return-value "opus")
      (spy-on 'cma--call :and-return-value
              '((agent . ((name . "agent-1") (kitty_window_id . "42")))))
      (cma/spawn-agent)
      (expect 'cma--call :to-have-been-called)
      ;; Verify the args include --model opus
      (let ((call-args (spy-calls-args-for 'cma--call 0)))
        (expect (member "--model" call-args) :to-be-truthy)
        (expect (nth (1+ (cl-position "--model" call-args :test #'string=)) call-args)
                :to-equal "opus")))

    (it "includes --model in args list alongside other flags"
      (spy-on 'read-string :and-return-value "my task")
      (spy-on 'read-directory-name :and-return-value "/tmp/work/")
      (spy-on 'completing-read :and-return-value "haiku")
      (spy-on 'cma--call :and-return-value
              '((agent . ((name . "agent-2") (kitty_window_id . "99")))))
      (cma/spawn-agent)
      (let ((call-args (spy-calls-args-for 'cma--call 0)))
        ;; Should have: "spawn" "--task" "my task" "--dir" "/tmp/work/" "--json" "--model" "haiku"
        (expect (car call-args) :to-equal "spawn")
        (expect (member "--task" call-args) :to-be-truthy)
        (expect (member "--json" call-args) :to-be-truthy)
        (expect (member "--model" call-args) :to-be-truthy))))

  (describe "cma/spawn-agent-with-worktree"

    (it "passes --model flag alongside --branch"
      (spy-on 'read-string :and-call-fake
              (lambda (prompt &rest _)
                (cond
                 ((string-match-p "Task" prompt) "wt task")
                 ((string-match-p "Branch" prompt) "feature/foo")
                 (t ""))))
      (spy-on 'read-directory-name :and-return-value "/tmp/")
      (spy-on 'completing-read :and-return-value "sonnet")
      (spy-on 'cma--call :and-return-value
              '((agent . ((name . "agent-3")))))
      (cma/spawn-agent-with-worktree)
      (let ((call-args (spy-calls-args-for 'cma--call 0)))
        (expect (member "--branch" call-args) :to-be-truthy)
        (expect (member "--model" call-args) :to-be-truthy)
        (expect (nth (1+ (cl-position "--model" call-args :test #'string=)) call-args)
                :to-equal "sonnet")))))

;;; ─────────────────────────────────────────────────
;;; Worktree cleanup helpers
;;; ─────────────────────────────────────────────────

(describe "Worktree cleanup helpers"

  (describe "cma--maybe-cleanup-worktree"

    (it "does nothing when branch is nil"
      (spy-on 'cma--call)
      (cma--maybe-cleanup-worktree nil)
      (expect 'cma--call :not :to-have-been-called))

    (it "does nothing when branch is empty"
      (spy-on 'cma--call)
      (cma--maybe-cleanup-worktree "")
      (expect 'cma--call :not :to-have-been-called))

    (it "does nothing when no matching worktree exists"
      (spy-on 'cma--call :and-return-value
              '(((branch . "other-branch") (path . "/tmp/wt"))))
      (spy-on 'y-or-n-p)
      (cma--maybe-cleanup-worktree "my-branch")
      ;; cma--call was called to list, but y-or-n-p should not be called
      (expect 'y-or-n-p :not :to-have-been-called))

    (it "prompts and removes worktree when matching branch found"
      (spy-on 'cma--call :and-return-value
              '(((branch . "feature/cleanup") (path . "/tmp/wt-cleanup"))))
      (spy-on 'y-or-n-p :and-call-fake
              (lambda (prompt)
                ;; Yes to remove, no to delete branch
                (string-match-p "Remove worktree" prompt)))
      (spy-on 'cma--call-raw)
      (cma--maybe-cleanup-worktree "feature/cleanup")
      ;; Should have called cma worktree remove
      (expect 'cma--call-raw :to-have-been-called)
      (let ((raw-args (spy-calls-args-for 'cma--call-raw 0)))
        (expect (car raw-args) :to-equal "worktree")
        (expect (nth 1 raw-args) :to-equal "remove")
        (expect (nth 2 raw-args) :to-equal "feature/cleanup")
        ;; Should NOT include --delete-branch
        (expect (member "--delete-branch" raw-args) :not :to-be-truthy)))

    (it "removes worktree and deletes branch when user confirms both"
      (spy-on 'cma--call :and-return-value
              '(((branch . "feature/del") (path . "/tmp/wt-del"))))
      (spy-on 'y-or-n-p :and-return-value t) ; Yes to everything
      (spy-on 'cma--call-raw)
      (cma--maybe-cleanup-worktree "feature/del")
      (let ((raw-args (spy-calls-args-for 'cma--call-raw 0)))
        (expect (member "--delete-branch" raw-args) :to-be-truthy)))

    (it "does not remove when user declines"
      (spy-on 'cma--call :and-return-value
              '(((branch . "feature/keep") (path . "/tmp/wt-keep"))))
      (spy-on 'y-or-n-p :and-return-value nil)
      (spy-on 'cma--call-raw)
      (cma--maybe-cleanup-worktree "feature/keep")
      (expect 'cma--call-raw :not :to-have-been-called)))

  (describe "cma--maybe-cleanup-worktrees-batch"

    (it "skips nil and empty branches"
      (spy-on 'cma--maybe-cleanup-worktree)
      (cma--maybe-cleanup-worktrees-batch '(nil "" nil))
      (expect 'cma--maybe-cleanup-worktree :not :to-have-been-called))

    (it "calls cleanup for each valid branch"
      (spy-on 'cma--maybe-cleanup-worktree)
      (cma--maybe-cleanup-worktrees-batch '("branch-a" nil "branch-b" ""))
      (expect 'cma--maybe-cleanup-worktree :to-have-been-called-times 2)
      (expect (spy-calls-args-for 'cma--maybe-cleanup-worktree 0)
              :to-equal '("branch-a"))
      (expect (spy-calls-args-for 'cma--maybe-cleanup-worktree 1)
              :to-equal '("branch-b")))))

;;; ─────────────────────────────────────────────────
;;; Kill with worktree cleanup
;;; ─────────────────────────────────────────────────

(describe "Kill agent with worktree cleanup"

  (describe "cma/kill-agent"

    (it "calls worktree cleanup with agent branch after kill"
      (spy-on 'cma--call :and-return-value
              '(((name . "agent-1")
                 (status . "running")
                 (cwd . "/tmp")
                 (session_id . "sess-123")
                 (git_branch . "feature/test"))))
      (spy-on 'completing-read :and-return-value "agent-1 [RUNNING] - /tmp")
      (spy-on 'y-or-n-p :and-return-value t)
      (spy-on 'cma--call-raw)
      (spy-on 'cma--maybe-cleanup-worktree)
      (spy-on 'cma-table/refresh)
      (cma/kill-agent)
      ;; Should have killed the agent
      (expect 'cma--call-raw :to-have-been-called)
      ;; Should have attempted worktree cleanup with the branch
      (expect 'cma--maybe-cleanup-worktree :to-have-been-called-with "feature/test"))

    (it "passes nil branch when agent has no worktree"
      (spy-on 'cma--call :and-return-value
              '(((name . "agent-2")
                 (status . "running")
                 (cwd . "/tmp")
                 (session_id . "sess-456"))))
      (spy-on 'completing-read :and-return-value "agent-2 [RUNNING] - /tmp")
      (spy-on 'y-or-n-p :and-return-value t)
      (spy-on 'cma--call-raw)
      (spy-on 'cma--maybe-cleanup-worktree)
      (spy-on 'cma-table/refresh)
      (cma/kill-agent)
      ;; Should pass nil branch (no git_branch key in alist)
      (expect 'cma--maybe-cleanup-worktree :to-have-been-called-with nil)))

  (describe "cma/kill-all-agents"

    (it "collects branches before killing and runs batch cleanup"
      (spy-on 'cma--call :and-return-value
              '(((session_id . "s1") (git_branch . "branch-a"))
                ((session_id . "s2") (git_branch . "branch-b"))
                ((session_id . "s3"))))
      (spy-on 'y-or-n-p :and-return-value t)
      (spy-on 'cma--call-raw)
      (spy-on 'cma--maybe-cleanup-worktrees-batch)
      (spy-on 'cma-table/refresh)
      (cma/kill-all-agents)
      ;; Should have called kill --all
      (expect 'cma--call-raw :to-have-been-called-with "kill" "--all")
      ;; Should have passed branches (including nil for agent without branch)
      (expect 'cma--maybe-cleanup-worktrees-batch :to-have-been-called)
      (let ((branches (car (spy-calls-args-for 'cma--maybe-cleanup-worktrees-batch 0))))
        (expect branches :to-equal '("branch-a" "branch-b" nil))))))

;;; ─────────────────────────────────────────────────
;;; Worktree create/remove commands
;;; ─────────────────────────────────────────────────

(describe "Worktree management commands"

  (describe "cma/worktree-create"

    (it "calls cma worktree create with branch name"
      (spy-on 'read-string :and-return-value "feature/new-wt")
      (spy-on 'cma--call :and-return-value '((branch . "feature/new-wt")))
      (cma/worktree-create)
      (expect 'cma--call :to-have-been-called)
      (let ((args (spy-calls-args-for 'cma--call 0)))
        (expect (car args) :to-equal "worktree")
        (expect (nth 1 args) :to-equal "create")
        (expect (nth 2 args) :to-equal "feature/new-wt")))

    (it "does not call cma when branch is empty"
      (spy-on 'read-string :and-return-value "")
      (spy-on 'cma--call)
      (cma/worktree-create)
      (expect 'cma--call :not :to-have-been-called)))

  (describe "cma/worktree-remove"

    (it "shows message when no worktrees exist"
      (spy-on 'cma--call :and-return-value nil)
      (spy-on 'completing-read)
      (cma/worktree-remove)
      (expect 'completing-read :not :to-have-been-called))

    (it "offers selection and removes chosen worktree"
      (spy-on 'cma--call :and-return-value
              '(((branch . "feat/a") (path . "/tmp/wt-a"))
                ((branch . "feat/b") (path . "/tmp/wt-b"))))
      (spy-on 'completing-read :and-return-value "feat/a (/tmp/wt-a)")
      (spy-on 'y-or-n-p :and-call-fake
              (lambda (prompt)
                ;; Yes to remove, no to delete branch
                (string-match-p "Remove worktree" prompt)))
      (spy-on 'cma--call-raw)
      (cma/worktree-remove)
      (expect 'cma--call-raw :to-have-been-called)
      (let ((args (spy-calls-args-for 'cma--call-raw 0)))
        (expect (car args) :to-equal "worktree")
        (expect (nth 1 args) :to-equal "remove")
        (expect (nth 2 args) :to-equal "feat/a")))

    (it "includes --delete-branch when user confirms"
      (spy-on 'cma--call :and-return-value
              '(((branch . "feat/del") (path . "/tmp/wt-del"))))
      (spy-on 'completing-read :and-return-value "feat/del (/tmp/wt-del)")
      (spy-on 'y-or-n-p :and-return-value t) ; Yes to everything
      (spy-on 'cma--call-raw)
      (cma/worktree-remove)
      (let ((args (spy-calls-args-for 'cma--call-raw 0)))
        (expect (member "--delete-branch" args) :to-be-truthy)))))

(provide 'test-cma-commands)
;;; test-cma-commands.el ends here
