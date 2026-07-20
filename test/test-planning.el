;; -*- lexical-binding: t; -*-
;;; test-planning.el --- Tests for claude-multi-planning.el

;;; Commentary:
;; Tests for the org task triage module: export-on-save wiring, the
;; autosave timer, save-all, refile helpers, and capture template setup.
;; External side effects (processes, buffer saves, org-refile) are always
;; mocked with spy-on — this file never shells out or touches disk.

;;; Code:

(require 'buttercup)
(load-file "test/test-helper.el")

(require 'claude-multi-planning)

(defvar test-planning--dir "/tmp/claude-multi-planning-test/"
  "Fake planning directory used to isolate tests from the real default.")

(defun test-planning--make-buffer (name &optional file-name mode modified)
  "Create a temp buffer NAME, optionally set FILE-NAME, MODE, and MODIFIED.
Returns the buffer.  Caller is responsible for killing it."
  (let ((buf (generate-new-buffer name)))
    (with-current-buffer buf
      (when mode (funcall mode))
      (when file-name (setq-local buffer-file-name file-name))
      (set-buffer-modified-p (and modified t)))
    buf))

(describe "claude-multi-planning"

  (before-each
    (setq claude-multi-planning-directory test-planning--dir)
    (setq claude-multi-planning-tasks-file
          (expand-file-name "tasks.org" test-planning--dir))
    (setq claude-multi-planning--autosave-timer nil)
    (setq claude-multi-planning-autosave-mode nil))

  (describe "claude-multi-planning--maybe-setup-buffer"

    (it "adds the export after-save-hook for an org file under the planning dir"
      (let ((buf (test-planning--make-buffer
                   "planning-org"
                   (expand-file-name "tasks.org" test-planning--dir)
                   #'org-mode)))
        (unwind-protect
            (with-current-buffer buf
              (claude-multi-planning--maybe-setup-buffer)
              (expect (memq 'claude-multi-planning--export-buffer after-save-hook)
                      :to-be-truthy))
          (kill-buffer buf))))

    (it "does not add the hook for an org file outside the planning dir"
      (let ((buf (test-planning--make-buffer
                   "other-org" "/tmp/elsewhere/notes.org" #'org-mode)))
        (unwind-protect
            (with-current-buffer buf
              (claude-multi-planning--maybe-setup-buffer)
              (expect (memq 'claude-multi-planning--export-buffer after-save-hook)
                      :not :to-be-truthy))
          (kill-buffer buf))))

    (it "does not add the hook for a non-org buffer under the planning dir"
      (let ((buf (test-planning--make-buffer
                   "planning-text"
                   (expand-file-name "notes.txt" test-planning--dir)
                   #'fundamental-mode)))
        (unwind-protect
            (with-current-buffer buf
              (claude-multi-planning--maybe-setup-buffer)
              (expect (memq 'claude-multi-planning--export-buffer after-save-hook)
                      :not :to-be-truthy))
          (kill-buffer buf)))))

  (describe "claude-multi-planning--export-buffer"

    (it "invokes start-process with python3, the expanded script, and the file"
      (let ((buf (test-planning--make-buffer
                   "export-buf"
                   (expand-file-name "tasks.org" test-planning--dir)
                   #'org-mode))
            (claude-multi-planning-export-script "~/org/scripts/org-to-jsonl.py"))
        (unwind-protect
            (progn
              (spy-on 'start-process :and-return-value 'fake-proc)
              (spy-on 'process-put)
              (spy-on 'set-process-sentinel)
              (with-current-buffer buf
                (claude-multi-planning--export-buffer))
              (expect 'start-process :to-have-been-called)
              (let ((call-args (spy-calls-args-for 'start-process 0)))
                (expect (nth 2 call-args) :to-equal "python3")
                (expect (nth 3 call-args)
                        :to-equal (expand-file-name "~/org/scripts/org-to-jsonl.py"))
                (expect (nth 4 call-args)
                        :to-equal (expand-file-name "tasks.org" test-planning--dir))))
          (kill-buffer buf))))

    (it "does nothing when the buffer has no file"
      (let ((buf (generate-new-buffer "no-file-buf")))
        (unwind-protect
            (progn
              (spy-on 'start-process)
              (with-current-buffer buf
                (claude-multi-planning--export-buffer))
              (expect 'start-process :not :to-have-been-called))
          (kill-buffer buf)))))

  (describe "claude-multi-planning--export-sentinel"

    (it "is silent on successful exit"
      (spy-on 'process-live-p :and-return-value nil)
      (spy-on 'process-exit-status :and-return-value 0)
      (spy-on 'process-get :and-return-value "tasks.org")
      (spy-on 'message)
      (claude-multi-planning--export-sentinel 'fake-proc "finished\n")
      (expect 'message :not :to-have-been-called))

    (it "messages on non-zero exit"
      (spy-on 'process-live-p :and-return-value nil)
      (spy-on 'process-exit-status :and-return-value 1)
      (spy-on 'process-get :and-return-value "tasks.org")
      (spy-on 'message)
      (claude-multi-planning--export-sentinel 'fake-proc "finished\n")
      (expect 'message :to-have-been-called)))

  (describe "claude-multi-planning-autosave-mode"

    (it "creates exactly one timer when enabled"
      (spy-on 'run-with-timer :and-return-value 'fake-timer)
      (spy-on 'cancel-timer)
      (claude-multi-planning-autosave-mode 1)
      (expect 'run-with-timer :to-have-been-called-times 1)
      (expect claude-multi-planning--autosave-timer :to-equal 'fake-timer))

    (it "does not stack timers when enabled twice"
      (spy-on 'run-with-timer :and-return-value 'fake-timer)
      (spy-on 'cancel-timer)
      (claude-multi-planning-autosave-mode 1)
      (claude-multi-planning-autosave-mode 1)
      (expect 'run-with-timer :to-have-been-called-times 1))

    (it "cancels the timer when disabled"
      (spy-on 'run-with-timer :and-return-value 'fake-timer)
      (spy-on 'cancel-timer)
      (claude-multi-planning-autosave-mode 1)
      (claude-multi-planning-autosave-mode -1)
      (expect 'cancel-timer :to-have-been-called-with 'fake-timer)
      (expect claude-multi-planning--autosave-timer :to-be nil))

    (it "does nothing on disable when no timer is running"
      (spy-on 'cancel-timer)
      (claude-multi-planning-autosave-mode -1)
      (expect 'cancel-timer :not :to-have-been-called)))

  (describe "claude-multi-planning-save-all"

    (it "saves only modified org buffers under the planning dir and returns their names"
      (let ((planning-buf (test-planning--make-buffer
                             "save-all-planning"
                             (expand-file-name "tasks.org" test-planning--dir)
                             #'org-mode 'modified))
            (unmodified-buf (test-planning--make-buffer
                               "save-all-unmodified"
                               (expand-file-name "other.org" test-planning--dir)
                               #'org-mode nil))
            (non-org-buf (test-planning--make-buffer
                           "save-all-non-org"
                           (expand-file-name "notes.txt" test-planning--dir)
                           #'fundamental-mode 'modified))
            (outside-buf (test-planning--make-buffer
                           "save-all-outside"
                           "/tmp/elsewhere/tasks.org" #'org-mode 'modified)))
        (unwind-protect
            (progn
              (spy-on 'save-buffer)
              (let ((saved (claude-multi-planning-save-all)))
                (expect 'save-buffer :to-have-been-called-times 1)
                (expect saved :to-equal
                        (list (expand-file-name "tasks.org" test-planning--dir)))))
          (dolist (b (list planning-buf unmodified-buf non-org-buf outside-buf))
            (kill-buffer b)))))

    (it "returns nil when no planning buffers are modified"
      (spy-on 'save-buffer)
      (expect (claude-multi-planning-save-all) :to-be nil)
      (expect 'save-buffer :not :to-have-been-called)))

  (describe "claude-multi-planning--refile-to"

    (it "builds an RFLOC for the requested bucket and calls org-refile"
      (spy-on 'claude-multi-planning--bucket-position :and-return-value 4242)
      (spy-on 'org-refile)
      (spy-on 'find-buffer-visiting :and-return-value nil)
      (spy-on 'derived-mode-p :and-return-value nil)
      (claude-multi-planning--refile-to "Backlog")
      (expect 'org-refile :to-have-been-called)
      (let ((args (spy-calls-args-for 'org-refile 0)))
        (expect (car args) :to-be nil)
        (expect (cadr args) :to-be nil)
        (expect (nth 2 args)
                :to-equal (list "Backlog" claude-multi-planning-tasks-file nil 4242))))

    (it "uses org-agenda-refile when called from org-agenda-mode"
      (spy-on 'claude-multi-planning--bucket-position :and-return-value 99)
      (spy-on 'org-refile)
      (spy-on 'org-agenda-refile)
      (spy-on 'find-buffer-visiting :and-return-value nil)
      (spy-on 'derived-mode-p :and-call-fake
              (lambda (mode) (eq mode 'org-agenda-mode)))
      (claude-multi-planning--refile-to "Inbox")
      (expect 'org-agenda-refile :to-have-been-called)
      (expect 'org-refile :not :to-have-been-called))

    (it "saves the tasks buffer after refiling when it is open"
      (spy-on 'claude-multi-planning--bucket-position :and-return-value 1)
      (spy-on 'org-refile)
      (spy-on 'derived-mode-p :and-return-value nil)
      (let ((tasks-buf (generate-new-buffer "fake-tasks-buffer")))
        (unwind-protect
            (progn
              (spy-on 'find-buffer-visiting :and-return-value tasks-buf)
              (spy-on 'save-buffer)
              (claude-multi-planning--refile-to "Someday")
              (expect 'save-buffer :to-have-been-called))
          (kill-buffer tasks-buf)))))

  (describe "claude-multi-planning-setup-capture"

    (it "adds the \"t\" template when none exists"
      (let ((org-capture-templates nil))
        (claude-multi-planning-setup-capture)
        (expect (assoc "t" org-capture-templates) :to-be-truthy)))

    (it "does not clobber an existing \"t\" template"
      (let ((org-capture-templates '(("t" "Existing" entry (file "x.org") "* %?"))))
        (claude-multi-planning-setup-capture)
        (expect (length org-capture-templates) :to-equal 1)
        (expect (nth 1 (assoc "t" org-capture-templates)) :to-equal "Existing")))))

(provide 'test-planning)
;;; test-planning.el ends here
