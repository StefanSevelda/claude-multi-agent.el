;; -*- lexical-binding: t; -*-
;;; test-cma-table.el --- Tests for cma-table notification display

;;; Commentary:
;; Tests for table entry generation including notification-type-based
;; status text and face coloring.

;;; Code:

(require 'buttercup)
(load-file "test/test-helper.el")

(require 'cma-core)
(require 'cma-table)

(describe "CMA Table Entry Generation"

  (describe "cma-table--agent-to-entry"

    (it "shows RUNNING status for active agent"
      (let* ((agent '((session_id . "s1")
                      (name . "agent-1")
                      (status . "running")
                      (cwd . "/tmp/project")
                      (window_id . "10")
                      (waiting_for_input . nil)))
             (entry (cma-table--agent-to-entry agent nil))
             (vec (cadr entry)))
        (expect (aref vec 5) :to-equal "RUNNING")))

    (it "shows PERMISSION status for permission_prompt"
      (let* ((agent '((session_id . "s2")
                      (name . "agent-2")
                      (status . "waiting-input")
                      (cwd . "/tmp/project")
                      (window_id . "10")
                      (waiting_for_input . t)
                      (notification_type . "permission_prompt")))
             (entry (cma-table--agent-to-entry agent nil))
             (vec (cadr entry)))
        ;; Status column should say PERMISSION
        (expect (substring-no-properties (aref vec 5))
                :to-equal "PERMISSION")))

    (it "shows QUESTION status for elicitation_dialog"
      (let* ((agent '((session_id . "s3")
                      (name . "agent-3")
                      (status . "waiting-input")
                      (cwd . "/tmp/project")
                      (window_id . "10")
                      (waiting_for_input . t)
                      (notification_type . "elicitation_dialog")))
             (entry (cma-table--agent-to-entry agent nil))
             (vec (cadr entry)))
        (expect (substring-no-properties (aref vec 5))
                :to-equal "QUESTION")))

    (it "shows IDLE status for idle_prompt"
      (let* ((agent '((session_id . "s4")
                      (name . "agent-4")
                      (status . "waiting-input")
                      (cwd . "/tmp/project")
                      (window_id . "10")
                      (waiting_for_input . t)
                      (notification_type . "idle_prompt")))
             (entry (cma-table--agent-to-entry agent nil))
             (vec (cadr entry)))
        (expect (substring-no-properties (aref vec 5))
                :to-equal "IDLE")))

    (it "applies permission face (red) for permission_prompt"
      (let* ((agent '((session_id . "s2")
                      (name . "agent-2")
                      (status . "waiting-input")
                      (cwd . "/tmp/project")
                      (window_id . "10")
                      (waiting_for_input . t)
                      (notification_type . "permission_prompt")))
             (entry (cma-table--agent-to-entry agent nil))
             (vec (cadr entry))
             (status-cell (aref vec 5)))
        (expect (get-text-property 0 'face status-cell)
                :to-equal 'cma-table-face-permission)))

    (it "applies elicitation face (yellow) for elicitation_dialog"
      (let* ((agent '((session_id . "s3")
                      (name . "agent-3")
                      (status . "waiting-input")
                      (cwd . "/tmp/project")
                      (window_id . "10")
                      (waiting_for_input . t)
                      (notification_type . "elicitation_dialog")))
             (entry (cma-table--agent-to-entry agent nil))
             (vec (cadr entry))
             (status-cell (aref vec 5)))
        (expect (get-text-property 0 'face status-cell)
                :to-equal 'cma-table-face-elicitation)))

    (it "applies idle face (green) for idle_prompt"
      (let* ((agent '((session_id . "s4")
                      (name . "agent-4")
                      (status . "waiting-input")
                      (cwd . "/tmp/project")
                      (window_id . "10")
                      (waiting_for_input . t)
                      (notification_type . "idle_prompt")))
             (entry (cma-table--agent-to-entry agent nil))
             (vec (cadr entry))
             (status-cell (aref vec 5)))
        (expect (get-text-property 0 'face status-cell)
                :to-equal 'cma-table-face-idle)))

    (it "does not apply face for non-waiting agent"
      (let* ((agent '((session_id . "s5")
                      (name . "agent-5")
                      (status . "running")
                      (cwd . "/tmp/project")
                      (window_id . "10")
                      (waiting_for_input . nil)))
             (entry (cma-table--agent-to-entry agent nil))
             (vec (cadr entry))
             (status-cell (aref vec 5)))
        (expect (get-text-property 0 'face status-cell)
                :to-equal nil)))

    (it "falls back to status string for unknown notification type"
      (let* ((agent '((session_id . "s6")
                      (name . "agent-6")
                      (status . "waiting-input")
                      (cwd . "/tmp/project")
                      (window_id . "10")
                      (waiting_for_input . t)
                      (notification_type . "auth_success")))
             (entry (cma-table--agent-to-entry agent nil))
             (vec (cadr entry)))
        (expect (aref vec 5) :to-equal "WAITING-INPUT")))

    (it "indents child agent name"
      (let* ((agent '((session_id . "child-1")
                      (name . "child-agent")
                      (status . "running")
                      (cwd . "/tmp/project")
                      (window_id . "10")
                      (waiting_for_input . nil)))
             (entry (cma-table--agent-to-entry agent t))
             (vec (cadr entry)))
        (expect (aref vec 2) :to-equal "|-> child-agent")))

    (it "shows location with git branch"
      (let* ((agent '((session_id . "loc-1")
                      (name . "loc-agent")
                      (status . "running")
                      (cwd . "/tmp/my-project")
                      (window_id . "10")
                      (waiting_for_input . nil)
                      (git_branch . "feat/auth")))
             (entry (cma-table--agent-to-entry agent nil))
             (vec (cadr entry)))
        (expect (aref vec 4) :to-equal "my-project (feat/auth)")))

    (it "shows context percentage"
      (let* ((agent '((session_id . "ctx-1")
                      (name . "ctx-agent")
                      (status . "running")
                      (cwd . "/tmp/project")
                      (window_id . "10")
                      (waiting_for_input . nil)
                      (context_used . 75.5)))
             (entry (cma-table--agent-to-entry agent nil))
             (vec (cadr entry)))
        (expect (aref vec 3) :to-equal "75.5%")))

    (it "uses agent_id as table entry id (primary key)"
      (let* ((agent '((agent_id . "my-agent")
                      (session_id . "my-session-id")
                      (name . "test")
                      (status . "running")
                      (cwd . "/tmp")
                      (pane_id . "%5")
                      (waiting_for_input . nil)))
             (entry (cma-table--agent-to-entry agent nil)))
        (expect (car entry) :to-equal "my-agent")))

    (it "falls back to session_id when agent_id is absent (legacy)"
      (let* ((agent '((session_id . "my-session-id")
                      (name . "test")
                      (status . "running")
                      (cwd . "/tmp")
                      (pane_id . "%5")
                      (waiting_for_input . nil)))
             (entry (cma-table--agent-to-entry agent nil)))
        (expect (car entry) :to-equal "my-session-id")))

    (it "does not apply face when waiting_for_input is :false (json-parse-string false)"
      ;; json-parse-string maps JSON false to :false, which is truthy — regression test
      (let* ((agent '((session_id . "s7")
                      (name . "agent-7")
                      (status . "running")
                      (cwd . "/tmp/project")
                      (window_id . "10")
                      (waiting_for_input . :false)
                      (notification_type . "permission_prompt")))
             (entry (cma-table--agent-to-entry agent nil))
             (vec (cadr entry))
             (status-cell (aref vec 5)))
        (expect (get-text-property 0 'face status-cell)
                :to-equal nil)))

    (it "shows plain status icon (not yellow) when waiting_for_input is :false"
      (let* ((agent '((session_id . "s8")
                      (name . "agent-8")
                      (status . "running")
                      (cwd . "/tmp/project")
                      (window_id . "10")
                      (waiting_for_input . :false)))
             (entry (cma-table--agent-to-entry agent nil))
             (vec (cadr entry)))
        ;; Should use 🟢 for running, not 🟡 for waiting
        (expect (aref vec 0) :to-equal "🟢")))

    (it "nests a handoff child with an -> prefix"
      (let* ((agent '((agent_id . "child")
                      (name . "child")
                      (parent_id . "parent")
                      (status . "running")
                      (cwd . "/tmp/project")
                      (waiting_for_input . nil)))
             (entry (cma-table--agent-to-entry agent nil 1))
             (vec (cadr entry)))
        (expect (aref vec 2) :to-equal "-> child")))

    (it "indents deeper handoff generations"
      (let* ((agent '((agent_id . "grandchild")
                      (name . "grandchild")
                      (parent_id . "child")
                      (status . "running")
                      (cwd . "/tmp/project")
                      (waiting_for_input . nil)))
             (entry (cma-table--agent-to-entry agent nil 2))
             (vec (cadr entry)))
        (expect (aref vec 2) :to-equal "  -> grandchild")))

    (it "handoff nesting outranks the domain-sibling marker"
      (let* ((agent '((agent_id . "child")
                      (name . "child")
                      (parent_id . "parent")
                      (domain . "auth")
                      (status . "running")
                      (cwd . "/tmp/project")
                      (waiting_for_input . nil)))
             (entry (cma-table--agent-to-entry agent t 1))
             (vec (cadr entry)))
        (expect (aref vec 2) :to-equal "-> child"))))

  (describe "cma-table--order-by-parent"

    (it "keeps parentless agents flat in given order"
      (let* ((members '(((agent_id . "a")) ((agent_id . "b"))))
             (ordered (cma-table--order-by-parent members)))
        (expect (mapcar (lambda (p) (alist-get 'agent_id (car p))) ordered)
                :to-equal '("a" "b"))
        (expect (mapcar #'cdr ordered) :to-equal '(0 0))))

    (it "moves a handoff child directly below its parent"
      (let* ((members '(((agent_id . "child") (parent_id . "zeta"))
                        ((agent_id . "other"))
                        ((agent_id . "zeta"))))
             (ordered (cma-table--order-by-parent members)))
        (expect (mapcar (lambda (p) (alist-get 'agent_id (car p))) ordered)
                :to-equal '("other" "zeta" "child"))
        (expect (mapcar #'cdr ordered) :to-equal '(0 0 1))))

    (it "nests grandchildren one level deeper"
      (let* ((members '(((agent_id . "child") (parent_id . "root"))
                        ((agent_id . "grandchild") (parent_id . "child"))
                        ((agent_id . "root"))))
             (ordered (cma-table--order-by-parent members)))
        (expect (mapcar (lambda (p) (alist-get 'agent_id (car p))) ordered)
                :to-equal '("root" "child" "grandchild"))
        (expect (mapcar #'cdr ordered) :to-equal '(0 1 2))))

    (it "treats an unknown parent as a root"
      (let* ((members '(((agent_id . "orphan") (parent_id . "gone"))))
             (ordered (cma-table--order-by-parent members)))
        (expect (mapcar #'cdr ordered) :to-equal '(0))))

    (it "emits every agent exactly once on a parent cycle"
      (let* ((members '(((agent_id . "a") (parent_id . "b"))
                        ((agent_id . "b") (parent_id . "a"))))
             (ordered (cma-table--order-by-parent members)))
        (expect (sort (mapcar (lambda (p) (alist-get 'agent_id (car p))) ordered)
                      #'string<)
                :to-equal '("a" "b")))))

  (describe "cma-table--populate"

    (it "orders and marks handoff children inside their domain group"
      (spy-on 'cma--call :and-return-value
              '(((agent_id . "beta-child")
                 (name . "beta-child")
                 (parent_id . "alpha")
                 (domain . "auth")
                 (status . "running")
                 (cwd . "/tmp/p")
                 (waiting_for_input . nil))
                ((agent_id . "alpha")
                 (name . "alpha")
                 (domain . "auth")
                 (status . "running")
                 (cwd . "/tmp/p")
                 (waiting_for_input . nil))
                ((agent_id . "solo")
                 (name . "solo")
                 (status . "running")
                 (cwd . "/tmp/p")
                 (waiting_for_input . nil))))
      (with-temp-buffer
        (cma-table--populate)
        (let ((ids (mapcar #'car tabulated-list-entries))
              (titles (mapcar (lambda (e)
                                (substring-no-properties (aref (cadr e) 2)))
                              tabulated-list-entries)))
          ;; No-domain agents sort first, then the auth group parent-first.
          (expect ids :to-equal '("solo" "alpha" "beta-child"))
          (expect titles :to-equal '("solo" "alpha" "-> beta-child")))))))

(provide 'test-cma-table)
;;; test-cma-table.el ends here
