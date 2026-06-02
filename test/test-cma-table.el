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
        (expect (aref vec 0) :to-equal "🟢")))))

(provide 'test-cma-table)
;;; test-cma-table.el ends here
