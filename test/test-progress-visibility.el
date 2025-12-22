;; -*- lexical-binding: t; -*-
;;; test-progress-visibility.el --- Tests for STATUS drawer visibility

;;; Commentary:
;; Unit tests for progress buffer STATUS drawer visibility features

;;; Code:

(require 'buttercup)

;; Load test helper to initialize variables and mocks
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'test-helper)

;; Load modules under test
(add-to-list 'load-path (expand-file-name "../autoload" (file-name-directory load-file-name)))
(require 'claude-multi-agents)
(require 'claude-multi-progress)
(require 'org)

(describe "Progress Buffer STATUS Drawer Visibility"

  (before-each
    ;; Initialize session state
    (setq claude-multi--session-start-time (current-time))
    (setq claude-multi--agents nil)
    (setq claude-multi--progress-buffer (get-buffer-create "*test-progress*"))

    ;; Mock functions that have side effects for non-watching tests
    (spy-on 'claude-multi--watch-agent-status-file))

  (after-each
    ;; Cleanup
    (when (buffer-live-p claude-multi--progress-buffer)
      (kill-buffer claude-multi--progress-buffer))
    (setq claude-multi--agents nil)
    (setq claude-multi--progress-buffer nil))

  (describe "claude-multi--add-agent-section"

    (it "creates STATUS drawer with visible content by default"
      (let ((agent (make-claude-agent
                    :id "agent-1"
                    :name "claude-agent-1"
                    :color "#FF4444"
                    :task-description "Test task"
                    :status 'running
                    :created-at (current-time)
                    :working-directory default-directory)))

        (claude-multi--init-progress-buffer)
        (claude-multi--add-agent-section agent)

        (with-current-buffer claude-multi--progress-buffer
          ;; Check that STATUS drawer exists
          (goto-char (point-min))
          (expect (re-search-forward ":STATUS:" nil t) :to-be-truthy)

          ;; Check that status marker exists
          (goto-char (point-min))
          (expect (re-search-forward "<!-- status-marker-agent-1 -->" nil t) :to-be-truthy)

          ;; Check that drawer has placeholder text
          (goto-char (point-min))
          (expect (re-search-forward "/Waiting for status update.../" nil t) :to-be-truthy)

          ;; Check that :END: tag exists
          (goto-char (point-min))
          (expect (re-search-forward ":END:" nil t) :to-be-truthy))))

    (it "includes agent ID with correct color in headline"
      (let ((agent (make-claude-agent
                    :id "agent-1"
                    :name "claude-agent-1"
                    :color "#FF4444"
                    :task-description "Test task"
                    :status 'running
                    :created-at (current-time)
                    :working-directory default-directory)))

        (claude-multi--init-progress-buffer)
        (claude-multi--add-agent-section agent)

        (with-current-buffer claude-multi--progress-buffer
          ;; Find the agent ID in the headline
          (goto-char (point-min))
          (expect (re-search-forward "agent-1" nil t) :to-be-truthy)

          ;; Check that the agent ID has color face property
          (let ((pos (match-beginning 0)))
            (expect (get-text-property pos 'face) :to-be-truthy)))))

    (it "includes status icon in headline"
      (let ((agent (make-claude-agent
                    :id "agent-1"
                    :name "claude-agent-1"
                    :color "#FF4444"
                    :task-description "Test task"
                    :status 'running
                    :created-at (current-time)
                    :working-directory default-directory)))

        (claude-multi--init-progress-buffer)
        (claude-multi--add-agent-section agent)

        (with-current-buffer claude-multi--progress-buffer
          ;; Check for running status icon
          (goto-char (point-min))
          (expect (re-search-forward "🟢" nil t) :to-be-truthy)))))

  (describe "claude-multi--update-agent-status-display"

    (it "updates STATUS drawer content with parsed JSON"
      (let* ((agent (make-claude-agent
                     :id "agent-1"
                     :name "claude-agent-1"
                     :color "#FF4444"
                     :task-description "Test task"
                     :status 'running
                     :created-at (current-time)
                     :working-directory default-directory))
             (test-content "- Status :: Working...\n"))

        (claude-multi--init-progress-buffer)
        (claude-multi--add-agent-section agent)

        ;; Mock the status file parser
        (spy-on 'claude-multi--parse-status-json :and-return-value test-content)
        (spy-on 'file-exists-p :and-return-value t)

        (claude-multi--update-agent-status-display agent)

        (with-current-buffer claude-multi--progress-buffer
          ;; Check that the content was updated
          (goto-char (point-min))
          (expect (re-search-forward "Working..." nil t) :to-be-truthy))))

    (it "auto-expands drawer when agent is waiting for input"
      (let* ((agent (make-claude-agent
                     :id "agent-1"
                     :name "claude-agent-1"
                     :color "#FF4444"
                     :task-description "Test task"
                     :status 'waiting-input
                     :created-at (current-time)
                     :working-directory default-directory))
             (test-content "⏸ *WAITING FOR INPUT*\n"))

        (claude-multi--init-progress-buffer)
        (claude-multi--add-agent-section agent)

        ;; Mock functions
        (spy-on 'claude-multi--parse-status-json :and-return-value test-content)
        (spy-on 'file-exists-p :and-return-value t)
        (spy-on 'org-show-subtree)

        (claude-multi--update-agent-status-display agent)

        ;; Check that org-show-subtree was called to expand the drawer
        (expect 'org-show-subtree :to-have-been-called)))

    (it "handles missing status.json gracefully"
      (let ((agent (make-claude-agent
                    :id "agent-1"
                    :name "claude-agent-1"
                    :color "#FF4444"
                    :task-description "Test task"
                    :status 'running
                    :created-at (current-time)
                    :working-directory default-directory)))

        (claude-multi--init-progress-buffer)
        (claude-multi--add-agent-section agent)

        ;; Mock file-exists-p to return nil
        (spy-on 'file-exists-p :and-return-value nil)
        (spy-on 'claude-multi--parse-status-json :and-return-value nil)

        (expect (claude-multi--update-agent-status-display agent) :not :to-throw))))

  (describe "Toggle Commands"

    (it "claude-multi/show-all-status-drawers shows all drawers"
      (let ((agent1 (make-claude-agent
                     :id "agent-1"
                     :name "claude-agent-1"
                     :color "#FF4444"
                     :task-description "Task 1"
                     :status 'running
                     :created-at (current-time)
                     :working-directory default-directory))
            (agent2 (make-claude-agent
                     :id "agent-2"
                     :name "claude-agent-2"
                     :color "#00D9FF"
                     :task-description "Task 2"
                     :status 'running
                     :created-at (current-time)
                     :working-directory default-directory)))

        (claude-multi--init-progress-buffer)
        (claude-multi--add-agent-section agent1)
        (claude-multi--add-agent-section agent2)

        ;; Mock org-show-subtree to track calls
        (spy-on 'org-show-subtree)

        (claude-multi/show-all-status-drawers)

        ;; Should be called once for each agent
        (expect (spy-calls-count 'org-show-subtree) :to-be-greater-than 0)))

    (it "claude-multi/hide-all-status-drawers hides all drawers"
      (let ((agent (make-claude-agent
                    :id "agent-1"
                    :name "claude-agent-1"
                    :color "#FF4444"
                    :task-description "Test task"
                    :status 'running
                    :created-at (current-time)
                    :working-directory default-directory)))

        (claude-multi--init-progress-buffer)
        (claude-multi--add-agent-section agent)

        ;; Mock org-hide-drawer-all
        (spy-on 'org-hide-drawer-all)

        (claude-multi/hide-all-status-drawers)

        (expect 'org-hide-drawer-all :to-have-been-called)))

    (it "claude-multi/toggle-all-status-drawers toggles drawer visibility"
      (let ((agent (make-claude-agent
                    :id "agent-1"
                    :name "claude-agent-1"
                    :color "#FF4444"
                    :task-description "Test task"
                    :status 'running
                    :created-at (current-time)
                    :working-directory default-directory)))

        (claude-multi--init-progress-buffer)
        (claude-multi--add-agent-section agent)

        ;; Mock org-cycle
        (spy-on 'org-cycle)

        (claude-multi/toggle-all-status-drawers)

        (expect 'org-cycle :to-have-been-called)))

    (it "toggle commands handle missing progress buffer gracefully"
      (setq claude-multi--progress-buffer nil)

      (expect (claude-multi/show-all-status-drawers) :not :to-throw)
      (expect (claude-multi/hide-all-status-drawers) :not :to-throw)
      (expect (claude-multi/toggle-all-status-drawers) :not :to-throw)))

  (describe "Status File Watching"

    (it "watches agent's status.json file"
      ;; Re-enable the spy for this test only
      (spy-calls-reset 'claude-multi--watch-agent-status-file)
      (spy-on 'file-notify-add-watch :and-return-value 'test-watch)

      (let ((agent (make-claude-agent
                    :id "agent-1"
                    :name "claude-agent-1"
                    :color "#FF4444"
                    :task-description "Test task"
                    :status 'running
                    :created-at (current-time)
                    :working-directory default-directory)))

        (claude-multi--init-progress-buffer)

        ;; Mock file operations and actually call the function
        (spy-on 'file-exists-p :and-return-value t)
        (spy-on 'claude-multi--update-agent-status-display)

        ;; Don't spy on the function we're testing - call it directly
        (spy-calls-reset 'claude-multi--watch-agent-status-file)
        (claude-multi--watch-agent-status-file agent)

        ;; Check that file-notify-add-watch was called
        (expect 'file-notify-add-watch :to-have-been-called)))

    (it "handles directory watching when status.json doesn't exist yet"
      ;; Re-enable for this test
      (spy-calls-reset 'claude-multi--watch-agent-status-file)
      (spy-on 'file-notify-add-watch :and-return-value 'test-watch)

      (let ((agent (make-claude-agent
                    :id "agent-1"
                    :name "claude-agent-1"
                    :color "#FF4444"
                    :task-description "Test task"
                    :status 'running
                    :created-at (current-time)
                    :working-directory default-directory)))

        (claude-multi--init-progress-buffer)

        ;; Mock file-exists-p to return nil (status.json doesn't exist)
        (spy-on 'file-exists-p :and-return-value nil)
        (spy-on 'claude-multi--update-agent-status-display)

        ;; Don't spy on the function we're testing
        (spy-calls-reset 'claude-multi--watch-agent-status-file)
        (claude-multi--watch-agent-status-file agent)

        ;; Should still set up a watch (on the directory)
        (expect 'file-notify-add-watch :to-have-been-called)))))

(provide 'test-progress-visibility)
;;; test-progress-visibility.el ends here
