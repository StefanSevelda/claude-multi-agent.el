;; -*- lexical-binding: t; -*-
;;; test-simple.el --- Simple smoke tests without complex mocking

;;; Commentary:
;; Basic tests to verify test infrastructure works

;;; Code:

(require 'buttercup)

;; Load test helper
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'test-helper)

;; Load modules under test
(add-to-list 'load-path (expand-file-name "../autoload" (file-name-directory load-file-name)))
(require 'claude-multi-agents)
(require 'claude-multi-progress)

(describe "Simple Smoke Tests"

  (it "can create an agent structure"
    (let ((agent (make-claude-agent
                  :id "test-1"
                  :name "claude-test-1"
                  :color "#FF4444"
                  :status 'pending)))
      (expect (claude-agent-id agent) :to-equal "test-1")
      (expect (claude-agent-name agent) :to-equal "claude-test-1")
      (expect (claude-agent-color agent) :to-equal "#FF4444")
      (expect (claude-agent-status agent) :to-equal 'pending)))

  (it "can format agent status"
    (let ((agent (make-claude-agent :status 'running)))
      (expect (claude-multi--format-agent-status agent) :to-match "RUNNING")))

  (it "assigns colors to agents"
    (let ((color (claude-multi--assign-color "agent-1")))
      (expect color :to-be-truthy)
      (expect (stringp color) :to-be-truthy)))

  (it "can get status icon for different states"
    (expect (claude-multi--get-status-icon 'running) :to-equal "🟢")
    (expect (claude-multi--get-status-icon 'waiting-input) :to-equal "🟡")
    (expect (claude-multi--get-status-icon 'completed) :to-equal "🔵")
    (expect (claude-multi--get-status-icon 'failed) :to-equal "🔴")
    (expect (claude-multi--get-status-icon 'pending) :to-equal "⚪"))

  (it "can format duration"
    (let* ((start (current-time))
           (duration (claude-multi--format-duration start)))
      (expect duration :to-be-truthy)
      (expect (stringp duration) :to-be-truthy)))

  (it "can create progress buffer"
    (let ((buf (get-buffer-create "*test-simple-progress*")))
      (expect (buffer-live-p buf) :to-be-truthy)
      (kill-buffer buf)))

  (it "can check variable initialization"
    (expect (boundp 'claude-multi--agents) :to-be-truthy)
    (expect (boundp 'claude-multi--progress-buffer) :to-be-truthy)
    (expect (boundp 'claude-multi--session-start-time) :to-be-truthy)))

(provide 'test-simple)
;;; test-simple.el ends here
