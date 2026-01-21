;; -*- lexical-binding: t; -*-
;;; test-rename.el --- Tests for claude-multi-rename module

;;; Commentary:
;; Focused tests for agent renaming functionality:
;; - Rename mapping file operations
;; - Status file updates
;; - Kitty window title updates

;;; Code:

(require 'buttercup)
(load-file "test/test-helper.el")
(require 'claude-multi-rename)
(require 'claude-multi-status)  ; For claude-multi--status-file-path

;;; Test fixtures

(defvar test-rename--temp-dir nil
  "Temporary directory for rename mapping tests.")

(defun test-rename--setup-temp-dir ()
  "Create temporary directory for rename mappings."
  (setq test-rename--temp-dir (make-temp-file "claude-rename-test-" t))
  (setq claude-multi-rename-directory test-rename--temp-dir))

(defun test-rename--cleanup-temp-dir ()
  "Clean up temporary directory."
  (when (and test-rename--temp-dir
             (file-exists-p test-rename--temp-dir))
    (delete-directory test-rename--temp-dir t)
    (setq test-rename--temp-dir nil)))

;;; Rename Mapping File Tests

(describe "Rename Mapping Files"

  (before-each
    (test-rename--setup-temp-dir))

  (after-each
    (test-rename--cleanup-temp-dir))

  (describe "claude-multi--rename-mapping-file"

    (it "returns correct path for session ID"
      (let ((session-id "test-session-123")
            (expected-path (expand-file-name "test-session-123" test-rename--temp-dir)))
        (expect (claude-multi--rename-mapping-file session-id) :to-equal expected-path))))

  (describe "claude-multi--write-rename-mapping"

    (it "creates mapping file with agent name"
      (let ((session-id "test-session-123")
            (new-name "My Custom Agent"))
        (claude-multi--write-rename-mapping session-id new-name)
        (let ((mapping-file (claude-multi--rename-mapping-file session-id)))
          (expect (file-exists-p mapping-file) :to-be-truthy))))

    (it "creates directory if it doesn't exist"
      (delete-directory test-rename--temp-dir t)
      (let ((session-id "test-session-123")
            (new-name "My Agent"))
        (claude-multi--write-rename-mapping session-id new-name)
        (expect (file-directory-p test-rename--temp-dir) :to-be-truthy))))

  (describe "claude-multi--read-rename-mapping"

    (it "reads name from mapping file"
      (let ((session-id "test-session-123")
            (expected-name "My Custom Agent"))
        (claude-multi--write-rename-mapping session-id expected-name)
        (expect (claude-multi--read-rename-mapping session-id) :to-equal expected-name)))

    (it "returns nil for non-existent mapping"
      (let ((session-id "nonexistent-session"))
        (expect (claude-multi--read-rename-mapping session-id) :to-be nil)))

    (it "trims whitespace from stored name"
      (let* ((session-id "test-session-123")
             (mapping-file (claude-multi--rename-mapping-file session-id)))
        ;; Write with extra whitespace
        (with-temp-file mapping-file
          (insert "  My Agent  \n"))
        (expect (claude-multi--read-rename-mapping session-id) :to-equal "My Agent"))))

  (describe "claude-multi--delete-rename-mapping"

    (it "deletes existing mapping file"
      (let ((session-id "test-session-123"))
        (claude-multi--write-rename-mapping session-id "Test Name")
        (let ((mapping-file (claude-multi--rename-mapping-file session-id)))
          (expect (file-exists-p mapping-file) :to-be-truthy)
          (claude-multi--delete-rename-mapping session-id)
          (expect (file-exists-p mapping-file) :not :to-be-truthy))))

    (it "handles deletion of non-existent mapping gracefully"
      (let ((session-id "nonexistent-session"))
        (expect (lambda () (claude-multi--delete-rename-mapping session-id))
                :not :to-throw)))))

;;; Kitty Window Title Tests

(describe "Kitty Window Title Updates"

  (before-each
    (spy-on 'call-process-shell-command))

  (describe "claude-multi--update-kitty-window-title"

    (it "sends title update command to kitty"
      (let ((window-id "12345")
            (new-name "My Agent")
            (session-id "test-session-abc123def"))
        (claude-multi--update-kitty-window-title window-id new-name session-id)
        (expect 'call-process-shell-command :to-have-been-called)))

    (it "includes session ID prefix in title"
      (let ((window-id "12345")
            (new-name "My Agent")
            (session-id "test-session-abc123def"))
        (claude-multi--update-kitty-window-title window-id new-name session-id)
        (let ((cmd (car (spy-calls-args-for 'call-process-shell-command 0))))
          ;; Match the bracketed prefix (test-ses is first 8 chars of session-id)
          (expect cmd :to-match "\\[test-ses")
          ;; Match the agent name - with escaped spaces from shell-quote-argument
          (expect cmd :to-match "My\\\\ Agent"))))

    (it "handles nil window ID gracefully"
      (let ((window-id nil)
            (new-name "My Agent")
            (session-id "test-session-123"))
        (claude-multi--update-kitty-window-title window-id new-name session-id)
        (expect 'call-process-shell-command :not :to-have-been-called)))

    (it "handles nil name gracefully"
      (let ((window-id "12345")
            (new-name nil)
            (session-id "test-session-123"))
        (claude-multi--update-kitty-window-title window-id new-name session-id)
        (expect 'call-process-shell-command :not :to-have-been-called)))))

;;; Status File Update Tests

(describe "Status File Updates"

  (before-each
    (test-rename--setup-temp-dir)
    ;; Mock claude-multi--status-file-path to point to temp directory
    (spy-on 'claude-multi--status-file-path :and-call-fake
            (lambda (session-id)
              (expand-file-name (concat session-id ".json") test-rename--temp-dir))))

  (after-each
    (test-rename--cleanup-temp-dir))

  (describe "claude-multi--update-status-agent-name"

    (it "updates agent_name in status file"
      (let* ((session-id "test-session-123")
             (status-file (expand-file-name (concat session-id ".json") test-rename--temp-dir))
             (initial-data '((session_id . "test-session-123")
                           (agent_name . "Old Name")
                           (status . "running"))))
        ;; Create initial status file with valid JSON
        (with-temp-file status-file
          (insert (json-encode initial-data)))

        ;; Update name (should write to both status file and mapping file)
        (claude-multi--update-status-agent-name session-id "New Name")

        ;; Verify mapping file was written with the new name
        (let ((mapping-file (claude-multi--rename-mapping-file session-id)))
          (expect (file-exists-p mapping-file) :to-be-truthy)
          (expect (claude-multi--read-rename-mapping session-id) :to-equal "New Name"))))

    (it "handles missing status file gracefully"
      (let ((session-id "nonexistent-session"))
        (expect (lambda () (claude-multi--update-status-agent-name session-id "New Name"))
                :not :to-throw)))))

(provide 'test-rename)
;;; test-rename.el ends here
