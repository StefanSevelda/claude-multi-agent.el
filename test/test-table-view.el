;; -*- lexical-binding: t; -*-
;;; test-table-view.el --- Tests for claude-multi-table module

;;; Commentary:
;; Focused tests for table view core functionality:
;; - Table mode initialization
;; - Duration calculation
;; - Data conversion

;;; Code:

(require 'buttercup)
(load-file "test/test-helper.el")
(require 'claude-multi-agents)
(require 'claude-multi-table)

;;; Mode Initialization Tests

(describe "Table View Mode"

  (describe "claude-multi-table-mode initialization"

    (it "sets up tabulated-list-mode correctly"
      (with-temp-buffer
        (claude-multi-table-mode)
        (expect major-mode :to-equal 'claude-multi-table-mode)
        (expect (derived-mode-p 'tabulated-list-mode) :to-be-truthy)))

    (it "defines expected columns"
      (with-temp-buffer
        (claude-multi-table-mode)
        (expect (length tabulated-list-format) :to-equal 8)
        ;; Verify column names - tabulated-list-format is a vector of vectors
        (expect (elt (elt tabulated-list-format 0) 0) :to-equal "Icon")
        (expect (elt (elt tabulated-list-format 1) 0) :to-equal "Window")
        (expect (elt (elt tabulated-list-format 2) 0) :to-equal "Name")
        (expect (elt (elt tabulated-list-format 3) 0) :to-equal "Location")
        (expect (elt (elt tabulated-list-format 4) 0) :to-equal "Status")
        (expect (elt (elt tabulated-list-format 5) 0) :to-equal "Model")
        (expect (elt (elt tabulated-list-format 6) 0) :to-equal "Time")
        (expect (elt (elt tabulated-list-format 7) 0) :to-equal "Tokens")))

    (it "enables sorting on sortable columns"
      (with-temp-buffer
        (claude-multi-table-mode)
        ;; Window column should be sortable (index 1)
        (expect (elt (elt tabulated-list-format 1) 2) :to-be-truthy)
        ;; Name column should be sortable (index 2)
        (expect (elt (elt tabulated-list-format 2) 2) :to-be-truthy)
        ;; Location column should be sortable (index 3)
        (expect (elt (elt tabulated-list-format 3) 2) :to-be-truthy)
        ;; Status column should be sortable (index 4)
        (expect (elt (elt tabulated-list-format 4) 2) :to-be-truthy)))))

;;; Duration Calculation Tests

(describe "Duration Calculation"

  (describe "claude-multi--calculate-duration"

    (it "formats seconds correctly"
      (let ((timestamp (- (time-to-seconds) 45)))
        (expect (claude-multi--calculate-duration timestamp) :to-match "s")))

    (it "formats minutes correctly"
      (let ((timestamp (- (time-to-seconds) 185))) ; ~3m
        (expect (claude-multi--calculate-duration timestamp) :to-match "m")))

    (it "handles zero duration"
      (let ((timestamp (time-to-seconds)))
        (expect (claude-multi--calculate-duration timestamp) :to-match "s")))))

;;; Interactive Command Tests

(describe "Interactive Commands"

  (describe "claude-multi-table/refresh"

    (it "refreshes table display"
      (with-temp-buffer
        (claude-multi-table-mode)
        (spy-on 'tabulated-list-revert)
        (claude-multi-table/refresh)
        (expect 'tabulated-list-revert :to-have-been-called)))))

(provide 'test-table-view)
;;; test-table-view.el ends here
