;; -*- lexical-binding: t; -*-
;;; run-tests.el --- Test runner for claude-multi-agent

;;; Commentary:
;; Script to run all buttercup tests for the project

;;; Code:

;; Load test helper (sets up dependencies)
(load (expand-file-name "test-helper.el"
                        (file-name-directory load-file-name)))

;; Load test files
(load (expand-file-name "test-kitty-integration.el"
                        (file-name-directory load-file-name)))

;; Run tests
(buttercup-run)

(provide 'run-tests)
;;; run-tests.el ends here
