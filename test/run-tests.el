;; -*- lexical-binding: t; -*-
;;; run-tests.el --- Test runner for claude-multi-agent

;;; Commentary:
;; Script to run all buttercup tests for the project

;;; Code:

;; Add parent directory to load path
(let ((project-root (file-name-directory
                     (directory-file-name
                      (file-name-directory load-file-name)))))
  (add-to-list 'load-path project-root)
  (add-to-list 'load-path (expand-file-name "autoload" project-root)))

;; Load dependencies
(require 'buttercup)

;; Load test files
(load (expand-file-name "test-vterm-integration.el"
                        (file-name-directory load-file-name)))

;; Run tests
(buttercup-run)

(provide 'run-tests)
;;; run-tests.el ends here
