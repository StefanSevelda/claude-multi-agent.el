;; -*- lexical-binding: t; -*-
;;; test-helper.el --- Test helper for claude-multi-agent tests

;;; Commentary:
;; Helper functions and setup for tests.
;; Mocks Doom Emacs macros and provides shared test utilities.

;;; Code:

;; Mock Doom Emacs macros that may not be available
(unless (fboundp 'map!)
  (defmacro map! (&rest _)
    "Mock map! macro for non-Doom environments."
    nil))

;; Variables that config.el normally defines (needed by cma modules)
(defvar claude-multi-default-model "sonnet"
  "Default Claude model for new agents.")

(defvar claude-multi-progress-buffer-name "*Claude Multi-Agent Progress*"
  "Name of the progress buffer.")

(defvar claude-multi--progress-buffer nil
  "Buffer for displaying agent progress.")

(defvar claude-multi-agent-colors
  '("#FF4444" "#00D9FF" "#7B68EE" "#FF8C00" "#00FF7F")
  "Color palette for agents.")

;; Mock ediff variable that may not be available
(defvar ediff-control-buffer nil
  "Mock ediff control buffer for tests.")

(provide 'test-helper)
;;; test-helper.el ends here
