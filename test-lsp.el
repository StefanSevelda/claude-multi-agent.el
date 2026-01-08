;;; test-lsp.el --- Test file for LSP functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is used to test the elisp-lsp plugin functionality

;;; Code:

(defun test-function (arg1 arg2)
  "Test function that adds two arguments.
ARG1 is the first number.
ARG2 is the second number."
  (+ arg1 arg2))

(defvar test-variable 42
  "A test variable with the answer to everything.")

(defun another-test-function ()
  "Call test-function with some values."
  (test-function 10 20))

(provide 'test-lsp)
;;; test-lsp.el ends here
