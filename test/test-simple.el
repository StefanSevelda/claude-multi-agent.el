;; -*- lexical-binding: t; -*-
;;; test-simple.el --- Simple smoke tests for cma backend

;;; Commentary:
;; Basic tests to verify test infrastructure and cma modules load correctly.

;;; Code:

(require 'buttercup)
(load-file "test/test-helper.el")

;; Load cma modules under test
(require 'cma-core)

(describe "Simple Smoke Tests"

  (it "cma-core module loads"
    (expect (featurep 'cma-core) :to-be-truthy))

  (it "cma--call function exists"
    (expect (fboundp 'cma--call) :to-be-truthy))

  (it "cma--call-raw function exists"
    (expect (fboundp 'cma--call-raw) :to-be-truthy))

  (it "cma--call-async function exists"
    (expect (fboundp 'cma--call-async) :to-be-truthy))

  (it "cma--available-p function exists"
    (expect (fboundp 'cma--available-p) :to-be-truthy))

  (it "cma-binary-path is a string"
    (expect (stringp cma-binary-path) :to-be-truthy))

  (it "cma--call returns nil on failure"
    (spy-on 'call-process-shell-command :and-return-value 1)
    (let ((result (cma--call "nonexistent-subcommand")))
      (expect result :to-be nil)))

  (it "cma--last-error is set on failure"
    (spy-on 'call-process-shell-command :and-return-value 1)
    (cma--call "nonexistent-subcommand")
    (expect cma--last-error :to-be-truthy))

  (it "can create progress buffer"
    (let ((buf (get-buffer-create "*test-simple-progress*")))
      (expect (buffer-live-p buf) :to-be-truthy)
      (kill-buffer buf))))

(provide 'test-simple)
;;; test-simple.el ends here
