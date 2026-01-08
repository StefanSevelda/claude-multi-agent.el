;; -*- lexical-binding: t; -*-
;;; test-helper.el --- Test helper for claude-multi-agent

;;; Commentary:
;; Helper to set up test environment and load dependencies

;;; Code:

;; Add project root to load path
(let ((project-root (file-name-directory
                     (directory-file-name
                      (file-name-directory load-file-name)))))
  (add-to-list 'load-path project-root)
  (add-to-list 'load-path (expand-file-name "autoload" project-root)))

;; Setup package.el for installing test dependencies
(require 'package)
(setq package-user-dir (expand-file-name ".packages"
                                         (file-name-directory
                                          (directory-file-name
                                           (file-name-directory load-file-name)))))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install required packages if not present
(let ((required-packages '(buttercup alert f s dash)))
  (let ((need-refresh nil))
    (dolist (pkg required-packages)
      (unless (package-installed-p pkg)
        (setq need-refresh t)))
    (when need-refresh
      (package-refresh-contents))
    (dolist (pkg required-packages)
      (unless (package-installed-p pkg)
        (package-install pkg)))))

;; Load buttercup
(require 'buttercup)

;; Load project modules (loads config which defines variables)
(require 'claude-multi-config (expand-file-name "config.el"
                                                (file-name-directory
                                                 (directory-file-name
                                                  (file-name-directory load-file-name)))))

(provide 'test-helper)
;;; test-helper.el ends here
