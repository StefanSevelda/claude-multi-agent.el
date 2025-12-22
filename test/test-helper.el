;; -*- lexical-binding: t; -*-
;;; test-helper.el --- Test helper for claude-multi-agent tests

;;; Commentary:
;; Helper functions and setup for tests

;;; Code:

;; Mock Doom Emacs macros that may not be available
(unless (fboundp 'map!)
  (defmacro map! (&rest _)
    "Mock map! macro for non-Doom environments."
    nil))

;; Initialize defcustom variables that config.el defines
(defvar claude-multi-agent-colors
  '("#FF4444" "#00D9FF" "#7B68EE" "#FF8C00" "#00FF7F")
  "Color palette for agents.")

(defvar claude-multi-agent-color-schemes
  '((1  :name "Bright Red"       :color "#FF4444" :text "#FFE5E5" :bg "#1a0808")
    (2  :name "Cyan"             :color "#00D9FF" :text "#E0F8FF" :bg "#081418"))
  "Full color schemes for agents.")

(defvar claude-multi-progress-buffer-name "*Claude Multi-Agent Progress*"
  "Name of the progress buffer.")

(defvar claude-multi-use-org-tags t
  "Whether to use org-mode tags in progress buffer.")

(defvar claude-multi-kitty-listen-address nil
  "Kitty listen address for remote control.")

(defvar claude-multi--progress-buffer nil
  "Buffer for displaying agent progress.")

(defvar claude-multi--current-session-window-id nil
  "Kitty window ID for current session.")

(defvar claude-multi--agents nil
  "List of active agents.")

(defvar claude-multi--session-start-time nil
  "Session start timestamp.")

(defvar claude-multi--agent-id-counter 0
  "Counter for generating agent IDs.")

;; Mock org functions that can hang in test environment
(defun org-show-subtree ()
  "Mock version for tests - does nothing."
  nil)

(defun org-hide-drawer-all ()
  "Mock version for tests - does nothing."
  nil)

(defun org-cycle (&optional arg)
  "Mock version for tests - does nothing."
  nil)

;; Mock file-notify functions that may not be available in batch mode
(unless (fboundp 'file-notify-add-watch)
  (defun file-notify-add-watch (file flags callback)
    "Mock version for tests - returns a dummy descriptor."
    'test-watch-descriptor))

(unless (fboundp 'file-notify-rm-watch)
  (defun file-notify-rm-watch (descriptor)
    "Mock version for tests - does nothing."
    nil))

;; Define the toggle commands that are in config.el
;; These need to be available for the progress visibility tests
(defun claude-multi/show-all-status-drawers ()
  "Show all agent STATUS drawers in the progress buffer."
  (interactive)
  (when (and claude-multi--progress-buffer
             (buffer-live-p claude-multi--progress-buffer))
    (with-current-buffer claude-multi--progress-buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\*\\* " nil t)
          (beginning-of-line)
          (org-show-subtree))))))

(defun claude-multi/hide-all-status-drawers ()
  "Hide all agent STATUS drawers in the progress buffer."
  (interactive)
  (when (and claude-multi--progress-buffer
             (buffer-live-p claude-multi--progress-buffer))
    (with-current-buffer claude-multi--progress-buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\*\\* " nil t)
          (beginning-of-line)
          (org-hide-drawer-all))))))

(defun claude-multi/toggle-all-status-drawers ()
  "Toggle visibility of all agent STATUS drawers in the progress buffer."
  (interactive)
  (when (and claude-multi--progress-buffer
             (buffer-live-p claude-multi--progress-buffer))
    (with-current-buffer claude-multi--progress-buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\*\\* " nil t)
          (beginning-of-line)
          (org-cycle))))))

(provide 'test-helper)
;;; test-helper.el ends here
