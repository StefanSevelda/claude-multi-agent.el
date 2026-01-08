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

;;; Mock functions for status tracking

(unless (fboundp 'claude-multi--notify-input-needed)
  (defun claude-multi--notify-input-needed (agent)
    "Mock version for tests - does nothing."
    nil))

(unless (fboundp 'claude-multi--update-session-from-status)
  (defun claude-multi--update-session-from-status (agent status-data)
    "Mock version for tests - does nothing."
    nil))

(unless (fboundp 'claude-multi--kitty-is-alive)
  (defun claude-multi--kitty-is-alive (agent)
    "Mock version for tests - always returns true."
    t))

;;; Mock status file helpers

(defvar claude-multi-status-directory "/tmp/claude-status-test/"
  "Test directory for status files (overrides production value).")

(defun test-helper--create-mock-status-file (session-id cwd)
  "Create a mock status file for testing.
SESSION-ID is the agent session ID, CWD is the working directory path."
  (let ((status-file (expand-file-name
                      (format "status-%s.json" session-id)
                      claude-multi-status-directory)))
    (make-directory claude-multi-status-directory t)
    (with-temp-file status-file
      (insert (json-encode `((cwd . ,cwd)
                            (session_id . ,session-id)
                            (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S"))
                            (claude_status . "running")
                            (waiting_for_input . :json-false)
                            (context_window . ((tokens_used . 1000)
                                             (tokens_total . 200000)
                                             (percentage_used . 0.5)))))))
    status-file))

(defun test-helper--cleanup-mock-status-files ()
  "Clean up mock status files after tests."
  (when (file-exists-p claude-multi-status-directory)
    (dolist (file (directory-files claude-multi-status-directory t "^status-.*\\.json$"))
      (delete-file file))
    (ignore-errors (delete-directory claude-multi-status-directory))))

(provide 'test-helper)
;;; test-helper.el ends here
