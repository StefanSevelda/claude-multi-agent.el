;; -*- lexical-binding: t; -*-
;;; autoload/claude-multi-planning.el --- Org task triage for Claude Multi-Agent

;;; Commentary:
;; Presentation layer over a single org file (`claude-multi-planning-tasks-file')
;; that acts as the triage inbox for the user's task system.  Top-level
;; headings (Inbox / This Week / Backlog / Someday) are refile buckets;
;; tasks live as level-2 headings underneath.
;;
;; On every save of an org file under `claude-multi-planning-directory', a
;; Python script exports it to a sidecar .jsonl file that other tooling
;; (including agents) reads as the machine interface.  This module wires up
;; that export hook, a periodic auto-save timer so the sidecar stays fresh
;; even without an explicit save, and fast refile/triage commands.

;;; Code:

(require 'org)
(require 'org-capture)

(defgroup claude-multi-planning nil
  "Org-based task triage for Claude Multi-Agent."
  :group 'claude-multi
  :prefix "claude-multi-planning-")

;;; Customization

(defcustom claude-multi-planning-directory "~/org/planning/"
  "Directory containing the planning org files."
  :type 'directory
  :group 'claude-multi-planning)

(defcustom claude-multi-planning-tasks-file "~/org/planning/tasks.org"
  "The single triage file: top-level buckets, level-2 task headings."
  :type 'file
  :group 'claude-multi-planning)

(defcustom claude-multi-planning-export-script "~/org/scripts/org-to-jsonl.py"
  "Python script that exports a planning org file to a sidecar .jsonl file."
  :type 'file
  :group 'claude-multi-planning)

(defcustom claude-multi-planning-week-export-script "~/org/scripts/week-to-state.py"
  "Python script that exports a week org file's focus blocks to state.json."
  :type 'file
  :group 'claude-multi-planning)

(defcustom claude-multi-planning-autosave-interval 300
  "Seconds between automatic saves of modified planning org buffers."
  :type 'integer
  :group 'claude-multi-planning)

;;; Internal state

(defvar claude-multi-planning--autosave-timer nil
  "Timer driving `claude-multi-planning-autosave-mode', or nil when disabled.")

;;; Export on save

(defun claude-multi-planning--under-planning-dir-p (file)
  "Return non-nil if FILE lives under `claude-multi-planning-directory'.
Uses a plain expanded-path prefix comparison rather than
`file-in-directory-p', which refuses to answer (always returns nil)
when the directory does not yet exist on disk — a real possibility
the first time a planning file is created."
  (and file
       (string-prefix-p
        (file-name-as-directory (expand-file-name claude-multi-planning-directory))
        (expand-file-name file))))

;;;###autoload
(defun claude-multi-planning--maybe-setup-buffer ()
  "Wire up export-on-save and auto-revert for planning org buffers.
Intended for `find-file-hook'.  Only acts on org buffers whose file is
under `claude-multi-planning-directory'."
  (when (and buffer-file-name
             (derived-mode-p 'org-mode)
             (claude-multi-planning--under-planning-dir-p buffer-file-name))
    (add-hook 'after-save-hook #'claude-multi-planning--export-buffer nil t)
    (auto-revert-mode 1)))

(defun claude-multi-planning--export-sentinel (process _event)
  "Sentinel for the export PROCESS: silent on success, message on failure."
  (unless (process-live-p process)
    (let ((exit-code (process-exit-status process)))
      (unless (zerop exit-code)
        (message "claude-multi-planning: export failed (exit %d) for %s"
                 exit-code (process-get process 'claude-multi-planning-file))))))

(defun claude-multi-planning--week-file-p (file)
  "Return non-nil if FILE is a week plan file (week-YYYY-Www.org)."
  (and file
       (string-match-p "\\`week-[0-9]\\{4\\}-W[0-9]\\{2\\}\\.org\\'"
                       (file-name-nondirectory file))))

;;;###autoload
(defun claude-multi-planning--export-buffer ()
  "Export the current buffer's file to its machine sidecar via Python.
Week files (week-YYYY-Www.org) export their focus-block ticks to
state.json via `claude-multi-planning-week-export-script'; every other
planning org file exports to its .jsonl sidecar.  Runs asynchronously;
intended as a buffer-local `after-save-hook'."
  (when buffer-file-name
    (let* ((file (expand-file-name buffer-file-name))
           (script (expand-file-name
                    (if (claude-multi-planning--week-file-p file)
                        claude-multi-planning-week-export-script
                      claude-multi-planning-export-script)))
           (proc (start-process "claude-multi-planning-export"
                                 nil "python3" script file)))
      (process-put proc 'claude-multi-planning-file file)
      (set-process-sentinel proc #'claude-multi-planning--export-sentinel))))

;;; Auto-save timer

(defun claude-multi-planning--planning-org-buffer-p (buffer)
  "Return non-nil if BUFFER is a modified org buffer under the planning dir."
  (with-current-buffer buffer
    (and buffer-file-name
         (buffer-modified-p)
         (derived-mode-p 'org-mode)
         (claude-multi-planning--under-planning-dir-p buffer-file-name))))

(defun claude-multi-planning--autosave-tick ()
  "Silently save every modified planning org buffer.
Runs on `claude-multi-planning--autosave-timer'."
  (dolist (buffer (buffer-list))
    (when (claude-multi-planning--planning-org-buffer-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-message t))
          (save-buffer))))))

;;;###autoload
(define-minor-mode claude-multi-planning-autosave-mode
  "Global minor mode that periodically saves modified planning org buffers.
Saving triggers the export-on-save hook, which keeps the .jsonl sidecar
in sync even when the user hasn't explicitly saved."
  :global t
  :group 'claude-multi-planning
  (if claude-multi-planning-autosave-mode
      (unless claude-multi-planning--autosave-timer
        (setq claude-multi-planning--autosave-timer
              (run-with-timer claude-multi-planning-autosave-interval
                               claude-multi-planning-autosave-interval
                               #'claude-multi-planning--autosave-tick)))
    (when claude-multi-planning--autosave-timer
      (cancel-timer claude-multi-planning--autosave-timer)
      (setq claude-multi-planning--autosave-timer nil))))

;;;###autoload
(defun claude-multi-planning-save-all ()
  "Save all modified planning org buffers; return the list of saved files.
Safe to call when no buffers match (returns nil) and never prompts.
Intended to be invoked from an external merge script via
emacsclient --eval \"(claude-multi-planning-save-all)\"."
  (interactive)
  (let (saved)
    (dolist (buffer (buffer-list))
      (when (claude-multi-planning--planning-org-buffer-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-message t))
            (save-buffer))
          (push buffer-file-name saved))))
    (nreverse saved)))

;;; Refile helpers

(defun claude-multi-planning--bucket-heading-regexp (bucket-heading)
  "Return a regexp matching the level-1 org heading BUCKET-HEADING."
  (format "^\\* +%s\\([ \t]+:.*:\\)?[ \t]*$" (regexp-quote bucket-heading)))

(defun claude-multi-planning--bucket-position (bucket-heading)
  "Return the buffer position of top-level BUCKET-HEADING in the tasks file.
Signals an error if the heading cannot be found."
  (with-current-buffer (find-file-noselect (expand-file-name claude-multi-planning-tasks-file))
    (org-with-wide-buffer
     (goto-char (point-min))
     (unless (re-search-forward
              (claude-multi-planning--bucket-heading-regexp bucket-heading)
              nil t)
       (error "claude-multi-planning: bucket heading %S not found in %s"
              bucket-heading claude-multi-planning-tasks-file))
     (line-beginning-position))))

(defun claude-multi-planning--refile-to (bucket-heading)
  "Refile the heading at point to BUCKET-HEADING in the tasks file.
Works both from a regular org buffer (point on a heading) and from
`org-agenda-mode' (point on an agenda line).  Saves the tasks file
buffer afterwards so the export-on-save hook fires."
  (let* ((tasks-file (expand-file-name claude-multi-planning-tasks-file))
         (pos (claude-multi-planning--bucket-position bucket-heading))
         (rfloc (list bucket-heading tasks-file nil pos)))
    (if (derived-mode-p 'org-agenda-mode)
        (org-agenda-refile nil rfloc)
      (org-refile nil nil rfloc))
    (let ((tasks-buffer (find-buffer-visiting tasks-file)))
      (when tasks-buffer
        (with-current-buffer tasks-buffer
          (let ((inhibit-message t))
            (save-buffer)))))))

;;;###autoload
(defun claude-multi-planning-to-week ()
  "Refile the heading at point to the \"This Week\" bucket."
  (interactive)
  (claude-multi-planning--refile-to "This Week"))

;;;###autoload
(defun claude-multi-planning-to-backlog ()
  "Refile the heading at point to the \"Backlog\" bucket."
  (interactive)
  (claude-multi-planning--refile-to "Backlog"))

;;;###autoload
(defun claude-multi-planning-to-someday ()
  "Refile the heading at point to the \"Someday\" bucket."
  (interactive)
  (claude-multi-planning--refile-to "Someday"))

;;;###autoload
(defun claude-multi-planning-to-inbox ()
  "Refile the heading at point to the \"Inbox\" bucket."
  (interactive)
  (claude-multi-planning--refile-to "Inbox"))

;;; Triage / planning views

;;;###autoload
(defun claude-multi-planning-triage-inbox ()
  "Open the tasks file and narrow to the Inbox subtree for fast triage."
  (interactive)
  (find-file (expand-file-name claude-multi-planning-tasks-file))
  (widen)
  (goto-char (point-min))
  (re-search-forward (claude-multi-planning--bucket-heading-regexp "Inbox"))
  (goto-char (line-beginning-position))
  (org-narrow-to-subtree))

;;;###autoload
(defun claude-multi-planning-plan-week ()
  "Show a TODO-list agenda view restricted to the tasks file."
  (interactive)
  (let ((org-agenda-files (list (expand-file-name claude-multi-planning-tasks-file))))
    (org-todo-list)))

;;; Capture template

;;;###autoload
(defun claude-multi-planning-setup-capture ()
  "Register the \"t\" capture template (Task -> Inbox) if none exists yet.
`org-capture' is already required at module load time, so this is safe
to call any time after `claude-multi-planning' has loaded, e.g. from
`config.el'."
  (unless (assoc "t" org-capture-templates)
    (add-to-list 'org-capture-templates
                 `("t" "Task → Inbox" entry
                   (file+headline ,claude-multi-planning-tasks-file "Inbox")
                   "* TODO %?\n")
                 t)))

;;; Focus-block ticking (week files)

(defconst claude-multi-planning--block-line-regexp
  "^- \\[\\([ X-]\\)\\] [0-9]\\{2\\}:[0-9]\\{2\\}-[0-9]\\{2\\}:[0-9]\\{2\\} "
  "Matches a tickable focus-block line; group 1 is the checkbox state.")

(defun claude-multi-planning--flip-checkbox-line (line target)
  "Pure helper: return LINE with its checkbox set to TARGET (a string).
If the checkbox is already TARGET, revert it to \" \" (toggle).  Returns
nil when LINE is not a tickable focus-block line."
  (when (string-match claude-multi-planning--block-line-regexp line)
    (let* ((current (match-string 1 line))
           (new (if (string= current target) " " target)))
      (concat "- [" new "]" (substring line (+ (match-beginning 1) 2))))))

(defun claude-multi-planning--tick-block (target)
  "Set the focus-block checkbox on the current line to TARGET and save.
Updates the day heading's [a/b] cookie and saves the buffer silently so
the export-on-save hook pushes the tick into state.json."
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
         (new (claude-multi-planning--flip-checkbox-line line target)))
    (unless new
      (user-error "Not on a focus-block line"))
    (let ((col (current-column))
          (inhibit-read-only t))
      (delete-region (line-beginning-position) (line-end-position))
      (insert new)
      (move-to-column col))
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (ignore-errors (org-back-to-heading t))
        (ignore-errors (org-update-checkbox-count))))
    (when buffer-file-name
      (let ((inhibit-message t))
        (save-buffer)))))

;;;###autoload
(defun claude-multi-planning-tick-block-done ()
  "Tick the focus block at point as completed ([X]); tick again to undo."
  (interactive)
  (claude-multi-planning--tick-block "X"))

;;;###autoload
(defun claude-multi-planning-tick-block-skipped ()
  "Tick the focus block at point as skipped ([-]); tick again to undo."
  (interactive)
  (claude-multi-planning--tick-block "-"))

;;; Buffer hook wiring

(add-hook 'find-file-hook #'claude-multi-planning--maybe-setup-buffer)

(provide 'claude-multi-planning)
;;; claude-multi-planning.el ends here
