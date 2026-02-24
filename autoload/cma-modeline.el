;; -*- lexical-binding: t; -*-
;;; autoload/cma-modeline.el --- Modeline indicator for waiting agents

;;; Commentary:
;; Polls `cma list --json` on a timer to update the modeline when
;; any agent has waiting_for_input: true.

;;; Code:

(require 'cma-core)

(defvar cma-modeline--timer nil
  "Timer for modeline refresh.")

(defvar cma-modeline--text ""
  "Current modeline text for waiting agents.")

(defvar cma-modeline--interval 5
  "Seconds between modeline checks.")

;;;###autoload
(defun cma-modeline--start ()
  "Start modeline polling timer."
  (cma-modeline--stop)
  (setq cma-modeline--timer
        (run-with-timer 2 cma-modeline--interval #'cma-modeline--update)))

;;;###autoload
(defun cma-modeline--stop ()
  "Stop modeline polling timer."
  (when cma-modeline--timer
    (cancel-timer cma-modeline--timer)
    (setq cma-modeline--timer nil)))

(defun cma-modeline--update ()
  "Check for waiting agents and update modeline."
  (cma--call-async
   (lambda (agents)
     (let ((waiting (when agents
                      (seq-filter (lambda (a) (alist-get 'waiting_for_input a))
                                  agents))))
       (setq cma-modeline--text
             (if waiting
                 (format " [CMA:%d waiting]" (length waiting))
               ""))
       (force-mode-line-update t)))))

;;;###autoload
(defun cma-modeline--format ()
  "Return modeline string for Claude Multi-Agent status."
  cma-modeline--text)

(provide 'cma-modeline)
;;; cma-modeline.el ends here
