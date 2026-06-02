;; -*- lexical-binding: t; -*-
;;; autoload/cma-core.el --- Bridge between Emacs and the cma Go CLI

;;; Commentary:
;; Provides synchronous and asynchronous shell-out to the `cma` binary,
;; with JSON parsing. All cma-commands.el and cma-table.el functions
;; call through this layer.

;;; Code:

(require 'json)

(defvar cma-binary-path (or (executable-find "cma") "cma")
  "Path to the cma binary.
Auto-detected from PATH, or can be set explicitly.")

(defvar cma--last-error nil
  "Last error message from cma invocation.")

;;;###autoload
(defun cma--call (&rest args)
  "Call cma binary with ARGS, return parsed JSON as alist.
Returns nil and sets `cma--last-error' on failure."
  (let* ((cmd (mapconcat #'shell-quote-argument
                         (cons cma-binary-path args) " "))
         (output (with-temp-buffer
                   (let ((exit-code (call-process-shell-command cmd nil t)))
                     (if (and (numberp exit-code) (= exit-code 0))
                         (progn
                           (setq cma--last-error nil)
                           (buffer-string))
                       (setq cma--last-error
                             (if (stringp exit-code)
                                 exit-code
                               (string-trim (buffer-string))))
                       nil)))))
    (when output
      (condition-case err
          (json-parse-string output :object-type 'alist :array-type 'list)
        (json-parse-error
         (setq cma--last-error (format "JSON parse error: %s" err))
         nil)))))

;;;###autoload
(defun cma--call-raw (&rest args)
  "Call cma binary with ARGS, return raw string output.
Returns nil on failure."
  (let* ((cmd (mapconcat #'shell-quote-argument
                         (cons cma-binary-path args) " "))
         (output (shell-command-to-string cmd)))
    (unless (string-empty-p output)
      (string-trim output))))

;;;###autoload
(defun cma--call-async (callback &rest args)
  "Call cma asynchronously, pass parsed JSON to CALLBACK.
CALLBACK receives one argument: the parsed JSON result (or nil on error)."
  (let* ((buf (generate-new-buffer " *cma-async*"))
         (proc (apply #'start-process "cma" buf cma-binary-path args)))
    (set-process-sentinel
     proc
     (lambda (p _event)
       (when (memq (process-status p) '(exit signal))
         (let ((result nil))
           (when (= (process-exit-status p) 0)
             (with-current-buffer (process-buffer p)
               (condition-case nil
                   (setq result (json-parse-string
                                 (buffer-string)
                                 :object-type 'alist
                                 :array-type 'list))
                 (json-parse-error nil))))
           (funcall callback result)
           (kill-buffer (process-buffer p))))))
    proc))

;;;###autoload
(defun cma--available-p ()
  "Return non-nil if the cma binary is available."
  (and cma-binary-path
       (executable-find cma-binary-path)))

(provide 'cma-core)
;;; cma-core.el ends here
