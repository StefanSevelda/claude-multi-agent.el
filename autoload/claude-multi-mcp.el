;; -*- lexical-binding: t; -*-
;;; autoload/claude-multi-mcp.el --- MCP Protocol implementation for Claude Multi-Agent

;;; Commentary:
;; Model Context Protocol (MCP) implementation for bidirectional communication
;; between Claude agents and Emacs. Provides tools for file operations, git,
;; agent management, and interactive features like diff review.
;; All git operations delegate to the `cma git` subcommands.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'cma-core)

;; Forward declarations
(declare-function claude-multi-ediff--create-session "claude-multi-ediff")
(declare-function claude-multi-ediff--get-changed-files "claude-multi-ediff")
(declare-function claude-multi-ediff--show-diff "claude-multi-ediff")
(declare-function claude-multi-ediff--set-mcp-request-id "claude-multi-ediff")
(declare-function flycheck-error-line "flycheck")
(declare-function flycheck-error-column "flycheck")
(declare-function flycheck-error-message "flycheck")
(declare-function flycheck-error-level "flycheck")
(declare-function flycheck-overlay-errors-in "flycheck")
(declare-function flymake-diagnostic-text "flymake")
(declare-function flymake-diagnostic-beg "flymake")
(declare-function flymake-diagnostic-end "flymake")
(declare-function flymake-diagnostic-type "flymake")

;;; Global variables

(defvar claude-multi--mcp-tools (make-hash-table :test 'equal)
  "Hash table mapping tool names to handler functions.")

(defvar claude-multi--mcp-deferred-responses (make-hash-table :test 'equal)
  "Hash table storing deferred response callbacks.
Key format: \"agent-id:request-id\" -> callback function.")

;;; Tool registration

;;;###autoload
(defun claude-multi-mcp--register-tool (tool-name handler-fn)
  "Register MCP tool TOOL-NAME with HANDLER-FN.
HANDLER-FN should accept (agent-id params) and return result or error."
  (puthash tool-name handler-fn claude-multi--mcp-tools))

;;;###autoload
(defun claude-multi-mcp--handle-request (agent-id request)
  "Handle MCP REQUEST from AGENT-ID.
REQUEST should be a JSON-RPC 2.0 formatted alist with id, method, and params.
Returns response alist ready for JSON encoding."
  (let* ((request-id (cdr (assoc 'id request)))
         (method (cdr (assoc 'method request)))
         (params (cdr (assoc 'params request))))

    (condition-case err
        (let ((handler (gethash method claude-multi--mcp-tools)))
          (if handler
              (let ((result (funcall handler agent-id params)))
                (if (eq result 'deferred)
                    `((jsonrpc . "2.0")
                      (id . ,request-id)
                      (result . ((status . "deferred")
                                (message . "Request accepted, response will be sent when ready"))))
                  `((jsonrpc . "2.0")
                    (id . ,request-id)
                    (result . ,result))))
            `((jsonrpc . "2.0")
              (id . ,request-id)
              (error . ((code . -32601)
                       (message . ,(format "Method not found: %s" method)))))))
      (error
       `((jsonrpc . "2.0")
         (id . ,request-id)
         (error . ((code . -32603)
                  (message . ,(format "Internal error: %s" (error-message-string err))))))))))

;;;###autoload
(defun claude-multi-mcp--defer-response (agent-id request-id callback)
  "Defer response for REQUEST-ID from AGENT-ID.
CALLBACK will be called with the result when ready."
  (let ((key (format "%s:%s" agent-id request-id)))
    (puthash key callback claude-multi--mcp-deferred-responses)))

;;;###autoload
(defun claude-multi-mcp--complete-deferred-response (agent-id request-id result)
  "Complete deferred response for REQUEST-ID from AGENT-ID with RESULT."
  (let* ((key (format "%s:%s" agent-id request-id))
         (callback (gethash key claude-multi--mcp-deferred-responses)))
    (when callback
      (remhash key claude-multi--mcp-deferred-responses)
      (funcall callback result))))

;;; Helper to find agent by ID from cma list

(defun claude-multi-mcp--find-agent (agent-id)
  "Find agent with AGENT-ID from cma list --json."
  (let ((agents (cma--call "list" "--json")))
    (cl-find-if (lambda (a)
                  (string= (alist-get 'session_id a) agent-id))
                agents)))

;;; Built-in MCP Tools

;; File operations

(defun claude-multi-mcp--tool-file-read (_agent-id params)
  "Read file contents. PARAMS: ((path . \"/path/to/file\"))."
  (let ((path (cdr (assoc 'path params))))
    (unless path
      (error "Missing required parameter: path"))
    (unless (file-exists-p path)
      (error "File not found: %s" path))
    (condition-case err
        `((content . ,(with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string)))
          (path . ,path))
      (error
       (signal 'error (list (format "Failed to read file: %s" (error-message-string err))))))))

(defun claude-multi-mcp--tool-file-write (_agent-id params)
  "Write file contents. PARAMS: ((path . \"/path\") (content . \"...\"))."
  (let ((path (cdr (assoc 'path params)))
        (content (cdr (assoc 'content params))))
    (unless path
      (error "Missing required parameter: path"))
    (unless content
      (error "Missing required parameter: content"))
    (condition-case err
        (progn
          (with-temp-file path
            (insert content))
          `((success . t)
            (path . ,path)
            (bytes . ,(length content))))
      (error
       (signal 'error (list (format "Failed to write file: %s" (error-message-string err))))))))

(defun claude-multi-mcp--tool-file-list (_agent-id params)
  "List files in directory. PARAMS: ((path . \"/path\") (pattern . \"*.el\"))."
  (let ((path (cdr (assoc 'path params)))
        (pattern (or (cdr (assoc 'pattern params)) "*")))
    (unless path
      (error "Missing required parameter: path"))
    (unless (file-directory-p path)
      (error "Not a directory: %s" path))
    (condition-case err
        (let ((files (directory-files path nil pattern)))
          `((files . ,(apply 'vector files))
            (count . ,(length files))))
      (error
       (signal 'error (list (format "Failed to list directory: %s" (error-message-string err))))))))

;; Git operations — delegate to cma git subcommands

(defun claude-multi-mcp--tool-git-status (agent-id _params)
  "Get git status for agent's working directory."
  (let* ((agent (claude-multi-mcp--find-agent agent-id))
         (dir (alist-get 'cwd agent)))
    (condition-case err
        (let ((output (cma--call-raw "git" "status" "--dir" dir)))
          `((status . ,(or output ""))
            (directory . ,dir)))
      (error
       (signal 'error (list (format "Git command failed: %s" (error-message-string err))))))))

(defun claude-multi-mcp--tool-git-diff (agent-id params)
  "Get git diff output. PARAMS: ((staged . t/nil) (file . \"path\"))."
  (let* ((agent (claude-multi-mcp--find-agent agent-id))
         (dir (alist-get 'cwd agent))
         (staged (cdr (assoc 'staged params)))
         (file (cdr (assoc 'file params)))
         (args (list "git" "diff" "--dir" dir)))
    (when staged
      (setq args (append args (list "--staged"))))
    (when file
      (setq args (append args (list "--file" file))))
    (condition-case err
        (let ((output (apply #'cma--call-raw args)))
          `((diff . ,(or output ""))
            (directory . ,dir)))
      (error
       (signal 'error (list (format "Git diff failed: %s" (error-message-string err))))))))

;; Agent operations — delegate to cma CLI

(defun claude-multi-mcp--tool-agent-focus (agent-id _params)
  "Focus the agent's kitty window."
  (condition-case err
      (progn
        (cma--call-raw "focus" agent-id)
        `((success . t)
          (agent-id . ,agent-id)))
    (error
     (signal 'error (list (format "Failed to focus window: %s" (error-message-string err)))))))

(defun claude-multi-mcp--tool-agent-status (agent-id _params)
  "Get status of the requesting agent."
  (let ((agent (claude-multi-mcp--find-agent agent-id)))
    (if agent
        `((agent-id . ,agent-id)
          (status . ,(alist-get 'status agent))
          (working-directory . ,(alist-get 'cwd agent)))
      (error "Agent not found: %s" agent-id))))

(defun claude-multi-mcp--tool-agent-list (_agent-id _params)
  "List all active agents."
  (let ((agents (cma--call "list" "--json")))
    `((agents . ,(apply 'vector
                        (mapcar (lambda (a)
                                  `((id . ,(alist-get 'session_id a))
                                    (status . ,(alist-get 'status a))))
                                (or agents '()))))
      (count . ,(length (or agents '()))))))

;; Diff/review operations (deferred)

(defun claude-multi-mcp--tool-diff-request (agent-id params)
  "Request interactive diff review (deferred response).
PARAMS: ((file . \"path\") (description . \"...\") (request-id . \"...\"))."
  (let* ((request-id (cdr (assoc 'request-id params)))
         (file (cdr (assoc 'file params)))
         (agent (claude-multi-mcp--find-agent agent-id)))

    (unless agent
      (error "Agent not found: %s" agent-id))

    ;; Store callback for later completion
    (claude-multi-mcp--defer-response
     agent-id
     request-id
     (lambda (result)
       (message "Diff review completed for %s: %s" agent-id result)))

    ;; Launch ediff review
    (let ((dir (alist-get 'cwd agent)))
      (if file
          ;; Single file review
          (progn
            (claude-multi-ediff--create-session agent (list file))
            (claude-multi-ediff--set-mcp-request-id request-id)
            (claude-multi-ediff--show-diff file))
        ;; No file specified - review all changed files
        (let ((changed-files (claude-multi-ediff--get-changed-files dir)))
          (if (null changed-files)
              (progn
                (claude-multi-mcp--complete-deferred-response
                 agent-id request-id
                 '((status . "no_changes")
                   (accepted . [])
                   (rejected . [])))
                'deferred)
            ;; Create session and start review
            (claude-multi-ediff--create-session agent changed-files)
            (claude-multi-ediff--set-mcp-request-id request-id)
            (let* ((session claude-multi--ediff-session)
                   (first-file (car changed-files)))
              (plist-put session :files-to-review (cdr changed-files))
              (claude-multi-ediff--show-diff first-file))))))

    ;; Return deferred marker
    'deferred))

;; Session operations — delegate to cma CLI

(defun claude-multi-mcp--tool-session-save (_agent-id _params)
  "Save current multi-agent session."
  (condition-case err
      (let ((output (cma--call-raw "session" "save")))
        `((success . t)
          (message . ,(or output "Session saved"))))
    (error
     (signal 'error (list (format "Failed to save session: %s" (error-message-string err)))))))

(defun claude-multi-mcp--tool-session-list (_agent-id _params)
  "List available saved sessions."
  (condition-case err
      (let ((sessions (cma--call "session" "list" "--json")))
        `((sessions . ,(apply 'vector (or sessions '())))
          (count . ,(length (or sessions '())))))
    (error
     (signal 'error (list (format "Failed to list sessions: %s" (error-message-string err)))))))

;; Diagnostics operations

(defun claude-multi-mcp--tool-diagnostics-get (_agent-id params)
  "Get Flymake/Flycheck diagnostics.
PARAMS: ((file . \"path\") (severity . \"error\"))."
  (let* ((file (cdr (assoc 'file params)))
         (severity (cdr (assoc 'severity params)))
         (diagnostics '()))

    ;; Check if file buffer exists and has diagnostics
    (when file
      (when-let ((buf (find-buffer-visiting file)))
        (with-current-buffer buf
          ;; Try Flymake first
          (when (and (boundp 'flymake-mode) flymake-mode)
            (when-let ((diags (flymake-diagnostics)))
              (dolist (diag diags)
                (let ((diag-severity (flymake-diagnostic-type diag)))
                  (when (or (not severity)
                           (string= severity (symbol-name diag-severity)))
                    (push `((line . ,(line-number-at-pos (flymake-diagnostic-beg diag)))
                           (column . ,(save-excursion
                                       (goto-char (flymake-diagnostic-beg diag))
                                       (current-column)))
                           (severity . ,(symbol-name diag-severity))
                           (message . ,(flymake-diagnostic-text diag)))
                         diagnostics)))))))

          ;; Try Flycheck if Flymake not available
          (when (and (not diagnostics)
                    (boundp 'flycheck-mode)
                    flycheck-mode)
            (when-let ((errs (flycheck-overlay-errors-in (point-min) (point-max))))
              (dolist (err errs)
                (let ((err-severity (flycheck-error-level err)))
                  (when (or (not severity)
                           (string= severity (symbol-name err-severity)))
                    (push `((line . ,(flycheck-error-line err))
                           (column . ,(flycheck-error-column err))
                           (severity . ,(symbol-name err-severity))
                           (message . ,(flycheck-error-message err)))
                         diagnostics))))))))

    `((diagnostics . ,(apply 'vector (nreverse diagnostics)))
      (count . ,(length diagnostics))
      (file . ,file))))

;; Selection/region operations

(defun claude-multi-mcp--tool-selection-get (_agent-id _params)
  "Get current Emacs selection/region."
  (if (use-region-p)
      `((has-selection . t)
        (text . ,(buffer-substring-no-properties (region-beginning) (region-end)))
        (start . ,(region-beginning))
        (end . ,(region-end))
        (buffer . ,(buffer-name)))
    `((has-selection . :json-false)
      (buffer . ,(buffer-name)))))

;;; Initialize built-in tools

(defun claude-multi-mcp--register-builtin-tools ()
  "Register all built-in MCP tools."
  (claude-multi-mcp--register-tool "file/read" #'claude-multi-mcp--tool-file-read)
  (claude-multi-mcp--register-tool "file/write" #'claude-multi-mcp--tool-file-write)
  (claude-multi-mcp--register-tool "file/list" #'claude-multi-mcp--tool-file-list)
  (claude-multi-mcp--register-tool "git/status" #'claude-multi-mcp--tool-git-status)
  (claude-multi-mcp--register-tool "git/diff" #'claude-multi-mcp--tool-git-diff)
  (claude-multi-mcp--register-tool "agent/focus" #'claude-multi-mcp--tool-agent-focus)
  (claude-multi-mcp--register-tool "agent/status" #'claude-multi-mcp--tool-agent-status)
  (claude-multi-mcp--register-tool "agent/list" #'claude-multi-mcp--tool-agent-list)
  (claude-multi-mcp--register-tool "diff/request" #'claude-multi-mcp--tool-diff-request)
  (claude-multi-mcp--register-tool "session/save" #'claude-multi-mcp--tool-session-save)
  (claude-multi-mcp--register-tool "session/list" #'claude-multi-mcp--tool-session-list)
  (claude-multi-mcp--register-tool "diagnostics/get" #'claude-multi-mcp--tool-diagnostics-get)
  (claude-multi-mcp--register-tool "selection/get" #'claude-multi-mcp--tool-selection-get))

;; Register tools on load
(claude-multi-mcp--register-builtin-tools)

(provide 'claude-multi-mcp)
;;; claude-multi-mcp.el ends here
