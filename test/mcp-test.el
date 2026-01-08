;; -*- lexical-binding: t; -*-
;;; test/test-mcp.el --- Tests for MCP protocol implementation

;;; Commentary:
;; Comprehensive tests for the Model Context Protocol implementation

;;; Code:

(require 'buttercup)
(require 'claude-multi-mcp)

;;; Test utilities

(defun test-mcp--make-request (method params &optional request-id)
  "Create a test MCP request alist."
  `((jsonrpc . "2.0")
    (id . ,(or request-id 1))
    (method . ,method)
    (params . ,params)))

(defun test-mcp--make-agent ()
  "Create a mock agent for testing."
  (make-claude-agent
   :id "test-agent-1"
   :name "test-agent"
   :status 'running
   :working-directory default-directory  ; Use current directory which exists
   :created-at (current-time)))

;;; Tool registration tests

(describe "MCP Tool Registration"
  (before-all
    (clrhash claude-multi--mcp-tools))

  (it "registers a new tool"
    (claude-multi-mcp--register-tool "test/echo"
                                     (lambda (_agent-id params)
                                       `((echo . ,(cdr (assoc 'message params))))))
    (expect (gethash "test/echo" claude-multi--mcp-tools) :to-be-truthy))

  (it "overwrites existing tool"
    (claude-multi-mcp--register-tool "test/echo"
                                     (lambda (_agent-id _params) '((new . t))))
    (let ((handler (gethash "test/echo" claude-multi--mcp-tools)))
      (expect (funcall handler "agent-1" nil) :to-equal '((new . t))))))

;;; Request handling tests

(describe "MCP Request Handling"
  (before-each
    (clrhash claude-multi--mcp-tools)
    (claude-multi-mcp--register-tool "test/success"
                                     (lambda (_agent-id params)
                                       `((result . ,(cdr (assoc 'input params))))))
    (claude-multi-mcp--register-tool "test/error"
                                     (lambda (_agent-id _params)
                                       (error "Test error")))
    (claude-multi-mcp--register-tool "test/deferred"
                                     (lambda (_agent-id _params) 'deferred)))

  (it "handles successful request"
    (let* ((request (test-mcp--make-request "test/success" '((input . "hello"))))
           (response (claude-multi-mcp--handle-request "agent-1" request)))
      (expect (cdr (assoc 'jsonrpc response)) :to-equal "2.0")
      (expect (cdr (assoc 'id response)) :to-equal 1)
      (expect (cdr (assoc 'result response)) :to-be-truthy)
      (expect (cdr (assoc 'result (cdr (assoc 'result response)))) :to-equal "hello")))

  (it "handles method not found"
    (let* ((request (test-mcp--make-request "nonexistent/method" '()))
           (response (claude-multi-mcp--handle-request "agent-1" request)))
      (expect (cdr (assoc 'error response)) :to-be-truthy)
      (expect (cdr (assoc 'code (cdr (assoc 'error response)))) :to-equal -32601)))

  (it "handles handler errors"
    (let* ((request (test-mcp--make-request "test/error" '()))
           (response (claude-multi-mcp--handle-request "agent-1" request)))
      (expect (cdr (assoc 'error response)) :to-be-truthy)
      (expect (cdr (assoc 'code (cdr (assoc 'error response)))) :to-equal -32603)))

  (it "handles deferred response"
    (let* ((request (test-mcp--make-request "test/deferred" '()))
           (response (claude-multi-mcp--handle-request "agent-1" request)))
      (expect (cdr (assoc 'status (cdr (assoc 'result response)))) :to-equal "deferred"))))

;;; Deferred response tests

(describe "MCP Deferred Responses"
  (before-each
    (clrhash claude-multi--mcp-deferred-responses))

  (it "stores deferred callback"
    (let ((called nil)
          (result-data nil))
      (claude-multi-mcp--defer-response
       "agent-1" 42
       (lambda (result)
         (setq called t
               result-data result)))
      (expect (gethash "agent-1:42" claude-multi--mcp-deferred-responses) :to-be-truthy)))

  (it "completes deferred response"
    (let ((called nil)
          (result-data nil)
          (sent-message nil))
      ;; Mock send function
      (cl-letf (((symbol-function 'claude-multi-ws--send-message)
                 (lambda (_agent-id msg)
                   (setq sent-message msg))))
        (claude-multi-mcp--defer-response
         "agent-1" 42
         (lambda (result)
           (setq called t
                 result-data result)))
        (claude-multi-mcp--complete-deferred-response
         "agent-1" 42 '((data . "completed")))
        (expect sent-message :to-be-truthy)
        (expect (cdr (assoc 'result sent-message)) :to-equal '((data . "completed")))))))

;;; File operations tests

(describe "MCP File Operations"
  (let ((test-file "/tmp/claude-mcp-test.txt")
        (test-dir "/tmp"))

    (before-each
      (when (file-exists-p test-file)
        (delete-file test-file)))

    (after-each
      (when (file-exists-p test-file)
        (delete-file test-file)))

    (it "reads file contents"
      (with-temp-file test-file
        (insert "test content"))
      (let ((result (claude-multi-mcp--tool-file-read
                    "agent-1"
                    `((path . ,test-file)))))
        (expect (cdr (assoc 'content result)) :to-equal "test content")
        (expect (cdr (assoc 'path result)) :to-equal test-file)))

    (it "fails to read nonexistent file"
      (expect (claude-multi-mcp--tool-file-read
              "agent-1"
              '((path . "/nonexistent/file.txt")))
             :to-throw))

    (it "writes file contents"
      (let ((result (claude-multi-mcp--tool-file-write
                    "agent-1"
                    `((path . ,test-file)
                      (content . "new content")))))
        (expect (cdr (assoc 'success result)) :to-be-truthy)
        (expect (file-exists-p test-file) :to-be-truthy)
        (expect (with-temp-buffer
                 (insert-file-contents test-file)
                 (buffer-string))
               :to-equal "new content")))

    (it "lists directory files"
      (let ((result (claude-multi-mcp--tool-file-list
                    "agent-1"
                    `((path . ,test-dir)
                      (pattern . "*.txt")))))
        (expect (cdr (assoc 'files result)) :to-be-truthy)
        (expect (>= (cdr (assoc 'count result)) 0) :to-be-truthy)))))

;;; Git operations tests

(describe "MCP Git Operations"
  :var ((mock-agent nil))

  (before-each
    (setq mock-agent (test-mcp--make-agent))
    (spy-on 'claude-multi--get-agent-by-id :and-return-value mock-agent))

  (it "gets git status"
    (let ((result (claude-multi-mcp--tool-git-status "agent-1" '())))
      (expect (cdr (assoc 'status result)) :to-be-truthy)
      (expect (cdr (assoc 'directory result)) :to-be-truthy)))

  (it "gets git diff"
    (let ((result (claude-multi-mcp--tool-git-diff "agent-1" '())))
      (expect (cdr (assoc 'diff result)) :to-be-truthy)
      (expect (cdr (assoc 'directory result)) :to-be-truthy)))

  (it "gets staged git diff"
    (let ((result (claude-multi-mcp--tool-git-diff
                  "agent-1"
                  '((staged . t)))))
      (expect (cdr (assoc 'diff result)) :to-be-truthy))))

;;; Agent operations tests

(describe "MCP Agent Operations"
  :var ((mock-agent nil)
        (mock-agents nil))

  (before-each
    (setq mock-agent (test-mcp--make-agent))
    (setq mock-agents (list mock-agent))
    (setq claude-multi--agents mock-agents)
    (spy-on 'claude-multi--get-agent-by-id :and-return-value mock-agent))

  (it "gets agent status"
    (let ((result (claude-multi-mcp--tool-agent-status "test-agent-1" '())))
      (expect (cdr (assoc 'agent-id result)) :to-equal "test-agent-1")
      (expect (cdr (assoc 'status result)) :to-equal "running")
      (expect (cdr (assoc 'working-directory result)) :to-be-truthy)))

  (it "lists all agents"
    (let ((result (claude-multi-mcp--tool-agent-list "agent-1" '())))
      (expect (cdr (assoc 'count result)) :to-equal 1)
      (expect (length (cdr (assoc 'agents result))) :to-equal 1)))

  (it "focuses agent window"
    (let ((focused nil))
      (cl-letf (((symbol-function 'claude-multi--kitty-focus-window)
                 (lambda (_agent) (setq focused t))))
        (let ((result (claude-multi-mcp--tool-agent-focus "test-agent-1" '())))
          (expect (cdr (assoc 'success result)) :to-be-truthy)
          (expect focused :to-be-truthy))))))

;;; Session operations tests

(describe "MCP Session Operations"
  (it "saves session"
    (cl-letf (((symbol-function 'claude-multi-session--save)
               (lambda () "/tmp/session-test.el")))
      (let ((result (claude-multi-mcp--tool-session-save "agent-1" '())))
        (expect (cdr (assoc 'success result)) :to-be-truthy)
        (expect (cdr (assoc 'session-file result)) :to-equal "/tmp/session-test.el"))))

  (it "lists sessions"
    (cl-letf (((symbol-function 'claude-multi-session--list-sessions)
               (lambda () '("session-1" "session-2"))))
      (let ((result (claude-multi-mcp--tool-session-list "agent-1" '())))
        (expect (cdr (assoc 'count result)) :to-equal 2)
        (expect (length (cdr (assoc 'sessions result))) :to-equal 2)))))

;;; Diff/review operations tests

(describe "MCP Diff Operations"
  (before-each
    (clrhash claude-multi--mcp-deferred-responses))

  (it "defers diff request"
    (let ((result (claude-multi-mcp--tool-diff-request
                  "agent-1"
                  '((request-id . 100)
                    (file . "test.el")
                    (description . "Review changes")))))
      (expect result :to-equal 'deferred)
      (expect (gethash "agent-1:100" claude-multi--mcp-deferred-responses) :to-be-truthy))))

;;; Diagnostics operations tests

(describe "MCP Diagnostics Operations"
  (it "gets diagnostics for file"
    (let ((result (claude-multi-mcp--tool-diagnostics-get
                  "agent-1"
                  '((file . "/tmp/test.el")))))
      (expect (cdr (assoc 'diagnostics result)) :to-be-truthy)
      (expect (>= (cdr (assoc 'count result)) 0) :to-be-truthy)))

  (it "handles missing file gracefully"
    (let ((result (claude-multi-mcp--tool-diagnostics-get
                  "agent-1"
                  '((file . "/nonexistent.el")))))
      (expect (cdr (assoc 'count result)) :to-equal 0))))

;;; Selection operations tests

(describe "MCP Selection Operations"
  (it "gets no selection when none active"
    (with-temp-buffer
      (let ((result (claude-multi-mcp--tool-selection-get "agent-1" '())))
        (expect (cdr (assoc 'has-selection result)) :to-equal :json-false)
        (expect (cdr (assoc 'buffer result)) :to-be-truthy))))

  (it "gets active selection"
    (with-temp-buffer
      (insert "test content")
      (set-mark (point-min))
      (goto-char (point-max))
      (activate-mark)
      (let ((result (claude-multi-mcp--tool-selection-get "agent-1" '())))
        (expect (cdr (assoc 'has-selection result)) :to-be-truthy)
        (expect (cdr (assoc 'text result)) :to-equal "test content")))))

;;; Built-in tools registration tests

(describe "MCP Built-in Tools Registration"
  (it "registers all built-in tools"
    (clrhash claude-multi--mcp-tools)
    (claude-multi-mcp--register-builtin-tools)
    (expect (gethash "file/read" claude-multi--mcp-tools) :to-be-truthy)
    (expect (gethash "file/write" claude-multi--mcp-tools) :to-be-truthy)
    (expect (gethash "file/list" claude-multi--mcp-tools) :to-be-truthy)
    (expect (gethash "git/status" claude-multi--mcp-tools) :to-be-truthy)
    (expect (gethash "git/diff" claude-multi--mcp-tools) :to-be-truthy)
    (expect (gethash "agent/focus" claude-multi--mcp-tools) :to-be-truthy)
    (expect (gethash "agent/status" claude-multi--mcp-tools) :to-be-truthy)
    (expect (gethash "agent/list" claude-multi--mcp-tools) :to-be-truthy)
    (expect (gethash "diff/request" claude-multi--mcp-tools) :to-be-truthy)
    (expect (gethash "session/save" claude-multi--mcp-tools) :to-be-truthy)
    (expect (gethash "session/list" claude-multi--mcp-tools) :to-be-truthy)
    (expect (gethash "diagnostics/get" claude-multi--mcp-tools) :to-be-truthy)
    (expect (gethash "selection/get" claude-multi--mcp-tools) :to-be-truthy)))

(provide 'test-mcp)
;;; test-mcp.el ends here
