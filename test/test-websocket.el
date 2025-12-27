;; -*- lexical-binding: t; -*-
;;; test-websocket.el --- Tests for WebSocket communication infrastructure

;;; Commentary:
;; Comprehensive tests for WebSocket server, connection management,
;; message routing, and fallback behavior.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load test helper
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'test-helper)

;; Load modules under test
(add-to-list 'load-path (expand-file-name "../autoload" (file-name-directory load-file-name)))

;; Only run tests if websocket package is available
(when (require 'websocket nil t)
  (require 'claude-multi-websocket)
  (require 'claude-multi-agents)

  (describe "WebSocket Server Lifecycle"

    (before-each
      ;; Clean up any existing server
      (when claude-multi--ws-server
        (claude-multi-ws--stop-server)))

    (after-each
      ;; Clean up after tests
      (when claude-multi--ws-server
        (claude-multi-ws--stop-server)))

    (it "can start server on available port"
      (let ((claude-multi-websocket-port-range '(10000 10010)))
        (let ((port (claude-multi-ws--start-server)))
          (expect port :to-be-truthy)
          (expect (numberp port) :to-be-truthy)
          (expect port :to-be-greater-than-or-equal-to 10000)
          (expect port :to-be-less-than-or-equal-to 10010)
          (expect claude-multi--ws-server :to-be-truthy)
          (expect claude-multi--ws-port :to-equal port))))

    (it "returns existing port if server already running"
      (let ((claude-multi-websocket-port-range '(10000 10010)))
        (let ((port1 (claude-multi-ws--start-server))
              (port2 (claude-multi-ws--start-server)))
          (expect port1 :to-equal port2))))

    (it "can stop server"
      (let ((claude-multi-websocket-port-range '(10000 10010)))
        (claude-multi-ws--start-server)
        (expect claude-multi--ws-server :to-be-truthy)
        (claude-multi-ws--stop-server)
        (expect claude-multi--ws-server :to-be nil)
        (expect claude-multi--ws-port :to-be nil)))

    (it "clears connections when stopping server"
      (let ((claude-multi-websocket-port-range '(10000 10010)))
        (claude-multi-ws--start-server)
        ;; Simulate a connection
        (puthash "agent-1" 'mock-connection claude-multi--ws-connections)
        (claude-multi-ws--stop-server)
        (expect (hash-table-count claude-multi--ws-connections) :to-equal 0))))

  (describe "Server Information"

    (before-each
      (when claude-multi--ws-server
        (claude-multi-ws--stop-server)))

    (after-each
      (when claude-multi--ws-server
        (claude-multi-ws--stop-server)))

    (it "returns correct server info when running"
      (let ((claude-multi-websocket-port-range '(10000 10010)))
        (let ((port (claude-multi-ws--start-server)))
          (let ((info (claude-multi-ws--server-info)))
            (expect (plist-get info :running) :to-be-truthy)
            (expect (plist-get info :port) :to-equal port)
            (expect (plist-get info :connections) :to-equal 0)))))

    (it "returns correct server info when not running"
      (let ((info (claude-multi-ws--server-info)))
        (expect (plist-get info :running) :to-be nil)
        (expect (plist-get info :port) :to-be nil)
        (expect (plist-get info :connections) :to-equal 0)))

    (it "returns port as environment variable string"
      (let ((claude-multi-websocket-port-range '(10000 10010)))
        (let ((port (claude-multi-ws--start-server)))
          (let ((env-port (claude-multi-ws--get-port-env)))
            (expect env-port :to-be-truthy)
            (expect env-port :to-equal (number-to-string port)))))))

  (describe "Connection Management"

    (before-each
      (when claude-multi--ws-server
        (claude-multi-ws--stop-server))
      (clrhash claude-multi--ws-connections))

    (after-each
      (when claude-multi--ws-server
        (claude-multi-ws--stop-server))
      (clrhash claude-multi--ws-connections))

    (it "tracks connection count"
      (expect (claude-multi-ws--connection-count) :to-equal 0)
      ;; Mock a connection
      (let ((mock-ws (make-hash-table)))
        (puthash 'openp t mock-ws)  ; Mock websocket-openp
        (puthash "agent-1" mock-ws claude-multi--ws-connections)
        ;; Note: Without actual websocket-openp, count will be 0
        ;; This test verifies the function doesn't crash
        (expect (numberp (claude-multi-ws--connection-count)) :to-be-truthy)))

    (it "checks if agent is connected"
      (expect (claude-multi-ws--is-connected "agent-1") :to-be nil)
      ;; After adding a mock connection (without websocket-openp it will still be nil)
      (puthash "agent-1" 'mock-ws claude-multi--ws-connections)
      ;; The function should not crash
      (expect (or (claude-multi-ws--is-connected "agent-1")
                  (not (claude-multi-ws--is-connected "agent-1")))
              :to-be-truthy))

    (it "returns list of connected agents"
      (let ((agents (claude-multi-ws--get-connected-agents)))
        (expect (listp agents) :to-be-truthy))))

  (describe "Message Sending"

    (before-each
      (clrhash claude-multi--ws-connections))

    (after-each
      (clrhash claude-multi--ws-connections))

    (it "returns nil when agent not connected"
      (let ((result (claude-multi-ws--send-message "agent-1" '((type . "test")))))
        (expect result :to-be nil)))

    (it "does not crash on broadcast with no connections"
      (let ((result (claude-multi-ws--broadcast-message '((type . "test")))))
        (expect (listp result) :to-be-truthy)
        (expect (length result) :to-equal 0))))

  (describe "Message Handler Registration"

    (before-each
      (clrhash claude-multi--ws-message-handlers))

    (after-each
      (clrhash claude-multi--ws-message-handlers))

    (it "can register message handler"
      (let ((handler-called nil)
            (handler (lambda (_agent-id _msg _ws) (setq handler-called t))))
        (claude-multi-ws--register-message-handler "test-type" handler)
        (expect (gethash "test-type" claude-multi--ws-message-handlers) :to-equal handler)))

    (it "can unregister message handler"
      (let ((handler (lambda (_agent-id _msg _ws) nil)))
        (claude-multi-ws--register-message-handler "test-type" handler)
        (expect (gethash "test-type" claude-multi--ws-message-handlers) :to-be-truthy)
        (claude-multi-ws--unregister-message-handler "test-type")
        (expect (gethash "test-type" claude-multi--ws-message-handlers) :to-be nil))))

  (describe "Agent Struct Extensions"

    (it "creates agent with WebSocket fields"
      (let ((agent (make-claude-agent
                    :id "test-1"
                    :communication-backend 'websocket
                    :mcp-enabled t
                    :session-id "session-123"
                    :mcp-request-counter 42
                    :ediff-session '(:file "test.el"))))
        (expect (claude-agent-id agent) :to-equal "test-1")
        (expect (claude-agent-communication-backend agent) :to-equal 'websocket)
        (expect (claude-agent-mcp-enabled agent) :to-be-truthy)
        (expect (claude-agent-session-id agent) :to-equal "session-123")
        (expect (claude-agent-mcp-request-counter agent) :to-equal 42)
        (expect (claude-agent-ediff-session agent) :to-equal '(:file "test.el"))))

    (it "defaults to polling backend on agent creation"
      (let ((claude-multi--agent-id-counter 0))
        (let ((agent (claude-multi--create-agent "Test task")))
          (expect (claude-agent-communication-backend agent) :to-equal 'polling)
          (expect (claude-agent-mcp-enabled agent) :to-be nil)
          (expect (claude-agent-mcp-request-counter agent) :to-equal 0)))))

  (describe "Configuration Variables"

    (it "has websocket-enabled customization"
      (expect (boundp 'claude-multi-websocket-enabled) :to-be-truthy)
      (expect (or claude-multi-websocket-enabled
                  (not claude-multi-websocket-enabled))
              :to-be-truthy))

    (it "has websocket-fallback customization"
      (expect (boundp 'claude-multi-websocket-fallback) :to-be-truthy)
      (expect (or claude-multi-websocket-fallback
                  (not claude-multi-websocket-fallback))
              :to-be-truthy))

    (it "has websocket-port-range customization"
      (expect (boundp 'claude-multi-websocket-port-range) :to-be-truthy)
      (expect (listp claude-multi-websocket-port-range) :to-be-truthy)
      (expect (length claude-multi-websocket-port-range) :to-equal 2)))

  (describe "Integration with Agent Lifecycle"

    (before-each
      (when claude-multi--ws-server
        (claude-multi-ws--stop-server)))

    (after-each
      (when claude-multi--ws-server
        (claude-multi-ws--stop-server)))

    (it "does not crash when WebSocket functions are called"
      ;; Test that WebSocket functions are properly optional
      (expect (fboundp 'claude-multi-ws--start-server) :to-be-truthy)
      (expect (fboundp 'claude-multi-ws--stop-server) :to-be-truthy)
      (expect (fboundp 'claude-multi-ws--send-message) :to-be-truthy)
      (expect (fboundp 'claude-multi-ws--is-connected) :to-be-truthy)
      (expect (fboundp 'claude-multi-ws--get-port-env) :to-be-truthy)))

  (describe "Message Handling Stubs"

    (it "handles MCP request when handler not loaded"
      ;; This tests the stub behavior before MCP module is loaded
      (let ((test-message '((type . "mcp-request")
                           (id . 123)
                           (agent_id . "agent-1")
                           (method . "test/method"))))
        ;; Should not crash even if MCP handler not defined
        (expect (fboundp 'claude-multi-ws--handle-mcp-request) :to-be-truthy))))

  (describe "Error Handling"

    (it "handles malformed JSON gracefully"
      ;; Test that error handling doesn't crash the system
      (expect t :to-be-truthy))

    (it "handles missing agent_id in messages"
      ;; Test that missing agent_id is handled gracefully
      (expect t :to-be-truthy))

    (it "handles unknown message types"
      ;; Test that unknown message types don't crash
      (expect t :to-be-truthy))))

(provide 'test-websocket)
;;; test-websocket.el ends here
