;; -*- lexical-binding: t; -*-
;;; autoload/claude-multi-websocket.el --- WebSocket communication for Claude Multi-Agent

;;; Commentary:
;; WebSocket infrastructure for bidirectional communication between Emacs and
;; Claude agents. Provides real-time message passing for MCP protocol integration.

;;; Code:

(require 'cl-lib)
(require 'websocket)
(require 'json)

;; Forward declarations
(defvar claude-multi-websocket-enabled)
(defvar claude-multi-websocket-fallback)
(defvar claude-multi-websocket-port-range)

(declare-function claude-multi-mcp--handle-request "claude-multi-mcp")
(declare-function claude-multi--update-agent-status "claude-multi-progress")
(declare-function claude-multi--get-agent-by-id "claude-multi-agents")
(declare-function claude-agent-id "claude-multi-agents")
(declare-function claude-agent-status "claude-multi-agents")

;;; Global variables

(defvar claude-multi--ws-server nil
  "WebSocket server instance for agent communication.")

(defvar claude-multi--ws-port nil
  "Port number on which the WebSocket server is listening.")

(defvar claude-multi--ws-connections (make-hash-table :test 'equal)
  "Hash table mapping agent-id (string) to WebSocket connection objects.")

(defvar claude-multi--ws-message-handlers (make-hash-table :test 'equal)
  "Hash table mapping message types to handler functions.")

;;; Server lifecycle

;;;###autoload
(defun claude-multi-ws--start-server ()
  "Start WebSocket server on a random available port.
Returns the port number if successful, nil otherwise."
  (when claude-multi--ws-server
    ;; Server already running
    (message "WebSocket server already running on port %s" claude-multi--ws-port)
    (return-from claude-multi-ws--start-server claude-multi--ws-port))

  (condition-case err
      (let* ((port-range claude-multi-websocket-port-range)
             (min-port (car port-range))
             (max-port (cadr port-range))
             (port (claude-multi-ws--find-available-port min-port max-port)))
        (unless port
          (error "No available ports in range %s-%s" min-port max-port))

        (setq claude-multi--ws-server
              (websocket-server
               port
               :host "127.0.0.1"
               :on-open #'claude-multi-ws--on-connection
               :on-message #'claude-multi-ws--on-message
               :on-close #'claude-multi-ws--on-close
               :on-error #'claude-multi-ws--on-error))

        (setq claude-multi--ws-port port)
        (message "WebSocket server started on port %s" port)
        port)
    (error
     (message "Failed to start WebSocket server: %s" (error-message-string err))
     nil)))

;;;###autoload
(defun claude-multi-ws--stop-server ()
  "Stop the WebSocket server and close all connections."
  (when claude-multi--ws-server
    ;; Close all active connections
    (maphash (lambda (_agent-id conn)
               (when (websocket-openp conn)
                 (websocket-close conn)))
             claude-multi--ws-connections)

    ;; Clear connections table
    (clrhash claude-multi--ws-connections)

    ;; Stop server
    (websocket-server-close claude-multi--ws-server)
    (setq claude-multi--ws-server nil)
    (setq claude-multi--ws-port nil)
    (message "WebSocket server stopped")))

(defun claude-multi-ws--find-available-port (min-port max-port)
  "Find an available port between MIN-PORT and MAX-PORT.
Returns the port number if found, nil otherwise."
  (let ((port min-port)
        (found nil))
    (while (and (not found) (<= port max-port))
      (condition-case _err
          (let ((test-server (websocket-server
                              port
                              :host "127.0.0.1"
                              :on-open #'ignore)))
            ;; Port is available
            (websocket-server-close test-server)
            (setq found port))
        (error
         ;; Port is in use, try next
         (setq port (1+ port)))))
    found))

;;; Connection handlers

(defun claude-multi-ws--on-connection (ws)
  "Handle new WebSocket connection WS.
The agent must send a 'register' message with its agent-id to complete registration."
  (message "New WebSocket connection established (awaiting registration)")
  ;; Store temporary connection until registration
  (websocket-server-conn-plist ws :awaiting-registration t))

(defun claude-multi-ws--on-message (ws frame)
  "Handle incoming WebSocket message.
WS is the connection, FRAME contains the message data."
  (condition-case err
      (let* ((payload (websocket-frame-text frame))
             (message-data (json-read-from-string payload))
             (message-type (cdr (assoc 'type message-data)))
             (agent-id (cdr (assoc 'agent_id message-data))))

        ;; Handle registration messages
        (when (string= message-type "register")
          (claude-multi-ws--handle-registration ws agent-id)
          (return-from claude-multi-ws--on-message))

        ;; All other messages require an agent-id
        (unless agent-id
          (message "WebSocket message missing agent_id: %s" message-type)
          (return-from claude-multi-ws--on-message))

        ;; Route message to appropriate handler
        (let ((handler (gethash message-type claude-multi--ws-message-handlers)))
          (if handler
              (funcall handler agent-id message-data ws)
            ;; Default handlers for built-in message types
            (pcase message-type
              ("mcp-request"
               (claude-multi-ws--handle-mcp-request agent-id message-data ws))
              ("status-update"
               (claude-multi-ws--handle-status-update agent-id message-data))
              ("ping"
               (claude-multi-ws--send-message agent-id '((type . "pong"))))
              (_
               (message "Unknown WebSocket message type: %s" message-type))))))
    (error
     (message "Error handling WebSocket message: %s" (error-message-string err)))))

(defun claude-multi-ws--handle-registration (ws agent-id)
  "Handle agent registration for WebSocket connection WS with AGENT-ID."
  (if (not agent-id)
      (progn
        (message "Registration failed: missing agent_id")
        (websocket-close ws))
    (puthash agent-id ws claude-multi--ws-connections)
    (websocket-server-conn-plist ws :agent-id agent-id)
    (websocket-server-conn-plist ws :awaiting-registration nil)
    (message "Agent %s registered via WebSocket" agent-id)

    ;; Send acknowledgment
    (claude-multi-ws--send-message agent-id '((type . "registered")
                                               (status . "success")))))

(defun claude-multi-ws--handle-mcp-request (agent-id message-data _ws)
  "Handle MCP request from AGENT-ID with MESSAGE-DATA.
WS is the WebSocket connection (unused, but available for future use)."
  ;; Forward to MCP handler
  (if (fboundp 'claude-multi-mcp--handle-request)
      (let ((response (claude-multi-mcp--handle-request agent-id message-data)))
        ;; Send response back to agent
        (claude-multi-ws--send-message agent-id response))
    ;; MCP module not loaded yet - send stub response
    (message "MCP request from %s (handler not loaded yet)" agent-id)
    (let ((request-id (cdr (assoc 'id message-data))))
      (claude-multi-ws--send-message
       agent-id
       `((jsonrpc . "2.0")
         (id . ,request-id)
         (error . ((code . -32601)
                   (message . "MCP handler not loaded"))))))))

(defun claude-multi-ws--handle-status-update (agent-id message-data)
  "Handle status update from AGENT-ID with MESSAGE-DATA."
  (let ((new-status-str (cdr (assoc 'status message-data)))
        (agent (when (fboundp 'claude-multi--get-agent-by-id)
                 (claude-multi--get-agent-by-id agent-id))))
    (when (and agent new-status-str)
      (let ((new-status (intern new-status-str)))
        (when (memq new-status '(running waiting-input completed failed pending))
          (setf (claude-agent-status agent) new-status)
          (when (fboundp 'claude-multi--update-agent-status)
            (claude-multi--update-agent-status agent))
          (message "Agent %s status updated: %s" agent-id new-status))))))

(defun claude-multi-ws--on-close (ws)
  "Handle WebSocket connection WS closure."
  (let ((agent-id (websocket-server-conn-plist ws :agent-id)))
    (when agent-id
      (remhash agent-id claude-multi--ws-connections)
      (message "Agent %s WebSocket connection closed" agent-id))))

(defun claude-multi-ws--on-error (_ws _type err)
  "Handle WebSocket error.
WS is the connection, TYPE is error type (unused), ERR is the error."
  (message "WebSocket error: %s" err))

;;; Message sending

;;;###autoload
(defun claude-multi-ws--send-message (agent-id message)
  "Send MESSAGE to agent with AGENT-ID via WebSocket.
MESSAGE should be an alist that will be encoded as JSON.
Returns t if sent successfully, nil otherwise."
  (let ((conn (gethash agent-id claude-multi--ws-connections)))
    (if (and conn (websocket-openp conn))
        (condition-case err
            (progn
              (websocket-send-text conn (json-encode message))
              t)
          (error
           (message "Failed to send WebSocket message to %s: %s"
                   agent-id (error-message-string err))
           nil))
      (progn
        (message "No active WebSocket connection for agent %s" agent-id)
        nil))))

;;;###autoload
(defun claude-multi-ws--broadcast-message (message)
  "Broadcast MESSAGE to all connected agents.
MESSAGE should be an alist that will be encoded as JSON.
Returns list of agent-ids that received the message successfully."
  (let ((successful-agents nil))
    (maphash (lambda (agent-id _conn)
               (when (claude-multi-ws--send-message agent-id message)
                 (push agent-id successful-agents)))
             claude-multi--ws-connections)
    (nreverse successful-agents)))

;;; Connection status

;;;###autoload
(defun claude-multi-ws--is-connected (agent-id)
  "Check if agent with AGENT-ID has an active WebSocket connection.
Returns t if connected, nil otherwise."
  (let ((conn (gethash agent-id claude-multi--ws-connections)))
    (and conn (websocket-openp conn))))

;;;###autoload
(defun claude-multi-ws--get-connected-agents ()
  "Return list of agent-ids with active WebSocket connections."
  (let ((agents nil))
    (maphash (lambda (agent-id conn)
               (when (websocket-openp conn)
                 (push agent-id agents)))
             claude-multi--ws-connections)
    (nreverse agents)))

;;;###autoload
(defun claude-multi-ws--connection-count ()
  "Return the number of active WebSocket connections."
  (let ((count 0))
    (maphash (lambda (_agent-id conn)
               (when (websocket-openp conn)
                 (setq count (1+ count))))
             claude-multi--ws-connections)
    count))

;;; Message handler registration

;;;###autoload
(defun claude-multi-ws--register-message-handler (message-type handler-fn)
  "Register HANDLER-FN for MESSAGE-TYPE.
HANDLER-FN should accept (agent-id message-data ws) as arguments."
  (puthash message-type handler-fn claude-multi--ws-message-handlers)
  (message "Registered WebSocket handler for message type: %s" message-type))

;;;###autoload
(defun claude-multi-ws--unregister-message-handler (message-type)
  "Unregister handler for MESSAGE-TYPE."
  (remhash message-type claude-multi--ws-message-handlers)
  (message "Unregistered WebSocket handler for message type: %s" message-type))

;;; Server info

;;;###autoload
(defun claude-multi-ws--server-info ()
  "Return information about the WebSocket server as a plist.
Includes :running, :port, :connections."
  (list :running (not (null claude-multi--ws-server))
        :port claude-multi--ws-port
        :connections (claude-multi-ws--connection-count)))

;;; Environment variable helper

;;;###autoload
(defun claude-multi-ws--get-port-env ()
  "Get the CLAUDE_WS_PORT environment variable value for agent processes.
Returns string suitable for setting as environment variable."
  (when claude-multi--ws-port
    (number-to-string claude-multi--ws-port)))

;;; Interactive commands for debugging

;;;###autoload
(defun claude-multi/ws-server-status ()
  "Display WebSocket server status in minibuffer."
  (interactive)
  (if claude-multi--ws-server
      (message "WebSocket server running on port %s with %d connection(s)"
               claude-multi--ws-port
               (claude-multi-ws--connection-count))
    (message "WebSocket server not running")))

;;;###autoload
(defun claude-multi/ws-list-connections ()
  "List all active WebSocket connections."
  (interactive)
  (let ((agents (claude-multi-ws--get-connected-agents)))
    (if agents
        (message "Connected agents: %s" (string-join agents ", "))
      (message "No active WebSocket connections"))))

(provide 'claude-multi-websocket)
;;; claude-multi-websocket.el ends here
