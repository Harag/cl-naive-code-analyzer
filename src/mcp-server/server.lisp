;;; src/mcp-server/server.lisp

(in-package :cl-naive-code-analyzer.mcp-server)

(defvar *server* nil "Holds the main server instance for the MCP server.")
(defvar *server-thread* nil "Holds the thread where the MCP server accept loop runs.")
(defvar *clients* (make-hash-table) "Holds active MCP client connections, keyed by client ID.")
(defvar *next-client-id* 0 "Counter for assigning unique IDs to MCP clients.")

(defclass mcp-server ()
  ((host :accessor server-host :initarg :host :initform "127.0.0.1"
         :documentation "The host address the MCP server will listen on.")
   (port :accessor server-port :initarg :port :initform 8080
         :documentation "The port number the MCP server will listen on.")
   (socket :accessor server-socket :initform nil
           :documentation "The listening socket for the MCP server.")
   (running :accessor server-running :initform nil
            :documentation "A boolean indicating whether the MCP server is currently running."))
  (:documentation "Represents an instance of the Model Context Protocol server."))

(defclass mcp-client ()
  ((id :reader client-id :initarg :id
       :documentation "A unique identifier for the connected MCP client.")
   (socket :reader client-socket :initarg :socket
           :documentation "The usocket socket object for the client connection.")
   (stream :reader client-stream :initarg :stream
           :documentation "The stream associated with the client socket for communication.")
   (thread :accessor client-thread :initform nil
           :documentation "The thread handling message processing for this client.")
   (initialized :accessor client-initialized :initform nil
                :documentation "Boolean, true if the client has successfully completed the MCP initialize handshake.")
   (capabilities :accessor client-capabilities :initform nil
                 :documentation "A plist representing the capabilities reported by the client during initialization."))
  (:documentation "Represents a connected Model Context Protocol client."))

(defun make-json-rpc-request (id method &optional params)
  `(:object-alist
    ("jsonrpc" . "2.0")
    ("id" . ,id)
    ("method" . ,method)
    ,@(when params `(("params" . ,params)))))

(defun make-json-rpc-response (id result)
  `(:object-alist
    ("jsonrpc" . "2.0")
    ("id" . ,id)
    ("result" . ,result)))

(defun make-json-rpc-error (id code message &optional data)
  `(:object-alist
    ("jsonrpc" . "2.0")
    ("id" . ,id)
    ("error" . (:object-alist
                ("code" . ,code)
                ("message" . ,message)
                ,@(when data `(("data" . ,data)))))))

(defun send-json-rpc (stream object)
  "Serializes OBJECT to JSON and sends it over STREAM."
  (let ((json-string (jojo:to-json object)))
    (write-line json-string stream)
    (finish-output stream)
    (format t "Sent: ~a~%" json-string))) ; For debugging

(defun read-json-rpc (stream)
  "Reads a line from STREAM and parses it as JSON."
  (let ((line (read-line stream nil :eof)))
    (unless (eq line :eof)
      (format t "Read: ~a~%" line) ; For debugging
      (jojo:parse line :as :alist))))

(defun handle-client (client)
  (loop while (server-running *server*)
        for message = (read-json-rpc (client-stream client))
        when (null message) do (return) ; Connection closed
        do (process-message client message)))

;; Helper to convert a cl-naive-code-analyzer definition (plist) to an MCP Resource object
(defun definition-to-mcp-resource (def-plist)
  "Converts a definition plist (from write-analysis) to an MCP Resource object."
  (let* ((name-info (getf def-plist :name))
         (name (getf name-info :name))
         (package (getf name-info :package))
         (kind-info (getf def-plist :kind))
         (kind (getf kind-info :name)) ; Assuming kind is also an exported symbol
         (filename (getf def-plist :filename))
         (line (getf def-plist :line))
         ;; Create a stable URI. Using filename, kind, package, and name.
         ;; Replace slashes in filename to avoid issues with URI paths.
         (uri (format nil "mcp://resource/~a/~a/~a/~a?file=~a&line=~a"
                      (url-encode (string kind) :char-enc :utf-8)
                      (url-encode (string package) :char-enc :utf-8)
                      (url-encode (if (consp name) (format nil "~S" name) (string name)) :char-enc :utf-8)
                      (url-encode (substitute #\_ #\/ filename) :char-enc :utf-8)
                      filename
                      line)))
    `(:object-alist
      ("uri" . ,uri)
      ("label" . ,(if (consp name)
                      (format nil "~S (~A)" name kind)
                      (format nil "~A:~A (~A)" package name kind)))
      ("description" . ,(or (getf def-plist :docstring) "No documentation available."))
      ("kind" . ,(string-downcase (string kind))) ; e.g., "defun", "defclass"
      ;; Add other MCP resource fields as needed, e.g., "icon", "tags"
      ("data" . (:object-alist ;; Custom data specific to this resource type
                 ("filename" . ,filename)
                 ("line" . ,line)
                 ("package" . ,(string package))
                 ("name" . ,(if (consp name) (format nil "~S" name) (string name))))))))

(defun handle-mcp-resources (client id params)
  "Handles the mcp/resources request."
  (declare (ignore client params)) ; Params might be used later for filtering
  (let* ((all-definitions nil)
         (mcp-resources nil))
    ;; For now, query all definitions from all loaded projects.
    ;; Later, this could be filtered by project if specified in params.
    ;; Also, consider which kinds of definitions to expose.
    ;; For simplicity, let's start with functions and classes.
    (handler-case
        (progn
          (cl-naive-code-analyzer:load-naive-store) ; Ensure stores are loaded
          (setf all-definitions
                (append (cl-naive-code-analyzer:query-analyzer :functions)
                        (cl-naive-code-analyzer:query-analyzer :classes)
                        (cl-naive-code-analyzer:query-analyzer :macros)
                        (cl-naive-code-analyzer:query-analyzer :variables))))
      (error (e)
        (format *error-output* "Error querying definitions for mcp/resources: ~a~%" e)
        (send-json-rpc (client-stream client)
                       (make-json-rpc-error id -32000 "Server error while querying resources"
                                            `(:object-alist ("details" . ,(princ-to-string e)))))
        (return-from handle-mcp-resources)))

    (setf mcp-resources (mapcar #'definition-to-mcp-resource all-definitions))
    (send-json-rpc (client-stream client)
                   (make-json-rpc-response id `(:object-alist ("resources" . ,mcp-resources))))))

(defun handle-mcp-resource-content (client id params)
  "Handles the mcp/resource/content request."
  (let* ((uri (assoc-value params "uri" :test #'string=))
         (parsed-uri (when uri (puri:parse-uri uri))))
    (unless (and parsed-uri (string= (puri:uri-scheme parsed-uri) "mcp")
                 (string= (puri:uri-host parsed-uri) "resource"))
      (send-json-rpc (client-stream client)
                     (make-json-rpc-error id -32602 "Invalid URI for mcp/resource/content"
                                          `(:object-alist ("uri" . ,(or uri :null)))))
      (return-from handle-mcp-resource-content))

    ;; Example URI: mcp://resource/defun/MY-PACKAGE/MY-FUNCTION/path_to_file.lisp?file=/path/to/file.lisp&line=10
    ;; Path segments: kind, package, name, encoded-filename
    (let* ((path-segments (rest (cl-ppcre:split "/" (puri:uri-path parsed-uri)))) ; Skip first empty segment
           (query-string (puri:uri-query parsed-uri))
           (query-params (when query-string
                            (loop for pair-string in (cl-ppcre:split "&" query-string)
                                  collect (let ((parts (cl-ppcre:split "=" pair-string :limit 2)))
                                            (cons (url-decode (first parts) :char-enc :utf-8)
                                                  (if (second parts) (url-decode (second parts) :char-enc :utf-8) ""))))))
           (kind-str (when (>= (length path-segments) 1) (url-decode (elt path-segments 0) :char-enc :utf-8)))
           (package-str (when (>= (length path-segments) 2) (url-decode (elt path-segments 1) :char-enc :utf-8)))
           (name-str (when (>= (length path-segments) 3) (url-decode (elt path-segments 2) :char-enc :utf-8)))
           (filename (assoc-value query-params "file" :test #'string=))
           (line (assoc-value query-params "line" :test #'string=))
           (definition nil))

      (unless (and kind-str package-str name-str filename line)
        (send-json-rpc (client-stream client)
                       (make-json-rpc-error id -32602 "Malformed resource URI path or missing query params"
                                            `(:object-alist ("uri" . ,uri)
                                                            ("path_segments" . ,path-segments)
                                                            ("query_params" . ,query-params))))
        (return-from handle-mcp-resource-content))

      ;; Query for the specific definition.
      ;; This query needs to be precise. The `write-analysis` output is a plist.
      (handler-case
          (progn
            (cl-naive-code-analyzer:load-naive-store) ; Ensure stores are loaded
            (setf definition
                  (car (cl-naive-code-analyzer:query-analyzer
                        (lambda (def)
                          (let ((def-name-info (getf def :name))
                                (def-kind-info (getf def :kind)))
                            (and (string-equal (getf def-kind-info :name) kind-str) ; Kind name from plist
                                 (string-equal (getf def-name-info :package) package-str)
                                 (string-equal (if (consp (getf def-name-info :name))
                                                   (format nil "~S" (getf def-name-info :name))
                                                   (getf def-name-info :name))
                                               name-str)
                                 (string-equal (getf def :filename) filename)
                                 (string-equal (princ-to-string (getf def :line)) line))))
                        ;; Potentially limit to a project if that info is in URI or client context
                        ))))
        (error (e)
          (format *error-output* "Error querying definition for mcp/resource/content (URI: ~a): ~a~%" uri e)
          (send-json-rpc (client-stream client)
                         (make-json-rpc-error id -32000 "Server error while querying resource content"
                                              `(:object-alist ("details" . ,(princ-to-string e))
                                                              ("uri" . ,uri))))
          (return-from handle-mcp-resource-content)))

      (if definition
          ;; The 'definition' is already the plist from write-analysis.
          ;; Convert symbol plists (:name "N" :package "P") to simple strings for MCP if needed,
          ;; or ensure jonathan handles plists correctly (it should).
          ;; MCP expects a JSON object for content.
          (send-json-rpc (client-stream client) (make-json-rpc-response id definition))
          (send-json-rpc (client-stream client)
                         (make-json-rpc-error id -32001 "Resource not found"
                                              `(:object-alist ("uri" . ,uri))))))))


(defun process-message (client message)
  "Processes a received JSON-RPC message."
  (let ((method (assoc-value message "method" :test #'string=))
        (id (assoc-value message "id"))
        (params (assoc-value message "params" :test #'string=)))
    (format t "Processing method: ~S, ID: ~S, Params: ~S~%" method id params)
    (cond
      ((string= method "initialize")
       ;; Handle initialize method
       (let* ((client-capabilities (assoc-value params "capabilities" :test #'string=))
              ;; Define server capabilities
              (server-capabilities
                `(:object-alist
                  ("mcpVersion" . "2025.3.26") ; Assuming we target this version of MCP spec
                  ("serverInfo" . (:object-alist
                                   ("name" . "cl-naive-code-analyzer-mcp-server")
                                   ("version" . ,(asdf:component-version (asdf:find-system :cl-naive-code-analyzer)))))
                  ("features" . (:object-alist
                                 ("resources" . (:object-alist
                                                 ("dynamic" . :true))) ; Indicates resources can be discovered
                                 ("prompts" . :null) ; Not implemented yet
                                 ("tools" . :null))))) ; Not implemented yet
              (response (make-json-rpc-response id server-capabilities)))
         (setf (client-capabilities client) client-capabilities)
         (send-json-rpc (client-stream client) response)
         (setf (client-initialized client) t)
         (format t "MCP Server: Client ~a initialized. Client capabilities: ~S~%" (client-id client) client-capabilities)))
      ((string= method "mcp/resources")
       (format t "MCP Server: Received mcp/resources from client ~a~%" (client-id client))
       (handle-mcp-resources client id params))
      ((string= method "mcp/resource/content")
       (format t "MCP Server: Received mcp/resource/content from client ~a for URI ~S~%" (client-id client) (assoc-value params "uri" :test #'string=))
       (handle-mcp-resource-content client id params))
      ((string= method "shutdown")
       (format t "MCP Server: Received shutdown request from client ~a~%" (client-id client))
       (send-json-rpc (client-stream client) (make-json-rpc-response id :null))
       ;; Client is expected to send 'exit' next.
       ;; The server itself doesn't fully stop here, but this client session will end.
       ;; We signal the client's handler loop to stop.
       (setf (server-running *server*) nil) ; This will stop this client's read loop.
                                           ; And also the server accept loop if it's the only client or if a full server shutdown is triggered.
                                           ; For true graceful shutdown of the whole server, a more sophisticated
                                           ; mechanism (e.g. ref counting clients, or explicit stop-server call) is needed.
       (format t "MCP Server: Responded to shutdown for client ~a. Waiting for exit notification or connection close.~%" (client-id client)))
      ((string= method "exit")
        ;; Notification, no response needed. Client is disconnecting.
        (format t "MCP Server: Received exit notification from client ~a. Cleaning up client resources.~%" (client-id client))
        (ignore-errors (socket-close (client-socket client))) ; Close socket if still open
        (remhash (client-id client) *clients*)
        (format t "MCP Server: Client ~a resources cleaned up.~%" (client-id client))
        ;; If this was the last client and server was shutting down, server might stop fully.
        ;; Currently, stop-server handles full shutdown.
        )
      ;; Add other method handlers here
      (t
       ;; Method not found
       (format *error-output* "MCP Server: Method not found ~S for client ~a~%" method (client-id client))
       (send-json-rpc (client-stream client)
                      (make-json-rpc-error id -32601 "Method not found"
                                           `(:object-alist ("method" . ,method))))))))

(defun server-accept-loop (server)
  (setf (server-running server) t)
  (format t "MCP Server: Listening on ~a:~a~%" (server-host server) (server-port server))
  (loop while (server-running server)
        do (handler-case
               (let* ((client-socket (socket-accept (server-socket server) :element-type 'character))
                      (client-stream (socket-stream client-socket))
                      (client-id (incf *next-client-id*))
                      (client (make-instance 'mcp-client :id client-id :socket client-socket :stream client-stream)))
                 (setf (gethash client-id *clients*) client)
                 (setf (client-thread client)
                       (bt:make-thread (lambda ()
                                         (unwind-protect
                                              (handle-client client)
                                           (progn
                                             (format t "MCP Server: Client ~a disconnected.~%" client-id)
                                             (ignore-errors (socket-close client-socket))
                                             (remhash client-id *clients*))))
                                       :name (format nil "MCP Client ~a" client-id)))
                 (format t "MCP Server: Client ~a connected from ~A:~A.~%"
                         client-id (usocket:get-peer-address client-socket) (usocket:get-peer-port client-socket)))
             (usocket:socket-error (e)
               (when (server-running server) ; Only log if we weren't trying to stop
                 (format *error-output* "MCP Server: Socket error in accept loop: ~a~%" e)
                 (sleep 1))) ; Avoid busy-looping on certain errors
             (error (e)
               (format *error-output* "MCP Server: Error in accept loop: ~a~%" e)
               (sleep 1)))))

(defun start-server (&key (host "127.0.0.1") (port 8080))
  "Starts the MCP server on the specified HOST and PORT.
Initializes server state, creates a listening socket, and spawns a thread
for the main accept loop. Does nothing if the server is already running.
Returns T on success, NIL on failure (e.g., server already running or socket error)."
  (when (and *server* (server-running *server*))
    (format t "MCP Server: Server is already running.~%")
    (return-from start-server nil))
  (format t "MCP Server: Starting server on ~a:~a...~%" host port)
  (setf *server* (make-instance 'mcp-server :host host :port port))
  (setf *clients* (make-hash-table))
  (setf *next-client-id* 0)
  (handler-case
      (setf (server-socket *server*)
            (socket-listen (server-host *server*)
                           (server-port *server*)
                           :reuse-address t
                           :element-type 'character ; Ensure character streams for read-line
                           :backlog 128))
    (usocket:socket-error (e)
      (format *error-output* "MCP Server: Failed to listen on ~a:~a. Error: ~a~%" host port e)
      (setf *server* nil)
      (return-from start-server nil)))
  (setf *server-thread* (bt:make-thread (lambda () (server-accept-loop *server*))
                                        :name "MCP Server Acceptor"))
  (format t "MCP Server: Server started successfully.~%")
  t)

(defun stop-server ()
  "Stops the MCP server if it is running.
Signals all client handler threads and the main accept loop to terminate,
closes client sockets, and closes the main server listening socket.
Returns T if the server was stopped, NIL otherwise."
  (unless (and *server* (server-running *server*))
    (format t "MCP Server: Server not running or already stopped.~%")
    (return-from stop-server nil))

  (format t "MCP Server: Stopping server...~%")
  (setf (server-running *server*) nil) ; Signal all loops to stop

  ;; Close the server socket to prevent new connections
  (when (server-socket *server*)
    (format t "MCP Server: Closing server socket.~%")
    (ignore-errors (socket-close (server-socket *server*))))
  (setf (server-socket *server*) nil)

  ;; Wait for the server acceptor thread to finish
  (when (and *server-thread* (bt:thread-alive-p *server-thread*))
    (format t "MCP Server: Waiting for server acceptor thread to exit...~%")
    (unless (bt:join-thread *server-thread* :timeout 5)
      (format t "MCP Server: Server acceptor thread did not exit cleanly, attempting to destroy.~%")
      (ignore-errors (bt:destroy-thread *server-thread*))))
  (setf *server-thread* nil)

  ;; Close client connections and stop their threads
  (format t "MCP Server: Closing client connections...~%")
  (maphash (lambda (id client)
             (declare (ignore id))
             (format t "MCP Server: Shutting down client ~a...~%" (client-id client))
             (ignore-errors (socket-close (client-socket client)))
             (when (and (client-thread client) (bt:thread-alive-p (client-thread client)))
               (format t "MCP Server: Waiting for client ~a thread to exit...~%" (client-id client))
               (unless (bt:join-thread (client-thread client) :timeout 2)
                 (format t "MCP Server: Client ~a thread did not exit cleanly, attempting to destroy.~%" (client-id client))
                 (ignore-errors (bt:destroy-thread (client-thread client))))))
           *clients*)
  (clrhash *clients*)

  (setf *server* nil)
  (format t "MCP Server: Server stopped.~%")
  t)
