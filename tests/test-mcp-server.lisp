;;; tests/test-mcp-server.lisp

(in-package :cl-naive-code-analyzer.tests)

;;; Utility to run server for a test block
(defmacro with-mcp-server ((&key (host "\"127.0.0.1\"") (port 8889)) &body body)
  `(let ((server-started-p nil))
     (unwind-protect
          (progn
            (setf server-started-p (cl-naive-code-analyzer.mcp-server:start-server :host ,host :port ,port))
            (when server-started-p
              (sleep 0.5) ; Give server a moment to start
              ,@body))
       (when server-started-p
         (cl-naive-code-analyzer.mcp-server:stop-server)
         (sleep 0.1))))) ; Give server a moment to stop

;;; Mock client connection
(defun connect-and-send (host port message-obj)
  (usocket:with-client-socket (socket stream host port :element-type 'character)
    (cl-naive-code-analyzer.mcp-server::send-json-rpc stream message-obj)
    (cl-naive-code-analyzer.mcp-server::read-json-rpc stream)))

;; For resource tests, we need some data in the naive-store.
(defun setup-test-data-for-mcp ()
  (cl-naive-code-analyzer:init-naive-store)
  (let ((cl-user-pkg (find-package :cl-user)))
    ;; Ensure MCP-TEST-PKG exists by analyzing its defpackage form first
    (cl-naive-code-analyzer:analyze-string "(defpackage :mcp-test-pkg (:use :cl))" :package cl-user-pkg))
  ;; Now that :mcp-test-pkg is defined, proceed with other definitions
  (let* ((project-name "mcp-test-project")
         (project (make-instance 'cl-naive-code-analyzer::code-project :name project-name))
         (code-file (make-instance 'cl-naive-code-analyzer::code-file :path "mcp-virtual-file.lisp"))
         (mcp-test-pkg (find-package :mcp-test-pkg)) ; Should exist now
         (test-func-source "(in-package :mcp-test-pkg) (defun test-mcp-func (a) (+ a 1))")
         (analyses (cl-naive-code-analyzer:analyze-string test-func-source :package mcp-test-pkg)))

    (setf (cl-naive-code-analyzer::project-files project) (list code-file))
    (setf (cl-naive-code-analyzer::file-analyses code-file) analyses)
    (dolist (an analyses)
      (setf (cl-naive-code-analyzer::analysis-package an) mcp-test-pkg))

    (cl-naive-code-analyzer::store-project project)
    (cl-naive-code-analyzer:load-naive-store (list project-name))))

(cl-naive-tests:testsuite :mcp-server
  ;; Removed :setup and :teardown

  (cl-naive-tests:testcase :json-rpc-construction
    :actual (list
             (cl-naive-code-analyzer.mcp-server::make-json-rpc-request 1 "testMethod" `(:object-alist ("param1" . "val1")))
             (cl-naive-code-analyzer.mcp-server::make-json-rpc-response 1 `(:object-alist ("resultData" . t)))
             (cl-naive-code-analyzer.mcp-server::make-json-rpc-error 1 -32000 "Test Error" `(:object-alist ("detail" . "info"))))
    :expected (list
               '(:object-alist ("jsonrpc" . "2.0") ("id" . 1) ("method" . "testMethod") ("params" . (:object-alist ("param1" . "val1"))))
               '(:object-alist ("jsonrpc" . "2.0") ("id" . 1) ("result" . (:object-alist ("resultData" . t))))
               '(:object-alist ("jsonrpc" . "2.0") ("id" . 1) ("error" . (:object-alist ("code" . -32000) ("message" . "Test Error") ("data" . (:object-alist ("detail" . "info")))))))
    :equal #'equalp)

  (cl-naive-tests:testcase :mcp-initialize-handshake
    :actual (with-mcp-server (:host "127.0.0.1" :port 8889)
              (let* ((request (cl-naive-code-analyzer.mcp-server::make-json-rpc-request
                               1 "initialize"
                               `(:object-alist
                                 ("processId" . :null)
                                 ("clientInfo" . (:object-alist ("name" . "test-client") ("version" . "0.1")))
                                 ("capabilities" . (:object-alist ("dummyClientCap" . :true))))))
                     (response (connect-and-send "127.0.0.1" 8889 request)))
                (list
                 (equal "2.0" (assoc-value response "jsonrpc" :test #'string=))
                 (equal 1 (assoc-value response "id"))
                 (let ((result (assoc-value response "result" :test #'string=)))
                   (list
                    (stringp (assoc-value result "mcpVersion" :test #'string=))
                    (typep (assoc-value result "serverInfo" :test #'string=) 'list)
                    (string= "cl-naive-code-analyzer-mcp-server"
                             (assoc-value (assoc-value result "serverInfo" :test #'string=) "name" :test #'string=))
                    (typep (assoc-value result "features" :test #'string=) 'list)))))) ; Corrected parenthesis here
    :expected '(t t (t t t t))
    :equal #'equalp)

  (cl-naive-tests:testcase :mcp-resources-request
      :actual (progn (setup-test-data-for-mcp)
                     (with-mcp-server (:host "127.0.0.1" :port 8890)
                       (let* ((request (cl-naive-code-analyzer.mcp-server::make-json-rpc-request 2 "mcp/resources" nil))
                              (response (connect-and-send "127.0.0.1" 8890 request)))
                         (list
                          (equal "2.0" (assoc-value response "jsonrpc" :test #'string=))
                          (equal 2 (assoc-value response "id"))
                          (let* ((result (assoc-value response "result" :test #'string=))
                                 (resources (assoc-value result "resources")))
                            (list
                             (typep result 'list)
                             (listp resources)
                             (plusp (length resources)) ; Should find at least defpackage and test-mcp-func
                             (let ((test-func-res (find "TEST-MCP-FUNC" resources
                                                        :key (lambda (r) (assoc-value (assoc-value r "data") "name" :test #'string=))
                                                        :test #'string=)))
                               (and test-func-res
                                    (string= "mcp://resource/DEFUN/MCP-TEST-PKG/TEST-MCP-FUNC/mcp-virtual-file.lisp?file=mcp-virtual-file.lisp&line=1"
                                             (assoc-value test-func-res "uri" :test #'string=))))))))))
      :expected '(t t (t t t t))
      :equal #'equalp)

    (cl-naive-tests:testcase :mcp-resource-content-request
      :actual (progn (setup-test-data-for-mcp)
                     (with-mcp-server (:host "127.0.0.1" :port 8891)
                       (let* ((resources-request (cl-naive-code-analyzer.mcp-server::make-json-rpc-request 3 "mcp/resources" nil))
                              (resources-response (connect-and-send "127.0.0.1" 8891 resources-request))
                              (resources (assoc-value (assoc-value resources-response "result" :test #'string=) "resources"))
                              (test-func-resource (find "TEST-MCP-FUNC" resources
                                                        :key (lambda (r) (assoc-value (assoc-value r "data") "name" :test #'string=))
                                                        :test #'string=))
                              (test-func-uri (when test-func-resource (assoc-value test-func-resource "uri" :test #'string=))))
                         (if test-func-uri
                             (let* ((content-request (cl-naive-code-analyzer.mcp-server::make-json-rpc-request
                                                      4 "mcp/resource/content"
                                                      `(:object-alist ("uri" . ,test-func-uri))))
                                    (content-response (connect-and-send "127.0.0.1" 8891 content-request)))
                               (list
                                (equal "2.0" (assoc-value content-response "jsonrpc" :test #'string=))
                                (equal 4 (assoc-value content-response "id"))
                                (let ((result (assoc-value content-response "result" :test #'string=)))
                                  (list
                                   (typep result 'list)
                                   (string= "TEST-MCP-FUNC" (assoc-value (assoc-value result :name) :name :test #'string=))
                                   (string= "MCP-TEST-PKG" (assoc-value (assoc-value result :name) :package :test #'string=))
                                   (string= "DEFUN" (assoc-value (assoc-value result :kind) :name :test #'string=))
                                   (string= "mcp-virtual-file.lisp" (assoc-value result :filename :test #'string=))))))
                             '(:uri-not-found))))) ; Return a distinct failure if URI wasn't found
        :expected '(t t (t t t t t))
        :equal #'equalp)
  )
