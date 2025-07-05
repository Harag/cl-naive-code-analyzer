;;; src/mcp-server/packages.lisp

(defpackage :cl-naive-code-analyzer.mcp-server
  (:use :cl :alexandria :jonathan :usocket :cl-naive-code-analyzer :puri :cl-ppcre)
  (:import-from :drakma :url-encode)
  (:documentation "Provides a Model Context Protocol (MCP) server for cl-naive-code-analyzer.
This allows LLM applications and other tools to query code analysis data via a standardized JSON-RPC interface.")
  (:export #:start-server
           #:stop-server))
