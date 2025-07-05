;;; cl-naive-code-analyzer.asd
;;;
;;; This file defines the system "cl-naive-code-analyzer".
;;; It specifies dependencies and components of the system.
;;;
;;; TODO: Add a more detailed description for the system.
;;; TODO: Specify the author of the system.

(defsystem "cl-naive-code-analyzer"
  :description "" ; TODO: Add a descriptive comment here.
  :version "2025.3.3"
  :author "" ; TODO: Add author information here.
  :licence "MIT"
  :depends-on (eclector eclector-concrete-syntax-tree concrete-syntax-tree trivial-gray-streams cl-naive-store alexandria jonathan usocket bordeaux-threads puri drakma cl-ppcre)
  :components ((:file "src/package")
               (:file "src/mcp-server/packages"
                :depends-on ("src/package"))
               (:file "src/mcp-server/server"
                :depends-on ("src/mcp-server/packages"))
               (:file "src/lambda-list-parser"
                :depends-on ("src/package"))
               (:file "src/utils"
                :depends-on ("src/package"))
               (:file "src/naive-store"
                :depends-on ("src/utils"))
               (:file "src/analyzers"
                :depends-on ("src/utils" "src/package"))
               (:file "src/cl-naive-code-analyzer"
                :depends-on ("src/naive-store"))
               (:file "src/writers"
                :depends-on ("src/cl-naive-code-analyzer"))
               (:file "src/query"
                :depends-on ("src/naive-store"))))

