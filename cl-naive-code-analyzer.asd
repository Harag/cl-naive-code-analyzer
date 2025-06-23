(defsystem "cl-naive-code-analyzer"
  :description ""
  :version "2025.3.3"
  :author ""
  :licence "MIT"
  :depends-on (eclector eclector-concrete-syntax-tree concrete-syntax-tree trivial-gray-streams cl-naive-store alexandria)
  :components ((:file "src/package")
               (:file "src/utils"
                :depends-on ("src/package"))
               (:file "src/naive-store"
                :depends-on ("src/utils"))
               (:file "src/analyzers"
                :depends-on ("src/cl-naive-code-analyzer"))
               (:file "src/cl-naive-code-analyzer"
                :depends-on ("src/naive-store"))
               (:file "src/query"
                :depends-on ("src/naive-store"))))

