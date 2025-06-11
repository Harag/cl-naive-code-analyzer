(defsystem "cl-naive-code-analyzer"
  :description ""
  :version "2025.3.3"
  :author ""
  :licence "MIT"
  :depends-on (eclector trivial-gray-streams cl-naive-store)
  :components ((:file "src/package")
               (:file "src/portable-sb-walk"
                :depends-on ("src/package"))
               (:file "src/loop"
                :depends-on ("src/package"))
               (:file "src/walk-macros"
                :depends-on ("src/portable-sb-walk"))
               (:file "src/utils"
                :depends-on ("src/portable-sb-walk"
                             "src/walk-macros"
                             "src/loop"))
               (:file "src/naive-store"
                :depends-on ("src/utils"))
               (:file "src/cl-naive-code-analyzer"
                :depends-on ("src/naive-store"))
               (:file "src/analyzers"
                :depends-on ("src/cl-naive-code-analyzer"))))

