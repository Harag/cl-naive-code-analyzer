(defsystem "cl-naive-code-analyzer.tests"
  ;; System attributes:
  :description "Tests the cl-naive-code-analyzer system."
  :author      "Phil Marneweck"
  :licence     "MIT"
  ;; Component attributes:
  :version "2023.12.19"
  :depends-on ("alexandria"
               "cl-getx"
               "cl-naive-tests"
               "cl-naive-code-analyzer"
               "usocket") ; for mock client in mcp tests
  :components
  ((:file "tests/package")
   (:file "tests/tests" :depends-on ("tests/package"))
   (:file "tests/test-mcp-server" :depends-on ("tests/package" "tests/tests")) ; Ensure base test setup runs first
  ))
