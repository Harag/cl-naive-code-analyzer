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
               "cl-naive-code-analyzer")
  :components
  ((:file "tests/package")
   (:file "tests/test-code/test" :depends-on ("tests/package"))
   (:file "tests/tests" :depends-on ("tests/package" "tests/test-code/test"))
  ))
