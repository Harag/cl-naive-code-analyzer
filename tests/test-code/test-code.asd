(defsystem "test-code"
  :description "Some test code to parse."
  :version "0.1.0"
  :author "Phil Marneweck"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "test" :depends-on ("package"))))
