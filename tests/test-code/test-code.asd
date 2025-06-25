;;; tests/test-code/test-code.asd
;;;
;;; This ASDF system definition file defines a system named "test-code".
;;; This system is likely used as a sample or fixture for testing the
;;; cl-naive-code-analyzer itself, providing a small, self-contained
;;; Lisp system that can be parsed and analyzed.
;;;
;;; TODO: Ensure that the version number "0.1.0" is updated if this
;;;       test code evolves significantly.
;;; TODO: Verify that ":serial t" is the intended compilation order behavior.
;;;       For simple test systems, it's often fine, but for more complex
;;;       systems, explicit dependencies are preferred.

(defsystem "test-code"
  :description "Some test code to parse."
  :version "0.1.0"
  :author "Phil Marneweck"
  :license "MIT"
  :depends-on () ; This system has no external dependencies.
  :serial t      ; Components will be compiled and loaded in the order they appear.
  :components ((:file "package") ; Defines the "test" package.
               (:file "test" :depends-on ("package")))) ; Contains test code, depends on the package definition.
