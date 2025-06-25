;;; tests/package.lisp
;;;
;;; This file defines the package for the tests of the cl-naive-code-analyzer system.
;;;
;;; TODO: Add specific exports if any test utilities or suites are meant to be
;;;       accessible from outside this test package directly. Currently, it exports nothing.

(in-package :common-lisp-user)

;; Defines the test package `:cl-naive-code-analyzer.tests`.
;; It uses Common Lisp (`:cl`), a testing framework (`:cl-naive-tests`),
;; and the system being tested (`:cl-naive-code-analyzer`).
(defpackage :cl-naive-code-analyzer.tests
  (:use :cl :cl-naive-tests :cl-naive-code-analyzer)
  (:export #:run-all-tests) ; Assuming a common test runner function might be exported.
                            ; If not, :export can be empty or nil.
  ;; TODO: If using a specific test framework that requires it,
  ;;       you might need to import test-running symbols or define test suites here.
  )
