;;; package.lisp
;;;
;;; This file defines the packages used by the cl-naive-code-analyzer system.
;;; It includes the main package `cl-naive-code-analyzer` and a utility package `analyzer-walker`.
;;;
;;; TODO: Review the exports from `cl-naive-code-analyzer` to ensure they represent
;;;       the intended public API of the system.
;;; TODO: Consider if `analyzer-walker` is still used or if its functionality
;;;       has been integrated elsewhere. If not used, it could be removed.

(in-package :common-lisp-user)

;; Package for a potential form walker utility.
;; TODO: Verify if this package and its export `walk-form` are actively used.
;;       If `walk-form` refers to a generic walker, ensure it's clearly defined
;;       and differentiated from CST walkers used elsewhere.
(defpackage :analyzer-walker
  (:use :cl)
  (:export :walk-form)) ; Exports a single function `walk-form`.

;; The main package for the Naive Code Analyzer system.
;; It imports necessary symbols from Common Lisp and various `cl-naive-store` sub-packages.
(defpackage :cl-naive-code-analyzer
  (:use
   :cl                         ; Standard Common Lisp symbols.
   :cl-getx                    ; TODO: Document purpose of cl-getx if it's a non-standard dependency.
   :cl-naive-store.naive-core  ; Core functionalities of cl-naive-store.
   :cl-naive-store.naive-indexed ; Indexed collections from cl-naive-store.
   :cl-naive-store.document-types ; Document type definitions from cl-naive-store.
   :cl-naive-store.naive-documents) ; Document handling from cl-naive-store.

  (:export

   :analysis
   :analysis-name
   :analysis-kind
   :analysis-cst
   :analysis-start
   :analysis-end
   :analysis-line
   :analysis-package
   :analysis-function-calls
   :analysis-macro-calls
   :analysis-variable-uses
   :analysis-local-function-calls
   :analysis-local-variable-uses
   :analysis-lexical-definitions
   :analysis-dynamic-definitions
   :analysis-raw-body
   :analysis-package

   :analysis-lambda-info
   :analysis-parameters
   :analysis-docstring
   :analysis-slots
   :analysis-superclasses

   ;;defpackage specific
   :analysis-nicknames
   :analysis-uses
   :analysis-exports
   :analysis-shadows
   :analysis-imports
   :analysis-interns
   :analysis-size

   :analyze-project

   ;;Query
   :load-project
   :defquery
   :match-symbol
   :make-callers-of-query
   :make-uses-symbol-query
   :make-functions-in-file-query
   :query-analyzer
   :find-function
   :uncalled-functions))

