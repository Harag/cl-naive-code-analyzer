;;; package.lisp
;;;
;;; This file defines the packages used by the cl-naive-code-analyzer system.
;;; It includes the main package `cl-naive-code-analyzer`

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

   ;;Store and Query
   :init-naive-store ; Added for MCP tests
   :load-naive-store ; Added for MCP server
   :load-project
   :defquery
   :match-symbol
   :make-callers-of-query
   :make-uses-symbol-query
   :make-functions-in-file-query
   :query-analyzer
   :find-function
   :uncalled-functions
   :analyze-string ; Added for MCP tests
   ))

