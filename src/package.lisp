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
   ;; Functions related to building and accessing the code index.
   :build-index        ; TODO: Document what this function builds (project-level index?).
   :build-file-index   ; TODO: Document what this function builds (file-level index?).
   :get-index          ; TODO: Document what kind of index this retrieves and its usage.

   ;; Functions for searching and querying the code analysis data.
   :search-symbol      ; TODO: Document how to use this, what it searches for (definitions, references?).
   :who-calls          ; TODO: Document its arguments (e.g., function name) and what it returns.
   :search-for-locals ; TODO: Document what kind of locals this searches for and in what context.
   :analyze-string
   :analyze-file
   :index-project-definitions
   ;; Classes
   :analysis
   :defun-analysis
   :defmethod-analysis
   :define-condition-analysis
   :defclass-analysis
   :defparameter-analysis
   :defmacro-analysis
   :deftype-analysis
   :defgeneric-analysis
   :defstruct-analysis
   :defsetf-analysis
   :define-symbol-macro-analysis
   :defpackage-analysis
   :code-file
   :code-project
   ;; Accessors for analysis classes (common ones)
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
   :analysis-lexical-definitions
   :analysis-raw-body
   ;; Specific accessors
   :analysis-lambda-info
   :analysis-parameters
   :analysis-docstring
   :analysis-slots
   :analysis-superclasses
   ;; DEFPACKAGE specific accessors
   :analysis-package-name
   :analysis-nicknames
   :analysis-uses
   :analysis-exports
   :analysis-shadows
   :analysis-shadowing-imports
   :analysis-imports
   :analysis-interns
   :analysis-other-options
   ))
