(in-package :common-lisp-user)

(defpackage :portable-sb-walker
  (:use :cl)
  (:export :walk-form))

(defpackage :cl-naive-code-analyzer
  (:use
   :cl
   :cl-getx
   :cl-naive-store.naive-core
   :cl-naive-store.naive-indexed
   :cl-naive-store.document-types
   :cl-naive-store.naive-documents)

  (:export
   :build-index
   :build-file-index
   :get-index
   :search-symbol
   :who-calls
   :search-for-locals))
