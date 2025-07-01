(in-package :cl-naive-code-analyzer)

(defmethod write-analysis :around((a analysis) filename &key)
  (let ((*export-symbol-standin* (analysis-package a)))
    (call-next-method)))
;;; Default method for WRITE-ANALYSIS. Serializes generic slots common
;;; to all analysis types.  Subclasses specialize this method to add
;;; slots like docstrings or parameters where applicable.
(defmethod write-analysis ((a analysis) filename &key)
  "Default method to serialize an 'analysis' object.
   Includes common properties like name, package, kind, position, code, and calls."
  ;; TODO: "Some time or another we need to figure out how to get the
  ;;        raw code using start and end so that we dont use the read
  ;;        in code! That would make it more usefull when giving it to
  ;;        an AI because then the AI" This is a good point. Storing
  ;;        the raw text segment would be more robust than
  ;;        re-serializing the CST.  It would require having the file
  ;;        content available here or passing start/end to
  ;;        `normalize-reader-macros`.
  `(,@`(:name , (export-symbol (analysis-name a))
        :package ,(if (packagep (analysis-package a))
                      (package-name (analysis-package a))
                      (analysis-package a))
        :filename ,filename
        :kind ,(export-symbol (analysis-kind a))
        :line ,(analysis-line a)
        :start ,(analysis-start a)
        :end ,(analysis-end a)
        ;; Serialize the CST back to a string representation.
        ;; `normalize-reader-macros` attempts to make this more
        ;; canonical.
        :code ,(format nil "~S"
                       (normalize-reader-macros
                        (concrete-syntax-tree:raw
                         (analysis-cst a))))

        :function-calls ,(mapcar #'export-symbol (analysis-function-calls a))
        :macro-calls ,(mapcar #'export-symbol (analysis-macro-calls a))
        :variable-uses ,(mapcar #'export-symbol (analysis-variable-uses a))
        :lexical-definitions ,(mapcar #'export-symbol (analysis-lexical-definitions a))
        :dynamic-definitions ,(mapcar #'export-symbol (analysis-dynamic-definitions a))
        ,@(when (analysis-local-function-calls a)
            `(:local-function-calls ,(mapcar #'export-symbol (analysis-local-function-calls a))))
        ,@(when (analysis-local-variable-uses a)
            `(:local-variable-uses ,(mapcar #'export-symbol (analysis-local-variable-uses a))))
        ,@(when (analysis-raw-body a)
            `(:raw-body ,(format nil "~S" (real-raw (analysis-raw-body a))))))))

(defun serialize-lambda-list-info (lambda-info)
  (when lambda-info
    `(:required ,(mapcar #'export-symbol (getf lambda-info :required))
      :optionals ,(mapcar (lambda (opt)
                            (list (export-symbol (first opt)) ; name
                                  (second opt) ; init-form (remains as is)
                                  ;; supplied-p
                                  (when (third opt) (export-symbol (third opt)))))
                          (getf lambda-info :optionals))
      :rest ,(when (getf lambda-info :rest)
               (export-symbol (getf lambda-info :rest)))
      :keywords ,(mapcar (lambda (kw)

                           (list
                            ;; keyword symbol itself (e.g. :foo)
                            (list (first (first kw))
                                  ;; var name
                                  (export-symbol (second (first kw))))
                            ;; init-form
                            (second kw)
                                        ; supplied-p
                            (when (third kw) (export-symbol (third kw)))))
                         (getf lambda-info :keywords))
      :allow-other-keys ,(getf lambda-info :allow-other-keys)
      :auxes ,(mapcar (lambda (aux)
                        (list
                         ;; name
                         (export-symbol (first aux))
                         ;; init-form
                         (second aux)))
                      (getf lambda-info :auxes)))))

;;; Specialized WRITE-ANALYSIS methods for different definition types.
;;; These add specific information like parameters, docstrings, slots, etc.

(defmethod write-analysis ((a defun-analysis) filename &key)
  "Serializes a 'defun-analysis' object, including lambda info, parameters, and docstring."
  `(;; Include generic analysis properties
    ,@(call-next-method)
    ,@(when (analysis-lambda-info a)
        `(:lambda-info ,(serialize-lambda-list-info (analysis-lambda-info a))))
    ,@(when (analysis-parameters a)
        `(:parameters ,(mapcar #'export-symbol (analysis-parameters a))))
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))))

(defmethod write-analysis ((a defmethod-analysis) filename &key)
  "Serializes a 'defmethod-analysis' object, including parameters and docstring."
  `(,@(call-next-method)
    ,@(when (analysis-parameters a)
        `(:parameters ,(mapcar #'export-symbol (analysis-parameters a))))
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))))

(defmethod write-analysis ((a defmacro-analysis) filename &key)
  "Serializes a 'defmacro-analysis' object, including parameters and docstring."
  `(,@(call-next-method)
    ,@(when (analysis-parameters a)
        `(:parameters ,(mapcar #'export-symbol (analysis-parameters a))))
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))))

(defmethod write-analysis ((a defgeneric-analysis) filename &key)
  "Serializes a 'defgeneric-analysis' object, including parameters and docstring."
  `(,@(call-next-method)
    ,@(when (analysis-parameters a)
        `(:parameters ,(mapcar #'export-symbol (analysis-parameters a))))
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))))

(defmethod write-analysis ((a defsetf-analysis) filename &key)
  "Serializes a 'defsetf-analysis' object, including parameters (if applicable) and docstring."
  ;; TODO: Parameters for defsetf need careful handling based on
  ;; short/long form.
  `(,@(call-next-method)
    ,@(when (analysis-parameters a)
        `(:parameters ,(mapcar #'export-symbol (analysis-parameters a))))
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))))

(defmethod write-analysis ((a deftype-analysis) filename &key)
  "Serializes a 'deftype-analysis' object, including parameters and docstring."
  `(,@(call-next-method)
    ,@(when (analysis-parameters a)
        `(:parameters ,(mapcar #'export-symbol (analysis-parameters a))))
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))))

(defmethod write-analysis ((a defstruct-analysis) filename &key)
  "Serializes a 'defstruct-analysis' object, including docstring and slots."
  ;; TODO: Add :slots serialization if `analysis-slots` is populated
  ;; for defstruct.
  `(,@(call-next-method)
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))
    ,@(when (analysis-slots a) ; Assuming analysis-slots exists and is populated
        `(:slots ,(mapcar #'export-symbol (analysis-slots a))))))

(defmethod write-analysis ((a defparameter-analysis) filename &key)
  "Serializes a 'defparameter-analysis' object (used for defparameter, defvar, defconstant), including docstring."
  `(,@(call-next-method)
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))))

(defmethod write-analysis ((a define-condition-analysis) filename &key)
  "Serializes a 'define-condition-analysis' object, including docstring, superclasses, and slots."

  `(,@(call-next-method)
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))
    ,@(when (analysis-superclasses a)
        `(:superclasses ,(mapcar #'export-symbol (analysis-superclasses a))))
    ,@(when (analysis-slots a)
        `(:slots ,(mapcar #'export-symbol (analysis-slots a))))))

(defmethod write-analysis ((a defpackage-analysis) filename &key)
  "Serializes a 'defpackage-analysis' object, including all package options like nicknames, uses, exports, etc."
  `(,@(call-next-method)
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))
    ,@(when (analysis-nicknames a)
        `(:nicknames ,(mapcar #'string (analysis-nicknames a))))
    ,@(when (analysis-uses a)
        `(:uses ,(mapcar #'package-name (analysis-uses a))))
    ,@(when (analysis-exports a)
        `(:exports ,(mapcar #'export-symbol (analysis-exports a))))
    ,@(when (analysis-shadows a)
        `(:shadows ,(mapcar #'export-symbol (analysis-shadows a))))
    ,@(when (analysis-shadowing-imports a)
        ;; TODO: Needs proper serialization
        `(:shadowing-imports ,(analysis-shadowing-imports a)))
    ,@(when (analysis-imports a)
        `(:imports
          ;; TODO: Needs proper serialization
          ,(analysis-imports a)))
    ,@(when (analysis-interns a)
        `(:interns ,(mapcar #'export-symbol (analysis-interns a))))
    ,@(when (analysis-size a)
        `(:other-size ,(analysis-size a)))))

;;; Specialized method for DEFCLASS to include superclasses and slots.

;; TODO: Needs implementation
(defun serialize-slot (slot)
  (export-symbol slot))

(defmethod write-analysis ((a defclass-analysis) filename &key)
  "Serializes a 'defclass-analysis' object, including docstring, superclasses, and slots."
  ;; TODO: Ensure `serialize-slot` is defined and properly serializes
  ;; slot definitions.
  `(,@(call-next-method)
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))
    :superclasses ,(mapcar #'export-symbol (analysis-superclasses a))
    :slots ,(mapcar #'serialize-slot (analysis-slots a))))

