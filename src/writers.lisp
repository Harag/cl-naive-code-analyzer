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

(defun serialize-parameter-detail (param-detail)
  "Serializes a parameter-detail plist for storage."
  (let ((name (getf param-detail :name))
        (kind (getf param-detail :kind))
        (default-value (getf param-detail :default-value))
        (supplied-p-var (getf param-detail :supplied-p-variable))
        (type-spec (getf param-detail :type-specifier))
        (keyword (getf param-detail :keyword)))
    (append
     (list :name (if (consp name) ; Handle destructured names (raw list)
                     (format nil "~S" name)
                     (export-symbol name)))
     (list :kind kind)
     (when (or default-value (eq kind :optional) (eq kind :key)) ; explicit nil for default is valid
       (list :default-value (if (or (symbolp default-value) (consp default-value))
                                (format nil "~S" default-value)
                                default-value)))
     (when supplied-p-var
       (list :supplied-p-variable (export-symbol supplied-p-var)))
     (when type-spec
       (list :type-specifier (if (or (symbolp type-spec) (consp type-spec))
                                 (format nil "~S" type-spec)
                                 type-spec)))
     (when keyword
       (list :keyword keyword))
     ;; Include other potential keys if necessary, like :destructured, :sub-parameters
     ;; For now, focusing on what Alexandria provided for basic compatibility in writers.
     ;; The full :sub-parameters structure might be too verbose for this serialization.
     (when (getf param-detail :destructured)
       (list :destructured t
             #|
             ;; Optionally, a string representation of sub-parameters if needed
             ;; :sub-parameters (format nil "~S" (getf param-detail :sub-parameters))))))|#)))))

(defun serialize-specializer (specializer-form)
  "Serializes a parameter specializer form.
   Input can be a SYMBOL (class name) or a list like (EQL object)."
  (cond
    ;; Class name specializer
    ((symbolp specializer-form)
     (export-symbol specializer-form))
    ;; (EQL object) specializer
    ((and (consp specializer-form)
          (eq (first specializer-form) 'eql)
          (consp (rest specializer-form)) ; Ensure there's a second element
          (null (cddr specializer-form))) ; Ensure it's exactly (EQL object)
     (let ((object-to-specialize (second specializer-form)))
       `(:eql ,(cond
                 ((symbolp object-to-specialize) (export-symbol object-to-specialize))
                 ((or (numberp object-to-specialize)
                      (stringp object-to-specialize)
                      (characterp object-to-specialize))
                  object-to-specialize) ; Literals can be used directly
                 ;; For other complex forms, serialize to string to be safe
                 (t (format nil "~S" object-to-specialize))))))
    ;; Unknown or malformed specializer
    (t
     `(:unknown-specializer ,(format nil "~S" specializer-form)))))

(defun serialize-param-name-and-specializer (param-name-or-pair)
  "Serializes a parameter name, which might include a specializer.
   Input can be a SYMBOL or a list (SYMBOL SPECIALIZER-FORM).
   Returns a plist (:name <exported-name> :specializer <serialized-specializer>)."
  (if (consp param-name-or-pair)
      (let ((name (first param-name-or-pair))
            (specializer (second param-name-or-pair)))
        `(:name ,(export-symbol name) :specializer ,(serialize-specializer specializer)))
      `(:name ,(export-symbol param-name-or-pair) :specializer nil)))

(defun serialize-lambda-list-info (lambda-info)
  (when lambda-info
    `(:required ,(mapcar #'serialize-param-name-and-specializer (getf lambda-info :required))
      :optionals ,(mapcar (lambda (opt)
                            (let* ((name-part (first opt))
                                   (init-form (second opt))
                                   (supplied-p (third opt))
                                   (serialized-name-spec (serialize-param-name-and-specializer name-part)))
                              ;; Merge the :name and :specializer with other optional parts
                              `(,@serialized-name-spec
                                :init-form ,(if (or (symbolp init-form) (consp init-form))
                                                (format nil "~S" init-form)
                                                init-form)
                                :supplied-p-var ,(when supplied-p (export-symbol supplied-p)))))
                          (getf lambda-info :optionals))
      :rest ,(when (getf lambda-info :rest)
               ;; &rest var is just a symbol, no specializer
               (export-symbol (getf lambda-info :rest)))
      :keywords ,(mapcar (lambda (kw)
                           (let* ((keyword-name (first (first kw))) ; The :keyword itself
                                  (var-name-part (second (first kw))) ; Var name or (var-name spec)
                                  (init-form (second kw))
                                  (supplied-p (third kw))
                                  (serialized-var-name-spec (serialize-param-name-and-specializer var-name-part)))
                             `(,@serialized-var-name-spec
                               :keyword ,keyword-name
                               :init-form ,(if (or (symbolp init-form) (consp init-form))
                                               (format nil "~S" init-form)
                                               init-form)
                               :supplied-p-var ,(when supplied-p (export-symbol supplied-p)))))
                         (getf lambda-info :keywords))
      :allow-other-keys ,(getf lambda-info :allow-other-keys)
      :auxes ,(mapcar (lambda (aux)
                        (let ((name (first aux))
                              (init-form (second aux)))
                          ;; Aux vars don't have specializers
                          `(:name ,(export-symbol name)
                            :init-form ,(if (or (symbolp init-form) (consp init-form))
                                            (format nil "~S" init-form)
                                            init-form))))
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
        `(:parameters ,(mapcar #'serialize-parameter-detail (analysis-parameters a))))
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))))

(defmethod write-analysis ((a defmethod-analysis) filename &key)
  "Serializes a 'defmethod-analysis' object, including parameters and docstring."
  `(,@(call-next-method)
    ,@(when (analysis-lambda-info a)
        `(:lambda-info ,(serialize-lambda-list-info (analysis-lambda-info a))))
    ,@(when (analysis-method-qualifier a) ; Use the new singular slot name
        `(:method-qualifier ,(analysis-method-qualifier a)))
    ,@(when (analysis-parameters a)
        `(:parameters ,(mapcar #'serialize-parameter-detail (analysis-parameters a))))
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))))

(defmethod write-analysis ((a defmacro-analysis) filename &key)
  "Serializes a 'defmacro-analysis' object, including parameters and docstring."
  `(,@(call-next-method)
    ,@(when (analysis-lambda-info a) ; Updated to include lambda-info
        `(:lambda-info ,(serialize-lambda-list-info (analysis-lambda-info a))))
    ,@(when (analysis-parameters a)
        `(:parameters ,(mapcar #'serialize-parameter-detail (analysis-parameters a))))
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))))

(defmethod write-analysis ((a defgeneric-analysis) filename &key)
  "Serializes a 'defgeneric-analysis' object, including parameters and docstring."
  `(,@(call-next-method)
    ,@(when (analysis-lambda-info a) ; Updated to include lambda-info
        `(:lambda-info ,(serialize-lambda-list-info (analysis-lambda-info a))))
    ,@(when (analysis-parameters a)
        `(:parameters ,(mapcar #'serialize-parameter-detail (analysis-parameters a))))
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))))

(defmethod write-analysis ((a defsetf-analysis) filename &key)
  "Serializes a 'defsetf-analysis' object, including parameters (if applicable) and docstring."
  `(,@(call-next-method)
    ,@(when (analysis-lambda-info a) ; For long form
        `(:lambda-info ,(serialize-lambda-list-info (analysis-lambda-info a))))
    ,@(when (analysis-store-variables a) ; For long form
        `(:store-variables ,(mapcar #'export-symbol (analysis-store-variables a))))
    ,@(when (analysis-parameters a) ; This will be nil for short form, populated for long
        `(:parameters ,(mapcar #'serialize-parameter-detail (analysis-parameters a))))
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))))

(defmethod write-analysis ((a deftype-analysis) filename &key)
  "Serializes a 'deftype-analysis' object, including parameters and docstring."
  `(,@(call-next-method)
    ,@(when (analysis-lambda-info a) ; Updated to include lambda-info
        `(:lambda-info ,(serialize-lambda-list-info (analysis-lambda-info a))))
    ,@(when (analysis-parameters a)
        `(:parameters ,(mapcar #'serialize-parameter-detail (analysis-parameters a))))
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

