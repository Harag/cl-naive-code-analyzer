(in-package :cl-naive-code-analyzer)

(defun register-form-analyzer (type fn)
  (setf (gethash type *form-analyzers*) fn))

(register-form-analyzer
 'defun
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defun-analysis)
     (destructuring-bind (defun name args &rest body) form
       (declare (ignore defun))
       (setf (analysis-name analysis) (export-symbol name))
       (when (stringp (first body))
         (setf (analysis-docstring analysis) (first body)))
       ;; ✅ Parse
       (multiple-value-bind (bound-vars param-info)
           (parse-lambda-list+metadata args)
         ;; ✅ Store directly in proper slot
         (setf (analysis-parameters analysis) param-info)
         ;; ✅ Walk: env = NIL, defs = (NAME + bound-vars)
         (multiple-value-bind (fn macro var lex dyn)
             (extract-calls-and-variables form nil (cons name bound-vars))
           (setf (analysis-fn-calls analysis) fn
                 (analysis-macro-calls analysis) macro
                 (analysis-variable-uses analysis) var
                 (analysis-lexical-definitions analysis)
                 (union lex (analysis-lexical-definitions analysis) :test #'equal)
                 (analysis-dynamic-definitions analysis) dyn))))
     analysis)))

(defclass defmethod-analysis (analysis) ())

(register-form-analyzer
 'defmethod
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defmethod-analysis)
     (destructuring-bind (defmethod &rest rest) form
       (declare (ignore defmethod))
       (let ((name (first rest))
             (args nil)
             (body nil))

         (setf (analysis-name analysis) (export-symbol name))
         ;; Heuristic: find args & body
         (loop for index from 1
               for item in (rest rest)
               when (and (listp item) (every #'listp item))
               do (setf args item
                        body (nthcdr (1+ index) (rest rest)))
               and return nil)
         ;; Normalize (setf name)
         (when (and (consp name) (eq (car name) 'setf))
           (setf name (list :setf (second name))))
         ;; Extract docstring if present
         (when (and (consp body) (stringp (first body)))
           (setf (analysis-docstring analysis) (first body)))
         (when args
           ;; ✅ Correct parse + bind
           (multiple-value-bind (bound-vars param-info)
               (parse-lambda-list+metadata args)
             (setf (analysis-parameters analysis) param-info)
             (multiple-value-bind (fn macro var lex dyn)
                 (extract-calls-and-variables form nil (cons name bound-vars))
               (setf (analysis-fn-calls analysis) fn
                     (analysis-macro-calls analysis) macro
                     (analysis-variable-uses analysis) var
                     (analysis-lexical-definitions analysis)
                     (union lex (analysis-lexical-definitions analysis) :test #'equal)
                     (analysis-dynamic-definitions analysis) dyn)))))
       analysis))))

(register-form-analyzer
 'defmacro
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defmacro-analysis)
     (destructuring-bind (defmacro name args &rest body) form
       (declare (ignore defmacro))
       (setf (analysis-name analysis) (export-symbol name))
       (when (stringp (first body))
         (setf (analysis-docstring analysis) (first body)))
       ;; ✅ Correct: parse + bind
       (multiple-value-bind (bound-vars param-info)
           (parse-lambda-list+metadata args)
         (setf (analysis-parameters analysis) param-info)
         (multiple-value-bind (fn macro var lex dyn)
             (extract-calls-and-variables form nil (cons name bound-vars))
           (setf (analysis-fn-calls analysis) fn
                 (analysis-macro-calls analysis) macro
                 (analysis-variable-uses analysis) var
                 (analysis-lexical-definitions analysis)
                 (union lex (analysis-lexical-definitions analysis) :test #'equal)
                 (analysis-dynamic-definitions analysis) dyn))))
     analysis)))

(defclass defclass-analysis (analysis)
  ((superclasses :accessor analysis-superclasses :initform nil)
   (slots :accessor analysis-slots :initform nil)))

(register-form-analyzer
 'defclass
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defclass-analysis)
     (let ((supers (ensure-list (third form)))
           (slots (fourth form)))
       (setf (analysis-superclasses analysis) (normalize-symbol-list supers))
       (dolist (slot slots)
         (let ((initform (getf (cdr slot) :initform)))
           (when initform
             (multiple-value-bind (fn-calls macro-calls var-uses)
                 (extract-calls-and-variables initform nil nil)
               (push `(:slot ,(car slot) :initform ,initform
                       :function-calls ,(mapcar #'export-symbol fn-calls)
                       :macro-calls ,(mapcar #'export-symbol macro-calls)
                       :variable-uses ,(mapcar #'export-symbol var-uses))
                     (analysis-slots analysis))
               (setf (analysis-fn-calls analysis)
                     (union (analysis-fn-calls analysis) fn-calls :test #'equal))
               (setf (analysis-macro-calls analysis)
                     (union (analysis-macro-calls analysis) macro-calls :test #'equal))
               (setf (analysis-variable-uses analysis)
                     (union (analysis-variable-uses analysis) var-uses :test #'equal)))))))
     analysis)))

(register-form-analyzer
 'defparameter
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defparameter-analysis)
     (let ((initform (third form)))
       (when initform
         (multiple-value-bind (fn-calls macro-calls var-uses)
             (extract-calls-and-variables initform nil nil)
           (setf (analysis-fn-calls analysis) fn-calls
                 (analysis-macro-calls analysis) macro-calls
                 (analysis-variable-uses analysis) var-uses)))))))

(register-form-analyzer
 'defvar
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defparameter-analysis)
     (let ((initform (third form)))
       (when initform
         (multiple-value-bind (fn-calls macro-calls var-uses)
             (extract-calls-and-variables initform nil nil)
           (setf (analysis-fn-calls analysis) fn-calls
                 (analysis-macro-calls analysis) macro-calls
                 (analysis-variable-uses analysis) var-uses)))))))

(register-form-analyzer
 'defconstant
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defparameter-analysis)
     (let ((initform (third form)))
       (when initform
         (multiple-value-bind (fn-calls macro-calls var-uses)
             (extract-calls-and-variables initform nil nil)
           (setf (analysis-fn-calls analysis) fn-calls
                 (analysis-macro-calls analysis) macro-calls
                 (analysis-variable-uses analysis) var-uses)))))))

(defclass defgeneric-analysis (analysis) ())

(register-form-analyzer
 'defgeneric
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defgeneric-analysis)
     (let ((body (cdddr form)))
       (when (and (consp body) (stringp (first body)))
         (setf (analysis-docstring analysis) (first body))))
     analysis)))

(defclass defstruct-analysis (analysis)
  ((slots :accessor analysis-slots :initform nil)))

(register-form-analyzer
 'defstruct
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defstruct-analysis)
     ;; Whole form walk first — let walker handle nested slot initforms too
     (multiple-value-bind (fn-calls macro-calls var-uses lex-defs dyn-defs)
         (extract-calls-and-variables form nil nil)
       ;; Store top-level calls in struct meta
       (setf (analysis-fn-calls analysis) fn-calls
             (analysis-macro-calls analysis) macro-calls
             (analysis-variable-uses analysis) var-uses
             (analysis-lexical-definitions analysis) lex-defs
             (analysis-dynamic-definitions analysis) dyn-defs))
     ;; Additional manual parse for slots + constructor for compatibility
     (destructuring-bind (defstruct name &rest options) form
       (declare (ignore defstruct))

       (setf (analysis-name analysis)
             (export-symbol (if (listp name) (first name) name)))

       ;; Optional constructor lambda list
       (let ((ctor (find-if (lambda (x)
                              (and (consp x) (eq (car x) :constructor)))
                            options)))
         (when (and ctor (consp (second ctor)))
           (setf (analysis-lexical-definitions analysis)
                 (append (analysis-lexical-definitions analysis)
                         (second ctor)))))
       ;; Slots
       (let ((slot-specs (remove-if-not #'listp options)))
         (dolist (slot slot-specs)
           (let* ((slot-name (car slot))
                  (details (cdr slot))
                  (initform (cond
                              ((and (consp details) (null (cdr details)))
                               (car details))
                              ((and (evenp (length details))
                                    (keywordp (car details)))
                               (getf details :initial-value))
                              (t nil))))
             (when initform
               ;; Walk initform in isolation for slot metadata
               (multiple-value-bind (fn-calls macro-calls var-uses)
                   (extract-calls-and-variables initform nil nil)
                 (push `(:slot ,slot-name :initform ,initform
                         :function-calls ,(mapcar #'export-symbol fn-calls)
                         :macro-calls ,(mapcar #'export-symbol macro-calls)
                         :variable-uses ,(mapcar #'export-symbol var-uses))
                       (analysis-slots analysis))
                 ;; Union slot-level calls too
                 (setf (analysis-fn-calls analysis)
                       (union (analysis-fn-calls analysis) fn-calls :test #'equal))
                 (setf (analysis-macro-calls analysis)
                       (union (analysis-macro-calls analysis) macro-calls :test #'equal))
                 (setf (analysis-variable-uses analysis)
                       (union (analysis-variable-uses analysis) var-uses :test #'equal)))))))
       analysis))))

(defclass define-condition-analysis (defclass-analysis) ())

(register-form-analyzer
 'define-condition
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis define-condition-analysis)
     (let ((supers (ensure-list (third form)))
           (slots (fourth form)))
       (setf (analysis-superclasses analysis) (normalize-symbol-list supers))
       (dolist (slot slots)
         (let ((initform (getf (cdr slot) :initform)))
           (when initform
             (multiple-value-bind (fn-calls macro-calls var-uses)
                 (extract-calls-and-variables initform nil nil)
               (push `(:slot ,(car slot) :initform ,initform
                       :function-calls ,(mapcar #'export-symbol fn-calls)
                       :macro-calls ,(mapcar #'export-symbol macro-calls)
                       :variable-uses ,(mapcar #'export-symbol var-uses))
                     (analysis-slots analysis))
               (setf (analysis-fn-calls analysis)
                     (union (analysis-fn-calls analysis) fn-calls :test #'equal))
               (setf (analysis-macro-calls analysis)
                     (union (analysis-macro-calls analysis) macro-calls :test #'equal))
               (setf (analysis-variable-uses analysis)
                     (union (analysis-variable-uses analysis) var-uses :test #'equal)))))))
     analysis)))

(defclass deftype-analysis (analysis) ())

(register-form-analyzer
 'deftype
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis deftype-analysis)
     (destructuring-bind (deftype name args expansion) form
       (declare (ignore deftype))

       (setf (analysis-name analysis) (export-symbol name))
       ;; ✅ parse
       (multiple-value-bind (bound-vars param-info)
           (parse-lambda-list+metadata args)
         (setf (analysis-parameters analysis) param-info)
         ;; ✅ pass env as NIL, pass lexical defs properly
         (multiple-value-bind (fn-calls macro-calls var-uses lex-defs dyn-defs)
             (extract-calls-and-variables expansion nil (cons name bound-vars))
           (setf (analysis-fn-calls analysis) fn-calls
                 (analysis-macro-calls analysis) macro-calls
                 (analysis-variable-uses analysis) var-uses
                 (analysis-lexical-definitions analysis)
                 (union lex-defs (analysis-lexical-definitions analysis) :test #'equal)
                 (analysis-dynamic-definitions analysis) dyn-defs))))
     analysis)))

(defclass defsetf-analysis (analysis) ())

(register-form-analyzer
 'defsetf
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defsetf-analysis)
     ;; defsetf can be (defsetf name args expansion)
     ;; or (defsetf name lambda-list update-fn)
     ;; Here: simple heuristic for the expansion.
     (let ((name (second form))
           (args (third form))
           (expansion (fourth form)))

       (setf (analysis-name analysis) (export-symbol name))

       (when expansion
         (multiple-value-bind (fn-calls macro-calls var-uses lex-defs dyn-defs)
             (extract-calls-and-variables expansion args (list name))
           (setf (analysis-fn-calls analysis) fn-calls
                 (analysis-macro-calls analysis) macro-calls
                 (analysis-variable-uses analysis) var-uses
                 (analysis-lexical-definitions analysis)
                 (union lex-defs (analysis-lexical-definitions analysis) :test #'equal)
                 (analysis-dynamic-definitions analysis) dyn-defs))))
     analysis)))

(defclass define-symbol-macro-analysis (analysis) ())

(register-form-analyzer
 'define-symbol-macro
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis define-symbol-macro-analysis)
     (destructuring-bind (define-symbol-macro name expansion) form
       (declare (ignore define-symbol-macro))

       (setf (analysis-name analysis) (export-symbol name))

       ;; Symbol macros don’t have a param list — just the expansion.
       (when expansion
         (multiple-value-bind (fn-calls macro-calls var-uses lex-defs dyn-defs)
             (extract-calls-and-variables expansion nil (list name))
           (setf (analysis-fn-calls analysis) fn-calls
                 (analysis-macro-calls analysis) macro-calls
                 (analysis-variable-uses analysis) var-uses
                 (analysis-lexical-definitions analysis)
                 (union lex-defs (analysis-lexical-definitions analysis) :test #'equal)
                 (analysis-dynamic-definitions analysis) dyn-defs))))
     analysis)))

(defclass defpackage-analysis (analysis)
  ((package-name
    :initform nil
    :accessor analysis-package-name)
   (nicknames
    :initform nil
    :accessor analysis-nicknames)
   (uses
    :initform nil
    :accessor analysis-uses)
   (exports
    :initform nil
    :accessor analysis-exports)
   (shadows
    :initform nil
    :accessor analysis-shadows)
   (shadowing-imports
    :initform nil
    :accessor analysis-shadowing-imports)
   (imports
    :initform nil
    :accessor analysis-imports)
   (interns
    :initform nil
    :accessor analysis-interns)
   (docstring
    :initform nil
    :accessor analysis-docstring)
   (other-options
    :initform nil
    :accessor analysis-other-options)))

(register-form-analyzer
 'defpackage
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defpackage-analysis)
     (destructuring-bind (defpackage name &rest options) form
       (declare (ignore defpackage))
       ;; Store package name properly
       (setf (analysis-name analysis) (export-symbol name)
             (analysis-package-name analysis) (export-symbol name))
       ;; Process options, pass `name` as fallback for each exported symbol
       (dolist (option options)
         (case (car option)
           (:nicknames
            (setf (analysis-nicknames analysis)
                  (mapcar (lambda (x) (export-symbol x name))
                          (cdr option))))
           (:use
            (setf (analysis-uses analysis)
                  (mapcar (lambda (x) (export-symbol x :common-lisp))
                          (cdr option))))
           (:export
            (setf (analysis-exports analysis)
                  (mapcar (lambda (x) (export-symbol x name))
                          (cdr option))))
           (:shadow
            (setf (analysis-shadows analysis)
                  (mapcar (lambda (x) (export-symbol x name))
                          (cdr option))))
           (:shadowing-import-from
            (setf (analysis-shadowing-imports analysis)
                  (mapcar (lambda (x) (export-symbol x name))
                          (cdr option))))
           (:import-from
            (setf (analysis-imports analysis)
                  (mapcar (lambda (x) (export-symbol x name))
                          (cdr option))))
           (:intern
            (setf (analysis-interns analysis)
                  (mapcar (lambda (x) (export-symbol x name))
                          (cdr option))))
           (otherwise
            (push option (slot-value analysis 'other-options))))))
     analysis)))

