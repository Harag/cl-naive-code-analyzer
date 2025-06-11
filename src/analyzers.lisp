(in-package :cl-naive-code-analyzer)

(defun register-form-analyzer (type fn)
  (setf (gethash type *form-analyzers*) fn))

;;; analyzers.lisp
(register-form-analyzer
 'defun
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defun-analysis)
     (let* ((args (third form))
            (body (cdddr form)))
       (when (and (consp body) (stringp (first body)))
         (setf (analysis-docstring analysis) (first body)
               body (rest body)))
       (multiple-value-bind (fn-calls macro-calls var-uses lex-defs dyn-defs)
           (extract-calls-and-variables body args)
         (setf (analysis-fn-calls analysis) fn-calls
               (analysis-macro-calls analysis) macro-calls
               (analysis-variable-uses analysis) var-uses
               (analysis-lexical-definitions analysis) lex-defs
               (analysis-dynamic-definitions analysis) dyn-defs))))))

(defclass defmethod-analysis (analysis) ())

(register-form-analyzer
 'defstruct
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defclass-analysis)
     (destructuring-bind (defstruct name &rest options) form
       (declare (ignore defstruct name))
       ;; Extract constructor lambda-list if present
       (let* ((constructor-spec (find-if (lambda (x)
                                           (and (consp x)
                                                (eq (first x) :constructor)))
                                         options))
              (ctor-lambda-list (when (and (consp constructor-spec)
                                           (consp (second constructor-spec)))
                                  (second constructor-spec))))
         (when ctor-lambda-list
           (setf (analysis-lexical-definitions analysis) ctor-lambda-list)))
       ;; Analyze slots
       (let ((slot-specs (remove-if-not #'listp options)))
         (dolist (slot slot-specs)
           (let* ((slot-name (car slot))
                  (details (cdr slot))
                  (initform (cond
                              ((and (consp details)
                                    (null (cdr details)))
                               (car details))
                              ((and (evenp (length details))
                                    (keywordp (car details)))
                               (getf details :initial-value))
                              (t nil))))
             (when initform
               (multiple-value-bind (fn-calls macro-calls var-uses)
                   (extract-calls-and-variables initform nil)
                 (push `(:slot ,slot-name :initform ,initform
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
       analysis))))

(register-form-analyzer
 'defmethod
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defun-analysis)
     (destructuring-bind (defmethod &rest rest) form
       (declare (ignore defmethod))
       (let ((name (first rest))
             (args nil)
             (body nil))

         ;; Determine where the lambda list starts
         (loop
           for index from 1
           for item in (rest rest)
           when (and (listp item)
                     (every #'listp item))
           do (progn
                (setf args item)
                (setf body (nthcdr (1+ index) (rest rest)))
                (return))

              ;; Edge case: malformed
           finally (return))

         ;; (setf ...) names
         (when (and (consp name) (eq (car name) 'setf))
           (setf name (list :setf (second name))))

         ;; Optional docstring
         (when (and (consp body) (stringp (first body)))
           (setf (analysis-docstring analysis) (first body))
           (setf body (rest body)))

         ;; Extract variable and function references
         (when (and args (listp args))
           (let ((normalized-args (flatten-typed-lambda-list args)))
             (multiple-value-bind (fn-calls macro-calls var-uses lex-defs dyn-defs)
                 (extract-calls-and-variables body normalized-args)
               (setf (analysis-fn-calls analysis) fn-calls
                     (analysis-macro-calls analysis) macro-calls
                     (analysis-variable-uses analysis) var-uses
                     (analysis-lexical-definitions analysis) lex-defs
                     (analysis-dynamic-definitions analysis) dyn-defs)))))
       analysis))))

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
                 (extract-calls-and-variables initform nil)
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
                     (union (analysis-variable-uses analysis) var-uses :test #'equal))))))))))

(register-form-analyzer
 'defparameter
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defparameter-analysis)
     (let ((initform (third form)))
       (when initform
         (multiple-value-bind (fn-calls macro-calls var-uses)
             (extract-calls-and-variables initform nil)
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
             (extract-calls-and-variables initform nil)
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
             (extract-calls-and-variables initform nil)
           (setf (analysis-fn-calls analysis) fn-calls
                 (analysis-macro-calls analysis) macro-calls
                 (analysis-variable-uses analysis) var-uses)))))))

(defclass defgeneric-analysis (analysis) ())

(register-form-analyzer
 'defgeneric
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defgeneric-analysis)
     ;; Optional docstring check, similar to defun
     (let ((body (cdddr form)))
       (when (and (consp body) (stringp (first body)))
         (setf (analysis-docstring analysis) (first body))))
     analysis)))

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
                 (extract-calls-and-variables initform nil)
               (push `(:slot ,(car slot) :initform ,initform
                       :function-calls ,(mapcar #'export-symbol fn-calls)
                       :macro-calls ,(mapcar #'export-symbol macro-calls)
                       :variable-uses ,(mapcar #'export-symbol var-uses))

                     (analysis-slots analysis))
               (setf (analysis-fn-calls analysis)
                     (union (analysis-fn-calls analysis)
                            fn-calls
                            :test #'equal))
               (setf (analysis-macro-calls analysis)
                     (union (analysis-macro-calls analysis)
                            macro-calls
                            :test #'equal))
               (setf (analysis-variable-uses analysis)
                     (union (analysis-variable-uses analysis)
                            var-uses
                            :test #'equal))))))))))

(register-form-analyzer
 'defmacro
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defmacro-analysis)
     (let* ((args (third form))
            (body (cdddr form)))
       (when (and (consp body) (stringp (first body)))
         (setf (analysis-docstring analysis) (first body)
               body (rest body)))
       (multiple-value-bind (fn-calls macro-calls var-uses lex-defs dyn-defs)
           (extract-calls-and-variables body args)
         (setf (analysis-fn-calls analysis) fn-calls
               (analysis-macro-calls analysis) macro-calls
               (analysis-variable-uses analysis) var-uses
               (analysis-lexical-definitions analysis) lex-defs
               (analysis-dynamic-definitions analysis) dyn-defs))))))

(defclass deftype-analysis (analysis) ())

(register-form-analyzer
 'deftype
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis deftype-analysis)
     (let* ((args (third form))
            (body (cdddr form)))
       (multiple-value-bind (fn-calls macro-calls vars)
           (extract-calls-and-variables body args)
         (setf (analysis-fn-calls analysis) fn-calls
               (analysis-macro-calls analysis) macro-calls
               (analysis-variable-uses analysis) vars))))))

(defclass defsetf-analysis (analysis) ())

(register-form-analyzer
 'defsetf
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defsetf-analysis)
     ;; Structure: (defsetf name lambda-list &body body)
     (let ((args (third form))
           (body (cdddr form)))
       (multiple-value-bind (fn-calls macro-calls vars)
           (extract-calls-and-variables body args)
         (setf (analysis-fn-calls analysis) fn-calls
               (analysis-macro-calls analysis) macro-calls
               (analysis-variable-uses analysis) vars))))))

(defclass define-symbol-macro-analysis (analysis) ())

(register-form-analyzer
 'define-symbol-macro
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis define-symbol-macro-analysis)
     (let ((expansion (third form)))
       (when expansion
         (multiple-value-bind (fn-calls macro-calls vars)
             (extract-calls-and-variables expansion nil)
           (setf (analysis-fn-calls analysis) fn-calls
                 (analysis-macro-calls analysis) macro-calls
                 (analysis-variable-uses analysis) vars)))))))

(defclass defpackage-analysis (analysis)
  ((package-name :accessor analysis-package-name :initform nil)
   (exports :accessor analysis-exports :initform nil)
   (uses :accessor analysis-uses :initform nil)))

(register-form-analyzer
 'defpackage
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defpackage-analysis)
     (destructuring-bind (defpackage name &rest options) form
       (declare (ignore defpackage))
       (setf (analysis-package-name analysis) name)
       (dolist (option options)
         (case (car option)
           (:use (setf (analysis-uses analysis) (cdr option)))
           (:export (setf (analysis-exports analysis) (cdr option)))))))))
