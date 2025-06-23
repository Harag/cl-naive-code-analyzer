(in-package :cl-naive-code-analyzer)

(defclass defun-analysis (analysis) ())

(defclass defmethod-analysis (analysis) ())

(defclass define-condition-analysis (analysis) ())

(defclass defclass-analysis (analysis)
  ((slots :accessor analysis-slots :initform nil)
   (superclasses :accessor analysis-superclasses :initform nil)))

(defclass defparameter-analysis (analysis) ())

(defclass defmacro-analysis (analysis) ())

(defclass defclass-analysis (analysis)
  ((superclasses :accessor analysis-superclasses :initform nil)
   (slots :accessor analysis-slots :initform nil)))

(defclass deftype-analysis (analysis) ())

(defclass defgeneric-analysis (analysis) ())

(defclass defstruct-analysis (analysis)
  ((slots :accessor analysis-slots :initform nil)))

(defclass defsetf-analysis (analysis) ())

(defclass define-symbol-macro-analysis (analysis) ())

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

(defun real-raw (cst)
  (if (concrete-syntax-tree:consp cst)
      (concrete-syntax-tree:raw
       (concrete-syntax-tree:first cst))
      (concrete-syntax-tree:raw
       cst)))

(defgeneric make-analyzer (type)
  (:documentation "Return an analyzer instance for a given top-level form TYPE."))

(defmethod make-analyzer (type)
  (declare (ignore type))
  (make-instance 'analysis))

(defmethod make-analyzer ((type (eql 'defun)))
  (make-instance 'defun-analysis))

(defun classify-syntax (cst)
  "Return the specific form name (symbol) or nil."
  (when (concrete-syntax-tree:consp cst)
    (let ((head (concrete-syntax-tree:raw
                 (concrete-syntax-tree:first cst))))
      head)))

(defun classify-semantic (cst)
  (if (not (concrete-syntax-tree:consp cst))
      (if (symbolp (concrete-syntax-tree:raw cst))
          :symbol
          :literal)
      (let ((head (concrete-syntax-tree:raw (concrete-syntax-tree:first cst))))
        (if (not (consp head))
            (cond
              ;;TODO: Expand list
              ((member head '(setf setq push pop))
               :assignment)
              ((and (symbolp head) (special-operator-p head))
               :special)
              ((and (symbolp head) (macro-function head))
               :macro-call)
              ((and (symbolp head) (fboundp head))
               :call)
              ((atom head)
               (if (symbolp (concrete-syntax-tree:raw cst))
                   :symbol
                   :literal))
              (t
               :other))
            :other))))

(defun walk-cst (cst fn)
  (let ((table (make-hash-table :test #'eq)))
    (loop for remaining = cst then (concrete-syntax-tree:rest remaining)
          while (concrete-syntax-tree:consp remaining)
          until (gethash remaining table)
          do
          (progn
            (funcall fn remaining)
            (when (concrete-syntax-tree:atom (concrete-syntax-tree:first remaining))
              (funcall fn (concrete-syntax-tree:first remaining)))
            (unless (concrete-syntax-tree:null (concrete-syntax-tree:rest remaining))
              (when (concrete-syntax-tree:atom (concrete-syntax-tree:rest remaining))
                (funcall fn (concrete-syntax-tree:rest remaining))))
            (setf (gethash remaining table) t))
          finally (return (values (hash-table-count table)
                                  (cond ((concrete-syntax-tree:null remaining) :proper)
                                        ((concrete-syntax-tree:atom remaining) :dotted)
                                        (t :circular)))))))

(defun walk-cst-with-context (cst fn)
  "Walk the CST in a depth-first manner using your original loop.
Call FN with (path current-cst remaining-tail) at each node—both CONS and ATOM.
Preserves structure and is iterative with cycle detection."
  (let ((table (make-hash-table :test #'eq)))
    (labels ((step-cst (remaining path)
               (loop for rem = remaining then (concrete-syntax-tree:rest rem)
                     while (concrete-syntax-tree:consp rem)
                     until (gethash rem table)
                     do
                     (let* ((head-cst (concrete-syntax-tree:first rem))
                            (head (and head-cst (concrete-syntax-tree:raw head-cst)))
                            (tail (concrete-syntax-tree:rest rem)))
                       ;; Visit this CONS node
                       (funcall fn rem path tail)
                       (setf (gethash rem table) t)
                       ;; If tail begins with atom, visit that leaf too
                       (when (and (concrete-syntax-tree:atom tail))
                         (funcall fn
                                  tail
                                  (if head (append path (list head)) path)
                                  ;; no further tail for that leaf
                                  nil))
                       ;; Descend deeper with updated path
                       (step-cst tail (if head (append path (list head)) path)))
                     ;; At the end of this branch
                     (return))))
      (step-cst cst nil)
      ;; Return diagnostic
      (values (hash-table-count table)
              (cond ((concrete-syntax-tree:null cst) :proper)
                    ((concrete-syntax-tree:atom cst) :dotted)
                    (t :circular))))))

(defun gather-info (cst analysis)
  (case (classify-semantic cst)
    (:call
     (pushnew  (real-raw cst)
               (analysis-function-calls analysis)))
    (:macro-call
     (pushnew  (real-raw cst)
               (analysis-macro-calls analysis)))
    (:assignment
     (format t "Assignment CST: ~S~%" cst))
    (:symbol
     ;;TODO: What should we do here?
     (format t "Literal CST: ~S~%" cst))
    (:literal
     ;;TODO: What should we do here?
     (format t "Literal CST: ~S~%" cst))
    (otherwise
     ;;TODO: What should we do here?
     (format t "Other CST: ~S~%" cst))))

(defgeneric analyze-cst (cst analysis))

(defmethod analyze-cst :around (cst analysis)
  ;;TODO: Is this the right kind? Is kind the right word?
  (setf (analysis-kind analysis) (real-raw cst))
  (call-next-method))

(defmethod analyze-cst (cst analysis)
  (walk-cst-with-context
   cst
   (lambda (cst path tail)
     (declare (ignore path tail))
     (gather-info cst analysis)))
  analysis)

(defmethod analyze-cst (cst (analysis defun-analysis))
  (let* ((name-cst (concrete-syntax-tree:second cst))
         (args-cst (concrete-syntax-tree:third cst))
         (next (concrete-syntax-tree:fourth cst))
         (doc (prog1
                  (and (concrete-syntax-tree:atom next)
                       (stringp (concrete-syntax-tree:raw next))
                       ;;fith crashes if it does not exist or is not a
                       ;;consp. Not very nice of context-trees
                       (concrete-syntax-tree:nthrest 4 cst)
                       (concrete-syntax-tree:consp
                        (concrete-syntax-tree:nthrest 4 cst))
                       (prog1

                           (concrete-syntax-tree:raw
                            next)
                         (setf next (concrete-syntax-tree:fifth cst))))))
         (body-cst next)
         (name (concrete-syntax-tree:raw name-cst))
         (params

           (when (concrete-syntax-tree:consp args-cst)
             (mapcar #'concrete-syntax-tree:raw
                     (cst:listify args-cst)))))
    (setf (analysis-name analysis) name
          (analysis-kind analysis) :defun
          (analysis-parameters analysis) params
          (analysis-docstring analysis) doc
          (analysis-raw-body analysis) body-cst)

    (dolist (p params)
      (pushnew p (analysis-lexical-definitions analysis)))

    (when body-cst
      (gather-info body-cst analysis))

    analysis))

(defmethod make-analyzer ((type (eql 'defmethod)))
  (make-instance 'defmethod-analysis))

(defmethod make-analyzer ((type (eql 'defmacro)))
  (make-instance 'defmacro-analysis))

(defmethod make-analyzer ((type (eql 'defclass)))
  (make-instance 'defclass-analysis))

(defmethod make-analyzer ((type (eql 'defpackage)))
  (make-instance 'defpackage-analysis))

(defmethod make-analyzer ((type (eql 'defstruct)))
  (make-instance 'defstruct-analysis))

(defmethod make-analyzer ((type (eql 'define-condition)))
  (make-instance 'define-condition-analysis))

(defmethod make-analyzer ((type (eql 'deftype)))
  (make-instance 'deftype-analysis))

(defmethod make-analyzer ((type (eql 'defsetf)))
  (make-instance 'defsetf-analysis))

(defmethod make-analyzer ((type (eql 'define-symbol-macro)))
  (make-instance 'define-symbol-macro-analysis))

(defmethod make-analyzer ((type (eql 'defparameter)))
  (make-instance 'defparameter-analysis))

(defmethod make-analyzer ((type (eql 'defvar)))
  (make-instance 'defparameter-analysis))

(defmethod make-analyzer ((type (eql 'defconstant)))
  (make-instance 'defparameter-analysis))
