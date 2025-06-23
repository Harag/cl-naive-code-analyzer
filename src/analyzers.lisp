(in-package :cl-naive-code-analyzer)

(defclass defun-analysis (analysis)
  ((lambda-info :accessor analysis-lambda-info :initform nil)
   (parameters :accessor analysis-parameters :initform nil)
   (docstring :accessor analysis-docstring :initform nil)))

(defclass defmethod-analysis (analysis)
  ((parameters :accessor analysis-parameters :initform nil)
   (docstring :accessor analysis-docstring :initform nil)))

(defclass define-condition-analysis (analysis)
  ((docstring :accessor analysis-docstring :initform nil)))


(defclass defclass-analysis (analysis)
  ((slots :accessor analysis-slots :initform nil)
   (superclasses :accessor analysis-superclasses :initform nil)
   (docstring :accessor analysis-docstring :initform nil)))

(defclass defparameter-analysis (analysis)
  ((docstring :accessor analysis-docstring :initform nil)))

(defclass defmacro-analysis (analysis)
  ((parameters :accessor analysis-parameters :initform nil)
   (docstring :accessor analysis-docstring :initform nil)))


(defclass deftype-analysis (analysis)
  ((parameters :accessor analysis-parameters :initform nil)
   (docstring :accessor analysis-docstring :initform nil)))

(defclass defgeneric-analysis (analysis)
  ((parameters :accessor analysis-parameters :initform nil)
   (docstring :accessor analysis-docstring :initform nil)))

(defclass defstruct-analysis (analysis)
  ((slots :accessor analysis-slots :initform nil)
   (docstring :accessor analysis-docstring :initform nil)))

(defclass defsetf-analysis (analysis)
  ((parameters :accessor analysis-parameters :initform nil)
   (docstring :accessor analysis-docstring :initform nil)))

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
  (let* ((name-cst      (concrete-syntax-tree:second cst))
         (args-cst      (concrete-syntax-tree:third cst))
         (possible-doc  (concrete-syntax-tree:fourth cst))
         (doc           (when (and (concrete-syntax-tree:atom possible-doc)
                                   (stringp (concrete-syntax-tree:raw possible-doc)))
                          (concrete-syntax-tree:raw possible-doc)))
         (body-cst      (if doc
                            (concrete-syntax-tree:nthrest 3 cst)
                            (concrete-syntax-tree:nth 3 cst)))
         (name          (concrete-syntax-tree:raw name-cst))
         ;; Parse the lambda list using Alexandria
         (lambda-list   (and (concrete-syntax-tree:consp args-cst)
                             (mapcar #'concrete-syntax-tree:raw
                                     (cst:listify args-cst))))
         (parsed        (when lambda-list
                          (alexandria:parse-ordinary-lambda-list
                           lambda-list
                           :normalize t
                           :allow-specializers t
                           :normalize-optional t
                           :normalize-keyword t
                           :normalize-auxilary t)))
         ;; Destructure parsed components
         (required      (first parsed))
         (optionals     (second parsed)) ; list of (name init suppliedp)
         (rest-name     (third parsed))
         (keywords      (fourth parsed)) ; list of ((keyword name) init suppliedp)
         (allow-other-p (fifth parsed))
         (auxes         (sixth parsed))) ; list of (name init)
    ;; Populate analysis slots
    (setf (analysis-name analysis)      name
          (analysis-kind analysis)      :defun
          (analysis-docstring analysis) doc
          (analysis-raw-body analysis)  body-cst
          (analysis-parameters analysis)
          (append required
                  (mapcar #'car optionals)
                  (when rest-name (list rest-name))
                  (mapcar (lambda (x) (cadr x)) keywords)
                  (mapcar #'car auxes)))
    ;; For detailed parameter meta, you could store elsewhere, e.g.:
    (setf (analysis-lambda-info analysis)
          (list :required required
                :optionals optionals
                :rest rest-name
                :keywords keywords
                :allow-other-keys allow-other-p
                :auxes auxes))
    ;; Lexical defs for each symbol parameter
    (dolist (p (append required
                       (mapcar #'car optionals)
                       (when rest-name (list rest-name))
                       (mapcar (lambda (x) (cadr x)) keywords)
                       (mapcar #'car auxes)))
      (pushnew p (analysis-lexical-definitions analysis)))

    ;; Walk the body for calls, uses, assignments, etc.
    (when body-cst
      (walk-cst-with-context
       body-cst
       (lambda (cst path tail)
         (declare (ignore path tail))
         (gather-info cst analysis))))
    analysis))

;;; ---------------------------------------------------------------------------
;;; Additional analyzers

(defun %parse-lambda-info (args-cst)
  (let* ((lambda-list (and (concrete-syntax-tree:consp args-cst)
                           (mapcar #'concrete-syntax-tree:raw
                                   (cst:listify args-cst))))
         (parsed (when lambda-list
                   (alexandria:parse-ordinary-lambda-list
                    lambda-list
                    :normalize t
                    :allow-specializers t
                    :normalize-optional t
                    :normalize-keyword t
                    :normalize-auxilary t)))
         (required      (first parsed))
         (optionals     (second parsed))
         (rest-name     (third parsed))
         (keywords      (fourth parsed))
         (allow-other-p (fifth parsed))
         (auxes         (sixth parsed)))
    (values required optionals rest-name keywords allow-other-p auxes)))

(defun %parameter-list-from-lambda-info (required optionals rest-name keywords auxes)
  (append required
          (mapcar #'car optionals)
          (when rest-name (list rest-name))
          (mapcar (lambda (x) (cadr x)) keywords)
          (mapcar #'car auxes)))

(defmethod analyze-cst (cst (analysis defmacro-analysis))
  (let* ((name-cst      (concrete-syntax-tree:second cst))
         (args-cst      (concrete-syntax-tree:third cst))
         (possible-doc  (concrete-syntax-tree:fourth cst))
         (doc           (when (and (concrete-syntax-tree:atom possible-doc)
                                   (stringp (concrete-syntax-tree:raw possible-doc)))
                          (concrete-syntax-tree:raw possible-doc)))
         (body-cst      (if doc
                            (concrete-syntax-tree:nthrest 3 cst)
                            (concrete-syntax-tree:nth 3 cst)))
         (name          (concrete-syntax-tree:raw name-cst)))
    (multiple-value-bind (required optionals rest-name keywords allow-other-p auxes)
        (%parse-lambda-info args-cst)
      (setf (analysis-name analysis) name
            (analysis-kind analysis) :defmacro
            (analysis-docstring analysis) doc
            (analysis-raw-body analysis) body-cst
            (analysis-parameters analysis)
            (%parameter-list-from-lambda-info required optionals rest-name keywords auxes))
      (dolist (p (analysis-parameters analysis))
        (pushnew p (analysis-lexical-definitions analysis)))
      (when body-cst
        (walk-cst-with-context
         body-cst
         (lambda (cst path tail)
           (declare (ignore path tail))
           (gather-info cst analysis))))
      analysis))

(defmethod analyze-cst (cst (analysis defmethod-analysis))
  (let* ((tail (concrete-syntax-tree:rest cst))
         (qualifiers '()))
    ;; skip method qualifiers
    (loop while (and (concrete-syntax-tree:consp tail)
                     (concrete-syntax-tree:atom (concrete-syntax-tree:first tail))
                     (keywordp (concrete-syntax-tree:raw (concrete-syntax-tree:first tail))))
          do (progn
               (push (concrete-syntax-tree:raw (concrete-syntax-tree:first tail)) qualifiers)
               (setf tail (concrete-syntax-tree:rest tail))))
    (let* ((name-cst (concrete-syntax-tree:first tail))
           (tail     (concrete-syntax-tree:rest tail))
           (args-cst (concrete-syntax-tree:first tail))
           (tail     (concrete-syntax-tree:rest tail))
           (possible-doc (and (concrete-syntax-tree:consp tail)
                              (concrete-syntax-tree:first tail)))
           (doc  (when (and possible-doc
                            (concrete-syntax-tree:atom possible-doc)
                            (stringp (concrete-syntax-tree:raw possible-doc)))
                   (concrete-syntax-tree:raw possible-doc)))
           (body-cst (if doc
                         (concrete-syntax-tree:rest tail)
                         tail))
           (name (real-raw name-cst)))
      (multiple-value-bind (required optionals rest-name keywords allow-other-p auxes)
          (%parse-lambda-info args-cst)
        (setf (analysis-name analysis) name
              (analysis-kind analysis) :defmethod
              (analysis-docstring analysis) doc
              (analysis-raw-body analysis) body-cst
              (analysis-parameters analysis)
              (%parameter-list-from-lambda-info required optionals rest-name keywords auxes))
        (dolist (p (analysis-parameters analysis))
          (pushnew p (analysis-lexical-definitions analysis)))
        (when body-cst
          (walk-cst-with-context
           body-cst
           (lambda (cst path tail)
             (declare (ignore path tail))
             (gather-info cst analysis))))
        analysis)))

(defmethod analyze-cst (cst (analysis defgeneric-analysis))
  (let* ((name-cst (concrete-syntax-tree:second cst))
         (args-cst (concrete-syntax-tree:third cst))
         (options-cst (concrete-syntax-tree:nthrest 3 cst))
         (name (concrete-syntax-tree:raw name-cst))
         (doc nil))
    ;; look for (:documentation "...")
    (when (concrete-syntax-tree:consp options-cst)
      (dolist (opt (cst:listify options-cst))
        (when (and (concrete-syntax-tree:consp opt)
                   (eq (real-raw (concrete-syntax-tree:first opt)) :documentation))
          (let ((d (concrete-syntax-tree:second opt)))
            (when (and d (concrete-syntax-tree:atom d)
                       (stringp (concrete-syntax-tree:raw d)))
              (setf doc (concrete-syntax-tree:raw d))))))
    (multiple-value-bind (required optionals rest-name keywords allow-other-p auxes)
        (%parse-lambda-info args-cst)
      (setf (analysis-name analysis) name
            (analysis-kind analysis) :defgeneric
            (analysis-docstring analysis) doc
            (analysis-raw-body analysis) options-cst
            (analysis-parameters analysis)
            (%parameter-list-from-lambda-info required optionals rest-name keywords auxes))
      (dolist (p (analysis-parameters analysis))
        (pushnew p (analysis-lexical-definitions analysis)))
      (when options-cst
        (walk-cst-with-context
         options-cst
         (lambda (cst path tail)
           (declare (ignore path tail))
           (gather-info cst analysis))))
      analysis))

(defmethod analyze-cst (cst (analysis deftype-analysis))
  (let* ((name-cst (concrete-syntax-tree:second cst))
         (args-cst (concrete-syntax-tree:third cst))
         (possible-doc (concrete-syntax-tree:fourth cst))
         (doc (when (and (concrete-syntax-tree:atom possible-doc)
                         (stringp (concrete-syntax-tree:raw possible-doc)))
                (concrete-syntax-tree:raw possible-doc)))
         (body-cst (if doc
                       (concrete-syntax-tree:nthrest 3 cst)
                       (concrete-syntax-tree:nth 3 cst)))
         (name (concrete-syntax-tree:raw name-cst)))
    (multiple-value-bind (required optionals rest-name keywords allow-other-p auxes)
        (%parse-lambda-info args-cst)
      (setf (analysis-name analysis) name
            (analysis-kind analysis) :deftype
            (analysis-docstring analysis) doc
            (analysis-raw-body analysis) body-cst
            (analysis-parameters analysis)
            (%parameter-list-from-lambda-info required optionals rest-name keywords auxes))
      (dolist (p (analysis-parameters analysis))
        (pushnew p (analysis-lexical-definitions analysis)))
      (when body-cst
        (walk-cst-with-context
         body-cst
         (lambda (cst path tail)
           (declare (ignore path tail))
           (gather-info cst analysis))))
      analysis))

(defmethod analyze-cst (cst (analysis defsetf-analysis))
  (let* ((name-cst (concrete-syntax-tree:second cst))
         (tail (concrete-syntax-tree:nthrest 2 cst))
         (doc nil))
    ;; simple docstring detection
    (when (and (concrete-syntax-tree:consp tail)
               (concrete-syntax-tree:atom (concrete-syntax-tree:last tail))
               (stringp (concrete-syntax-tree:raw (concrete-syntax-tree:last tail))))
      (setf doc (concrete-syntax-tree:raw (concrete-syntax-tree:last tail))))
    (setf (analysis-name analysis) (real-raw name-cst)
          (analysis-kind analysis) :defsetf
          (analysis-docstring analysis) doc
          (analysis-raw-body analysis) tail)
    (call-next-method)))

(defmethod analyze-cst (cst (analysis defparameter-analysis))
  (let* ((name-cst (concrete-syntax-tree:second cst))
         (init-cst (concrete-syntax-tree:third cst))
         (possible-doc (concrete-syntax-tree:fourth cst))
         (doc (when (and (concrete-syntax-tree:atom possible-doc)
                         (stringp (concrete-syntax-tree:raw possible-doc)))
                (concrete-syntax-tree:raw possible-doc)))
         (name (concrete-syntax-tree:raw name-cst)))
    (setf (analysis-name analysis) name
          (analysis-kind analysis) :defparameter
          (analysis-docstring analysis) doc
          (analysis-raw-body analysis) init-cst)
    (when init-cst
      (walk-cst-with-context
       init-cst
       (lambda (cst path tail)
         (declare (ignore path tail))
         (gather-info cst analysis))))
    analysis))

(defmethod analyze-cst (cst (analysis defclass-analysis))
  (let* ((name-cst (concrete-syntax-tree:second cst))
         (supers-cst (concrete-syntax-tree:third cst))
         (slots-cst (concrete-syntax-tree:fourth cst))
         (options-cst (concrete-syntax-tree:nthrest 4 cst))
         (doc nil))
    (when (concrete-syntax-tree:consp options-cst)
      (dolist (opt (cst:listify options-cst))
        (when (and (concrete-syntax-tree:consp opt)
                   (eq (real-raw (concrete-syntax-tree:first opt)) :documentation))
          (let ((d (concrete-syntax-tree:second opt)))
            (when (and d (concrete-syntax-tree:atom d)
                       (stringp (concrete-syntax-tree:raw d)))
              (setf doc (concrete-syntax-tree:raw d))))))
    (setf (analysis-name analysis) (concrete-syntax-tree:raw name-cst)
          (analysis-kind analysis) :defclass
          (analysis-superclasses analysis)
          (when (concrete-syntax-tree:consp supers-cst)
            (mapcar #'concrete-syntax-tree:raw (cst:listify supers-cst)))
          (analysis-slots analysis)
          (when (concrete-syntax-tree:consp slots-cst)
            (mapcar (lambda (s) (mapcar #'concrete-syntax-tree:raw (cst:listify s)))
                    (cst:listify slots-cst)))
          (analysis-docstring analysis) doc
          (analysis-raw-body analysis) slots-cst)
    (call-next-method))

(defmethod analyze-cst (cst (analysis defstruct-analysis))
  (let* ((name-options (concrete-syntax-tree:second cst))
         (name-cst (if (concrete-syntax-tree:consp name-options)
                       (concrete-syntax-tree:first name-options)
                       name-options))
         (slots-cst (if (concrete-syntax-tree:consp name-options)
                        (concrete-syntax-tree:nthrest 2 cst)
                        (concrete-syntax-tree:nthrest 2 cst)))
         (doc nil))
    (setf (analysis-name analysis) (concrete-syntax-tree:raw name-cst)
          (analysis-kind analysis) :defstruct
          (analysis-docstring analysis) doc
          (analysis-slots analysis)
          (when (concrete-syntax-tree:consp slots-cst)
            (mapcar (lambda (s) (mapcar #'concrete-syntax-tree:raw (cst:listify s)))
                    (cst:listify slots-cst)))
          (analysis-raw-body analysis) slots-cst)
    (call-next-method))

(defmethod analyze-cst (cst (analysis define-condition-analysis))
  (let* ((name-cst (concrete-syntax-tree:second cst))
         (supers-cst (concrete-syntax-tree:third cst))
         (slots-cst (concrete-syntax-tree:nth 3 cst))
         (options-cst (concrete-syntax-tree:nthrest 4 cst))
         (doc nil))
    (when (concrete-syntax-tree:consp options-cst)
      (dolist (opt (cst:listify options-cst))
        (when (and (concrete-syntax-tree:consp opt)
                   (eq (real-raw (concrete-syntax-tree:first opt)) :documentation))
          (let ((d (concrete-syntax-tree:second opt)))
            (when (and d (concrete-syntax-tree:atom d)
                       (stringp (concrete-syntax-tree:raw d)))
              (setf doc (concrete-syntax-tree:raw d))))))
    (setf (analysis-name analysis) (concrete-syntax-tree:raw name-cst)
          (analysis-kind analysis) :define-condition
          (analysis-superclasses analysis)
          (when (concrete-syntax-tree:consp supers-cst)
            (mapcar #'concrete-syntax-tree:raw (cst:listify supers-cst)))
          (analysis-slots analysis)
          (when (concrete-syntax-tree:consp slots-cst)
            (mapcar (lambda (s) (mapcar #'concrete-syntax-tree:raw (cst:listify s)))
                    (cst:listify slots-cst)))
          (analysis-docstring analysis) doc
          (analysis-raw-body analysis) slots-cst)
    (call-next-method))

(defmethod analyze-cst (cst (analysis defpackage-analysis))
  (let* ((name-cst (concrete-syntax-tree:second cst))
         (options (concrete-syntax-tree:nthrest 2 cst))
         (name (concrete-syntax-tree:raw name-cst))
         (doc nil)
         (nicknames nil)
         (uses nil)
         (exports nil)
         (shadows nil)
         (shadowing-imports nil)
         (imports nil)
         (interns nil)
         (other nil))
    (when (concrete-syntax-tree:consp options)
      (dolist (opt (cst:listify options))
        (let ((head (real-raw (concrete-syntax-tree:first opt)))
              (tail (concrete-syntax-tree:rest opt)))
          (case head
            (:nicknames (setf nicknames (mapcar #'concrete-syntax-tree:raw (cst:listify tail))))
            (:use       (setf uses (mapcar #'concrete-syntax-tree:raw (cst:listify tail))))
            (:export    (setf exports (mapcar #'concrete-syntax-tree:raw (cst:listify tail))))
            (:shadow    (setf shadows (mapcar #'concrete-syntax-tree:raw (cst:listify tail))))
            (:shadowing-import-from (setf shadowing-imports (mapcar #'concrete-syntax-tree:raw (cst:listify tail))))
            (:import-from (setf imports (mapcar #'concrete-syntax-tree:raw (cst:listify tail))))
            (:intern     (setf interns (mapcar #'concrete-syntax-tree:raw (cst:listify tail))))
            (:documentation (let ((d (concrete-syntax-tree:first tail)))
                              (when (and d (concrete-syntax-tree:atom d)
                                         (stringp (concrete-syntax-tree:raw d)))
                                (setf doc (concrete-syntax-tree:raw d))))
            (t (push (mapcar #'concrete-syntax-tree:raw (cst:listify opt)) other))))))
    (setf (analysis-name analysis) name
          (analysis-kind analysis) :defpackage
          (analysis-package-name analysis) name
          (analysis-nicknames analysis) nicknames
          (analysis-uses analysis) uses
          (analysis-exports analysis) exports
          (analysis-shadows analysis) shadows
          (analysis-shadowing-imports analysis) shadowing-imports
          (analysis-imports analysis) imports
          (analysis-interns analysis) interns
          (analysis-other-options analysis) other
          (analysis-docstring analysis) doc
          (analysis-raw-body analysis) options)
    analysis)

(defmethod analyze-cst (cst (analysis define-symbol-macro-analysis))
  (let ((name-cst (concrete-syntax-tree:second cst))
        (expansion-cst (concrete-syntax-tree:third cst)))
    (setf (analysis-name analysis) (concrete-syntax-tree:raw name-cst)
          (analysis-kind analysis) :define-symbol-macro
          (analysis-raw-body analysis) expansion-cst)
    (call-next-method)))


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
