(in-package :portable-sb-walker)

;;; Macros
(define-walker-template dolist               (nil (nil eval return) repeat (eval)))
(define-walker-template dotimes               (nil (nil eval return) repeat (eval)))
(define-walker-template and               (nil repeat (eval)))
(define-walker-template or               (nil repeat (eval)))
(define-walker-template defun             walk-defun)
(define-walker-template defmacro             walk-defmacro)
(define-walker-template defvar             (nil nil eval))
(define-walker-template defparameter   (nil nil eval))
(define-walker-template defconstant    (nil nil eval))
(define-walker-template defpackage    (nil))
(define-walker-template declaim    (nil))
(define-walker-template incf    (nil  nil eval))
(define-walker-template decf    (nil  nil eval))
(define-walker-template print-unreadable-object    (nil  (eval eval repeat (eval)) repeat (return)))
(define-walker-template defclass    walk-defclass)
(define-walker-template defmethod walk-defmethod)
(define-walker-template defstruct   walk-defstruct)
(define-walker-template ignore-errors (nil repeat (eval)))
(define-walker-template handler-case walk-handler-case)
(define-walker-template handler-bind walk-handler-bind)
(define-walker-template loop walk-loop)
(define-walker-template setf walk-setf)

(defun walk-defclass (form context env)
  (destructuring-bind (defclass name superclasses slots &rest options) form
    (relist* form defclass name superclasses
             (relist* slots
                      (loop for slot in slots
                            collect
                            (if (consp slot)
                                (destructuring-bind (name &rest args
                                                     &key reader
                                                     accessor
                                                     initarg
                                                     (initform nil initformp)
                                                     type-specifier
                                                     documentation)
                                    slot
                                  (declare (ignore reader accessor initarg type-specifier documentation))
                                  (cond (initformp
                                         (let ((args (copy-list args)))
                                           (setf (getf args :initform)
                                                 (walk-form-internal initform context env))
                                           (relist* slot name args)))
                                        (t
                                         slot)))
                                slot)))
             (loop for option in options
                   collect (ecase (car option)
                             ((:documentation :metaclass) option)
                             (:default-initargs
                              (relist* option
                                       :default-initargs
                                       (loop for (initarg form) on (cdr option) by #'cddr
                                             collect initarg
                                             collect (walk-form-internal form context env)))))))))

(defun walk-defstruct (form context env)
  (destructuring-bind (defstruct name &rest slots) form
    (flet ((walk-slot-options (slots)
             (loop for slot in slots
                   collect
                   (if (consp slot)
                       (destructuring-bind (name &optional (initform nil initformp)
                                            &rest args &key read-only type)
                           slot
                         (declare (ignore read-only type))
                         (cond (initformp
                                (relist* slot name (walk-form-internal initform context env) args))
                               (t
                                slot)))
                       slot))))
      (relist* form defstruct
               (if (consp name)
                   (relist* name
                            (car name)
                            (loop for option in (cdr name)
                                  collect (if (symbolp option)
                                              option
                                              (ecase (car option)
                                                ((:type :predicate :conc-name) option)
                                                ((:print-function :print-object)
                                                 (relist option
                                                         (car option)
                                                         (if (symbolp (cadr option))
                                                             (walk-form-internal `(function ,(cadr option)) context env)
                                                             (walk-form-internal (cadr option) context env))))
                                                (:constructor
                                                    (destructuring-bind (constructor name &optional lambda-list)
                                                        option
                                                      (if lambda-list
                                                          (relist* option constructor (walk-lambda (list name lambda-list) context env))
                                                          option)))
                                                (:include
                                                 (destructuring-bind (include name &rest slots)
                                                     option
                                                   (if slots
                                                       (relist* option include name
                                                                (walk-slot-options slots))
                                                       option)))))))
                   name)
               (walk-slot-options slots)))))

(defun walk-defun (form context env)
  (recons form (car form) (walk-lambda (cdr form) context env)))

(defun walk-defmacro (form context env)
  (recons form (car form) (walk-macro-lambda (cdr form) context env)))

(defun walk-method-lambda (form context old-env)
  (walker-environment-bind (new-env old-env)
                           (let* ((arglist (car form))
                                  (body (cdr form))
                                  (walked-arglist (walk-arglist arglist context new-env nil t))
                                  (walked-body
                                    (walk-declarations body #'walk-repeat-eval new-env)))
                             (relist* form
                                      walked-arglist
                                      walked-body))))

(defun walk-defmethod (form context env)
  (destructuring-bind (defmethod name &rest args) form
    (relist* form
             defmethod
             name
             (let* ((non-qualifiers-args args)
                    (qualifiers (loop while (not (consp (car non-qualifiers-args)))
                                      collect (pop non-qualifiers-args))))
               (if qualifiers
                   (relist* args
                            qualifiers
                            (walk-method-lambda non-qualifiers-args context env))
                   (relist* args
                            (walk-method-lambda non-qualifiers-args context env)))))))

(defun walk-handler-case (form context env)
  (destructuring-bind (handler-case h-form &rest cases) form
    (relist* form
             handler-case
             (walk-form-internal h-form context env)
             (loop for case in cases
                   collect (if (eq (car case) :no-error)
                               (walk-lambda case context env)
                               (relist* case (car case) (cadr case)
                                        (walk-repeat-eval (cddr case) env)))))))

(defun walk-handler-bind (form context env)
  (declare (ignore context))
  (destructuring-bind (handler-bind handlers &rest cases) form
    (relist* form
             handler-bind
             (loop for handler in handlers
                   collect (relist handler
                                   (car handler)
                                   (walk-form-internal (cadr handler) :call env)))
             (walk-repeat-eval cases env))))

;;; From SBCL
(labels ((singleton-p (x)
           (typep x '(cons * null)))
         (gen-let* (bindings body-forms)
           (cond ((not bindings) body-forms)
                 (t
                  (when (and (singleton-p body-forms)
                             (listp (car body-forms))
                             (eq (caar body-forms) 'let*))
                    (let ((nested (cdar body-forms))) ; extract the nested LET*
                      (setq bindings (append bindings (car nested))
                            body-forms (cdr nested))))
                  `((let* ,bindings ,@body-forms)))))
         (gen-mv-bind (stores values body-forms)
           (if (singleton-p stores)
               (gen-let* `((,(car stores) ,values)) body-forms)
               `((multiple-value-bind ,stores ,values ,@body-forms))))
         (forms-list (form)
           (if (and (consp form) (eq (car form) 'progn))
               (cdr form)
               (list form))))

  (defun walk-setf (form context env)
    `(progn
       ,@(loop for (place value) on (cdr form) by #'cddr
               collect
               (multiple-value-bind (temps vals newval setter)
                   (get-setf-expansion place)
                 (car (gen-let* (mapcar (lambda (temp val)
                                          (list temp
                                                (walk-form-internal val context env)))
                                        temps vals)
                                (gen-mv-bind newval
                                             (walk-form-internal value context env)
                                             (forms-list setter)))))))))

