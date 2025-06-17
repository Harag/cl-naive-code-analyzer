(in-package :test)
;;;; Some code to test with

;; A global variable
(defparameter *global-var* 42
  "A global variable used elsewhere.")

(defun default-name ()
  "Returns a default name."
  "Anonymous")

(defun compute-age (n)
  "Computes an age based on a name (dummy logic)."
  (length n))

(defun default-x ()
  "Default x coordinate."
  5)

(defun generate-y (x)
  "Generates y based on x."
  (+ x 3))

(defun default-age ()
  30)

(defclass human ()
  ((name :initarg :name :accessor person-name :initform 'default-name)
   (age :initarg :age :accessor person-age :initform #'default-age)
   (id :initform (lambda () (random 10000)))
   (nick :initform "Anonymous")
   (height :initform 180)))

(defclass thing ()
  ((foo :accessor foo :initform 0)))

(defmethod (setf foo) ((val number) (target thing))
  (setf (slot-value target 'foo) val))

;; A class definition with computed initforms
(defclass person (human)
  ((id :initarg :name
       :accessor id
       :initform nil))
  (:documentation "A simple person class."))

;; A struct definition with function-based defaults
(defstruct (point (:constructor make-point (x y)))
  (x (default-x))
  (y (generate-y x)))

;; A condition definition to test define-condition
(define-condition custom-error (error)
  ((code :initform #'equalp)
   (reason :initform "Bad input")))

;; A function using LET and quoting
(defun use-let ()
  "Demonstrates local binding via LET."
  (let ((local (format nil "Hello")))
    local))

;; A function using MULTIPLE-VALUE-BIND
(defun multi-bind-fn ()
  "Tests MULTIPLE-VALUE-BIND handling."
  (multiple-value-bind (a b) (values 1 2)
    (format t "~A ~A" a b)
    (list a b)))

;;; A function with parameters
(defun with-args (arg1 arg2)
  "Takes two arguments and formats them."
  (list arg1 arg2)
  (format nil "~A-~A" arg1 arg2))

;; A function calling others and using the class + struct
(defun wrapper ()
  "Calls other functions, uses a macro, class and struct."
  (use-let)
  (multi-bind-fn)
  (with-args "x" "y")
  (macro-user)
  (let ((p (make-instance 'person :name "Phil" :age 40 :id (random 100000)))
        (pt (make-point 10 20)))
    (list (person-name p)
          (person-age p)
          (point-x pt)
          (point-y pt))))

;;; Generic function with method specialization
(defgeneric process (x))

(defmethod process ((x integer))
  "Processes an integer."
  (+ x 10))

;;;; Some more file comments
;;; for testing

(defmethod no-doc-string-method ((y (eql :y)))
  "yes")

(no-doc-string-method :y)

(defun no-doc-string-function ()
  "yes")

(no-doc-string-function)

(defmethod process ((x string))
  "Processes a string."
  (concatenate 'string "Processed: " x))

;; Function referencing a global var
(defun ref-global ()
  "Reads the global var and adds 1."
  (1+ *global-var*))

;; A simple macro with a binding
(defmacro my-macro (x)
  "Macro that evaluates its argument twice."
  `(list ,x ,x))

;; Function using a macro
(defun macro-user ()
  "Invokes the test macro."
  (my-macro 'foo))

;; defmethod with (setf ...) specialization
(defmethod (setf person-name) ((new-name string) (obj person))
  "Sets the name of a person."
  (setf (slot-value obj 'name) new-name))

;; defclass with diverse initforms
(defclass mixed-initforms ()
  ((literal-number :initform 42)
   (literal-string :initform "hello")
   (quoted-symbol :initform 'foo)
   (function-ref :initform #'default-age)
   (inline-lambda :initform (lambda (x) (+ x 1))))
  (:documentation "Class to test different initforms."))

(defun unused-function ()
  "unused")

;; Track lambda binding separately
(defparameter *inc-fn* (lambda (n) (+ n 1)))

(defun test-optional (&optional (x 42) (y *global-var*))
  (+ x y))

(defun test-key (&key (foo "bar") (baz 100))
  (list foo baz))

(defun test-mixed (a &optional (b 1) &key (k 10) &aux (tmp 20))
  (+ a b k tmp))

(defun test-rest (x &rest args)
  (cons x args))

(defmethod test-method ((x integer) (y (eql :foo)))
  (+ x 1))
