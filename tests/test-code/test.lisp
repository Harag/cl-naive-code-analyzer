;;; tests/test-code/test.lisp
;;;
;;; This file contains a variety of Lisp forms intended for testing the
;;; capabilities of the cl-naive-code-analyzer.
;;; The definitions are organized by the type of Lisp form (e.g., DEFUN, DEFCLASS)
;;; to allow for systematic testing.

(defpackage test-package-simple
  (:use :cl)
  (:nicknames :tps :test-package-simple) ; Added :test nickname
  (:export #:test-export-sym1 #:test-export-sym2)
  (:documentation "A simple test package for analysis."))

(in-package :test-package-simple) ; This should now work as test-package-simple has :test nickname

;;;;----------------------------------------------------------------------------
;;;; DEFPACKAGE
;;;;----------------------------------------------------------------------------
;;; The actual defpackage form has been moved to the top of the file.
;;; The placeholder example below is no longer needed here.
#|
(defpackage test-package-simple
(:use :cl)
(:nicknames :tps)
(:export #:test-export-sym1 #:test-export-sym2)
(:documentation "A simple test package for analysis."))
|#

;;;;----------------------------------------------------------------------------
;;;; DEFPARAMETER / DEFVAR / DEFCONSTANT
;;;;----------------------------------------------------------------------------

(defparameter *test-defparameter-simple* 100
  "A simple defparameter with a docstring.")

(defvar *test-defvar-simple* "hello"
  "A simple defvar with a docstring and initial value.")

(defvar *test-defvar-no-initval*
  "A defvar with no initial value.")

(defconstant +test-defconstant-simple+ 3.14
  "A simple defconstant with a docstring.")

(defparameter *test-defparameter-lambda* (lambda (x) (* x x))
  "A defparameter whose value is a lambda expression.")

;;;;----------------------------------------------------------------------------
;;;; DEFUN
;;;;----------------------------------------------------------------------------

(defun test-defun-simple ()
  "A simple function with no arguments and a docstring."
  (list 1 2 3))

(defun test-defun-no-docstring (a b)
  ;; This is a comment, not a docstring.
  (+ a b))

(defun test-defun-required-args (name count)
  "A function with required arguments."
  (format nil "Name: ~A, Count: ~D" name count))

(defun test-defun-optional-args (a &optional (b 10) (c "default-c" c-supplied-p))
  "A function with optional arguments, one with a default value, one with a supplied-p var."
  (list a b c c-supplied-p))

(defun test-defun-keyword-args (&key (mode :fast) (value 100 value-supplied-p))
  "A function with keyword arguments."
  (list mode value value-supplied-p))

(defun test-defun-rest-args (first &rest other-args)
  "A function with &rest arguments."
  (cons first other-args))

(defun test-defun-aux-vars (x &aux (y (* x 2)) (z 10))
  "A function with &aux variables."
  (+ x y z))

(defun test-defun-mixed-lambda-list (req1 &optional (opt1 1) &rest rst &aux (aux1 'foo)) ; Temporarily removed &key
  "A function with a complex lambda list: required, optional, keyword, rest, and aux."
  (list req1 opt1 rst aux1)) ; Temporarily removed key1

(defun test-defun-uses-global ()
  "A function that references a global variable *test-defparameter-simple*."
  (1+ *test-defparameter-simple*))

(defun test-defun-internal-let ()
  "A function with an internal LET binding."
  (let ((message "Internal"))
    message))

(defun test-defun-multiple-value-bind ()
  "A function using MULTIPLE-VALUE-BIND."
  (multiple-value-bind (q r) (truncate 10 3)
    (list q r)))

(defun test-defun-returns-string-not-docstring ()
  "This is a return value, not a docstring because it's not the first form after the lambda list."
  (let ((x 10)) x) ; A form before the string
  "Explicit return string")

;; Helper functions used by other test forms (e.g. class initforms, struct defaults)
(defun helper-default-value-for-slot ()
  "Provides a default value for a slot."
  12345)

(defun helper-generate-slot-value (input)
  "Generates a slot value based on an input."
  (* input 10))

;;;;----------------------------------------------------------------------------
;;;; DEFMACRO
;;;;----------------------------------------------------------------------------

(defmacro test-defmacro-simple (form)
  "A simple macro that wraps the form in a PROGN."
  `(progn ,form))

(defmacro test-defmacro-with-body (name &body body)
  "A macro using &body for multiple forms."
  `(let ((,name "macro-name"))
     (declare (ignorable ,name))
     ,@body))

(defun test-defun-uses-macro ()
  "A function that uses test-defmacro-simple."
  (test-defmacro-simple (print "Hello from macro")))

(defun test-defun-uses-macro-with-body ()
  "A function that uses test-defmacro-with-body."
  (test-defmacro-with-body my-var
                           (print "First body form")
                           (print "Second body form")))

;;;;----------------------------------------------------------------------------
;;;; DEFCLASS
;;;;----------------------------------------------------------------------------

(defclass test-class-simple ()
  ((slot-a :accessor class-slot-a :initarg :slot-a :initform 100)
   (slot-b :reader class-slot-b :initform "default-b"
           :documentation "Docstring for slot-b."))
  (:documentation "A simple class definition with slots and a docstring."))

(defclass test-class-no-docstring ()
  ((slot-x :initform nil)))

(defclass test-class-inheritance (test-class-simple)
  ((slot-c :accessor class-slot-c :initarg :slot-c :initform 'symbol-initform)
   (slot-d :initform (helper-default-value-for-slot))) ; Initform using a function call
  (:documentation "A class that inherits from test-class-simple."))

(defclass test-class-multiple-inheritance (test-class-simple test-class-no-docstring)
  ()
  (:documentation "A class with multiple inheritance. Note: test-class-no-docstring has no direct slots to inherit data-wise here, testing structure."))

(defclass test-class-initforms ()
  ((slot-literal-num :initform 42)
   (slot-literal-str :initform "string value")
   (slot-quoted-sym :initform 'a-symbol)
   (slot-func-ref :initform #'helper-default-value-for-slot)
   (slot-inline-lambda :initform (lambda (y) (* y y))
                       :documentation "Slot with an inline lambda initform."))
  (:documentation "A class demonstrating various types of initforms for slots."))

(defclass test-class-for-setf-method ()
  ((data :accessor data-of :initform 0))
  (:documentation "A class used to test (setf data-of) method."))

;;;;----------------------------------------------------------------------------
;;;; DEFSTRUCT
;;;;----------------------------------------------------------------------------

(defstruct test-struct-simple
  "A simple structure definition with a docstring."
  (field-a 0 :type integer)
  (field-b "default" :type string :read-only t))

(defstruct (test-struct-with-options (:conc-name tswo-) (:constructor make-tswo) (:predicate is-tswo))
  "A structure with various options like :conc-name, :constructor, and :predicate."
  (x 1 :type fixnum)
  (y (helper-generate-slot-value 5) :type fixnum)) ; Slot default using function call

(defstruct test-struct-no-doc
  field-no-doc)

;;;;----------------------------------------------------------------------------
;;;; DEFGENERIC
;;;;----------------------------------------------------------------------------

(defgeneric test-defgeneric-simple (obj)
  (:documentation "A simple generic function with one argument."))

(defgeneric test-defgeneric-no-doc (data))

(defgeneric test-defgeneric-with-method-option (x)
  (:method ((x integer))
    (+ x 100))
  (:documentation "A generic function defined with a :method option."))

;;;;----------------------------------------------------------------------------
;;;; DEFMETHOD
;;;;----------------------------------------------------------------------------

(defmethod test-defgeneric-simple ((obj test-class-simple))
  "A method specializing test-defgeneric-simple for test-class-simple."
  (class-slot-a obj))

(defmethod test-defgeneric-simple ((obj string))
  "A method specializing test-defgeneric-simple for strings."
  (length obj))

(defmethod test-defgeneric-no-doc ((data number))
  ;; No docstring for this method
  (* data data))

(defmethod (setf data-of) ((new-value integer) (obj test-class-for-setf-method))
  "A (setf ...) method for the 'data' slot of test-class-for-setf-method."
  (setf (slot-value obj 'data) new-value))

(defmethod test-method-eql-specializer ((item (eql :special-key)))
  "A method with an EQL specializer."
  (format nil "Received special key: ~S" item))

(defmethod test-method-qualifiers :before ((obj test-class-simple))
  "A :before method."
  (print "Before method for test-class-simple"))

(defmethod test-method-qualifiers :after ((obj test-class-simple))
  "An :after method."
  (print "After method for test-class-simple"))

(defmethod test-method-qualifiers :around ((obj test-class-simple))
  "An :around method."
  (print "Around method - start")
  (let ((result (call-next-method)))
    (print "Around method - end")
    result))

;;;;----------------------------------------------------------------------------
;;;; DEFINE-CONDITION
;;;;----------------------------------------------------------------------------

(define-condition test-condition-simple (error)
  ((error-code :initarg :code :reader condition-error-code :initform "UNKNOWN")
   (error-message :initarg :message :reader condition-error-message :initform "An error occurred."))
  (:report (lambda (condition stream)
             (format stream "Test Condition [~A]: ~A"
                     (condition-error-code condition)
                     (condition-error-message condition))))
  (:documentation "A simple custom condition definition."))

(define-condition test-condition-no-doc (warning)
  ((warning-type :initarg :type :reader warning-type)))

(define-condition test-condition-inheritance (test-condition-simple)
  ((additional-info :initarg :info :reader condition-additional-info))
  (:documentation "A condition that inherits from test-condition-simple."))

;;;;----------------------------------------------------------------------------
;;;; DEFTYPE
;;;;----------------------------------------------------------------------------

(deftype test-deftype-simple ()
  "A simple custom type definition for positive integers."
  `(integer 0 *))

(deftype test-deftype-params (min max)
  "A custom type definition with parameters, defining a range."
  `(integer ,min ,max))

(deftype test-deftype-no-doc ()
  `string)

;;;;----------------------------------------------------------------------------
;;;; DEFSETF
;;;;----------------------------------------------------------------------------

;; Short form of defsetf
(defun test-access-my-value (obj) (gethash 'my-key obj))
(defun test-set-my-value (obj new-val) (setf (gethash 'my-key obj) new-val))
(defsetf test-access-my-value test-set-my-value
  "Docstring for short form defsetf for test-access-my-value.")

;; Long form of defsetf
(defun test-get-nth-char (s n) (char s n))
(defsetf test-get-nth-char (s n) (new-char)
  "Sets the Nth character of string S to NEW-CHAR."
  `(setf (char (the string ,s) (the fixnum ,n)) (the character ,new-char)))

;;;;----------------------------------------------------------------------------
;;;; DEFINE-SYMBOL-MACRO
;;;;----------------------------------------------------------------------------

(define-symbol-macro test-symbol-macro-simple *test-defparameter-simple*)
;; Docstring removed as it's not standard for define-symbol-macro:
;; "A simple symbol macro aliasing *test-defparameter-simple*."

;;;;----------------------------------------------------------------------------
;;;; MISCELLANEOUS / TOP-LEVEL FORMS
;;;;----------------------------------------------------------------------------

;; A top-level progn with some forms inside
(progn
  (defvar *inside-progn-var* 1)
  (defun func-inside-progn () *inside-progn-var*))

;; A top-level function call (might be ignored or noted by analyzer)
(test-defun-simple)

;; An unused function, potentially for testing dead code detection if supported.
(defun test-defun-unused ()
  "This function is defined but not called within this test file."
  "unused result")

;;;;----------------------------------------------------------------------------
;;;; Examples from original file that might need specific testing or verification
;;;;----------------------------------------------------------------------------

;; A function that returns a string, but it's a return value not a docstring.
;; (Covered by test-defun-returns-string-not-docstring)

;; A method without a docstring, where a string literal is a return value.
;; (Covered by test-defgeneric-no-doc method for number)

;; An unused parameter *esh* from the original file (removed as it was just a placeholder)

;;;; End of test code
