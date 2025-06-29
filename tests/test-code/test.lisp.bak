;;; tests/test-code/test.lisp
;;;
;;; This file contains a variety of Lisp forms intended for testing the
;;; capabilities of the cl-naive-code-analyzer. It includes definitions
;;; for parameters, functions, classes, structs, conditions, macros,
;;; generic functions, and methods, with various features like docstrings,
;;; initforms, and different lambda list keywords.
;;;
;;; TODO: Add examples of more complex macro definitions if macro analysis is a key feature.
;;; TODO: Include forms that might be tricky to parse or analyze, such as those
;;;       involving reader macros extensively, or complex nested structures.
;;; TODO: Add more comments explaining the purpose of specific test forms if unclear.

(in-package :test)

;;;; Some code to test with

;; A simple defparameter.
;; TODO: This parameter `*esh*` is defined but not used. Consider using it or removing it.
(defparameter *esh* nil
  "An example defparameter, currently unused in this test file.")

;; A global variable with a docstring.
(defparameter *global-var* 42
  "A global variable used elsewhere in test functions.")

;; A simple function definition.
(defun default-name ()
  "Returns a default name string."
  "Anonymous")

;; Another simple function.
(defun compute-age (n)
  "Computes an age based on a name (dummy logic using length)."
  (length n))

;; Function used as a default initform.
(defun default-x ()
  "Default x coordinate for a point."
  5)

;; Function used as a default initform, taking an argument.
(defun generate-y (x)
  "Generates y coordinate based on x."
  (+ x 3))

;; Function used as a default initform.
(defun default-age ()
  "Default age for a person."
  30)

;; A class definition with various initforms including symbols and function references.
(defclass human ()
  ((name :initarg :name :accessor person-name :initform 'default-name ; Symbol as initform
         :documentation "The name of the human.")
   (age :initarg :age :accessor person-age :initform #'default-age   ; Function reference as initform
        :documentation "The age of the human.")
   (id :initform (lambda () (random 10000)) ; Inline lambda as initform
       :documentation "A randomly generated ID for the human.")
   (nick :initform "Anonymous" ; String literal as initform
         :documentation "A nickname for the human.")
   (height :initform 180 ; Number literal as initform
           :documentation "Height of the human in cm."))
  (:documentation "Represents a human with name, age, and other attributes."))

;; Another simple class for testing (setf foo) method.
(defclass thing ()
  ((foo :accessor foo :initform 0
        :documentation "A slot in the 'thing' class."))
  (:documentation "A generic 'thing' class, primarily for testing slot accessors and methods."))

;; A (setf foo) method for the 'thing' class.
(defmethod (setf foo) ((val number) (target thing))
  "Sets the 'foo' slot of a 'thing' object to a numeric value."
  (setf (slot-value target 'foo) val))

;; A class definition inheriting from 'human', with its own slots and docstring.
(defclass person (human)
  ((id :initarg :id ; Changed from :name to :id to avoid confusion with human's name
       :accessor person-id ; Changed accessor name
       :initform nil ; Simple nil initform
       :documentation "A specific ID for the person, potentially overriding human's random ID if provided."))
  (:documentation "A simple person class, inheriting from human."))

;; A struct definition with function-based default values for slots.
(defstruct (point (:constructor make-point (x y)))
  "Represents a point with x and y coordinates."
  (x (default-x) :type number :read-only t) ; Slot x defaults to (default-x)
  (y (generate-y x) :type number :read-only t)) ; Slot y defaults to (generate-y x), x refers to the x slot of the same struct.

;; A condition definition for testing.
(define-condition custom-error (error)
  ((code :initarg :code :reader custom-error-code :initform #'equalp
         :documentation "An error code, defaults to #'equalp (example).")
   (reason :initarg :reason :reader custom-error-reason :initform "Bad input"
           :documentation "A human-readable reason for the error."))
  (:report (lambda (condition stream)
             (format stream "Custom error (Code: ~S): ~A"
                     (custom-error-code condition)
                     (custom-error-reason condition))))
  (:documentation "A custom error condition for testing define-condition."))

;; A function using LET for local bindings.
(defun use-let ()
  "Demonstrates local binding via LET and string formatting."
  (let ((local-greeting (format nil "Hello")))
    local-greeting))

;; A function using MULTIPLE-VALUE-BIND.
(defun multi-bind-fn ()
  "Tests MULTIPLE-VALUE-BIND handling by returning multiple values."
  (multiple-value-bind (val1 val2) (values 1 2)
    (format t "Values from multi-bind-fn: ~A, ~A~%" val1 val2) ; Side effect for observation
    (list val1 val2)))

;;; A function with required parameters.
(defun with-args (arg1 arg2)
  "Takes two arguments, creates a list, and formats them into a string."
  (list arg1 arg2) ; This list creation is a side effect, result is from format.
  (format nil "~A-~A" arg1 arg2))

;; A wrapper function that calls other defined functions and uses classes/structs.
;; This helps test analysis of function calls and object instantiations.
(defun wrapper ()
  "Calls other functions, uses a macro, instantiates a class and a struct."
  (use-let)
  (multi-bind-fn)
  (with-args "alpha" "beta") ; Changed args for clarity
  (macro-user)
  (let ((p (make-instance 'person :name "Phil" :age 40 :id (random 100000)))
        (pt (make-point 10 20)))
    (list (person-name p)
          (person-age p)
          (person-id p) ; Added person-id
          (point-x pt)
          (point-y pt))))

;;; Generic function definition.
(defgeneric process (x)
  (:documentation "A generic function to process different types of input X."))

;; Method specializing 'process' for integers.
(defmethod process ((x integer))
  "Processes an integer by adding 10 to it."
  (+ x 10))

;;;; Some more file comments
;;; for testing analysis of comments or file structure.

;; A method without a docstring.
(defmethod no-doc-string-method ((y (eql :y)))
  "yes") ; This string is a return value, not a docstring.
;; TODO: Verify how the analyzer distinguishes return values from docstrings in methods.

(no-doc-string-method :y) ; A call to the method.

;; A function without a docstring.
(defun no-doc-string-function ()
  "yes") ; This string is a return value.
;; TODO: Verify analyzer behavior for functions like this.

(no-doc-string-function) ; A call to the function.

;; Method specializing 'process' for strings.
(defmethod process ((x string))
  "Processes a string by concatenating it with a prefix."
  (concatenate 'string "Processed: " x))

;; Function referencing the global variable *global-var*.
(defun ref-global ()
  "Reads the global variable *global-var* and adds 1 to its value."
  (1+ *global-var*))

;; A simple macro definition with one argument.
(defmacro my-macro (x)
  "A simple test macro that wraps its argument in a list with itself."
  `(list ,x ,x))

;; Function that uses the defined macro 'my-macro'.
(defun macro-user ()
  "Invokes the test macro 'my-macro' with the argument 'foo."
  (my-macro 'foo))

;; A (setf ...) method specialization for person-name.
(defmethod (setf person-name) ((new-name string) (obj person))
  "Sets the name slot of a person object."
  ;; TODO: person class does not have a 'name' slot directly, it's inherited from 'human'.
  ;;       This should try to set 'name' on the 'human' part of 'person'.
  ;;       Slot accessors like `(slot-value obj 'name)` should work if 'name' is the slot name.
  ;;       `person-name` is the accessor for the 'name' slot in 'human'.
  (setf (slot-value obj 'name) new-name))

;; A class definition with a variety of initform types.
(defclass mixed-initforms ()
  ((literal-number :initform 42)
   (literal-string :initform "hello")
   (quoted-symbol :initform 'foo)
   (function-ref :initform #'default-age) ; Reference to an existing function
   (inline-lambda :initform (lambda (x) (+ x 1)) ; An anonymous lambda function
                  :documentation "Slot initialized with an inline lambda."))
  (:documentation "Class to test analysis of different initform types."))

;; An unused function, potentially for testing dead code detection.
(defun unused-function ()
  "This function is defined but not called within this test file."
  "unused result")

;; Defparameter binding a lambda function to a variable.
(defparameter *inc-fn* (lambda (n) (+ n 1))
  "A global parameter bound to a lambda function that increments its argument.")

;; Function with optional arguments, one defaulting to a literal, another to a global variable.
(defun test-optional (&optional (x 42) (y *global-var*))
  "Tests optional arguments with different default value types."
  (+ x y))

;; Function with keyword arguments.
(defun test-key (&key (foo "bar") (baz 100))
  "Tests keyword arguments with default values."
  (list foo baz))

;; Function with a mix of required, optional, keyword, and aux arguments.
(defun test-mixed (a &optional (b 1) &key (k 10) &aux (tmp 20))
  "Tests a mixed lambda list: required, optional, keyword, and aux variables."
  (+ a b k tmp))

;; Function with a &rest argument.
(defun test-rest (x &rest args)
  "Tests &rest arguments, collecting them into a list."
  (cons x args))

;; Another method definition for testing.
(defmethod test-method ((x integer) (y (eql :foo)))
  "A test method with integer and eql-specialized parameters."
  (+ x 1))
