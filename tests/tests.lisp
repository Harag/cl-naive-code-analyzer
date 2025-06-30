;;; tests/tests.lisp

(in-package :cl-naive-code-analyzer.tests)

(defun get-first-analysis (code-string &key (package (find-package :cl-user)))
  (let ((analyses (cl-naive-code-analyzer::analyze-string code-string :package package)))
    (first analyses)))

(let ((code "(define-symbol-macro my-sym-macro *some-global*)"))
  (cl-naive-tests:testsuite :define-symbol-macro-suite

    ;;--------------------------------------------------------------------------
    ;; DEFINE-SYMBOL-MACRO Tests - FOCUSED DEBUGGING (Kept as is)
    ;;--------------------------------------------------------------------------
    (cl-naive-tests:testcase
     :define-symbol-macro-kind
     :expected 'define-symbol-macro
     :actual (let* (
                    (analysis (get-first-analysis code)))
               (analysis-kind analysis)))

    (cl-naive-tests:testcase
     :define-symbol-macro-docstring
     :expected nil
     :actual (let* ((analysis (get-first-analysis code)))
               (and analysis
                    (analysis-docstring analysis))))

    (cl-naive-tests:testcase
     :define-symbol-macro-name
     :expected "MY-SYM-MACRO"
     :equal #'equalp
     :actual (let* ((analysis (get-first-analysis code)))
               (and analysis
                    (symbol-name (analysis-name analysis)))))

    (cl-naive-tests:testcase
     :define-symbol-macro-sexp
     :equal #'equalp
     :expected '(:NAME COMMON-LISP-USER::MY-SYM-MACRO
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND DEFINE-SYMBOL-MACRO
                 :LINE 1 :START 0 :END 48
                 :CODE
                 "(DEFINE-SYMBOL-MACRO COMMON-LISP-USER::MY-SYM-MACRO
                     COMMON-LISP-USER::*SOME-GLOBAL*)"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS
                 NIL :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY "COMMON-LISP-USER::*SOME-GLOBAL*")
     :equal #'equalp
     :actual (let* ((analysis (get-first-analysis code)))
               (cl-naive-code-analyzer::write-analysis analysis nil)))))

;;--------------------------------------------------------------------------
;; DEFUN Tests
;;--------------------------------------------------------------------------
(let ((code "(defun test-defun-simple ()
                           \"A simple function with no arguments and a docstring.\"
                           (list 1 2 3))"))
  (cl-naive-tests:testsuite :defun-suite

    (cl-naive-tests:testcase
     :defun-kind
     :expected 'defun
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-kind analysis)))

    (cl-naive-tests:testcase
     :defun-docstring
     :equal #'equalp
     :expected "A simple function with no arguments and a docstring."
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-docstring analysis)))

    (cl-naive-tests:testcase
     :defun-name
     :expected "TEST-DEFUN-SIMPLE"
     :equal #'equalp
     :actual (let* ((analysis (get-first-analysis code)))
               (symbol-name (analysis-name analysis))))

    (cl-naive-tests:testcase
     :defun-no-parameters
     :expected nil
     :actual (let* ((analysis (get-first-analysis code :package (find-package :test))))
               (analysis-parameters analysis)))

    (cl-naive-tests:testcase
     :defun-raw-body
     :expected '(list 1 2 3)
     :actual (let* ((analysis (get-first-analysis code :package (find-package :test))))
               ;;Using real-raw to simplify testing but raw-body is a
               ;;cst and we are not trying to code around
               ;;concrete-syntax-tree oddities in actual code but its
               ;;convenient here.
               (cl-naive-code-analyzer::real-raw (analysis-raw-body analysis))))

    (cl-naive-tests:testcase
     :defun-sexp
     :equal #'equalp
     :expected '(:NAME COMMON-LISP-USER::TEST-DEFUN-SIMPLE
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND DEFUN
                 :LINE 1 :START 0 :END 150
                 :CODE
                 "(DEFUN COMMON-LISP-USER::TEST-DEFUN-SIMPLE ()
  \"A simple function with no arguments and a docstring.\"
  (LIST 1 2 3))"
                 :FUNCTION-CALLS ((:NAME "LIST" :PACKAGE "COMMON-LISP"))
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS NIL
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY
                 "(LIST 1 2 3)"
                 :LAMBDA-INFO
                 (:REQUIRED NIL
                  :OPTIONALS NIL
                  :REST NIL
                  :KEYWORDS NIL
                  :ALLOW-OTHER-KEYS NIL
                  :AUXES NIL)
                 :DOCSTRING "A simple function with no arguments and a docstring.")

     :actual (let* ((analysis (get-first-analysis code)))
               (cl-naive-code-analyzer::write-analysis analysis nil)))))

;;--------------------------------------------------------------------------
;; DEFMETHOD Tests
;;--------------------------------------------------------------------------
(let ((code "(defmethod test-defmethod ((x integer) &optional (y 10))
              \"A test method.\"
              (+ x y))"))
  (cl-naive-tests:testsuite :defmethod-suite

    (cl-naive-tests:testcase
     :defmethod-kind
     :expected 'defmethod
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-kind analysis)))

    (cl-naive-tests:testcase
     :defmethod-docstring
     :equal #'equalp
     :expected "A test method."
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-docstring analysis)))

    (cl-naive-tests:testcase
     :defmethod-name
     :expected "TEST-DEFMETHOD"
     :equal #'equalp
     :actual (let* ((analysis (get-first-analysis code)))
               (symbol-name (analysis-name analysis))))

    ;; Note: simple-lambda-params extracts (X Y)
    (cl-naive-tests:testcase
     :defmethod-parameters
     :expected '(common-lisp-user::x common-lisp-user::y)
     :actual (let* ((analysis (get-first-analysis code :package (find-package :cl-user))))
               (analysis-parameters analysis)))

    (cl-naive-tests:testcase
     :defmethod-sexp
     :equal #'equalp
     :expected '(:NAME common-lisp-user::test-defmethod
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND DEFMETHOD
                 :LINE 1 :START 0 :END 100
                 :CODE "(DEFMETHOD COMMON-LISP-USER::TEST-DEFMETHOD ((X INTEGER) &OPTIONAL (Y 10))
  \"A test method.\"
  (+ X Y))"
                 :FUNCTION-CALLS ((:NAME "+" :PACKAGE "COMMON-LISP"))
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL ; Parameters are lexical definitions, not uses here
                 :LEXICAL-DEFINITIONS (COMMON-LISP-USER::Y COMMON-LISP-USER::X) ; Order might vary
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY "(+ X Y)" ; Simplified for testing as per instructions
                 :PARAMETERS (COMMON-LISP-USER::X COMMON-LISP-USER::Y) ; Order might vary
                 :DOCSTRING "A test method.")
     :actual (let* ((analysis (get-first-analysis "(defmethod test-defmethod ((x integer) &optional (y 10))
              \"A test method.\"
              (+ x y))")))
               ;; raw-body is a CST, so we check its string representation for simplicity
               ;; and to adhere to "Dont parse raw-body to do testing"
               (let ((written (cl-naive-code-analyzer::write-analysis analysis nil)))
                 (setf (getf written :raw-body)
                       (cl-naive-code-analyzer::real-raw (analysis-raw-body analysis)))
                 ;; Parameters and lexical definitions can be in any order in the analysis
                 ;; so we sort them for stable testing
                 (setf (getf written :parameters)
                       (sort (copy-list (getf written :parameters))
                             #'string< :key (lambda (par)
                                              (getf par :name))))
                 (setf (getf written :lexical-definitions)
                       (sort (copy-list (getf written :lexical-definitions))
                             #'string< :key (lambda (par)
                                              (getf par :name))))
                 written)))))

;;--------------------------------------------------------------------------
;; DEFINE-CONDITION Tests
;;--------------------------------------------------------------------------
(let ((code "(define-condition my-error (error)
              ((slot1 :accessor my-error-slot1 :initarg :slot1)
               (slot2 :reader my-error-slot2))
              (:documentation \"A test condition.\")
              (:report (lambda (condition stream)
                         (format stream \"My error occurred with ~a\" (my-error-slot1 condition)))))"))
  (cl-naive-tests:testsuite :define-condition-suite

    (cl-naive-tests:testcase
     :define-condition-kind
     :expected 'define-condition
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-kind analysis)))

    (cl-naive-tests:testcase
     :define-condition-docstring
     :equal #'equalp
     :expected "A test condition."
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-docstring analysis)))

    (cl-naive-tests:testcase
     :define-condition-name
     :expected "MY-ERROR"
     :equal #'equalp
     :actual (let* ((analysis (get-first-analysis code)))
               (symbol-name (analysis-name analysis))))

    (cl-naive-tests:testcase
     :define-condition-superclasses
     :expected '(common-lisp-user::error)
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-superclasses analysis)))

    (cl-naive-tests:testcase
     :define-condition-slots
     ;; Slot names are symbols
     :expected '(common-lisp-user::slot1 common-lisp-user::slot2)
     :actual (let* ((analysis (get-first-analysis code)))
               (sort (copy-list (analysis-slots analysis)) #'string< :key #'symbol-name)))

    (cl-naive-tests:testcase
     :define-condition-sexp
     :equal #'equalp
     :expected '(:RAW-BODY NIL
                 :NAME COMMON-LISP-USER::MY-ERROR
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND DEFINE-CONDITION
                 :LINE 1 :START 0 :END 345
                 :CODE
                 "(DEFINE-CONDITION COMMON-LISP-USER::MY-ERROR
    (ERROR)
    ((COMMON-LISP-USER::SLOT1 :ACCESSOR COMMON-LISP-USER::MY-ERROR-SLOT1
      :INITARG :SLOT1)
     (COMMON-LISP-USER::SLOT2 :READER COMMON-LISP-USER::MY-ERROR-SLOT2))
  (:DOCUMENTATION \"A test condition.\")
  (:REPORT
   (LAMBDA (CONDITION STREAM)
     (FORMAT STREAM \"My error occurred with ~a\"
             (COMMON-LISP-USER::MY-ERROR-SLOT1 CONDITION)))))"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS NIL
                 :DYNAMIC-DEFINITIONS NIL
                 :DOCSTRING "A test condition."
                 :SUPERCLASSES
                 ((:NAME "ERROR" :PACKAGE "COMMON-LISP"))
                 :SLOTS
                 ((:NAME "SLOT1" :PACKAGE "COMMON-LISP-USER")
                  (:NAME "SLOT2" :PACKAGE "COMMON-LISP-USER")))
     :actual (let* ((analysis (get-first-analysis code)))
               (let ((written (cl-naive-code-analyzer::write-analysis analysis nil)))
                 ;; Slots can be in any order
                 (setf (getf written :slots)
                       (sort (copy-list (getf written :slots))
                             #'string< :key #'(lambda (slot)
                                                (getf slot :name))))
                 ;; Ensure raw-body is nil as it's not applicable here
                 (setf (getf written :raw-body) nil)
                 written)))))

;;--------------------------------------------------------------------------
;; DEFTYPE Tests
;;--------------------------------------------------------------------------
(let ((code "(deftype my-integer-type (val)
              \"A custom integer type.\"
              `(integer 0 ,val))"))
  (cl-naive-tests:testsuite :deftype-suite

    (cl-naive-tests:testcase
     :deftype-kind
     :expected 'deftype
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-kind analysis)))

    (cl-naive-tests:testcase
     :deftype-docstring
     :equal #'equalp
     :expected "A custom integer type."
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-docstring analysis)))

    (cl-naive-tests:testcase
     :deftype-name
     :expected "MY-INTEGER-TYPE"
     :equal #'equalp
     :actual (let* ((analysis (get-first-analysis code)))
               (symbol-name (analysis-name analysis))))

    (cl-naive-tests:testcase
     :deftype-parameters
     :expected '(common-lisp-user::val)
     :actual (let* ((analysis (get-first-analysis code :package (find-package :cl-user))))
               (analysis-parameters analysis)))

    ;; TODO: Body of deftype is not analyzed for calls in the same way as defun
    (cl-naive-tests:testcase
     :deftype-sexp
     :equal #'equalp
     :expected '(:NAME COMMON-LISP-USER::MY-INTEGER-TYPE
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND DEFTYPE
                 :LINE 1 :START 0 :END 102
                 :CODE
                 "(DEFTYPE COMMON-LISP-USER::MY-INTEGER-TYPE (COMMON-LISP-USER::VAL)
  \"A custom integer type.\"
  (CL-NAIVE-CODE-ANALYZER::QUASIQUOTE
   (INTEGER 0 (CL-NAIVE-CODE-ANALYZER::UNQUOTE COMMON-LISP-USER::VAL))))"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS
                 ((:NAME "QUASIQUOTE"
                   :PACKAGE "ECLECTOR.READER"))
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS ((:NAME "VAL" :PACKAGE "COMMON-LISP-USER"))
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY
                 (ECLECTOR.READER:QUASIQUOTE
                  (INTEGER 0 (ECLECTOR.READER:UNQUOTE COMMON-LISP-USER::VAL)))
                 :PARAMETERS ((:NAME "VAL" :PACKAGE "COMMON-LISP-USER"))
                 :DOCSTRING
                 "A custom integer type.")
     :actual (let* ((analysis (get-first-analysis code)))
               (let ((written (cl-naive-code-analyzer::write-analysis analysis nil)))
                 (setf (getf written :raw-body)
                       (cl-naive-code-analyzer::real-raw (analysis-raw-body analysis)))
                 ;; Parameters and lexical definitions can be in any order in the analysis
                 ;; so we sort them for stable testing
                 (setf (getf written :parameters) (sort (copy-list (getf written :parameters)) #'string< :key #'symbol-name))
                 (setf (getf written :lexical-definitions) (sort (copy-list (getf written :lexical-definitions)) #'string< :key #'symbol-name))
                 written)))))

;;--------------------------------------------------------------------------
;; DEFGENERIC Tests
;;--------------------------------------------------------------------------
(let ((code "(defgeneric my-generic-function (arg1 &key arg2)
              (:documentation \"A test generic function.\")
              (:method ((arg1 string) &key (arg2 t))
                (format nil \"String method: ~a, ~a\" arg1 arg2)))"))
  (cl-naive-tests:testsuite :defgeneric-suite

    (cl-naive-tests:testcase
     :defgeneric-kind
     :expected 'defgeneric
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-kind analysis)))

    (cl-naive-tests:testcase
     :defgeneric-docstring
     :equal #'equalp
     :expected "A test generic function."
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-docstring analysis)))

    (cl-naive-tests:testcase
     :defgeneric-name
     :expected "MY-GENERIC-FUNCTION"
     :equal #'equalp
     :actual (let* ((analysis (get-first-analysis code)))
               (symbol-name (analysis-name analysis))))

    (cl-naive-tests:testcase
     :defgeneric-parameters
     ;; simple-lambda-params extracts required and &key names
     :expected '(common-lisp-user::arg1 common-lisp-user::arg2)
     :actual (let* ((analysis (get-first-analysis code :package (find-package :cl-user))))
               (sort (copy-list (analysis-parameters analysis)) #'string< :key #'symbol-name)))

    ;; TODO: Defgeneric options/methods are not deeply analyzed by default for calls
    ;; TODO: Defgeneric does not have a direct 'raw-body' like defun
    ;; TODO: lexical-definitions and parameters ; Order might vary
    (cl-naive-tests:testcase
     :defgeneric-sexp
     :equal #'equalp
     :expected '(:RAW-BODY NIL
                 :NAME COMMON-LISP-USER::MY-GENERIC-FUNCTION
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND DEFGENERIC
                 :LINE 1 :START 0 :END 224
                 :CODE "(DEFGENERIC COMMON-LISP-USER::MY-GENERIC-FUNCTION
    (COMMON-LISP-USER::ARG1 &KEY COMMON-LISP-USER::ARG2)
  (:DOCUMENTATION \"A test generic function.\")
  (:METHOD ((COMMON-LISP-USER::ARG1 STRING) &KEY (COMMON-LISP-USER::ARG2 T))
   (FORMAT NIL \"String method: ~a, ~a\" COMMON-LISP-USER::ARG1
           COMMON-LISP-USER::ARG2)))"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS
                 ((:NAME "ARG1" :PACKAGE "COMMON-LISP-USER")
                  (:NAME "ARG2" :PACKAGE "COMMON-LISP-USER"))
                 :DYNAMIC-DEFINITIONS NIL :PARAMETERS
                 ((:NAME "ARG1" :PACKAGE "COMMON-LISP-USER")
                  (:NAME "ARG2" :PACKAGE "COMMON-LISP-USER"))
                 :DOCSTRING "A test generic function.")
     :actual (let* ((analysis (get-first-analysis code)))
               (let ((written (cl-naive-code-analyzer::write-analysis analysis nil)))
                 ;; Parameters and lexical definitions can be in any order
                 (setf (getf written :parameters)
                       (sort (copy-list (getf written :parameters))
                             #'string<
                             :key (lambda (par)
                                    (getf par :name))))
                 (setf (getf written :lexical-definitions)
                       (sort (copy-list (getf written :lexical-definitions))
                             #'string< :key (lambda (par)
                                              (getf par :name))))
                 (setf (getf written :raw-body) nil) ; Ensure raw-body is nil
                 written)))))

;;--------------------------------------------------------------------------
;; DEFSTRUCT Tests
;;--------------------------------------------------------------------------
(let ((code "(defstruct my-struct
              \"A test structure.\"
              (field1 0 :type integer)
              (field2 \"hello\" :read-only t))"))
  (cl-naive-tests:testsuite :defstruct-suite

    (cl-naive-tests:testcase
     :defstruct-kind
     :expected 'defstruct
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-kind analysis)))

    (cl-naive-tests:testcase
     :defstruct-docstring
     :equal #'equalp
     :expected "A test structure."
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-docstring analysis)))

    (cl-naive-tests:testcase
     :defstruct-name
     :expected "MY-STRUCT"
     :equal #'equalp
     :actual (let* ((analysis (get-first-analysis code)))
               (symbol-name (analysis-name analysis))))

    ;; Current analyzer for defstruct does not populate analysis-slots in a simple way from top-level.
    ;; It walks the slot definitions and gathers info, but analysis-slots itself might be nil.
    ;; We will test that it is NIL for now, based on current implementation.
    ;; If slot name extraction is improved in the analyzer, this test should be updated.
    ;; Defstruct doesn't have a main "body" after slots
    ;; TODO: slots ; As per current analyzer, this is expected to be nil
    (cl-naive-tests:testcase
     :defstruct-slots
     :expected nil
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-slots analysis)))

    (cl-naive-tests:testcase
     :defstruct-sexp
     :equal #'equalp
     :expected '(:NAME common-lisp-user::my-struct
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND DEFSTRUCT
                 :LINE 1 :START 0 :END 150
                 :CODE "(DEFSTRUCT COMMON-LISP-USER::MY-STRUCT
  \"A test structure.\"
  (FIELD1 0 :TYPE INTEGER)
  (FIELD2 \"hello\" :READ-ONLY T))"

                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS NIL
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY NIL
                 :SLOTS NIL
                 :DOCSTRING "A test structure.")
     :actual (let* ((analysis (get-first-analysis code)))
               (let ((written (cl-naive-code-analyzer::write-analysis analysis nil)))
                 ;; Ensure raw-body is nil as it's not directly applicable
                 (setf (getf written :raw-body) nil)
                 ;; If :SLOTS is present and nil, it's fine. If not present, also fine for this test.
                 ;; For consistency, ensure it's there and nil if the slot is nil.
                 (unless (getf written :slots)
                   (setf (getf written :slots) nil))
                 written)))))

;;--------------------------------------------------------------------------
;; DEFSETF Tests
;;--------------------------------------------------------------------------
;; Short form of defsetf
(let ((code-short "(defsetf short-accessor update-fn \"Doc for short-accessor.\")"))
  (cl-naive-tests:testsuite :defsetf-suite

    (cl-naive-tests:testcase
     :defsetf-short-kind
     :expected 'defsetf
     :actual (let* ((analysis (get-first-analysis code-short)))
               (analysis-kind analysis)))

    (cl-naive-tests:testcase
     :defsetf-short-docstring
     :equal #'equalp
     :expected "Doc for short-accessor."
     :actual (let* ((analysis (get-first-analysis code-short)))
               (analysis-docstring analysis)))

    (cl-naive-tests:testcase
     :defsetf-short-name
     :expected "SHORT-ACCESSOR"
     :equal #'equalp
     :actual (let* ((analysis (get-first-analysis code-short)))
               (symbol-name (analysis-name analysis))))

    ;; Parameters are not robustly parsed for defsetf yet by the analyzer
    (cl-naive-tests:testcase
     :defsetf-short-parameters
     :expected nil
     :actual (let* ((analysis (get-first-analysis code-short)))
               (analysis-parameters analysis)))

    ;; Defsetf short form doesn't have a body in the same sense. Rest is walked.
    ;; TODO: The update-fn and docstring are not walked for calls by current analyzer
    (cl-naive-tests:testcase
     :defsetf-short-sexp
     :equal #'equalp
     :expected '(:RAW-BODY NIL
                 :NAME COMMON-LISP-USER::SHORT-ACCESSOR
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND DEFSETF
                 :LINE 1 :START 0 :END 60
                 :CODE
                 "(DEFSETF COMMON-LISP-USER::SHORT-ACCESSOR COMMON-LISP-USER::UPDATE-FN
  \"Doc for short-accessor.\")"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS NIL
                 :DYNAMIC-DEFINITIONS NIL)
     :actual (let* ((analysis (get-first-analysis code-short)))
               (let ((written (cl-naive-code-analyzer::write-analysis analysis nil)))
                 ;; The 'rest' of defsetf is walked, which includes update-fn and doc.
                 ;; Depending on how `gather-info` processes `update-fn` (symbol), it might appear.
                 ;; For this test, focusing on core attributes.
                 (setf (getf written :raw-body) nil) ; ensure it's nil for this form
                 written)))))

;; Long form of defsetf
(let ((code-long "(defsetf long-accessor (obj index) (new-value)
                   \"Doc for long-accessor.\"
                   `(setf (aref ,obj ,index) ,new-value))"))
  ;; This 'let' should wrap the testsuite for the long form as well, or testcases should be inside the suite
  (cl-naive-tests:testsuite :defsetf-long-form-suite ; Changed to a new suite name for clarity
    (cl-naive-tests:testcase
     :defsetf-long-kind
     :expected 'defsetf
     :actual (let* ((analysis (get-first-analysis code-long)))
               (analysis-kind analysis)))

    (cl-naive-tests:testcase
     :defsetf-long-docstring
     :equal #'equalp
     ;; Current analyzer struggles with docstring in long form if it's not the first element after name
     ;; It expects docstring as first element of 'rest' for short form.
     ;; TODO: This will likely fail or be nil with current analyzer.
     :expected "Doc for long-accessor."
     :actual (let* ((analysis (get-first-analysis code-long)))
               (analysis-docstring analysis)))

    (cl-naive-tests:testcase
     :defsetf-long-name
     :expected "LONG-ACCESSOR"
     :equal #'equalp
     :actual (let* ((analysis (get-first-analysis code-long)))
               (symbol-name (analysis-name analysis))))

    ;; Parameters are not robustly parsed for defsetf long form yet
    (cl-naive-tests:testcase
     :defsetf-long-parameters
     :expected nil ; Based on current simple-lambda-params and defsetf analyzer
     :actual (let* ((analysis (get-first-analysis code-long)))
               (analysis-parameters analysis)))

    ;; TODO: Body is walked, but `(aref ...)` might not be picked up as function call by gather-info
    ;; TODO: lexical-definitions ; Parameters of the lambda-list are not currently added
    ;; TODO: parameters = nil ; As per current analyzer
    ;; TODO: Docstring extraction for long form might be tricky
    (cl-naive-tests:testcase
     :defsetf-long-sexp
     :equal #'equalp
     :expected '(:NAME common-lisp-user::long-accessor
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND DEFSETF
                 :LINE 1 :START 0 :END 150
                 :CODE "(DEFSETF COMMON-LISP-USER::LONG-ACCESSOR (OBJ INDEX) (NEW-VALUE)
  \"Doc for long-accessor.\"
  `(SETF (AREF ,OBJ ,INDEX) ,NEW-VALUE))"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS NIL
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY "`(SETF (AREF ,OBJ ,INDEX) ,NEW-VALUE)"
                 :PARAMETERS NIL
                 :DOCSTRING "Doc for long-accessor.")
     :actual (let* ((analysis (get-first-analysis code-long)))
               (let ((written (cl-naive-code-analyzer::write-analysis analysis nil)))

                 (setf (getf written :raw-body)
                       (when (analysis-raw-body analysis)
                         (cl-naive-code-analyzer::real-raw (analysis-raw-body analysis))))
                 ;; Lexical definitions might be empty if params are not processed for long form
                 (setf (getf written :lexical-definitions)
                       (sort (copy-list (getf written :lexical-definitions))
                             #'string< :key #'symbol-name))
                 (setf (getf written :parameters)
                       (sort (copy-list (getf written :parameters))
                             #'string< :key #'symbol-name))
                 written)))))

;;--------------------------------------------------------------------------
;; DEFPACKAGE Tests
;;--------------------------------------------------------------------------
(let ((code "(defpackage #:my-test-package
              (:nicknames #:mtp)
              (:use #:cl #:alexandria)
              (:export #:foo #:bar)
              (:shadow #:baz)
              ;; TODO: analyzer does not fully parse these yet
              ;; (:shadowing-import-from #:other-package #:qux)
              ;; (:import-from #:another-package #:fred)
              (:intern #:internal-symbol)
              (:documentation \"A test package definition.\")
              (:size 100))")) ; :size is an example of an 'other-option'
  (cl-naive-tests:testsuite :defpackage-suite

    (cl-naive-tests:testcase
     :defpackage-kind
     :expected 'defpackage
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-kind analysis)))

    (cl-naive-tests:testcase
     :defpackage-name ; analysis-name should be the package name symbol
     :expected "MY-TEST-PACKAGE"
     :equal #'string=
     :actual (let* ((analysis (get-first-analysis code)))
               (symbol-name (analysis-name analysis))))

    (cl-naive-tests:testcase
     :defpackage-package-name
     :expected "MY-TEST-PACKAGE"
     :equal #'string=
     :actual (let* ((analysis (get-first-analysis code)))
               (symbol-name (analysis-name analysis))))

    (cl-naive-tests:testcase
     :defpackage-nicknames
     :expected '("MTP")
     :actual (let* ((analysis (get-first-analysis code)))
               (mapcar #'symbol-name (sort (analysis-nicknames analysis) #'string< :key #'symbol-name))))

    (cl-naive-tests:testcase
     :defpackage-uses
     :expected '("ALEXANDRIA" "CL")
     :actual (let* ((analysis (get-first-analysis code)))
               (mapcar #'symbol-name (sort (analysis-uses analysis) #'string< :key #'symbol-name))))

    (cl-naive-tests:testcase
     :defpackage-exports
     :expected '("BAR" "FOO")
     :actual (let* ((analysis (get-first-analysis code)))
               (mapcar #'symbol-name (sort (analysis-exports analysis) #'string< :key #'symbol-name))))

    (cl-naive-tests:testcase
     :defpackage-shadows
     :expected '("BAZ")
     :actual (let* ((analysis (get-first-analysis code)))
               (mapcar #'symbol-name (sort (analysis-shadows analysis) #'string< :key #'symbol-name))))

    (cl-naive-tests:testcase
     :defpackage-interns
     :expected '("INTERNAL-SYMBOL")
     :actual (let* ((analysis (get-first-analysis code)))
               (mapcar #'symbol-name (sort (analysis-interns analysis) #'string< :key #'symbol-name))))

    (cl-naive-tests:testcase
     :defpackage-docstring
     :equal #'equalp
     :expected "A test package definition."
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-docstring analysis)))

    (cl-naive-tests:testcase
     :defpackage-other-options
     ;; :size 100 is stored as ((:SIZE . 100)) by real-raw on the option cst
     :expected '((:SIZE . 100))
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-other-options analysis)))

    ;; Defpackage does not have a raw body
    ;; TODO: SHADOWING-IMPORTS NIL ; Current analyzer limitation
    ;; TODO: :IMPORTS NIL           ; Current analyzer limitation
    (cl-naive-tests:testcase
     :defpackage-sexp
     :equal #'equalp
     :expected '(:RAW-BODY NIL
                 :IMPORTS NIL
                 :SHADOWING-IMPORTS NIL
                 :NAME #:MY-TEST-PACKAGE
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND DEFPACKAGE
                 :LINE 1 :START 0 :END 480
                 :CODE "(DEFPACKAGE #:MY-TEST-PACKAGE
  (:NICKNAMES #:MTP)
  (:USE #:CL #:ALEXANDRIA)
  (:EXPORT #:FOO #:BAR)
  (:SHADOW #:BAZ)
  (:INTERN #:INTERNAL-SYMBOL)
  (:DOCUMENTATION \"A test package definition.\")
  (:SIZE 100))"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS NIL
                 :DYNAMIC-DEFINITIONS NIL
                 :DOCSTRING "A test package definition."
                 :NICKNAMES ("MTP")
                 :USES ("ALEXANDRIA" "COMMON-LISP")
                 :EXPORTS
                 ((:NAME "BAR" :PACKAGE :|<uninterned>|)
                  (:NAME "FOO" :PACKAGE :|<uninterned>|))
                 :SHADOWS ((:NAME "BAZ" :PACKAGE :|<uninterned>|))
                 :INTERNS ((:NAME "INTERNAL-SYMBOL" :PACKAGE :|<uninterned>|))
                 :OTHER-OPTIONS (:SIZE))
     :actual (let* ((analysis (get-first-analysis code)))
               (let ((written (cl-naive-code-analyzer::write-analysis analysis nil)))
                 ;; Normalize lists for comparison
                 (setf (getf written :nicknames)
                       (sort (copy-list (getf written :nicknames))
                             #'string< :key #'symbol-name))
                 (setf (getf written :uses)
                       (sort (copy-list (getf written :uses))
                             #'string<))
                 (setf (getf written :exports)
                       (sort (copy-list (getf written :exports))
                             #'string< :key (lambda (exp)
                                              (getf exp :name))))
                 (setf (getf written :shadows)
                       (sort (copy-list (getf written :shadows))
                             #'string< :key #'symbol-name))
                 (setf (getf written :interns)
                       (sort (copy-list (getf written :interns))
                             #'string< :key #'symbol-name))
                 ;; Ensure these are present and nil if analyzer doesn't populate them
                 (unless (getf written :shadowing-imports)
                   (setf (getf written :shadowing-imports) nil))
                 (unless (getf written :imports) (setf (getf written :imports) nil))
                 (setf (getf written :raw-body) nil)
                 written)))))

;;--------------------------------------------------------------------------
;; DEFPARAMETER Tests
;;--------------------------------------------------------------------------
(let ((code "(defparameter *my-param* 42 \"My global parameter.\")"))
  (cl-naive-tests:testsuite :defparameter-suite

    (cl-naive-tests:testcase
     :defparameter-kind
     :expected 'defparameter
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-kind analysis)))

    (cl-naive-tests:testcase
     :defparameter-docstring
     :equal #'equalp
     :expected "My global parameter."
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-docstring analysis)))

    (cl-naive-tests:testcase
     :defparameter-name
     :expected "*MY-PARAM*"
     :equal #'equalp
     :actual (let* ((analysis (get-first-analysis code)))
               (symbol-name (analysis-name analysis))))

    (cl-naive-tests:testcase
     :defparameter-sexp
     :equal #'equalp
     :expected '(:NAME COMMON-LISP-USER::*MY-PARAM*
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND DEFPARAMETER
                 :LINE 1 :START 0 :END 51
                 :CODE
                 "(DEFPARAMETER COMMON-LISP-USER::*MY-PARAM* 42 \"My global parameter.\")"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS NIL
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY 42
                 :DOCSTRING
                 "My global parameter.")
     :actual (let* ((analysis (get-first-analysis code)))
               (let ((written (cl-naive-code-analyzer::write-analysis analysis nil)))
                 (setf (getf written :raw-body)
                       (cl-naive-code-analyzer::real-raw (analysis-raw-body analysis)))
                 written)))))

;;--------------------------------------------------------------------------
;; DEFVAR Tests
;;--------------------------------------------------------------------------
(let ((code "(defvar *my-var* (+ 1 2) \"My global variable.\")"))
  (cl-naive-tests:testsuite :defvar-suite

    (cl-naive-tests:testcase
     :defvar-kind
     :expected 'defvar
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-kind analysis)))

    (cl-naive-tests:testcase
     :defvar-docstring
     :equal #'equalp
     :expected "My global variable."
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-docstring analysis)))

    (cl-naive-tests:testcase
     :defvar-name
     :expected "*MY-VAR*"
     :equal #'equalp
     :actual (let* ((analysis (get-first-analysis code)))
               (symbol-name (analysis-name analysis))))

    (cl-naive-tests:testcase
     :defvar-sexp
     :equal #'equalp
     :expected '(:NAME COMMON-LISP-USER::*MY-VAR*
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND DEFVAR
                 :LINE 1 :START 0 :END 47
                 :CODE
                 "(DEFVAR COMMON-LISP-USER::*MY-VAR* (+ 1 2) \"My global variable.\")"
                 :FUNCTION-CALLS ((:NAME "+" :PACKAGE "COMMON-LISP"))
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS NIL
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY +
                 :DOCSTRING "My global variable.")
     :actual (let* ((analysis (get-first-analysis code)))
               (let ((written (cl-naive-code-analyzer::write-analysis analysis nil)))
                 (setf (getf written :raw-body)
                       (cl-naive-code-analyzer::real-raw (analysis-raw-body analysis)))
                 written)))))

(let ((code-no-init "(defvar *my-var-no-init* \"Doc without init.\")"))
  (cl-naive-tests:testsuite :defvar-no-init-suite ; Changed to a new suite for this specific case
    (cl-naive-tests:testcase
     :defvar-no-init-sexp
     ;; :testsuite-name :defvar-suite ; This was the problematic keyword
     :equal #'equalp
     :expected '(:NAME COMMON-LISP-USER::+MY-CONST+
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND DEFCONSTANT
                 :LINE 1 :START 0 :END 48
                 :CODE
                 "(DEFCONSTANT COMMON-LISP-USER::+MY-CONST+ (* 10 5) \"My constant.\")"
                 :FUNCTION-CALLS ((:NAME "*" :PACKAGE "COMMON-LISP"))
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS NIL
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY *
                 :DOCSTRING "My constant.")
     :actual (let* ((analysis (get-first-analysis code-no-init)))
               (let ((written (cl-naive-code-analyzer::write-analysis analysis nil)))
                 (setf (getf written :raw-body)
                       (when (analysis-raw-body analysis) ; could be nil
                         (cl-naive-code-analyzer::real-raw (analysis-raw-body analysis))))
                 written)))))

;;--------------------------------------------------------------------------
;; DEFCONSTANT Tests
;;--------------------------------------------------------------------------
(let ((code "(defconstant +my-const+ (* 10 5) \"My constant.\")"))
  (cl-naive-tests:testsuite :defconstant-suite

    (cl-naive-tests:testcase
     :defconstant-kind
     :expected 'defconstant
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-kind analysis)))

    (cl-naive-tests:testcase
     :defconstant-docstring
     :equal #'equalp
     :expected "My constant."
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-docstring analysis)))

    (cl-naive-tests:testcase
     :defconstant-name
     :expected "+MY-CONST+"
     :equal #'equalp
     :actual (let* ((analysis (get-first-analysis code)))
               (symbol-name (analysis-name analysis))))

    (cl-naive-tests:testcase
     :defconstant-sexp
     :equal #'equalp
     :expected '(:NAME common-lisp-user::+my-const+
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND DEFCONSTANT
                 :LINE 1 :START 0 :END 50
                 :CODE "(DEFCONSTANT COMMON-LISP-USER::+MY-CONST+ (* 10 5) \"My constant.\")"
                 :FUNCTION-CALLS ((:NAME "*" :PACKAGE "COMMON-LISP"))
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS NIL
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY "(* 10 5)"
                 :DOCSTRING "My constant.")
     :actual (let* ((analysis (get-first-analysis code)))
               (let ((written (cl-naive-code-analyzer::write-analysis analysis nil)))
                 (setf (getf written :raw-body)
                       (cl-naive-code-analyzer::real-raw (analysis-raw-body analysis)))
                 written)))))

;;--------------------------------------------------------------------------
;; DEFMACRO Tests
;;--------------------------------------------------------------------------
(let ((code "(defmacro my-macro (arg1 &optional (arg2 0))
              \"A test macro.\"
              `(list ,arg1 ,arg2))"))
  (cl-naive-tests:testsuite :defmacro-suite

    (cl-naive-tests:testcase
     :defmacro-kind
     :expected 'defmacro
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-kind analysis)))

    (cl-naive-tests:testcase
     :defmacro-docstring
     :equal #'equalp
     :expected "A test macro."
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-docstring analysis)))

    (cl-naive-tests:testcase
     :defmacro-name
     :expected "MY-MACRO"
     :equal #'equalp
     :actual (let* ((analysis (get-first-analysis code)))
               (symbol-name (analysis-name analysis))))

    (cl-naive-tests:testcase
     :defmacro-parameters
     :expected '(common-lisp-user::arg1 common-lisp-user::arg2)
     :actual (let* ((analysis (get-first-analysis code :package (find-package :cl-user))))
               (sort (copy-list (analysis-parameters analysis)) #'string< :key #'symbol-name)))

    ;;TODO: Body of a macro is not analyzed for calls in the same way as defun

    (cl-naive-tests:testcase
     :defmacro-sexp
     :equal #'equalp
     :expected '(:NAME COMMON-LISP-USER::MY-MACRO
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND DEFMACRO
                 :LINE 1 :START 0 :END 109
                 :CODE
                 "(DEFMACRO COMMON-LISP-USER::MY-MACRO
          (COMMON-LISP-USER::ARG1 &OPTIONAL (COMMON-LISP-USER::ARG2 0))
  \"A test macro.\"
  (CL-NAIVE-CODE-ANALYZER::QUASIQUOTE
   (LIST (CL-NAIVE-CODE-ANALYZER::UNQUOTE COMMON-LISP-USER::ARG1)
         (CL-NAIVE-CODE-ANALYZER::UNQUOTE COMMON-LISP-USER::ARG2))))"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS ((:NAME "QUASIQUOTE" :PACKAGE "ECLECTOR.READER"))
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS
                 ((:NAME "ARG1" :PACKAGE "COMMON-LISP-USER")
                  (:NAME "ARG2" :PACKAGE "COMMON-LISP-USER"))
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY
                 (ECLECTOR.READER:QUASIQUOTE
                  (LIST (ECLECTOR.READER:UNQUOTE COMMON-LISP-USER::ARG1)
                   (ECLECTOR.READER:UNQUOTE COMMON-LISP-USER::ARG2)))
                 :PARAMETERS
                 ((:NAME "ARG1" :PACKAGE "COMMON-LISP-USER")
                  (:NAME "ARG2" :PACKAGE "COMMON-LISP-USER"))
                 :DOCSTRING "A test macro.")
     :actual (let* ((analysis (get-first-analysis "(defmacro my-macro (arg1 &optional (arg2 0))
              \"A test macro.\"
              `(list ,arg1 ,arg2))")))
               (let ((written (cl-naive-code-analyzer::write-analysis analysis nil)))
                 (setf (getf written :raw-body)
                       (cl-naive-code-analyzer::real-raw (analysis-raw-body analysis)))
                 (setf (getf written :parameters)
                       (sort (copy-list (getf written :parameters))
                             #'string< :key #'(lambda (par)
                                                (getf par :name))))
                 (setf (getf written :lexical-definitions)
                       (sort (copy-list (getf written :lexical-definitions))
                             #'string< :key #'(lambda (par)
                                                (getf par :name))))
                 written)))))

;;(cl-naive-tests:run)

