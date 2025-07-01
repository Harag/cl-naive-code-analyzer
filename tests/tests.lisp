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
     :actual (let* ((analysis (get-first-analysis code)))
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
     :expected '(:NAME (:NAME "MY-SYM-MACRO" :PACKAGE "COMMON-LISP-USER")
                 :PACKAGE
                 "COMMON-LISP-USER" :FILENAME NIL
                 :KIND (:NAME "DEFINE-SYMBOL-MACRO" :PACKAGE "COMMON-LISP")
                 :LINE 1 :START 0 :END 48
                 :CODE "(DEFINE-SYMBOL-MACRO COMMON-LISP-USER::MY-SYM-MACRO
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
     :expected '(:NAME (:NAME "TEST-DEFUN-SIMPLE" :PACKAGE "COMMON-LISP-USER")
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND (:NAME "DEFUN" :PACKAGE "COMMON-LISP")
                 :LINE 1 :START 0 :END 150
                 :CODE "(DEFUN COMMON-LISP-USER::TEST-DEFUN-SIMPLE ()
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
     :actual (let* ((analysis (get-first-analysis
                               code
                               :package (find-package :cl-user))))
               (analysis-parameters analysis)))

    (cl-naive-tests:testcase
     :defmethod-sexp
     :equal #'equalp
     :expected '(:NAME (:NAME "TEST-DEFMETHOD" :PACKAGE "COMMON-LISP-USER")
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND (:NAME "DEFMETHOD" :PACKAGE "COMMON-LISP")
                 :LINE 1 :START 0 :END 110
                 :CODE
                 "(DEFMETHOD COMMON-LISP-USER::TEST-DEFMETHOD
           ((COMMON-LISP-USER::X INTEGER) &OPTIONAL (COMMON-LISP-USER::Y 10))
  \"A test method.\"
  (+ COMMON-LISP-USER::X COMMON-LISP-USER::Y))"
                 :FUNCTION-CALLS ((:NAME "+" :PACKAGE "COMMON-LISP"))
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS
                 ((:NAME "Y" :PACKAGE "COMMON-LISP-USER")
                  (:NAME "X" :PACKAGE "COMMON-LISP-USER"))
                 :DYNAMIC-DEFINITIONS NIL :RAW-BODY
                 "(+ COMMON-LISP-USER::X COMMON-LISP-USER::Y)"
                 :PARAMETERS
                 ((:NAME "X" :PACKAGE "COMMON-LISP-USER")
                  (:NAME "Y" :PACKAGE "COMMON-LISP-USER"))
                 :DOCSTRING "A test method.")
     :actual (let* ((analysis (get-first-analysis code)))
               (cl-naive-code-analyzer::write-analysis analysis nil)))))

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
     :expected '(:NAME (:NAME "MY-ERROR" :PACKAGE "COMMON-LISP-USER")
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND (:NAME "DEFINE-CONDITION" :PACKAGE "COMMON-LISP")
                 :LINE 1 :START 0 :END 345
                 :CODE "(DEFINE-CONDITION COMMON-LISP-USER::MY-ERROR
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
               (cl-naive-code-analyzer::write-analysis analysis nil)))))

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
     :expected '(:NAME (:NAME "MY-INTEGER-TYPE" :PACKAGE "COMMON-LISP-USER")
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND (:NAME "DEFTYPE" :PACKAGE "COMMON-LISP")
                 :LINE 1 :START 0 :END 102
                 :CODE
                 "(DEFTYPE COMMON-LISP-USER::MY-INTEGER-TYPE (COMMON-LISP-USER::VAL)
  \"A custom integer type.\"
  (CL-NAIVE-CODE-ANALYZER::QUASIQUOTE
   (INTEGER 0 (CL-NAIVE-CODE-ANALYZER::UNQUOTE COMMON-LISP-USER::VAL))))"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS
                 ((:NAME "QUASIQUOTE" :PACKAGE "ECLECTOR.READER"))
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS ((:NAME "VAL" :PACKAGE "COMMON-LISP-USER"))
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY "(ECLECTOR.READER:QUASIQUOTE
 (INTEGER 0 (ECLECTOR.READER:UNQUOTE COMMON-LISP-USER::VAL)))"
                 :PARAMETERS ((:NAME "VAL" :PACKAGE "COMMON-LISP-USER"))
                 :DOCSTRING
                 "A custom integer type.")
     :actual (let* ((analysis (get-first-analysis code)))
               (cl-naive-code-analyzer::write-analysis analysis nil)))))

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
     :expected '(:NAME (:NAME "MY-GENERIC-FUNCTION" :PACKAGE "COMMON-LISP-USER")
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND (:NAME "DEFGENERIC" :PACKAGE "COMMON-LISP")
                 :LINE 1 :START 0 :END 224
                 :CODE
                 "(DEFGENERIC COMMON-LISP-USER::MY-GENERIC-FUNCTION
    (COMMON-LISP-USER::ARG1 &KEY COMMON-LISP-USER::ARG2)
  (:DOCUMENTATION \"A test generic function.\")
  (:METHOD ((COMMON-LISP-USER::ARG1 STRING) &KEY (COMMON-LISP-USER::ARG2 T))
   (FORMAT NIL \"String method: ~a, ~a\" COMMON-LISP-USER::ARG1
           COMMON-LISP-USER::ARG2)))"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS
                 ((:NAME "ARG2" :PACKAGE "COMMON-LISP-USER")
                  (:NAME "ARG1" :PACKAGE "COMMON-LISP-USER"))
                 :DYNAMIC-DEFINITIONS NIL :PARAMETERS
                 ((:NAME "ARG1" :PACKAGE "COMMON-LISP-USER")
                  (:NAME "ARG2" :PACKAGE "COMMON-LISP-USER"))
                 :DOCSTRING "A test generic function.")
     :actual (let* ((analysis (get-first-analysis code)))
               (cl-naive-code-analyzer::write-analysis analysis nil)))))

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
     :expected '(:NAME (:NAME "MY-STRUCT" :PACKAGE "COMMON-LISP-USER")
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND (:NAME "DEFSTRUCT" :PACKAGE "COMMON-LISP")
                 :LINE 1 :START 0 :END 138
                 :CODE
                 "(DEFSTRUCT COMMON-LISP-USER::MY-STRUCT
  \"A test structure.\"
  (COMMON-LISP-USER::FIELD1 0 :TYPE INTEGER)
  (COMMON-LISP-USER::FIELD2 \"hello\" :READ-ONLY T))"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS NIL
                 :DYNAMIC-DEFINITIONS NIL
                 :DOCSTRING "A test structure.")
     :actual (let* ((analysis (get-first-analysis code)))
               (cl-naive-code-analyzer::write-analysis analysis nil)))))

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
     :expected '(:NAME (:NAME "SHORT-ACCESSOR" :PACKAGE "COMMON-LISP-USER")
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND (:NAME "DEFSETF" :PACKAGE "COMMON-LISP")
                 :LINE 1 :START 0 :END 60
                 :CODE
                 "(DEFSETF COMMON-LISP-USER::SHORT-ACCESSOR COMMON-LISP-USER::UPDATE-FN
  \"Doc for short-accessor.\")"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS NIL
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY "COMMON-LISP-USER::UPDATE-FN"
                 :DOCSTRING "Doc for short-accessor.")
     :actual (let* ((analysis (get-first-analysis code-short)))
               (cl-naive-code-analyzer::write-analysis analysis nil)))))

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
     :expected '(COMMON-LISP-USER::OBJ COMMON-LISP-USER::INDEX)
     :actual (let* ((analysis (get-first-analysis "(defsetf long-accessor (obj index) (new-value)
                   \"Doc for long-accessor.\"
                   `(setf (aref ,obj ,index) ,new-value))")))
               (analysis-parameters analysis)))

    ;; TODO: Body is walked, but `(aref ...)` might not be picked up as function call by gather-info
    ;; TODO: lexical-definitions ; Parameters of the lambda-list are not currently added
    ;; TODO: parameters = nil ; As per current analyzer
    ;; TODO: Docstring extraction for long form might be tricky
    (cl-naive-tests:testcase
     :defsetf-long-sexp
     :equal #'equalp
     :expected '(:NAME (:NAME "LONG-ACCESSOR"
                        :PACKAGE "COMMON-LISP-USER")
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND (:NAME "DEFSETF" :PACKAGE "COMMON-LISP")
                 :LINE 1 :START 0 :END 148
                 :CODE
                 "(DEFSETF COMMON-LISP-USER::LONG-ACCESSOR
         (COMMON-LISP-USER::OBJ COMMON-LISP-USER::INDEX)
  (COMMON-LISP-USER::NEW-VALUE)
  \"Doc for long-accessor.\"
  (CL-NAIVE-CODE-ANALYZER::QUASIQUOTE
   (SETF (AREF (CL-NAIVE-CODE-ANALYZER::UNQUOTE COMMON-LISP-USER::OBJ)
               (CL-NAIVE-CODE-ANALYZER::UNQUOTE COMMON-LISP-USER::INDEX))
           (CL-NAIVE-CODE-ANALYZER::UNQUOTE COMMON-LISP-USER::NEW-VALUE))))"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS
                 ((:NAME "INDEX" :PACKAGE "COMMON-LISP-USER")
                  (:NAME "OBJ" :PACKAGE "COMMON-LISP-USER"))
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY "(ECLECTOR.READER:QUASIQUOTE
 (SETF (AREF (ECLECTOR.READER:UNQUOTE COMMON-LISP-USER::OBJ)
             (ECLECTOR.READER:UNQUOTE COMMON-LISP-USER::INDEX))
         (ECLECTOR.READER:UNQUOTE COMMON-LISP-USER::NEW-VALUE)))"
                 :PARAMETERS
                 ((:NAME "OBJ" :PACKAGE "COMMON-LISP-USER")
                  (:NAME "INDEX" :PACKAGE "COMMON-LISP-USER"))
                 :DOCSTRING "Doc for long-accessor.")
     :actual (let* ((analysis (get-first-analysis code-long)))
               (cl-naive-code-analyzer::write-analysis analysis nil)))))

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
     :defpackage-other-size
     ;; :size 100 is stored as ((:SIZE . 100)) by real-raw on the option cst
     :expected 100
     :actual (let* ((analysis (get-first-analysis code)))
               (analysis-size analysis)))

    ;; Defpackage does not have a raw body
    ;; TODO: SHADOWING-IMPORTS NIL ; Current analyzer limitation
    ;; TODO: :IMPORTS NIL           ; Current analyzer limitation
    (cl-naive-tests:testcase
     :defpackage-sexp
     :equal #'equalp
     :expected '(:NAME (:NAME "MY-TEST-PACKAGE" :PACKAGE "COMMON-LISP-USER")
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND (:NAME "DEFPACKAGE" :PACKAGE "COMMON-LISP")
                 :LINE 1 :START 0 :END 480
                 :CODE
                 "(DEFPACKAGE #:MY-TEST-PACKAGE
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
                 :NICKNAMES ("MTP") :USES ("COMMON-LISP" "ALEXANDRIA")
                 :EXPORTS
                 ((:NAME "FOO" :PACKAGE "COMMON-LISP-USER")
                  (:NAME "BAR" :PACKAGE "COMMON-LISP-USER"))
                 :SHADOWS ((:NAME "BAZ" :PACKAGE "COMMON-LISP-USER"))
                 :INTERNS
                 ((:NAME "INTERNAL-SYMBOL" :PACKAGE "COMMON-LISP-USER"))
                 :OTHER-SIZE 100)
     :actual (let* ((analysis (get-first-analysis
                               code
                               :package (find-package :cl-user))))
               (cl-naive-code-analyzer::write-analysis analysis nil)))))

(defparameter *my-param* 42 "My global parameter.")
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
     :expected '(:NAME (:NAME "*MY-PARAM*" :PACKAGE "COMMON-LISP-USER")
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND (:NAME "DEFPARAMETER" :PACKAGE "COMMON-LISP")
                 :LINE 1 :START 0 :END 51
                 :CODE
                 "(DEFPARAMETER COMMON-LISP-USER::*MY-PARAM* 42 \"My global parameter.\")"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS NIL
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY "42"
                 :DOCSTRING "My global parameter.")
     :actual (let* ((analysis (get-first-analysis code)))
               (cl-naive-code-analyzer::write-analysis analysis nil)))))

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
     :expected '(:NAME (:NAME "*MY-VAR*" :PACKAGE "COMMON-LISP-USER")
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND (:NAME "DEFVAR" :PACKAGE "COMMON-LISP")
                 :LINE 1 :START 0 :END 47
                 :CODE
                 "(DEFVAR COMMON-LISP-USER::*MY-VAR* (+ 1 2) \"My global variable.\")"
                 :FUNCTION-CALLS ((:NAME "+" :PACKAGE "COMMON-LISP"))
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS NIL
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY "+"
                 :DOCSTRING "My global variable.")
     :actual (let* ((analysis (get-first-analysis code)))
               (cl-naive-code-analyzer::write-analysis analysis nil)))))

(let ((code-no-doc "(defvar *my-var-no-init* \"No documentation.\")"))
  (cl-naive-tests:testsuite :defvar-no-doc-suite ; Changed to a new suite for this specific case
    (cl-naive-tests:testcase
     :defvar-no-doc-sexp
     ;; :testsuite-name :defvar-suite ; This was the problematic keyword
     :equal #'equalp
     :expected '(:NAME (:NAME "*MY-VAR-NO-INIT*" :PACKAGE "COMMON-LISP-USER")
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND (:NAME "DEFVAR" :PACKAGE "COMMON-LISP")
                 :LINE 1 :START 0 :END 45
                 :CODE
                 "(DEFVAR COMMON-LISP-USER::*MY-VAR-NO-INIT* \"No documentation.\")"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS NIL
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY "\"No documentation.\"")
     :actual (let* ((analysis (get-first-analysis code-no-doc)))
               (cl-naive-code-analyzer::write-analysis analysis nil)))))

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
     :expected '(:NAME (:NAME "+MY-CONST+" :PACKAGE "COMMON-LISP-USER")
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND (:NAME "DEFCONSTANT" :PACKAGE "COMMON-LISP")
                 :LINE 1 :START 0 :END 48
                 :CODE
                 "(DEFCONSTANT COMMON-LISP-USER::+MY-CONST+ (* 10 5) \"My constant.\")"
                 :FUNCTION-CALLS ((:NAME "*" :PACKAGE "COMMON-LISP"))
                 :MACRO-CALLS NIL
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS NIL
                 :DYNAMIC-DEFINITIONS NIL
                 :RAW-BODY
                 "*" :DOCSTRING "My constant.")
     :actual (let* ((analysis (get-first-analysis code)))
               (cl-naive-code-analyzer::write-analysis analysis nil)))))

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
     :expected '(:NAME (:NAME "MY-MACRO" :PACKAGE "COMMON-LISP-USER")
                 :PACKAGE "COMMON-LISP-USER"
                 :FILENAME NIL
                 :KIND (:NAME "DEFMACRO" :PACKAGE "COMMON-LISP")
                 :LINE 1 :START 0 :END 109
                 :CODE
                 "(DEFMACRO COMMON-LISP-USER::MY-MACRO
          (COMMON-LISP-USER::ARG1 &OPTIONAL (COMMON-LISP-USER::ARG2 0))
  \"A test macro.\"
  (CL-NAIVE-CODE-ANALYZER::QUASIQUOTE
   (LIST (CL-NAIVE-CODE-ANALYZER::UNQUOTE COMMON-LISP-USER::ARG1)
         (CL-NAIVE-CODE-ANALYZER::UNQUOTE COMMON-LISP-USER::ARG2))))"
                 :FUNCTION-CALLS NIL
                 :MACRO-CALLS
                 ((:NAME "QUASIQUOTE" :PACKAGE "ECLECTOR.READER"))
                 :VARIABLE-USES NIL
                 :LEXICAL-DEFINITIONS
                 ((:NAME "ARG2" :PACKAGE "COMMON-LISP-USER")
                  (:NAME "ARG1" :PACKAGE "COMMON-LISP-USER"))
                 :DYNAMIC-DEFINITIONS NIL :RAW-BODY "(ECLECTOR.READER:QUASIQUOTE
 (LIST (ECLECTOR.READER:UNQUOTE COMMON-LISP-USER::ARG1)
       (ECLECTOR.READER:UNQUOTE COMMON-LISP-USER::ARG2)))"
                 :PARAMETERS
                 ((:NAME "ARG1" :PACKAGE "COMMON-LISP-USER")
                  (:NAME "ARG2" :PACKAGE "COMMON-LISP-USER"))
                 :DOCSTRING "A test macro.")
     :actual (let* ((analysis (get-first-analysis code)))
               (cl-naive-code-analyzer::write-analysis analysis nil)))))

(let* ((project "test-code"))

  (analyze-project project
                   (asdf:system-source-directory project))

  (cl-naive-tests:testsuite :project-suite
    (cl-naive-tests:testcase
     :project-analysis-persisted
     :expected t
     :actual (uiop:file-exists-p "~/code-multiverse/code-universe/test-code/code-definitions/code-definitions.log")))

  (cl-naive-tests:testsuite :project-suite
    (cl-naive-tests:testcase
     :load-project
     :expected '(:NAME (:NAME "TEST-DEFUN-NO-DOCSTRING"
                        :PACKAGE "TEST-PACKAGE-SIMPLE")
                 :PACKAGE "TEST-PACKAGE-SIMPLE"
                 :FILENAME nil
                 :KIND (:NAME "DEFUN" :PACKAGE "COMMON-LISP") :LINE 55 :START 2017 :END 2107
                 :CODE "(DEFUN TEST-PACKAGE-SIMPLE::TEST-DEFUN-NO-DOCSTRING
       (TEST-PACKAGE-SIMPLE::A TEST-PACKAGE-SIMPLE::B)
  (+ TEST-PACKAGE-SIMPLE::A TEST-PACKAGE-SIMPLE::B))"
                 :FUNCTION-CALLS ((:NAME "+" :PACKAGE "COMMON-LISP")) :MACRO-CALLS NIL
                 :VARIABLE-USES NIL :LEXICAL-DEFINITIONS
                 ((:NAME "B" :PACKAGE "TEST-PACKAGE-SIMPLE")
                  (:NAME "A" :PACKAGE "TEST-PACKAGE-SIMPLE"))
                 :DYNAMIC-DEFINITIONS NIL :RAW-BODY
                 "(+ TEST-PACKAGE-SIMPLE::A TEST-PACKAGE-SIMPLE::B)" :LAMBDA-INFO
                 (:REQUIRED (TEST-PACKAGE-SIMPLE::A TEST-PACKAGE-SIMPLE::B) :OPTIONALS NIL
                  :REST NIL :KEYWORDS NIL :ALLOW-OTHER-KEYS NIL :AUXES NIL)
                 :PARAMETERS
                 ((:NAME "A" :PACKAGE "TEST-PACKAGE-SIMPLE")
                  (:NAME "B" :PACKAGE "TEST-PACKAGE-SIMPLE"))
                 :HASH nil)
     :actual (let ((func (first (find-function (list project)
                                               "TEST-DEFUN-NO-DOCSTRING"))))
               ;;Clearing properties that would be environment
               ;;specific for tests.
               (setf (getf func :filename) nil)
               (setf (getf func :hash) nil)
               func))))

;;(cl-naive-tests:run)
