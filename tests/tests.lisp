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

;;(cl-naive-tests:run)
