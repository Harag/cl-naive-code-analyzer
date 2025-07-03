;;; analyzers.lisp
;;;
;;; This file defines various analysis classes and methods for
;;; different Lisp forms.  Each class typically stores information
;;; extracted from a specific type of Lisp form, such as its name,
;;; parameters, docstring, and body.

;;; TODO: Consider refactoring the common slot definitions (e.g.,
;;;       docstring, parameters) into a mixin class or a base class
;;;       for better organization.

;;; TODO: Add more specific analysis classes for other Lisp forms if
;;; needed.

(in-package :cl-naive-code-analyzer)

;;; Analysis class for DEFUN forms.
(defclass defun-analysis (analysis)
  ((lambda-info :accessor analysis-lambda-info
                :initform nil
                :documentation "Stores the parsed lambda list using alexandria:parse-ordinary-lambda-list.")

   (parameters :accessor analysis-parameters
               :initform nil
               :documentation "A list of `parameter-detail` structures providing detailed information about each parameter.")
   (docstring :accessor analysis-docstring
              :initform nil
              :documentation "The documentation string of the function, if present.")))

;;; Analysis class for DEFMETHOD forms.
(defclass defmethod-analysis (analysis)
  ((method-qualifier :accessor analysis-method-qualifier
                     :initform nil
                     :documentation "The method qualifier (e.g., :before, :after, :around), or nil for a primary method.")
   (parameters :accessor analysis-parameters
               :initform nil
               :documentation "A list of `parameter-detail` structures providing detailed information about each parameter including specializers.")
   (lambda-info :accessor analysis-lambda-info
                :initform nil
                :documentation "Stores the parsed specialized lambda list using alexandria:parse-ordinary-lambda-list.")
   (docstring :accessor analysis-docstring
              :initform nil
              :documentation "The documentation string of the method, if present.")))

;;; Analysis class for DEFINE-CONDITION forms.
(defclass define-condition-analysis (analysis)
  ((slots :accessor analysis-slots
          :initform nil
          :documentation "A list of slot names defined in the condition.")
   (superclasses :accessor analysis-superclasses
                 :initform nil
                 :documentation "A list of parent condition classes for this condition.")
   (docstring :accessor analysis-docstring
              :initform nil
              :documentation "The documentation string of the condition, if present.")))

;;; Analysis class for DEFCLASS forms.
(defclass defclass-analysis (analysis)
  ((slots :accessor analysis-slots
          :initform nil
          :documentation "A list of slot names defined in the class.")
   (superclasses :accessor analysis-superclasses
                 :initform nil
                 :documentation "A list of superclass names for this class.")
   (docstring :accessor analysis-docstring
              :initform nil
              :documentation "The documentation string of the class, if present.")))

;;; Analysis class for DEFPARAMETER forms.
;;; Also used for DEFVAR and DEFCONSTANT.
(defclass defparameter-analysis (analysis)
  ((docstring :accessor analysis-docstring
              :initform nil
              :documentation "The documentation string of the parameter, if present.")))

;;; Analysis class for DEFMACRO forms.
(defclass defmacro-analysis (analysis)
  ((lambda-info :accessor analysis-lambda-info
                :initform nil
                :documentation "Stores the parsed lambda list using alexandria:parse-ordinary-lambda-list.")
   (parameters :accessor analysis-parameters
               :initform nil
               :documentation "A simple list of all parameter names, extracted from the macro's lambda list using the hybrid parsing strategy.")
   (docstring :accessor analysis-docstring
              :initform nil
              :documentation "The documentation string of the macro, if present.")
   (whole-var :accessor analysis-whole-var
              :initform nil
              :documentation "Variable for &whole, if present.")
   (environment-var :accessor analysis-environment-var
                    :initform nil
                    :documentation "Variable for &environment, if present.")
   (body-var :accessor analysis-body-var
             :initform nil
             :documentation "Variable for &body or &rest, if present.")
   (ordinary-lambda-list-details :accessor analysis-ordinary-lambda-list-details
                                 :initform nil
                                 :documentation "Stores the parsed details (required, optional, key, aux) of the main segment of the macro lambda list, processed by alexandria:parse-ordinary-lambda-list.")))

;;; Analysis class for DEFTYPE forms.
(defclass deftype-analysis (analysis)
  ((lambda-info :accessor analysis-lambda-info
                :initform nil
                :documentation "Stores the parsed lambda list using alexandria:parse-ordinary-lambda-list.")
   (parameters :accessor analysis-parameters
               :initform nil
               :documentation "A simple list of parameter names, extracted from the type's lambda list.")
   (docstring :accessor analysis-docstring
              :initform nil
              :documentation "The documentation string of the type definition, if present.")))

;;; Analysis class for DEFGENERIC forms.
(defclass defgeneric-analysis (analysis)
  ((lambda-info :accessor analysis-lambda-info
                :initform nil
                :documentation "Stores the parsed generic function lambda list using alexandria:parse-ordinary-lambda-list.")
   (parameters :accessor analysis-parameters
               :initform nil
               :documentation "A list of `parameter-detail` structures providing detailed information about each parameter in the signature.")

   (docstring :accessor analysis-docstring
              :initform nil
              :documentation "The documentation string of the generic function, if present.")))

;;; Analysis class for DEFSTRUCT forms.
(defclass defstruct-analysis (analysis)
  ((slots :accessor analysis-slots
          :initform nil
          :documentation "A list of slot names defined in the structure.")
   (docstring :accessor analysis-docstring
              :initform nil
              :documentation "The documentation string of the structure, if present.")))

;;; Analysis class for DEFSETF forms.
(defclass defsetf-analysis (analysis)
  ((lambda-info :accessor analysis-lambda-info
                :initform nil
                :documentation "Stores the parsed lambda list for the long form of defsetf.")
   (store-variables :accessor analysis-store-variables ; For long form store variables
                    :initform nil
                    :documentation "List of store variables for the long form of defsetf.")
   (parameters :accessor analysis-parameters
               :initform nil
               :documentation "A list of `parameter-detail` structures (for long form's lambda-list) or nil (for short form).")

   (docstring :accessor analysis-docstring
              :initform nil
              :documentation "The documentation string of the setf definition, if present.")))

;;; Analysis class for DEFINE-SYMBOL-MACRO forms.
;; No specific slots beyond the base 'analysis' class yet.
;; TODO: Consider adding a slot for the expansion if needed for analysis.
(defclass define-symbol-macro-analysis (analysis)
  ((docstring :accessor analysis-docstring
              :initform nil
              :documentation "The documentation string of the setf definition, if present.")))

;;; Analysis class for DEFPACKAGE forms.
(defclass defpackage-analysis (analysis)
  ((package-name
    :initform nil
    :accessor analysis-package-name
    :documentation "The name of the package being defined.")
   (nicknames
    :initform nil
    :accessor analysis-nicknames
    :documentation "A list of nicknames for the package.")
   (uses
    :initform nil
    :accessor analysis-uses
    :documentation "A list of packages to use.")
   (exports
    :initform nil
    :accessor analysis-exports
    :documentation "A list of symbols to export from the package.")
   (shadows
    :initform nil
    :accessor analysis-shadows
    :documentation "A list of symbols to shadow.")
   (shadowing-imports
    :initform nil
    :accessor analysis-shadowing-imports
    :documentation "A list of symbols to shadowing-import.")
   (imports
    :initform nil
    :accessor analysis-imports
    :documentation "A list of symbols to import.")
   (interns
    :initform nil
    :accessor analysis-interns
    :documentation "A list of symbols to intern.")
   (docstring
    :initform nil
    :accessor analysis-docstring
    :documentation "The documentation string for the package.")
   (size
    :initform nil
    :accessor analysis-size
    :documentation "Size hint raw.")))

;;; Helper function to get the raw value from a CST node.
;;; If the CST is a cons, it gets the raw value of the first element.
;;; Otherwise, it gets the raw value of the CST itself.

;;; TODO: Evaluate if this helper is still the best approach or if
;;; cst:raw is sufficient in most cases.
(defun real-raw (cst)
  "Get the raw Lisp data from a CST node.
   If CST is a cons cell in the CST, extracts raw data from its CAR.
   Otherwise, extracts raw data from the CST atom itself."
  (if (concrete-syntax-tree:consp cst)
      (concrete-syntax-tree:raw
       (concrete-syntax-tree:first cst))
      (concrete-syntax-tree:raw
       cst)))

;;; Generic function to create an analyzer instance for a given form
;;; type.
(defgeneric make-analyzer (type)
  (:documentation "Return an analyzer instance for a given top-level form TYPE (a symbol like 'DEFUN)."))

;;; Default method for MAKE-ANALYZER.  Returns a basic 'analysis'
;;; instance if no specific analyzer is defined for the type.
(defmethod make-analyzer (type)
  (declare (ignore type))
  (make-instance 'analysis))

;;; Specialized method for MAKE-ANALYZER for DEFUN forms.
(defmethod make-analyzer ((type (eql 'defun)))
  (make-instance 'defun-analysis))

;;; Classifies the syntax of a CST node.
;;; Returns the specific form name (symbol) if the CST is a cons, otherwise nil.
;;; Example: (DEFUN FOO ()) -> DEFUN
(defun classify-syntax (cst)
  "Return the specific form name (symbol) if the CST represents a Lisp form, or NIL.
   E.g., for a CST representing (DEFUN FOO () ...), this returns 'DEFUN."
  ;; TODO: Consider if this needs to handle malformed CSTs more
  ;; gracefully.
  (when (concrete-syntax-tree:consp cst)
    (let ((head (concrete-syntax-tree:raw
                 (concrete-syntax-tree:first cst))))
      head)))

;;; Classifies the semantic category of a CST node.  Identifies
;;; categories like :symbol, :literal, :assignment, :special,
;;; :macro-call, :call, or :other.
(defun classify-semantic (cst)
  "Return a keyword representing the semantic category of the CST node.
   Categories include :SYMBOL, :LITERAL, :ASSIGNMENT, :SPECIAL (special operator),
   :MACRO-CALL, :CALL (function call), or :OTHER."
  ;; TODO: This classification is quite basic. Expand and refine categories for more detailed analysis.
  ;; TODO: Handle dotted lists and other complex structures if necessary.
  (if (not (concrete-syntax-tree:consp cst))
      (if (symbolp (concrete-syntax-tree:raw cst))
          :symbol
          :literal)
      (let ((head (concrete-syntax-tree:raw (concrete-syntax-tree:first cst))))
        ;; Ensure head is not a list itself (e.g. ((lambda (x) x) 1))
        (if (not (consp head))
            (cond
              ;; TODO: Expand this list of assignment
              ;; operators. Consider PSETF, PSETQ, INCF, DECF etc.
              ((member head '(setf setq push pop))
               :assignment)
              ((and (symbolp head) (special-operator-p head))
               :special)
              ((and (symbolp head) (macro-function head))
               :macro-call)
              ;; Check if it's a known function
              ((and (symbolp head) (fboundp head))
               :call)
              ;; If head is an atom but not matched above
              ((atom head)
               ;; TODO:  This condition seems redundant with the outer IF
               (if (symbolp (concrete-syntax-tree:raw cst))
                   :symbol
                   :literal))
              (t
               ;; Default for unrecognized forms starting with an atom
               :other))
            ;; Default for forms starting with a cons (e.g., lambda forms)
            :other))))

;;; Walks a Concrete Syntax Tree (CST) and applies a function FN to
;;; each node.  This is a simple walk, might not be suitable for all
;;; analysis tasks.

;;; TODO: This walker seems to have a potential issue with how it
;;;       handles the 'remaining' and 'first' parts. It might miss
;;;       some nodes or process parts incorrectly.  Consider replacing
;;;       with a more robust CST walking utility if available or
;;;       thoroughly testing and debugging this one.  The use of a
;;;       hash table suggests cycle detection, which is good.
(defun walk-cst (cst fn)
  "Walks the CST structure and applies FN to encountered CST nodes.
   Uses a hash table to detect and avoid infinite loops in circular list structures.
   Returns multiple values: the count of unique CONS cells visited, and a keyword
   indicating if the list was :PROPER, :DOTTED, or :CIRCULAR."
  (let ((table (make-hash-table :test #'eq)))
    (loop for remaining = cst then (concrete-syntax-tree:rest remaining)
          while (concrete-syntax-tree:consp remaining)
          until (gethash remaining table)
          do
          (progn
            ;; Apply FN to the current cons cell
            (funcall fn remaining)
            ;; Apply FN to the CAR of the current cons cell if it's an atom
            (when (concrete-syntax-tree:atom (concrete-syntax-tree:first remaining))
              (funcall fn (concrete-syntax-tree:first remaining)))
            ;; Apply FN to the CDR of the current cons cell if it's an atom and not null
            (unless (concrete-syntax-tree:null (concrete-syntax-tree:rest remaining))
              (when (concrete-syntax-tree:atom (concrete-syntax-tree:rest remaining))
                (funcall fn (concrete-syntax-tree:rest remaining))))
            (setf (gethash remaining table) t))
          finally (return (values (hash-table-count table)
                                  (cond ((concrete-syntax-tree:null remaining) :proper)
                                        ((concrete-syntax-tree:atom remaining) :dotted)
                                        (t :circular)))))))

;;; Walks a CST in a depth-first manner, providing context (path) to
;;; the function FN.  FN is called with (current-cst path
;;; remaining-tail).

;;; Helper functions for processing lambda list parameters

(defun process-parameter-lexical-defs (param-detail analysis)
  "Extracts and stores lexical variable names (parameter name and its
supplied-p variable, if any) from a parameter detail plist into the
analysis object."
  (let ((name (getf param-detail :name))
        (supplied-p-var (getf param-detail :supplied-p-variable)))
    (when name
      ;; Ensure name is a symbol before pushing, as it could be a list for destructuring
      (if (symbolp name)
          (pushnew name (analysis-lexical-definitions analysis) :test #'eq) ; CORRECTED SLOT NAME
          ;; If name is a list (destructuring), its components are handled by process-destructured-parameter
          nil))
    (when supplied-p-var
      (pushnew supplied-p-var (analysis-lexical-definitions analysis) :test #'eq)))) ; CORRECTED SLOT NAME

(defun process-parameter-csts (param-detail analysis context)
  "Identifies and walks specific CSTs within a parameter detail (default
values, type specifiers, destructuring forms) to gather information
like function calls or variable uses."
  (let ((default-value-cst (getf param-detail :default-value-cst))
        (type-specifier-cst (getf param-detail :type-specifier-cst))
        (name-cst (getf param-detail :name-cst)) ; CST of the parameter name/destructuring pattern
        (name (getf param-detail :name)))
    ;; Walk default value CST, if present
    (when default-value-cst
      (walk-cst-with-context default-value-cst
                             (lambda (current-cst path tail)
                               (declare (ignore path tail))
                               (gather-info current-cst analysis))))
    ;; Walk type specifier CST for relevant contexts (e.g., defmethod)
    (when (and type-specifier-cst (member context '(:specialized :method))) ; :method is an alias for :specialized context in some parts
      (walk-cst-with-context type-specifier-cst
                             (lambda (current-cst path tail)
                               (declare (ignore path tail))
                               (gather-info current-cst analysis))))
    ;; If the parameter name itself is a list (destructuring) and we have its CST representation
    (when (and name-cst (listp name) (not (null name))) ; Ensure it's a non-empty list
      (walk-cst-with-context name-cst
                             (lambda (current-cst path tail)
                               (declare (ignore path tail))
                               (gather-info current-cst analysis))))))

(defun process-destructured-parameter (param-detail analysis context)
  "Recursively process components of a destructured parameter."
  (when (getf param-detail :destructured)
    (let ((sub-parameters (getf param-detail :sub-parameters))) ; This is a lambda-list-like plist
      (when sub-parameters
        (dolist (section '(:required :optionals :keys :aux)) ; Standard sections in a nested lambda list
          (dolist (sub-param (getf sub-parameters section))
            (process-parameter-lexical-defs sub-param analysis)
            (process-parameter-csts sub-param analysis context)
            ;; Recursive call for nested destructuring
            (when (getf sub-param :destructured)
              (process-destructured-parameter sub-param analysis context))))
        ;; Handle :rest or :body if present in sub-parameters
        (let ((rest-param (or (getf sub-parameters :rest) (getf sub-parameters :body))))
          (when rest-param
            (process-parameter-lexical-defs rest-param analysis)
            (process-parameter-csts rest-param analysis context)
            (when (getf rest-param :destructured)
              (process-destructured-parameter rest-param analysis context))))))))

;;; TODO: The `path` construction `(append path (list head))` can be
;;;       inefficient for deep trees.  Consider alternative ways to
;;;       manage path information if performance becomes an issue.
;;;       TODO: Ensure `step-cst` correctly handles all CST node types
;;;       and structures.
(defun walk-cst-with-context (cst fn)
  "Walk the CST in a depth-first manner.
   Call FN with (current-cst path remaining-tail) at each node (both CONS and ATOM).
   'path' is a list of raw symbols from the heads of parent CSTs.
   Preserves structure and is iterative with cycle detection."
  (let ((table (make-hash-table :test #'eq)))
    (labels ((step-cst (remaining path)
               (loop for rem = remaining then (concrete-syntax-tree:rest rem)
                     while (concrete-syntax-tree:consp rem)
                     until (gethash rem table)
                     do
                     (let* ((head-cst (concrete-syntax-tree:first rem))
                            ;; Extract raw data if head-cst is not nil
                            (head (and head-cst (concrete-syntax-tree:raw head-cst)))
                            (tail (concrete-syntax-tree:rest rem)))
                       ;; Visit this CONS node
                       (funcall fn rem path tail)
                       (setf (gethash rem table) t)
                       ;; If tail is an atom (dotted pair or end of
                       ;; list with an atom), visit that leaf too
                       ;; Ensure tail is not nil before atom check
                       (when (and tail (concrete-syntax-tree:atom tail))
                         (funcall fn
                                  tail
                                  ;; Path to the atom in the CDR
                                  (if head
                                      (append path (list head))
                                      path)
                                  ;; No further tail for this leaf
                                  nil))

                       ;; Descend into the CAR of the current cons
                       ;; cell The original code was: (step-cst tail
                       ;; (if head (append path (list head)) path))
                       ;; This seems to descend into the tail (CDR),
                       ;; not the head (CAR).  Corrected to descend
                       ;; into head-cst (CAR)

                       ;; TODO: Verify this logic. Original was likely
                       ;; intended to explore the list structure
                       ;; sequentially.  If the intention is full
                       ;; depth-first (CAR then CDR), this needs
                       ;; restructuring.  Current structure processes
                       ;; the current cons, then its CAR (if it's a
                       ;; cons), then moves to CDR.
                       (when (concrete-syntax-tree:consp head-cst)
                         (step-cst head-cst
                                   (if head
                                       (append path (list head))
                                       path))))
                     ;; At the end of this branch (or if rem becomes
                     ;; an atom or circular)
                     (return))))
      (step-cst cst nil)
      ;; Return diagnostic information about the walk
      (values (hash-table-count table)
              (cond ((or (null cst) (concrete-syntax-tree:null cst)) :proper)
                    ((concrete-syntax-tree:atom cst) :dotted)
                    (t :circular))))))

;;; Gathers information from a CST node based on its semantic
;;; classification.  Updates the provided 'analysis' object with
;;; findings (e.g., function calls, macro calls).
(defun gather-info (cst analysis)
  "Gathers information from a CST node and updates the ANALYSIS object.
   Identifies function calls, macro calls, assignments, etc."
  ;; TODO: The FORMAT T calls are for debugging and should be removed
  ;; or replaced with proper logging/data collection.

  ;; TODO: Implement actual data gathering for :symbol, :literal, and
  ;; :other cases if relevant.
  (case (classify-semantic cst)
    (:call
     (pushnew (real-raw cst)
              (analysis-function-calls analysis)
              :test #'equal))
    (:macro-call
     (pushnew (real-raw cst)
              (analysis-macro-calls analysis)
              :test #'equal))
    (:assignment
     ;; TODO: Extract assigned variable and value.
     (format t "Assignment CST: ~S~%" cst))
    (:symbol
     ;; TODO: What should we do here? Potentially record symbol usage
     ;; if not part of a call.
     (format t "Symbol CST: ~S~%" cst))
    (:literal
     ;; TODO: What should we do here? Potentially record literal
     ;; values if significant.
     (format t "Literal CST: ~S~%" cst))
    (otherwise
     ;; TODO: What should we do for other CST types?
     (format t "Other CST: ~S~%" cst))))

;; Generic function to analyze a CST and populate an analysis object.
(defgeneric analyze-cst (cst analysis)
  (:documentation "Analyzes the given CST and populates the ANALYSIS object with extracted information."))

;; Around method for ANALYZE-CST.  Sets the 'analysis-kind' slot based
;; on the head of the CST.
(defmethod analyze-cst :around (cst analysis)
  "Around method to set the 'analysis-kind' slot before specific analysis.
   The 'kind' is usually the defining macro or function name (e.g., 'DEFUN)."
  ;; TODO: "Is this the right kind? Is kind the right word?" - This
  ;;       comment suggests uncertainty.  'kind' seems reasonable for
  ;;       the type of definition (defun, defclass, etc.).  Ensure
  ;;       `real-raw` correctly extracts the intended symbol.
  (setf (analysis-kind analysis) (real-raw cst))
  (call-next-method))

;; Default method for ANALYZE-CST.
;; Walks the CST using walk-cst-with-context and calls gather-info on each node.
(defmethod analyze-cst (cst analysis)
  "Default method to analyze a CST. It walks the tree and gathers general information.
   This method is typically called for the body of definitions or for forms
   not handled by more specific analyzers."
  ;; TODO: Ensure `walk-cst-with-context` is the appropriate walker
  ;;       for general analysis.  The `path` and `tail` arguments are
  ;;       ignored here; perhaps a simpler walker would suffice if
  ;;       context isn't used by `gather-info` in the general case.
  (walk-cst-with-context
   cst
   (lambda (current-cst path tail)
     (declare (ignore path tail)) ; Context not used by the general gather-info
     (gather-info current-cst analysis)))
  analysis)

;; Specialized method for ANALYZE-CST for DEFUN forms.
(defmethod analyze-cst (cst (analysis defun-analysis))
  "Analyzes a DEFUN CST to extract name, arguments, docstring, and body.
   Populates the DEFUN-ANALYSIS object."
  ;; TODO: Error handling for malformed DEFUN CSTs (e.g., missing name
  ;; or args).

  ;; TODO: Consider if deeper analysis of the body is needed here or
  ;; if the default method handles it.
  (let* ((name-cst      (concrete-syntax-tree:second cst))
         (args-cst      (concrete-syntax-tree:third cst))
         (possible-doc  (concrete-syntax-tree:fourth cst))
         ;; Extract docstring if it's a string atom
         (doc           (when (and possible-doc
                                   (concrete-syntax-tree:atom possible-doc)
                                   (stringp (concrete-syntax-tree:raw possible-doc)))
                          (concrete-syntax-tree:raw possible-doc)))
         ;; Body starts after name, args, and optional docstring
         (body-cst      (if doc
                            (concrete-syntax-tree:nthrest 4 cst)
                            (concrete-syntax-tree:nthrest 3 cst)))
         (name          (concrete-syntax-tree:raw name-cst))
         ;; Get the lambda list CST
         (lambda-list-cst args-cst)
         ;; Parse the lambda list CST using the new parser
         (parsed-ll (if (and lambda-list-cst
                             (concrete-syntax-tree:consp lambda-list-cst))
                        (parse-lambda-list-cst lambda-list-cst :context :ordinary)
                        nil)))

    ;; Populate analysis slots
    (setf (analysis-name analysis) name
          (analysis-docstring analysis) doc
          (analysis-raw-body analysis) body-cst)

    ;; Store detailed lambda list information from the new parser
    (setf (analysis-lambda-info analysis) parsed-ll)

    ;; Populate analysis-parameters with detailed structures
    (let ((detailed-params '()))
      (when parsed-ll
        ;; First, collect all parameter plists for (analysis-parameters analysis)
        (dolist (p (getf parsed-ll :required)) (push p detailed-params))
        (dolist (p (getf parsed-ll :optionals)) (push p detailed-params))
        (let ((rest-p (getf parsed-ll :rest)))
          (when rest-p (push rest-p detailed-params)))
        (dolist (p (getf parsed-ll :keys)) (push p detailed-params))
        (dolist (p (getf parsed-ll :aux)) (push p detailed-params))
        (setf (analysis-parameters analysis) (nreverse detailed-params)))

      ;; Now, iterate over the collected parameter details
      ;; to apply the new helper functions.
      (dolist (p (analysis-parameters analysis))
        (process-parameter-lexical-defs p analysis)
        (process-parameter-csts p analysis :ordinary)
        (when (getf p :destructured)
          (process-destructured-parameter p analysis :ordinary))))

    ;; Analyze the function body
    (when body-cst
      (walk-cst-with-context
       body-cst
       (lambda (current-body-cst path tail)
         (declare (ignore path tail))
         (gather-info current-body-cst analysis))))
    analysis))

;;; Specialized method for ANALYZE-CST for DEFMACRO forms.
;;; Specialized method for ANALYZE-CST for DEFMACRO forms.
(defmethod analyze-cst (cst (analysis defmacro-analysis))
  "Analyzes a DEFMACRO CST to extract name, arguments, docstring, and body.
   Populates the DEFMACRO-ANALYSIS object."
  (let* ((name-cst     (concrete-syntax-tree:second cst))
         (args-cst     (concrete-syntax-tree:third cst))
         (possible-doc (concrete-syntax-tree:fourth cst))
         (doc          (when (and possible-doc
                                  (concrete-syntax-tree:atom possible-doc)
                                  (stringp (concrete-syntax-tree:raw possible-doc)))
                         (concrete-syntax-tree:raw possible-doc)))
         (body-cst     (if doc
                           (concrete-syntax-tree:nthrest 4 cst)
                           (concrete-syntax-tree:nthrest 3 cst)))
         (name         (concrete-syntax-tree:raw name-cst))
         ;; Get the lambda list CST
         (lambda-list-cst args-cst)
         ;; Parse the lambda list CST using the new parser with :macro context
         (parsed-ll (if (and lambda-list-cst (concrete-syntax-tree:consp lambda-list-cst))
                        (parse-lambda-list-cst lambda-list-cst :context :macro)
                        nil)))

    (setf (analysis-name analysis) name
          (analysis-docstring analysis) doc
          (analysis-raw-body analysis) body-cst)

    (setf (analysis-lambda-info analysis) parsed-ll)
    (setf (analysis-ordinary-lambda-list-details analysis) nil) ; Clear old slot

    (when parsed-ll
      ;; Populate analysis-parameters first
      (let ((whole-p (getf parsed-ll :whole))
            (env-p (getf parsed-ll :environment))
            (body-p (getf parsed-ll :body))
            ;; temp list for all params that go into analysis-parameters
            (all-params-for-slot '()))

        (when whole-p
          (setf (analysis-whole-var analysis) (getf whole-p :name))
          (push whole-p all-params-for-slot))
        (when env-p
          (setf (analysis-environment-var analysis) (getf env-p :name))
          (push env-p all-params-for-slot))
        (when body-p
          (setf (analysis-body-var analysis) (getf body-p :name))
          (push body-p all-params-for-slot))

        (dolist (p (getf parsed-ll :required)) (push p all-params-for-slot))
        (dolist (p (getf parsed-ll :optionals)) (push p all-params-for-slot))
        (let ((rest-p (getf parsed-ll :rest)))
          (when (and rest-p (not (analysis-body-var analysis))) ; Prioritize &body if present
            (push rest-p all-params-for-slot)))
        (dolist (p (getf parsed-ll :keys)) (push p all-params-for-slot))
        (dolist (p (getf parsed-ll :aux)) (push p all-params-for-slot))
        (setf (analysis-parameters analysis) (nreverse all-params-for-slot)))

      ;; Now process all parameters stored in (analysis-parameters analysis)
      ;; including &whole, &environment, &body if they were added.
      (dolist (p (analysis-parameters analysis))
        (process-parameter-lexical-defs p analysis)
        (process-parameter-csts p analysis :macro)
        (when (getf p :destructured)
          (process-destructured-parameter p analysis :macro))))

    ;; Analyze the macro body
    (when body-cst
      (walk-cst-with-context
       body-cst
       (lambda (current-body-cst path tail)
         (declare (ignore path tail))
         (gather-info current-body-cst analysis))))
    analysis))

;;; Specialized method for ANALYZE-CST for DEFMETHOD forms.
;; Specialized method for ANALYZE-CST for DEFMETHOD forms.
(defmethod analyze-cst (cst (analysis defmethod-analysis))
  "Analyzes a DEFMETHOD CST to extract name, qualifiers, specialized lambda list, docstring, and body.
   Populates the DEFMETHOD-ANALYSIS object."
  (let* (;; Name or (SETF name)
         (name-cst (concrete-syntax-tree:second cst))
         ;; Start looking for qualifiers or args list from the 3rd element (index 2)
         (index 2)
         (part     (concrete-syntax-tree:nth index cst))
         (method-qualifier nil))
    ;; Extract the single method qualifier, if present
    (when (and part
               (concrete-syntax-tree:atom part)
               (keywordp (concrete-syntax-tree:raw part))
               (member (concrete-syntax-tree:raw part) '(:before :after :around))) ; Check if it's a standard qualifier
      (setf method-qualifier (concrete-syntax-tree:raw part))
      (incf index)
      (setf part (concrete-syntax-tree:nth index cst)))
    (setf (analysis-method-qualifier analysis) method-qualifier)

    ;; After the optional qualifier, 'part' should be the args-cst
    (let* ((args-cst     part)
           (next-idx (+ index 1)) ; Index for possible docstring
           (possible-doc (concrete-syntax-tree:nth next-idx cst))
           (doc(when (and possible-doc
                          (concrete-syntax-tree:atom possible-doc)
                          (stringp (concrete-syntax-tree:raw possible-doc)))
                 (concrete-syntax-tree:raw possible-doc)))
           (body-cst (if doc
                         (concrete-syntax-tree:nthrest (+ next-idx 1) cst)
                         (concrete-syntax-tree:nthrest next-idx cst)))
           (name (real-raw name-cst)) ; Handles (SETF name) correctly
           (lambda-list-cst (and args-cst
                                 (concrete-syntax-tree:consp args-cst)
                                 ;; No need to mapcar raw here, parser takes CST
                                 args-cst))
           ;; Parse the lambda list CST using the new parser
           (parsed-ll (if (and lambda-list-cst
                               (concrete-syntax-tree:consp lambda-list-cst))
                          (parse-lambda-list-cst lambda-list-cst :context :specialized)
                          nil)))

      (setf (analysis-name analysis) name
            (analysis-docstring analysis) doc
            (analysis-raw-body analysis) body-cst)

      ;; Store detailed lambda list information from the new parser
      (setf (analysis-lambda-info analysis) parsed-ll)

      ;; Populate analysis-parameters
      (let ((detailed-params '()))
        (when parsed-ll
          ;; First, collect all parameter plists for (analysis-parameters analysis)
          (dolist (p (getf parsed-ll :required)) (push p detailed-params))
          (dolist (p (getf parsed-ll :optionals)) (push p detailed-params))
          (let ((rest-p (getf parsed-ll :rest)))
            (when rest-p (push rest-p detailed-params)))
          (dolist (p (getf parsed-ll :keys)) (push p detailed-params))
          (dolist (p (getf parsed-ll :aux)) (push p detailed-params))
          (setf (analysis-parameters analysis) (nreverse detailed-params)))

        ;; Now, iterate over the collected parameter details
        ;; to apply the new helper functions.
        (dolist (p (analysis-parameters analysis))
          (process-parameter-lexical-defs p analysis)
          ;; Note: :specialized context for defmethod
          (process-parameter-csts p analysis :specialized)
          (when (getf p :destructured)
            (process-destructured-parameter p analysis :specialized))))

      ;; Analyze the method body
      (when body-cst
        (walk-cst-with-context
         body-cst
         (lambda (current-body-cst path tail)
           (declare (ignore path tail))
           (gather-info current-body-cst analysis)))))
    analysis))

;;; Specialized method for ANALYZE-CST for DEFTYPE forms.
(defmethod analyze-cst (cst (analysis deftype-analysis))
  "Analyzes a DEFTYPE CST to extract name, lambda list, docstring, and body.
   Populates the DEFTYPE-ANALYSIS object."
  (let* ((name-cst     (concrete-syntax-tree:second cst))
         (args-cst     (concrete-syntax-tree:third cst))
         (possible-doc (concrete-syntax-tree:fourth cst))
         (doc          (when (and possible-doc
                                  (concrete-syntax-tree:atom possible-doc)
                                  (stringp (concrete-syntax-tree:raw possible-doc)))
                         (concrete-syntax-tree:raw possible-doc)))
         (body-cst     (if doc
                           (concrete-syntax-tree:nthrest 4 cst)
                           (concrete-syntax-tree:nthrest 3 cst)))
         (name         (concrete-syntax-tree:raw name-cst))
         (lambda-list-cst args-cst)
         (parsed-ll (if (and lambda-list-cst (concrete-syntax-tree:consp lambda-list-cst))
                        (parse-lambda-list-cst lambda-list-cst :context :deftype)
                        nil)))

    (setf (analysis-name analysis) name
          (analysis-docstring analysis) doc
          (analysis-raw-body analysis) body-cst)

    (setf (analysis-lambda-info analysis) parsed-ll)

    (let ((detailed-params '()))
      (when parsed-ll
        ;; First, collect all parameter plists for (analysis-parameters analysis)
        (dolist (p (getf parsed-ll :required)) (push p detailed-params))
        (dolist (p (getf parsed-ll :optionals)) (push p detailed-params))
        (let ((rest-p (getf parsed-ll :rest)))
          (when rest-p (push rest-p detailed-params)))
        (dolist (p (getf parsed-ll :keys)) (push p detailed-params))
        (dolist (p (getf parsed-ll :aux)) (push p detailed-params))
        (setf (analysis-parameters analysis) (nreverse detailed-params)))

      ;; Now, iterate over the collected parameter details
      ;; to apply the new helper functions.
      (dolist (p (analysis-parameters analysis))
        (process-parameter-lexical-defs p analysis)
        (process-parameter-csts p analysis :deftype)
        (when (getf p :destructured)
          (process-destructured-parameter p analysis :deftype))))

    ;; Analyze the deftype body
    (when body-cst
      (walk-cst-with-context
       body-cst
       (lambda (current-body-cst path tail)
         (declare (ignore path tail))
         (gather-info current-body-cst analysis))))
    analysis))

;;; Specialized method for ANALYZE-CST for DEFGENERIC forms.
(defmethod analyze-cst (cst (analysis defgeneric-analysis))
  "Analyzes a DEFGENERIC CST to extract name, lambda list, and options (especially docstring).
   Populates the DEFGENERIC-ANALYSIS object."
  ;; TODO: Parse other defgeneric options if needed for analysis (e.g., :method-class, :generic-function-class).
  (let* ((name-cst (concrete-syntax-tree:second cst))
         (args-cst (concrete-syntax-tree:third cst))
         (options-cst  (concrete-syntax-tree:nthrest 3 cst)) ; Options start after name and args
         (name     (concrete-syntax-tree:raw name-cst))
         (doc      nil)
         (lambda-list-cst args-cst)
         (parsed-ll (if (and lambda-list-cst (concrete-syntax-tree:consp lambda-list-cst))
                        (parse-lambda-list-cst lambda-list-cst :context :generic-function-ordinary)
                        nil)))

    ;; Extract docstring from options
    (when (and options-cst (concrete-syntax-tree:consp options-cst))
      (dolist (opt (cst:listify options-cst))
        (when (and (concrete-syntax-tree:consp opt)
                   (eq (concrete-syntax-tree:raw (concrete-syntax-tree:first opt))
                       :documentation))
          (let ((v (concrete-syntax-tree:second opt)))
            (when (and v (concrete-syntax-tree:atom v)
                       (stringp (concrete-syntax-tree:raw v)))
              (setf doc (concrete-syntax-tree:raw v)))))))

    (setf (analysis-name analysis) name
          (analysis-docstring analysis) doc)

    (setf (analysis-lambda-info analysis) parsed-ll)

    (let ((detailed-params '()))
      (when parsed-ll
        ;; First, collect all parameter plists for (analysis-parameters analysis)
        (dolist (p (getf parsed-ll :required)) (push p detailed-params))
        (dolist (p (getf parsed-ll :optionals)) (push p detailed-params))
        (let ((rest-p (getf parsed-ll :rest)))
          (when rest-p (push rest-p detailed-params)))
        (dolist (p (getf parsed-ll :keys)) (push p detailed-params))
        (dolist (p (getf parsed-ll :aux)) (push p detailed-params))
        (setf (analysis-parameters analysis) (nreverse detailed-params)))

      ;; Now, iterate over the collected parameter details
      ;; to apply the new helper functions.
      (dolist (p (analysis-parameters analysis))
        (process-parameter-lexical-defs p analysis)
        (process-parameter-csts p analysis :generic-function-ordinary)
        (when (getf p :destructured)
          (process-destructured-parameter p analysis :generic-function-ordinary))))

    ;; Defgeneric doesn't have a "body" in the same way defun does,
    ;; but options might contain expressions.
    ;; If options need deeper analysis (beyond docstring), a walk could be added here.
    analysis))

;;; Specialized method for ANALYZE-CST for DEFPARAMETER, DEFVAR,
;;; DEFCONSTANT forms.
(defmethod analyze-cst (cst (analysis defparameter-analysis))
  "Analyzes a DEFPARAMETER/DEFVAR/DEFCONSTANT CST to extract name, initial value, and docstring.
   Populates the DEFPARAMETER-ANALYSIS object."
  (let* ((name-cst (concrete-syntax-tree:second cst))
         (init-cst (concrete-syntax-tree:third cst))
         (doc (and (concrete-syntax-tree:fourth cst)
                   (concrete-syntax-tree:raw (concrete-syntax-tree:fourth cst))))
         (name (concrete-syntax-tree:raw name-cst)))

    (setf (analysis-name analysis) name
          (analysis-docstring analysis) doc
          (analysis-raw-body analysis) init-cst)
    ;; Analyze the initial value form if present
    (when init-cst
      (walk-cst-with-context
       init-cst
       (lambda (current-init-cst path tail)
         (declare (ignore path tail))
         (gather-info current-init-cst analysis))))
    analysis))

(defmethod analyze-cst (cst (analysis defsetf-analysis))
  "Analyzes a DEFSETF CST. Extracts name, docstring, parameters (long form), store variables, and body (long form).
   Populates the DEFSETF-ANALYSIS object."
  (let* ((name (real-raw (concrete-syntax-tree:second cst)))
         (docstring nil)
         (parameters nil) ; Will hold parameter-detail list for long form
         (store-vars nil) ; Will hold symbols for store variables
         (raw-body-cst nil) ; Body for long form, or update-fn for short form
         (third-arg-cst (concrete-syntax-tree:third cst))
         (is-long-form (and third-arg-cst (concrete-syntax-tree:consp third-arg-cst))))

    (if is-long-form
        ;; Long Form: (defsetf access-fn lambda-list (store-vars) [doc/decls] body)
        (let* ((lambda-list-cst third-arg-cst)
               (store-vars-cst (concrete-syntax-tree:fourth cst))
               (doc-decl-body-cst-list (concrete-syntax-tree:nthrest 5 cst)) ; List of CSTs
               (current-docstring-cst (when (and doc-decl-body-cst-list
                                                 (concrete-syntax-tree:consp doc-decl-body-cst-list))
                                        (concrete-syntax-tree:first doc-decl-body-cst-list))))

          ;; Parse lambda-list for parameters using the new parser
          (let* ((parsed-ll (if (and lambda-list-cst (concrete-syntax-tree:consp lambda-list-cst))
                                (parse-lambda-list-cst lambda-list-cst :context :ordinary) ; defsetf long form LL is ordinary
                                nil))
                 (detailed-params '()))
            (when parsed-ll
              ;; First, collect all parameter plists for the local 'parameters' variable
              (dolist (p (getf parsed-ll :required)) (push p detailed-params))
              (dolist (p (getf parsed-ll :optionals)) (push p detailed-params))
              (let ((rest-p (getf parsed-ll :rest)))
                (when rest-p (push rest-p detailed-params)))
              (dolist (p (getf parsed-ll :keys)) (push p detailed-params))
              (dolist (p (getf parsed-ll :aux)) (push p detailed-params))
              (setf parameters (nreverse detailed-params)) ;; This 'parameters' is for the defsetf-analysis slot
              (setf (analysis-lambda-info analysis) parsed-ll))

            ;; Now, iterate over the collected parameter details (stored in local 'parameters')
            ;; to apply the new helper functions.
            (dolist (p parameters)
              (process-parameter-lexical-defs p analysis)
              (process-parameter-csts p analysis :ordinary)
              (when (getf p :destructured)
                (process-destructured-parameter p analysis :ordinary))))

          ;; Extract store variables
          (when (and store-vars-cst (concrete-syntax-tree:consp store-vars-cst))
            (setf store-vars (mapcar #'concrete-syntax-tree:raw (cst:listify store-vars-cst)))
            (dolist (sv store-vars)
              (when (symbolp sv)
                (pushnew sv (analysis-lexical-definitions analysis) :test #'eq)))) ; CORRECTED SLOT HERE TOO
          (setf (analysis-store-variables analysis) store-vars)

          ;; Extract docstring and body
          (if (and current-docstring-cst
                   (concrete-syntax-tree:atom current-docstring-cst)
                   (stringp (concrete-syntax-tree:raw current-docstring-cst)))
              (progn
                (setf docstring (concrete-syntax-tree:raw current-docstring-cst))
                (setf raw-body-cst (concrete-syntax-tree:rest doc-decl-body-cst-list))) ; Remaining are body forms
              (setf raw-body-cst doc-decl-body-cst-list))) ; No docstring, all are body/decls

        ;; Short Form: (defsetf access-fn update-fn [doc])
        (let ((possible-doc-cst (concrete-syntax-tree:fourth cst)))
          (if (and possible-doc-cst
                   (concrete-syntax-tree:atom possible-doc-cst)
                   (stringp (concrete-syntax-tree:raw possible-doc-cst)))
              (setf docstring (concrete-syntax-tree:raw possible-doc-cst)))
          ;; For short form, parameters slot remains nil.
          ;; Set raw-body to the update-fn/function-name CST (which is third-arg-cst).
          (setf raw-body-cst third-arg-cst)))

    (setf (analysis-name analysis) name
          (analysis-docstring analysis) docstring
          (analysis-parameters analysis) parameters ; List of parameter-detail for long, nil for short
          (analysis-raw-body analysis) raw-body-cst)

    ;; Walk the relevant parts for gather-info (covers lambda-list, store-vars, body for long form; update-fn for short).
    ;; For short form, it covers update-fn and docstring.
    (let ((rest-of-form (concrete-syntax-tree:nthrest 2 cst)))
      (when rest-of-form
        (walk-cst-with-context rest-of-form
                               (lambda (current-rest-cst path tail)
                                 (declare (ignore path tail))
                                 (gather-info current-rest-cst analysis)))))
    analysis))

;;; Specialized method for ANALYZE-CST for DEFINE-SYMBOL-MACRO forms.
(defmethod analyze-cst (cst (analysis define-symbol-macro-analysis))
  "Analyzes a DEFINE-SYMBOL-MACRO CST to extract its name and expansion.
   Populates the DEFINE-SYMBOL-MACRO-ANALYSIS object."
  (let ((name-cst (concrete-syntax-tree:second cst))
        (expansion-cst (concrete-syntax-tree:third cst))) ; The expansion form
    (setf (analysis-name analysis) (concrete-syntax-tree:raw name-cst)
          (analysis-raw-body analysis) expansion-cst) ; Store expansion as "raw-body"
    ;; Analyze the expansion form
    (when expansion-cst
      (walk-cst-with-context
       expansion-cst
       (lambda (current-expansion-cst path tail)
         (declare (ignore path tail))
         (gather-info current-expansion-cst analysis))))
    analysis))

;;; Specialized method for ANALYZE-CST for DEFCLASS forms.
(defmethod analyze-cst (cst (analysis defclass-analysis))
  "Analyzes a DEFCLASS CST to extract name, superclasses, slots, and options (docstring).
   Populates the DEFCLASS-ANALYSIS object."
  ;; TODO: Parse slot options (e.g., :accessor, :initform, :type) if needed for deeper analysis.
  ;; TODO: Parse other defclass options (e.g., :metaclass).
  (let* ((name-cst (concrete-syntax-tree:second cst))
         (supers-cst (concrete-syntax-tree:third cst))
         (slots-cst (concrete-syntax-tree:fourth cst))
         (options-cst (concrete-syntax-tree:nthrest 4 cst))
         (name (concrete-syntax-tree:raw name-cst))
         (doc nil)
         (supers (when (and supers-cst (concrete-syntax-tree:consp supers-cst))
                   (mapcar #'concrete-syntax-tree:raw
                           (cst:listify supers-cst))))
         ;; Extract just the slot names for now
         (slot-names      (when (and slots-cst (concrete-syntax-tree:consp slots-cst))
                            (mapcar (lambda (slot-def-cst)
                                      (if (concrete-syntax-tree:consp slot-def-cst)
                                          (concrete-syntax-tree:raw
                                           (concrete-syntax-tree:first slot-def-cst))
                                          ;; Slot can be just a name
                                          (concrete-syntax-tree:raw slot-def-cst)))
                                    (cst:listify slots-cst)))))
    ;; Extract docstring from options
    (when (and options-cst (concrete-syntax-tree:consp options-cst))
      (dolist (opt (cst:listify options-cst))
        (when (and (concrete-syntax-tree:consp opt)
                   (eq (concrete-syntax-tree:raw (concrete-syntax-tree:first opt))
                       :documentation))
          (let ((v (concrete-syntax-tree:second opt)))
            (when (and v (concrete-syntax-tree:atom v)
                       (stringp (concrete-syntax-tree:raw v)))
              (setf doc (concrete-syntax-tree:raw v)))))))
    (setf (analysis-name analysis) name
          (analysis-docstring analysis) doc
          (analysis-superclasses analysis) supers
          (analysis-slots analysis) slot-names)
    ;; Analyze slot definitions (e.g., for initforms that might
    ;; contain calls)
    (when (and slots-cst (concrete-syntax-tree:consp slots-cst))
      (dolist (slot-def-cst (cst:listify slots-cst))
        (walk-cst-with-context
         slot-def-cst
         (lambda (current-slot-cst path tail)
           (declare (ignore path tail))
           (gather-info current-slot-cst analysis)))))
    analysis))

;;; Specialized method for ANALYZE-CST for DEFSTRUCT forms.
(defmethod analyze-cst (cst (analysis defstruct-analysis))
  "Analyzes a DEFSTRUCT CST to extract name, options (docstring), and slot definitions.
   Populates the DEFSTRUCT-ANALYSIS object."
  ;; TODO: Parse defstruct options (e.g., :conc-name, :constructor,
  ;; :predicate, :include).

  ;; TODO: Parse slot options and default values.
  (let* (;; Can be name or (name option1 option2...)
         (name-and-options-cst (concrete-syntax-tree:second cst))
         (name (if (concrete-syntax-tree:consp name-and-options-cst)
                   (concrete-syntax-tree:raw
                    (concrete-syntax-tree:first name-and-options-cst))
                   (concrete-syntax-tree:raw name-and-options-cst)))
         ;; Docstring can appear before slots, or as an option if
         ;; name-and-options-cst is a list.  Slots start after
         ;; name-and-options-cst.  If the first element after
         ;; name-and-options-cst is a string, it's the main docstring.
         (docstring-or-first-slot (concrete-syntax-tree:third cst))
         (doc nil)
         (slots-start-index 2)
         (slots-cst nil))

    ;; Handle name and options part
    (if (concrete-syntax-tree:consp name-and-options-cst)
        (progn
          ;; Name and options are together, e.g., (my-struct (:conc-name ms-))

          ;; TODO: Extract options from name-and-options-cst if any
          ;; are docstrings.  For now, assume docstring is separate or
          ;; not in options.

          (setf slots-start-index 2))
        (progn
          ;; Just a name, e.g., my-struct
          (setf slots-start-index 2)))

    ;; Check for main docstring appearing before slot definitions
    (when (and docstring-or-first-slot
               (concrete-syntax-tree:atom docstring-or-first-slot)
               (stringp (concrete-syntax-tree:raw docstring-or-first-slot)))
      (setf doc (concrete-syntax-tree:raw docstring-or-first-slot))
      ;; Slots now start one position later
      (incf slots-start-index))

    (setf slots-cst (concrete-syntax-tree:nthrest slots-start-index cst))

    (setf (analysis-name analysis) name
          (analysis-docstring analysis) doc)
    ;; TODO: Extract slot names and populate (analysis-slots analysis)
    ;; (analysis-slots analysis) (extract-slot-names slots-cst)

    ;; Analyze slot definitions
    (when (and slots-cst (concrete-syntax-tree:consp slots-cst))
      (dolist (slot-def-cst (cst:listify slots-cst))
        (walk-cst-with-context
         slot-def-cst
         (lambda (current-slot-cst path tail)
           (declare (ignore path tail))
           (gather-info current-slot-cst analysis)))))
    analysis))

;;; Specialized method for ANALYZE-CST for DEFINE-CONDITION forms.
(defmethod analyze-cst (cst (analysis define-condition-analysis))
  "Analyzes a DEFINE-CONDITION CST. Extracts name, parent conditions, slots, and options (docstring, report).
   Populates the DEFINE-CONDITION-ANALYSIS object."
  ;; TODO: Parse slot details (initargs, readers, writers) and other options like :report.
  (let* ((name-cst (concrete-syntax-tree:second cst))
         (supers-cst (concrete-syntax-tree:third cst))
         (slots-cst (concrete-syntax-tree:fourth cst))
         (options (concrete-syntax-tree:nthrest 4 cst))
         (doc nil)
         (name (concrete-syntax-tree:raw name-cst))
         (supers (when (and supers-cst (concrete-syntax-tree:consp supers-cst))
                   (mapcar #'concrete-syntax-tree:raw
                           (cst:listify supers-cst))))
         (slot-names (when (and slots-cst (concrete-syntax-tree:consp slots-cst))
                       (mapcar (lambda (slot-def-cst)
                                 (if (concrete-syntax-tree:consp slot-def-cst)
                                     (concrete-syntax-tree:raw
                                      (concrete-syntax-tree:first slot-def-cst))
                                     (concrete-syntax-tree:raw slot-def-cst)))
                               (cst:listify slots-cst)))))
    ;; Extract docstring from options
    (when (and options (concrete-syntax-tree:consp options))
      (dolist (opt (cst:listify options))
        (when (and (concrete-syntax-tree:consp opt)
                   (eq (concrete-syntax-tree:raw (concrete-syntax-tree:first opt))
                       :documentation))
          (let ((v (concrete-syntax-tree:second opt)))
            (when (and v (concrete-syntax-tree:atom v)
                       (stringp (concrete-syntax-tree:raw v)))
              (setf doc (concrete-syntax-tree:raw v)))))))
    (setf (analysis-name analysis) name
          (analysis-docstring analysis) doc
          (analysis-superclasses analysis) supers
          (analysis-slots analysis) slot-names)

    ;; Analyze slot definitions
    (when (and slots-cst (concrete-syntax-tree:consp slots-cst))
      (dolist (slot-def-cst (cst:listify slots-cst))
        (walk-cst-with-context
         slot-def-cst
         (lambda (current-slot-cst path tail)
           (declare (ignore path tail))
           (gather-info current-slot-cst analysis)))))
    analysis))

;;; Specialized method for ANALYZE-CST for DEFPACKAGE forms.
(defmethod analyze-cst (cst (analysis defpackage-analysis))
  "Analyzes a DEFPACKAGE CST to extract package name and various options.
   Populates the DEFPACKAGE-ANALYSIS object."
  ;; TODO: Handle complex options like :shadowing-import-from and
  ;;       :import-from which take package names.  Currently, it just
  ;;       takes the raw list of symbols.
  (let* ((name-cst (concrete-syntax-tree:second cst))
         (options (concrete-syntax-tree:nthrest 2 cst))
         (name (concrete-syntax-tree:raw name-cst)))
    (setf (analysis-name analysis) name
          (analysis-package-name analysis) name)
    (when (and options (concrete-syntax-tree:consp options))

      ;; Each opt-cst is like (:use :cl) or (:export "FOO")
      (dolist (opt-cst (cst:listify options))

        (when (concrete-syntax-tree:consp opt-cst)

          (let* ((key-cst (concrete-syntax-tree:first opt-cst))
                 (key (concrete-syntax-tree:raw key-cst))
                 (vals-cst (concrete-syntax-tree:rest opt-cst)))
            (case key
              (:nicknames
               (setf (analysis-nicknames analysis)
                     (mapcar #'concrete-syntax-tree:raw (cst:listify vals-cst))))
              (:use
               (setf (analysis-uses analysis)
                     (mapcar #'concrete-syntax-tree:raw (cst:listify vals-cst))))
              (:export
               (setf (analysis-exports analysis)
                     (mapcar #'concrete-syntax-tree:raw (cst:listify vals-cst))))
              (:shadow
               (setf (analysis-shadows analysis)
                     (mapcar #'concrete-syntax-tree:raw (cst:listify vals-cst))))
              (:shadowing-import-from
               ;; TODO: This needs to parse the (package-name &rest
               ;; symbols) structure.  For now, storing the raw list.
               (setf (analysis-shadowing-imports analysis)
                     (mapcar #'concrete-syntax-tree:raw (cst:listify vals-cst))))
              (:import-from
               ;; TODO: This needs to parse the (package-name &rest
               ;; symbols) structure.
               (setf (analysis-imports analysis)
                     (mapcar #'concrete-syntax-tree:raw (cst:listify vals-cst))))
              (:intern
               (setf (analysis-interns analysis)
                     (mapcar #'concrete-syntax-tree:raw (cst:listify vals-cst))))
              (:documentation
               (let ((doc-val-cst (concrete-syntax-tree:first vals-cst)))
                 (when (and doc-val-cst
                            (concrete-syntax-tree:atom doc-val-cst)
                            (stringp (concrete-syntax-tree:raw doc-val-cst)))
                   (setf (analysis-docstring analysis)
                         (concrete-syntax-tree:raw doc-val-cst)))))
              (:size
               (setf (analysis-size analysis)
                     (real-raw
                      (concrete-syntax-tree:second opt-cst))))
              (:otherwise
               (error "Unknown depackage option.")))))))

    analysis))

;;;
;;; Additional MAKE-ANALYZER methods for various definition types.
;;; These ensure that the correct specialized analysis class instance is created.
;;;

;;; MAKE-ANALYZER for DEFMETHOD.
(defmethod make-analyzer ((type (eql 'defmethod)))
  (make-instance 'defmethod-analysis))

;;; MAKE-ANALYZER for DEFMACRO.
(defmethod make-analyzer ((type (eql 'defmacro)))
  (make-instance 'defmacro-analysis))

;;; MAKE-ANALYZER for DEFCLASS.
(defmethod make-analyzer ((type (eql 'defclass)))
  (make-instance 'defclass-analysis))

;;; MAKE-ANALYZER for DEFPACKAGE.
(defmethod make-analyzer ((type (eql 'defpackage)))
  (make-instance 'defpackage-analysis))

;;; MAKE-ANALYZER for DEFSTRUCT.
(defmethod make-analyzer ((type (eql 'defstruct)))
  (make-instance 'defstruct-analysis))

;;; MAKE-ANALYZER for DEFINE-CONDITION.
(defmethod make-analyzer ((type (eql 'define-condition)))
  (make-instance 'define-condition-analysis))

;;; MAKE-ANALYZER for DEFTYPE.
(defmethod make-analyzer ((type (eql 'deftype)))
  (make-instance 'deftype-analysis))

;;; MAKE-ANALYZER for DEFSETF.
(defmethod make-analyzer ((type (eql 'defsetf)))
  (make-instance 'defsetf-analysis))

;;; MAKE-ANALYZER for DEFINE-SYMBOL-MACRO.
(defmethod make-analyzer ((type (eql 'define-symbol-macro)))
  (make-instance 'define-symbol-macro-analysis))

;;; MAKE-ANALYZER for DEFPARAMETER.
(defmethod make-analyzer ((type (eql 'defparameter)))
  (make-instance 'defparameter-analysis))

;;; MAKE-ANALYZER for DEFVAR.
;;; Uses 'defparameter-analysis' as it shares similar structure for analysis purposes.
(defmethod make-analyzer ((type (eql 'defvar)))
  (make-instance 'defparameter-analysis))

;;; MAKE-ANALYZER for DEFCONSTANT.
;;; Uses 'defparameter-analysis' as it shares similar structure for analysis purposes.
(defmethod make-analyzer ((type (eql 'defconstant)))
  (make-instance 'defparameter-analysis))

(defmethod make-analyzer ((type (eql 'defgeneric)))
  (make-instance 'defgeneric-analysis))
