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
               :documentation "A simple list of parameter names, extracted from the lambda list.")
   (docstring :accessor analysis-docstring
              :initform nil
              :documentation "The documentation string of the function, if present.")))

;;; Analysis class for DEFMETHOD forms.
(defclass defmethod-analysis (analysis)
  ((parameters :accessor analysis-parameters
               :initform nil
               :documentation "A simple list of parameter names, extracted from the method's specialized lambda list.")
   (docstring :accessor analysis-docstring
              :initform nil
              :documentation "The documentation string of the method, if present.")))

;;; Analysis class for DEFINE-CONDITION forms.
(defclass define-condition-analysis (analysis)
  ((docstring :accessor analysis-docstring
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
  ((parameters :accessor analysis-parameters
               :initform nil
               :documentation "A simple list of parameter names, extracted from the macro's lambda list.")
   (docstring :accessor analysis-docstring
              :initform nil
              :documentation "The documentation string of the macro, if present.")))

;;; Analysis class for DEFTYPE forms.
(defclass deftype-analysis (analysis)
  ((parameters :accessor analysis-parameters
               :initform nil
               :documentation "A simple list of parameter names, extracted from the type's lambda list.")
   (docstring :accessor analysis-docstring
              :initform nil
              :documentation "The documentation string of the type definition, if present.")))

;;; Analysis class for DEFGENERIC forms.
(defclass defgeneric-analysis (analysis)
  ((parameters :accessor analysis-parameters
               :initform nil
               :documentation "A simple list of parameter names, extracted from the generic function's lambda list.")
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
  ((parameters :accessor analysis-parameters
               :initform nil
               :documentation "A simple list of parameter names. TODO: Verify how parameters are best represented for DEFSETF.")
   (docstring :accessor analysis-docstring
              :initform nil
              :documentation "The documentation string of the setf definition, if present.")))

;;; Analysis class for DEFINE-SYMBOL-MACRO forms.
;; No specific slots beyond the base 'analysis' class yet.
;; TODO: Consider adding a slot for the expansion if needed for analysis.
(defclass define-symbol-macro-analysis (analysis)
  ())

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
   (other-options
    :initform nil
    :accessor analysis-other-options
    :documentation "A list of any other DEFPACKAGE options encountered.")))

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

;;; Extracts a simple list of parameter symbols from a lambda list
;;; CST. Does not include specializers, default values, or &keywords.
(defun simple-lambda-params (args-cst)
  "Return a list of parameter symbols from ARGS-CST (a CST of a lambda list).
   This provides a simplified view, excluding &keywords, default values, and specializers."
  ;; TODO: This function could be more robust in handling complex
  ;;       lambda list features if a more detailed (but still
  ;;       simplified) parameter list is needed.  Currently, it's good
  ;;       for a basic list of variable names.
  (when (concrete-syntax-tree:consp args-cst)
    (let ((lambda-list (mapcar #'concrete-syntax-tree:raw
                               ;; Convert CST list to Lisp list of raw items
                               (cst:listify args-cst)))
          (params '()))
      (dolist (item lambda-list (nreverse params))
        (cond
          ;; Standard parameter symbol (not starting with '&')
          ((and (symbolp item)
                (not (char= (char (symbol-name item) 0) #\&)))
           (push item params))
          ;; Parameter that is a list (e.g., for destructuring or
          ;; specialized parameters) We take the first element if it's
          ;; a symbol (e.g., (VAR TYPE) -> VAR)
          ((and (consp item) (symbolp (car item)))
           ;; TODO: Handle other lambda list keywords like &optional,
           ;;       &key, &rest, &aux explicitly if needed.  Currently,
           ;;       they are filtered out by the (char= #\&) check or
           ;;       not handled by (consp item).
           (push (car item) params)))))))

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
         ;; Parse the lambda list using Alexandria for detailed info
        (lambda-list   (and args-cst
                             (concrete-syntax-tree:consp args-cst)
                             (mapcar #'concrete-syntax-tree:raw
                                     (cst:listify args-cst))))
        required optionals rest-name keywords allow-other-p auxes)
    ;; Parse the lambda list when present.  PARSE-ORDINARY-LAMBDA-LIST
    ;; returns multiple values rather than a list, so capture them with
    ;; MULTIPLE-VALUE-BIND.
    (when lambda-list
      (multiple-value-setq (required optionals rest-name keywords allow-other-p auxes)
        (alexandria:parse-ordinary-lambda-list
         lambda-list
         ;; Standardize lambda list keywords
         :normalize t
         ;; For generic function parameter lists (though this is defun)
         :allow-specializers t
         ;; Normalize (opt x) to (opt x nil opt-p)
         :normalize-optional t
         ;; Normalize (key ((:foo x))) to (key ((:foo x) nil foo-p))
         :normalize-keyword t
         ;; Normalize &aux (x y) to &aux (x nil) (y nil)
         :normalize-auxilary t)))

    ;; Populate analysis slots
    (setf (analysis-name analysis)      name
          ;; (analysis-kind analysis) is set by :around method, but :defun is more specific.
          (analysis-kind analysis)      'defun ; Explicitly set to :defun symbol
          (analysis-docstring analysis) doc
          (analysis-raw-body analysis)  body-cst
          ;; Populate simple parameters list
          (analysis-parameters analysis)
          (append required
                  ;; Just the names from optionals
                  (mapcar #'car optionals)
                  (when rest-name
                    (list rest-name))
                  ;; Just the names from ((kw var) init sp)
                  (mapcar (lambda (x)
                            (cadr (car x)))
                          keywords)
                  ;; Just the names from aux
                  (mapcar #'car auxes)))
    ;; Store detailed lambda list information
    (setf (analysis-lambda-info analysis)
          (list :required required
                :optionals optionals
                :rest rest-name
                :keywords keywords
                :allow-other-keys allow-other-p
                :auxes auxes))
    ;; Record lexical definitions for each parameter symbol
    (dolist (p (analysis-parameters analysis))
      (when (symbolp p)
        (pushnew p (analysis-lexical-definitions analysis) :test #'eq)))

    ;; Walk the body for calls, uses, assignments, etc.
    (when body-cst
      (walk-cst-with-context
       body-cst
       (lambda (current-body-cst path tail)
         (declare (ignore path tail))
         (gather-info current-body-cst analysis))))
    analysis))

;;; Specialized method for ANALYZE-CST for DEFMACRO forms.
(defmethod analyze-cst (cst (analysis defmacro-analysis))
  "Analyzes a DEFMACRO CST to extract name, arguments, docstring, and body.
   Populates the DEFMACRO-ANALYSIS object."
  ;; TODO: Error handling for malformed DEFMACRO CSTs.

  ;; TODO: Macro lambda lists can be complex
  ;;       (destructuring). `simple-lambda-params` might be too
  ;;       simple.  Consider using alexandria:parse-macro-lambda-list
  ;;       for more detailed parsing if needed.
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
         (params       (simple-lambda-params args-cst)))
    (setf (analysis-name analysis)      name
          (analysis-kind analysis)      'defmacro ; Explicitly set to :defmacro
          (analysis-docstring analysis) doc
          (analysis-raw-body analysis)  body-cst
          (analysis-parameters analysis) params)
    ;; Record lexical definitions for parameters
    (dolist (p params)
      (when (symbolp p)
        (pushnew p (analysis-lexical-definitions analysis) :test #'eq)))
    ;; Analyze the macro body
    (when body-cst
      (walk-cst-with-context
       body-cst
       (lambda (current-body-cst path tail)
         (declare (ignore path tail))
         (gather-info current-body-cst analysis))))
    analysis))

;;; Specialized method for ANALYZE-CST for DEFMETHOD forms.
(defmethod analyze-cst (cst (analysis defmethod-analysis))
  "Analyzes a DEFMETHOD CST to extract name, qualifiers, specialized lambda list, docstring, and body.
   Populates the DEFMETHOD-ANALYSIS object."
  ;; TODO: Handle method qualifiers more robustly.

  ;; TODO: Specialized lambda lists can be
  ;;       complex. `simple-lambda-params` might need enhancement or
  ;;       replacement with a parser that understands method parameter
  ;;       specializers.
  (let* (;; Name or (SETF name)
         (name-cst (concrete-syntax-tree:second cst))
         ;; Start looking for qualifiers or args list from the 3rd element (index 2)
         (index 2)
         (part     (concrete-syntax-tree:nth index cst)))
    ;; Skip qualifiers (keywords like :before, :after, :around)
    (loop while (and part
                     (concrete-syntax-tree:atom part)
                     (keywordp (concrete-syntax-tree:raw part)))
          do (incf index)
             (setf part (concrete-syntax-tree:nth index cst)))
    ;; After qualifiers (if any), 'part' should be the args-cst
    (let* ((args-cst     part)
           (next-idx (+ index 1))
           (possible-doc (concrete-syntax-tree:nth next-idx cst))
           (doc          (when (and possible-doc
                                    (concrete-syntax-tree:atom possible-doc)
                                    (stringp (concrete-syntax-tree:raw possible-doc)))
                           (concrete-syntax-tree:raw possible-doc)))
           (body-cst     (if doc
                             (concrete-syntax-tree:nthrest (+ next-idx 1) cst)
                             (concrete-syntax-tree:nthrest next-idx cst)))
           (name         (real-raw name-cst)) ; Handles (SETF name) correctly
           (params       (simple-lambda-params args-cst)))
      (setf (analysis-name analysis) name
            (analysis-kind analysis) 'defmethod ; Explicitly set
            (analysis-docstring analysis) doc
            (analysis-raw-body analysis) body-cst
            (analysis-parameters analysis) params)
      ;; Record lexical definitions for parameters
      (dolist (p params)
        (when (symbolp p)
          (pushnew p (analysis-lexical-definitions analysis) :test #'eq)))
      ;; Analyze the method body
      (when body-cst
        (walk-cst-with-context
         body-cst
         (lambda (current-body-cst path tail)
           (declare (ignore path tail))
           (gather-info current-body-cst analysis))))
      analysis)))

;;; Specialized method for ANALYZE-CST for DEFTYPE forms.
(defmethod analyze-cst (cst (analysis deftype-analysis))
  "Analyzes a DEFTYPE CST to extract name, lambda list, docstring, and body.
   Populates the DEFTYPE-ANALYSIS object."
  ;; TODO: Deftype lambda lists can be complex
  ;; (destructuring). `simple-lambda-params` might be too simple.
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
         (params       (simple-lambda-params args-cst)))
    (setf (analysis-name analysis)      name
          (analysis-kind analysis)      'deftype
          (analysis-docstring analysis) doc
          (analysis-raw-body analysis)  body-cst
          (analysis-parameters analysis) params)
    ;; Record lexical definitions for parameters
    (dolist (p params)
      (when (symbolp p)
        (pushnew p (analysis-lexical-definitions analysis) :test #'eq)))
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
         (options  (concrete-syntax-tree:nthrest 3 cst)) ; Options start after name and args
         (name     (concrete-syntax-tree:raw name-cst))
         (params   (simple-lambda-params args-cst))
         (doc      nil))
    ;; Extract docstring from options
    (when (and options (concrete-syntax-tree:consp options))
      (dolist (opt (cst:listify options))
        ;; Option should be a list like (:documentation "...")
        (when (and (concrete-syntax-tree:consp opt)
                   (eq (concrete-syntax-tree:raw (concrete-syntax-tree:first opt))
                       :documentation))
          (let ((v (concrete-syntax-tree:second opt))) ; The docstring CST itself
            (when (and v (concrete-syntax-tree:atom v)
                       (stringp (concrete-syntax-tree:raw v)))
              (setf doc (concrete-syntax-tree:raw v)))))))
    (setf (analysis-name analysis) name
          (analysis-kind analysis) 'defgeneric
          (analysis-docstring analysis) doc
          (analysis-parameters analysis) params)
    ;; Record lexical definitions for parameters
    (dolist (p params)
      (when (symbolp p)
        (pushnew p (analysis-lexical-definitions analysis) :test #'eq)))
    ;; Defgeneric doesn't have a "body" in the same way defun does,
    ;; but options might contain expressions.

    ;; However, typical analysis focuses on signature and docstring.
    ;; If options need deeper analysis, a walk could be added here.
    analysis))

;;; Specialized method for ANALYZE-CST for DEFPARAMETER, DEFVAR,
;;; DEFCONSTANT forms.
(defmethod analyze-cst (cst (analysis defparameter-analysis))
  "Analyzes a DEFPARAMETER/DEFVAR/DEFCONSTANT CST to extract name, initial value, and docstring.
   Populates the DEFPARAMETER-ANALYSIS object."
  ;; TODO: Distinguish between defparameter, defvar, defconstant if
  ;;       needed (currently uses analysis-kind set by :around).  The
  ;;       :around method sets it to 'defparameter, 'defvar, or
  ;;       'defconstant based on the cst head.
  (let* ((name-cst (concrete-syntax-tree:second cst))
         (init-cst (concrete-syntax-tree:third cst))
         (possible-doc (concrete-syntax-tree:fourth cst))
         (doc (when (and possible-doc
                         (concrete-syntax-tree:atom possible-doc)
                         (stringp (concrete-syntax-tree:raw possible-doc)))
                (concrete-syntax-tree:raw possible-doc)))
         (name (concrete-syntax-tree:raw name-cst)))
    (setf (analysis-name analysis) name
          ;; Kind is set by :around method, e.g. 'defparameter,
          ;; 'defvar, 'defconstant (analysis-kind analysis) ; No need
          ;; to set it here, already set.
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

;;; Specialized method for ANALYZE-CST for DEFSETF forms.
(defmethod analyze-cst (cst (analysis defsetf-analysis))
  "Analyzes a DEFSETF CST. Extracts name, parameters (for short form), and docstring.
   Populates the DEFSETF-ANALYSIS object."
  ;; TODO: DEFSETF has two forms (short and long). This currently
  ;;       seems to handle the short form's access-fn and potentially
  ;;       a docstring. The long form (with lambda list, store vars,
  ;;       body) would require more complex parsing for `parameters`
  ;;       and `body`.  `simple-lambda-params` might not be
  ;;       appropriate for the long form's lambda list.
  (let* (;; Access function name or (SETF place-access-fn)
         (name-cst (concrete-syntax-tree:second cst))
         ;; For short form, rest is (update-fn [documentation]) For
         ;; long form, rest is (lambda-list (store-variables)
         ;; body... [documentation])
         (rest(concrete-syntax-tree:nthrest 2 cst))
         (doc nil))
    ;; Attempt to find docstring, which is usually the last
    ;; string literal if present.  This is a simplification,
    ;; proper parsing of long form is needed.)  A simple attempt
    ;; to get docstring if it's the first element of 'rest' and a
    ;; string This is more typical for the short form: (defsetf
    ;; access-fn update-fn [doc]) Or if it's the *last* string
    ;; literal in the long form.  For now, this only checks the
    ;; first string in `rest`.
    (when (and rest (concrete-syntax-tree:consp rest))
      (let ((first-in-rest (concrete-syntax-tree:first rest)))
        (if (and first-in-rest
                 (concrete-syntax-tree:atom first-in-rest)
                 (stringp (concrete-syntax-tree:raw first-in-rest)))
            ;; If long form, docstring might be after the
            ;; body. This is harder to get simply.

            ;;TODO: Improve docstring extraction for long form of
            ;; defsetf.
            (setf doc (concrete-syntax-tree:raw first-in-rest)))))

    (setf (analysis-name analysis) (real-raw name-cst)
          (analysis-kind analysis) :defsetf
          (analysis-docstring analysis) doc)

    ;; TODO: Populate `analysis-parameters` correctly for both short and long forms.
    ;; For short form, there aren't "parameters" in the usual sense.
    ;; For long form, it's the lambda-list.
    ;; (analysis-parameters analysis) ...
    ;; (analysis-raw-body analysis) ... ; body for long form

    ;; Analyze the rest of the defsetf form (update function, or
    ;; lambda list + body

    (when rest
      (walk-cst-with-context
       rest
       (lambda (current-rest-cst path tail)
         (declare (ignore path tail))
         (gather-info current-rest-cst analysis))))
    analysis))

;;; Specialized method for ANALYZE-CST for DEFINE-SYMBOL-MACRO forms.
(defmethod analyze-cst (cst (analysis define-symbol-macro-analysis))
  "Analyzes a DEFINE-SYMBOL-MACRO CST to extract its name and expansion.
   Populates the DEFINE-SYMBOL-MACRO-ANALYSIS object."
  (let ((name-cst (concrete-syntax-tree:second cst))
        (expansion-cst (concrete-syntax-tree:third cst))) ; The expansion form
    (setf (analysis-name analysis) (concrete-syntax-tree:raw name-cst)
          (analysis-kind analysis) :define-symbol-macro
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
          (analysis-kind analysis) :defclass
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
          (analysis-kind analysis) :defstruct
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
                           (cst:listify supers-cst)))))
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
          (analysis-kind analysis) :define-condition
          (analysis-docstring analysis) doc
          ;; TODO: This should be analysis-superclasses, not
          ;;       analysis-superclasses analysis (typo) Assuming
          ;;       'define-condition-analysis' has a 'superclasses'
          ;;       slot similar to 'defclass-analysis'.  If not, this
          ;;       slot needs to be added or handled differently.  For
          ;;       now, let's assume it should be
          ;;       (analysis-superclasses analysis) supers.
          ;;       Correcting based on defclass-analysis structure.
          (analysis-superclasses analysis) supers)
    ;; TODO: Populate (analysis-slots analysis) from slots-cst.

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
          (analysis-kind analysis) :defpackage
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
              ;; TODO: Handle :size if important.
              (t (push (real-raw opt-cst) (analysis-other-options analysis))))))))
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
