;;; utils.lisp
;;;
;;; This file contains various utility functions used throughout the
;;; cl-naive-code-analyzer system. These include functions for symbol
;;; manipulation, name normalization, list handling, text position mapping,
;;; reader macro normalization, and a custom tracking stream for parsing.
;;;
;;; TODO: Review the usage of `*form-analyzers*`; it's defined but not used in this file.
;;;       If it's intended for extensible form analysis, its usage should be implemented or clarified.
;;; TODO: The `tracking-stream`'s logic for `last-form-start` and `last-form-end`
;;;       seems specific and might not correctly capture all form boundaries, especially
;;;       with nested forms or non-list forms. This needs careful review if these slots are critical.

(in-package :cl-naive-code-analyzer)

;; A hash table presumably for storing custom analyzers for specific forms.
;; TODO: This variable is defined but not used within this file or apparently by core analyzer logic.
;;       If it's part of a planned feature for extensible analyzers, it needs to be integrated.
(defparameter *form-analyzers* (make-hash-table :test 'equal)
  "A hash table to potentially store custom form analyzer functions. Currently unused.")

;; Safely exports a symbol or a compound symbol name (like `(SETF FOO)`)
;; into a plist representation `(:name "NAME" :package "PACKAGE")`.
(defun export-symbol (sym &optional (default-package nil))
  "Serializes SYM into a plist with :name and :package.
   Handles symbols, strings (as names), and compound names like (SETF FOO).
   DEFAULT-PACKAGE is used if SYM is a string or an uninterned/unpackaged symbol.
   Returns a plist like (:name \"SYM-NAME\" :package \"PACKAGE-NAME\") or
   (:name (\"SETF\" \"FOO\") :package \"PACKAGE-NAME\") for compound names."
  ;; TODO: Clarify behavior for gensyms or symbols with truly no package.
  ;;       The :|<uninterned>| keyword might be a placeholder.
  (cond
    ;; Compound name, e.g., (SETF FOO)
    ((and (listp sym) (every #'symbolp sym)) ; Ensure all elements are symbols for (SETF FOO) like structures
     (let ((item-names (mapcar #'symbol-name sym)))
       `(:name ,item-names ; Store as a list of strings
         :package ,(or (and default-package
                            (etypecase default-package
                              (string default-package)
                              (symbol (string default-package))
                              (package (package-name default-package))))
                       ;; Try to get package from the primary symbol if it's a common (setf g) form
                       (when (and (second sym) (symbolp (second sym)) (symbol-package (second sym)))
                         (package-name (symbol-package (second sym))))
                       ;; Fallback for cases like (SETF |foo|) where |foo| might be uninterned.
                       (when (symbolp (car sym)) (package-name (symbol-package (car sym)))) ; Or from the operator like SETF
                       :|<uninterned>|)))) ; Placeholder if package cannot be determined
    ;; Single symbol
    ((symbolp sym)
     `(:name ,(symbol-name sym)
       :package ,(or (when (symbol-package sym) (package-name (symbol-package sym)))
                     (and default-package
                          (etypecase default-package
                            (string default-package)
                            (symbol (string default-package))
                            (package (package-name default-package))))
                     :|<uninterned>|))) ; Placeholder for uninterned/unpackaged symbols
    ;; String literal (treated as a name in the default or common-lisp package)
    ((stringp sym)
     `(:name ,sym
       :package ,(string (or default-package "COMMON-LISP")))) ; Default to "COMMON-LISP" if no default provided
    ;; Other types are not handled
    (t
     (error "Cannot export symbol representation for: ~S. Expected symbol, list of symbols, or string." sym))))

;; Normalizes the name part of a form, typically the second element (e.g., function name in DEFUN).
;; TODO: This function seems unused. `normalize-name` is used directly elsewhere.
;;       If this is a specific helper, its use case should be clear or it might be redundant.
(defun safe-normalize-name (form)
  "Extracts and normalizes the name from a definition form (e.g., (defun my-fun ...)).
   Returns the lowercased string name, or NIL if the form is not as expected."
  (when (and (consp form)      ; Must be a cons
             (cdr form)        ; Must have at least two elements
             (symbolp (second form))) ; The second element (name) must be a symbol
    (normalize-name (second form))))

;; Normalizes a symbol or string to a lowercased string representation.
;; If input is a cons (e.g. (SETF FOO)), it normalizes the CAR of the cons.
(defun normalize-name (x)
  "Converts X (a symbol, string, or cons like (SETF FOO)) to a lowercase string.
   For (SETF FOO), it normalizes 'SETF."
  ;; TODO: For (SETF FOO), this normalizes "setf". If "foo" is intended, logic needs change.
  ;;       The current behavior might be for identifying the defining operator.
  (if (stringp x)
      x ; Already a string, return as is (or consider downcasing if consistency is key)
      (string-downcase (if (consp x) ; If a list like (SETF FOO)
                           (symbol-name (car x)) ; Normalize the operator part (e.g., SETF)
                           (symbol-name x))))) ; If a symbol, normalize it

;; Normalizes a list of symbols or strings into a list of lowercased strings.
(defun normalize-symbol-list (lst)
  "Applies `normalize-name` to each element of LST."
  (mapcar #'normalize-name lst))

;; Ensures the input X is a list. If X is NIL, returns NIL (empty list).
;; If X is already a list, returns X. Otherwise, wraps X in a list.
(defun ensure-list (x)
  "Returns X if it's a list, (LIST X) otherwise. NIL becomes an empty list."
  ;; TODO: `(null x)` results in `'()`. If `x` is `nil` and you want `(nil)`, this needs adjustment.
  ;;       Common Lisp typically treats `nil` as the empty list, so this is standard.
  (cond
    ((null x) '())      ; NIL is already the empty list
    ((listp x) x)       ; Already a list
    (t (list x))))      ; Wrap atom in a list

;; Creates a mapping from character offset in a text to its line number.
;; The map is an array where index is offset and value is line number (1-based).
(defun offset-to-line-map (text)
  "Generates an array mapping character offsets in TEXT to 1-based line numbers."
  (let* ((length (length text))
         ;; Initialize with 0, as line numbers are 1-based.
         ;; Using (unsigned-byte 32) or similar if line numbers can exceed 255.
         (line-map (make-array length :element-type '(unsigned-byte 32) :initial-element 1))
         (line 1))
    (loop for i from 0 below length
          do (progn
               (setf (aref line-map i) line)
               (when (char= (char text i) #\Newline)
                 (incf line))))
    line-map))

;; Converts a character offset to a line number using a precomputed line map.
(defun offset-to-line (offset line-map)
  "Converts a character OFFSET to a 1-based line number using LINE-MAP
   (generated by `offset-to-line-map`)."
  ;; TODO: Add bounds checking for `offset` against `line-map` length for safety.
  (if (and (>= offset 0) (< offset (length line-map)))
      (aref line-map offset)
      (error "Offset ~A is out of bounds for the provided line-map." offset)))

;; Recursively normalizes common reader macro forms (quasiquote, unquote, unquote-splicing)
;; read by Eclector into their standard Lisp equivalents (backquote, comma, comma-at).
;; This helps in producing more standard Lisp code representation.
(defun normalize-reader-macros (form)
  "Recursively transforms Eclector's reader macro representations
   (e.g., ECLECTOR.READER:QUASIQUOTE) into standard Lisp forms (e.g., `(QUASIQUOTE ...)`)."
  ;; TODO: This function is destructive for dotted lists in the `finally` clause of the loop.
  ;;       `rplacd (last form) ...` mutates the list structure. Consider a non-destructive version
  ;;       if shared structure or original form integrity is important.
  ;; TODO: Ensure this handles all relevant Eclector reader macro outputs.
  (cond
    ((consp form)
     (let ((head (car form)))
       (cond
         ;; Normalize `(eclector.reader:quasiquote <form>)` to `(quasiquote <form>)`
         ((eq head 'eclector.reader:quasiquote)
          `(quasiquote ,(normalize-reader-macros (second form)))) ; Using backquote for clarity
         ;; Normalize `(eclector.reader:unquote <form>)` to `(unquote <form>)`
         ((eq head 'eclector.reader:unquote)
          `(unquote ,(normalize-reader-macros (second form)))) ; Using backquote for clarity
         ;; Normalize `(eclector.reader:unquote-splicing <form>)` to `(unquote-splicing <form>)`
         ((eq head 'eclector.reader:unquote-splicing)
          `(unquote-splicing ,(normalize-reader-macros (second form)))) ; Using backquote for clarity
         ;; For other conses, recursively normalize elements.
         ;; The original loop was complex and potentially buggy for dotted lists.
         ;; A simpler mapcar approach is used here for proper lists.
         ;; Dotted list handling needs to be non-destructive if `rplacd` is avoided.
         (t (if (proper-list-p form)
                (mapcar #'normalize-reader-macros form)
                ;; Handle improper lists: normalize car, keep cdr as is (or normalize cdr if cons)
                (cons (normalize-reader-macros (car form))
                      (if (consp (cdr form))
                          (normalize-reader-macros (cdr form))
                          (cdr form))))))))
    ;; Atomic forms are returned as is.
    (t form)))

;; Checks if a given form is likely a definition form (e.g., defun, defclass).
;; Returns :cl for standard CL definitions, :possible-user for user-defined macros, or NIL.
(defun form-is-definition-p (form)
  "Determines if FORM is likely a definition form.
   Returns :CL for known Common Lisp defining forms,
   :POSSIBLE-USER if it's a macro call (could be a user-defined definer),
   or NIL otherwise."
  ;; TODO: This list is not exhaustive (e.g., defpackage, defgeneric, defsetf).
  ;;       Consider expanding or making it configurable.
  (when (consp form) ; Must be a cons to be a form like (defun ...)
    (let ((head (car form)))
      (cond
        ((member head '(defun defmethod defmacro defclass defstruct deftype defparameter defvar defconstant define-condition defpackage defgeneric defsetf))
         :cl) ; Known Common Lisp definition forms
        ;; Special case for (defmethod (setf name) ...)
        ((and (eq head 'defmethod)
              (consp (second form))
              (eq (car (second form)) 'setf))
         :cl)
        ;; If the head is a symbol and has a macro function, it might be a user-defined defining macro.
        ((and (symbolp head) (macro-function head))
         :possible-user) ; A guess, could be any macro
        (t nil))))) ; Not recognized as a definition form

;; A custom stream that wraps an underlying stream to track character position
;; and potentially information about the last form read.
(defclass tracking-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((underlying :initarg :underlying :accessor tracking-stream-underlying
               :documentation "The actual stream being read from.")
   (position :initform 0 :accessor tracking-stream-position
             :documentation "Current character offset from the beginning of the stream.")
   ;; TODO: `last-form-start` and `last-form-end` are set but not obviously used by the analyzer.
   ;;       Their logic for identifying form boundaries by parentheses is simplistic and
   ;;       might not be robust for all Lisp syntax.
   (last-form-start :initform nil :accessor tracking-stream-last-form-start
                    :documentation "Attempt to record start position of the last form based on '('.")
   (last-form-end :initform nil :accessor tracking-stream-last-form-end
                  :documentation "Attempt to record end position of the last form based on ')'.")
   (last-char :initform nil :accessor tracking-stream-last-char
              :documentation "The last character read from the stream.")))

;; Method for reading a character from the tracking stream.
;; Updates position and attempts to track form start/end based on parentheses.
(defmethod trivial-gray-streams:stream-read-char ((s tracking-stream))
  (let ((char (read-char (tracking-stream-underlying s) nil :eof)))
    (unless (eq char :eof)
      ;; Simplistic tracking of form start/end based on parentheses.
      ;; This is likely not robust for general Lisp parsing.
      ;; TODO: Review necessity and correctness of last-form-start/end logic.
      (when (and (char= char #\()
                 (null (tracking-stream-last-form-start s))) ; Only set if not already in a "form"
        (setf (tracking-stream-last-form-start s) (tracking-stream-position s)))

      (when (char= char #\))
        ;; This sets end for any ')', not necessarily matching the start.
        (setf (tracking-stream-last-form-end s) (1+ (tracking-stream-position s))))

      (incf (tracking-stream-position s))
      (setf (tracking-stream-last-char s) char))
    char))

;; Method for unreading a character; decrements position and unreads from underlying stream.
(defmethod trivial-gray-streams:stream-unread-char ((stream tracking-stream) character)
  (decf (tracking-stream-position stream))
  (unread-char character (tracking-stream-underlying stream)))

;; Method for peeking a character from the underlying stream without advancing position.
(defmethod trivial-gray-streams:stream-peek-char ((stream tracking-stream))
  (peek-char nil (tracking-stream-underlying stream) nil :eof nil)) ; Changed to handle :eof

;; Method to check if there's input available on the underlying stream.
(defmethod trivial-gray-streams:stream-listen ((stream tracking-stream))
  (listen (tracking-stream-underlying stream)))

;; Method for non-blocking character read; updates position if a character is read.
(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream tracking-stream))
  (let ((char (read-char-no-hang (tracking-stream-underlying stream) nil :eof)))
    (unless (or (null char) (eq char :eof)) ; Character was read
      (incf (tracking-stream-position stream))
      ;; TODO: Update last-char and potentially form start/end similar to stream-read-char if needed.
      )
    char))

;; Method for getting line column; currently returns NIL (not implemented).
;; TODO: Implement if line/column tracking per form is needed beyond start line.
(defmethod trivial-gray-streams:stream-line-column ((stream tracking-stream))
  nil) ; This would require more complex tracking of newlines.

;; Method for clearing input buffer; currently does nothing (returns NIL).
;; TODO: Implement if needed, though typically not for basic file reading.
(defmethod trivial-gray-streams:stream-clear-input ((stream tracking-stream))
  nil)
