;;; cl-naive-code-analyzer.lisp
;;;
;;; This file contains the core logic for the Naive Code Analyzer.
;;; It defines classes for representing code files, projects, and analysis results.
;;; It also includes the main functions for parsing and analyzing Lisp code files
;;; using Eclector and storing the analysis results.
;;;
;;; TODO: Review the overall structure and organization for clarity and maintainability.
;;; TODO: Add more comprehensive error handling and reporting.

(in-package :cl-naive-code-analyzer)

;;; Represents a single source code file.
(defclass code-file ()
  ((path :accessor file-path
         :initarg
         :path
         :documentation "The absolute path to the source file.")
   (analyses :accessor file-analyses
             :initform nil
             :documentation "A list of 'analysis' objects, each representing a top-level form found in the file.")))

;;; Represents a collection of code files, typically a Lisp system or project.
(defclass code-project ()
  ((name :accessor project-name
         :initarg :name
         :initform nil
         :documentation "A symbolic name for the project being analyzed (e.g., the ASDF system name).")
   (files :accessor project-files
          :initarg :files
          :initform nil
          :documentation "A list of 'code-file' objects that constitute this project.")))

;;; Base class for storing analysis results of a Lisp form.  Specific
;;; types of forms (defun, defclass, etc.) will have specialized
;;; subclasses.
(defclass analysis ()
  ((name :accessor analysis-name
         :initarg :name
         :initform nil
         :documentation "The name of the analyzed form (e.g., function name, class name).")
   (kind :accessor analysis-kind
         :initarg :kind
         :initform nil
         :documentation "The type of the form (e.g., 'defun, 'defclass).")
   (cst :accessor analysis-cst
        :initarg :form
        :initform nil
        :documentation "The Concrete Syntax Tree (CST) of the form.")
   (start :accessor analysis-start
          :initarg :start
          :initform nil
          :documentation "The starting character offset of the form in the source file.")
   (end :accessor analysis-end
        :initarg :end
        :initform nil
        :documentation "The ending character offset of the form in the source file.")
   (line :accessor analysis-line
         :initarg :line
         :initform nil
         :documentation "The starting line number of the form in the source file.")
   (package :accessor analysis-package
            :initarg :package
            :initform (find-package :cl)
            :documentation "The package in which the form was read.")
   (function-calls :accessor analysis-function-calls
                   :initform nil
                   :documentation "A list of global functions called within this form.")
   (macro-calls :accessor analysis-macro-calls
                :initform nil
                :documentation "A list of global macros called within this form.")
   (variable-uses :accessor analysis-variable-uses
                  :initform nil
                  :documentation "A list of global variables referenced within this form. TODO: Clarify if special vars only.")
   ;; TODO: Implement.
   (local-function-calls :accessor analysis-local-function-calls
                         :initform nil
                         :documentation "A list of locally defined functions (labels/flet) called. TODO: Implement.")
   ;; TODO: Implement.
   (local-variable-uses :accessor analysis-local-variable-uses
                        :initform nil
                        :documentation "A list of lexically bound variables referenced.")
   (lexical-definitions :accessor analysis-lexical-definitions
                        :initform nil
                        :documentation "A list of symbols defined lexically within this form (e.g., parameters, let-bound vars).")
   ;; TODO: Implement.
   (dynamic-definitions :accessor analysis-dynamic-definitions
                        :initform nil
                        :documentation "A list of symbols defined dynamically (e.g., special variables declared).")
   (raw-body :accessor analysis-raw-body
             :initform nil
             :documentation "The raw body of the form, typically a CST or list of CSTs. For definitions, this is the code after name, lambda-list, docstring.")))

;;; Custom Eclector client to hook into the reading process. Used to
;;; capture CSTs and their positions, and manage package context.
(defclass analyzer-client (eclector.concrete-syntax-tree:cst-client)
  ((package :accessor client-package
            :initarg :package
            :initform (find-package :cl)
            :documentation "The current package context for the reader.")))

;;; Method to customize symbol interpretation during reading.  Handles
;;; package qualifications and uninterned symbols.
(defmethod eclector.reader:interpret-symbol ((client analyzer-client)
                                             input-stream package-name
                                             symbol-name internp)
  "Handles symbol interpretation, respecting package context and interning rules.
   Ensures symbols are resolved or created in the correct package based on `client-package`."
  ;; TODO: Review logic for :current package handling, ensure it
  ;; aligns with CL expectations.
  (cond
    ;; #:foo => make-symbol (uninterned symbol)
    ((null package-name)
     (make-symbol symbol-name))
    ;; Qualified or current package symbol
    (t
     (let ((pkg (cond
                  ((eq package-name :current)
                   (or (client-package client)
                       ;; Safer fallback if client-package is nil
                       (find-package :cl-user)))
                  ((eq package-name :keyword) (find-package :keyword))
                  (t (or (find-package package-name)
                         (error "No package named ~a" package-name))))))
       (multiple-value-bind (sym status) (find-symbol symbol-name pkg)
         (cond
           ;; Symbol found in the package, use it.  If
           (status sym)
           ;; internp is true and not in CL package (to avoid
           ;; interning new symbols there by mistake)
           ((and internp (not (eq pkg (find-package "COMMON-LISP"))))
            (intern symbol-name pkg))
           ;; Otherwise (not internp or in CL package and not found),
           ;; create an uninterned symbol.  This typically happens for
           ;; symbols that are not meant to be interned or are being
           ;; defined.
           (t (make-symbol symbol-name))))))))

;;; Method to customize character name lookup during reading (e.g.,
;;; #\Newline).
(defmethod eclector.reader:find-character ((client analyzer-client) (name string))
  "Finds a character by its name, extending standard lookup with #\\NUL."
  ;; TODO: Consider if other non-standard character names need support.
  (or
   ;; Check standard character names first
   (eclector.reader::find-standard-character name)
   (when (string-equal name "NUL") ; Special case for #\NUL
     (code-char 0))
   (call-next-method)))

;;; Method to handle feature expressions during conditional
;;; compilation (#+/#-).  Currently, it prints the expression and
;;; returns NIL (feature not present).
(defmethod eclector.reader:evaluate-feature-expression
    ((client analyzer-client) expression)
  "Handles read-time feature expressions (#+/#-). Currently treats all features as false.
   Outputs the feature expression to *error-output* for debugging."
  ;; TODO: Implement proper feature expression evaluation if needed
  ;;       for accurate analysis of conditionally compiled code. This
  ;;       might involve querying the current system's features.
  ;; (format *error-output* "Feature expression: ~S~%" expression)
  ;; Default to feature not being present
  nil)

;;; Method to handle read-time evaluation (#. expression).  Currently,
;;; it prints the expression and returns a placeholder :TO-EXPAND.
(defmethod eclector.reader:evaluate-expression ((client analyzer-client) expression)
  "Handles read-time evaluation (#.). Currently wraps the expression and prints it.
   Outputs the expression to *error-output* for debugging."
  ;; TODO: Implement actual read-time evaluation if the results are
  ;;       critical for analysis.  This can be complex and have side
  ;;       effects. For static analysis, it's often skipped or
  ;;       approximated.
  ;;(format *error-output* "Read-time eval: ~S~%" expression)
  ;; Return a placeholder indicating it was a read-time eval
  `(:to-expand ,expression))

;;; Specialized CST classes

(defclass analyzer-cst-mixin ()
  ())

;;;We need these so we can specialize first and its friends, but that
;;;means we have to specialize make-expression-result because there is
;;;no make-cst mechanism we can override.
(defclass analyzer-atom-cst (concrete-syntax-tree:atom-cst analyzer-cst-mixin)
  ()
  (:documentation "A specialized atom CST node."))

(defclass analyzer-cons-cst (concrete-syntax-tree:cons-cst analyzer-cst-mixin)
  ()
  (:documentation "A specialized cons CST node."))

(defmethod concrete-syntax-tree:nth (n (cst analyzer-cons-cst))
  (let ((nth (concrete-syntax-tree:nthrest n cst)))
    (when (and nth (concrete-syntax-tree:consp nth))
      (concrete-syntax-tree:first nth))))

(defmethod concrete-syntax-tree:first ((cst analyzer-cons-cst))
  (unless (and (not cst) (concrete-syntax-tree:consp cst))
    (slot-value cst 'concrete-syntax-tree::%first)))

;;;We are specializing make-expression-result to introduce custom cst
;;;classes.

;;; This method is responsible for constructing CST results from
;;; ordinary s-expression results.  It can do this by itself for a
;;; number of simple cases and it calls
;;; `concrete-syntax-tree:reconstruct' for difficult cases.
(defmethod eclector.parse-result:make-expression-result
    ((client analyzer-client) expression children source)
  ;; Our goal is to return a CST, say c, with the following properties:
  ;;
  ;; 1. The structure and raw values of c should match the structure
  ;;    of EXPRESSION, abbreviated as e, in the sense that for any
  ;;    "path" p from the set U_{L >= 0} {car,cdr}^L that is "valid"
  ;;    for e the following should hold:
  ;;
  ;;      (eql (cst:raw (apply-path/cst p c)) (apply-path p e))
  ;;
  ;;    where the `apply-path' functions repeatedly apply appropriate
  ;;    readers according to the supplied path.
  ;;
  ;; 2. The elements of CHILDREN, which is a "pool" of available
  ;;    sub-CSTs, should be incorporated as nodes into the CST rooted
  ;;    as c whenever possible.
  ;;
  ;; Note that property 2. does not imply that all elements of
  ;; CHILDREN should appear in the CST rooted at c.  For example, when
  ;; this method is called for an EXPRESSION of the form (0 . 0),
  ;; there will be three elements in CHILDREN: an atom for the first
  ;; 0, an atom for the consing dot and another atom for the second 0.
  ;; The middle child which represents the consing dot should not
  ;; appear as a node in the CST rooted at c.
  ;;
  ;; Furthermore, there are often multiple ways for c to satisfy the
  ;; properties 1. and 2.  Consider again the example (0 . 0).
  ;; Property 1. can be fulfilled by setting the car and cdr of c to
  ;; either the first or the third child, so there are four equally
  ;; valid combinations.
  ;;
  ;; The code below tries to construct good CSTs by picking off a few
  ;; special cases and falling back to
  ;; `concrete-syntax-tree:reconstruct' for the general case. There
  ;; are two reasons for this approach:
  ;;
  ;; 1. For special cases, more information may be available.
  ;;    Consider once again (0 . 0).  It is obvious that the car of c
  ;;    should be the `atom-cst' which corresponds to the first 0 and
  ;;    the cdr of c should be the `atom-cst' which corresponds to the
  ;;    second 0.  In contrast, the reconstructing heuristic for the
  ;;    general case would use the first `atom-cst' in both cases
  ;;    since it has no way of distinguishing (0 . 0) and (morally)
  ;;    (#1=0 . #1#).
  ;;
  ;; 2. `concrete-syntax-tree:reconstruct' is an expensive operation.
  ;;    Special-casing common expression shapes improves performance
  ;;    for typical inputs.
  (let (children-length)
    (cond ((atom expression)
           (make-instance 'analyzer-atom-cst :raw expression :source source))
          ;; EXPRESSION has a list structure with elements
          ;; corresponding to the elements of CHILDREN.
          ((and (eql (ignore-errors (list-length expression))
                     (setf children-length (length children)))
                (every (lambda (sub-expression child)
                         (eql sub-expression (cst:raw child)))
                       expression children))
           (loop for expression in (loop with reversed = '()
                                         for sub-expression on expression
                                         do (push sub-expression reversed)
                                         finally (return reversed))
                 for child in (reverse children)
                 for previous = (make-instance 'analyzer-atom-cst :raw nil) then node
                 for node = (make-instance 'analyzer-cons-cst :raw expression
                                                              :first child
                                                              :rest previous)
                 finally (return (reinitialize-instance node :source source))))
          ;; EXPRESSION is a CONS that resulted from reading a dotted
          ;; list such that the elements of CHILDREN correspond to car
          ;; of EXPRESSION, the consing dot and the cdr of EXPRESSION.
          ((and (not (consp (cdr expression)))
                (= 3 children-length)
                (destructuring-bind (car dot cdr) children
                  (eql (car expression)               (cst:raw car))
                  (eql eclector.reader::*consing-dot* (cst:raw dot))
                  (eql (cdr expression)               (cst:raw cdr))))
           (make-instance 'analyzer-cons-cst :raw expression
                                             :first (first children)
                                             :rest (third children)
                                             :source source))
          ;; Structure mismatch, try heuristic reconstruction.
          (t
           ;; We don't use
           ;;
           ;;   (cst:reconstruct client expression children)
           ;;
           ;; because we want SOURCE for the outer `cons-cst' but not
           ;; any of its children.
           (destructuring-bind (car . cdr) expression
             (make-instance 'analyzer-cons-cst
                            :raw expression
                            :first (cst:reconstruct client car children)
                            :rest (cst:reconstruct client cdr children)
                            :source source))))))

;;; Parses a Lisp file using Eclector and generates initial analysis
;;; objects for each top-level form.
(defun parse-file-with-eclector (file-path)
  "Parses the Lisp source file at FILE-PATH using Eclector.
   Returns a list of 'analysis' objects, each corresponding to a top-level form.
   Tracks file positions and package changes (IN-PACKAGE)."
  ;; TODO: Enhance error handling for file I/O and parsing issues.
  (let* ((analyses '())
         (client (make-instance 'analyzer-client))
         (eclector.reader:*client* client)
         (file-contents (alexandria:read-file-into-string file-path))
         ;; Precompute offset-to-line mapping
         (line-map (offset-to-line-map file-contents)))
    (with-open-file (raw-stream file-path :direction :input :external-format :utf-8)
      (let* ((tracking-stream (make-instance 'tracking-stream :underlying raw-stream))
             (*readtable* (copy-readtable nil)))

        ;; Reset package to CL at the start of parsing each file
        (setf (client-package client) (find-package :cl))

        (loop
          for start = (tracking-stream-position tracking-stream)
          for cst = (handler-case
                        ;; Read one form
                        (eclector.concrete-syntax-tree:read tracking-stream nil nil)
                      (eclector.reader:unknown-character-name (c)
                        ;; Handle unknown character names gracefully
                        (format
                         *error-output*
                         "Reader error (unknown char name) at position ~A in ~S: ~A~%"
                         (tracking-stream-position tracking-stream) file-path c)
                        ;; Consume the problematic part if possible
                        (read-char tracking-stream nil nil)
                        nil)
                      ;; Stop at EOF
                      (end-of-file () nil)
                      ;; Catch other generic reader errors
                      (error (e)
                        (format *error-output*
                                "Generic reader error at position ~A in ~S: ~A~%"
                                (tracking-stream-position tracking-stream)
                                file-path
                                e)
                        nil))
          ;; Continue as long as forms are read
          while cst
          for end = (tracking-stream-position tracking-stream)
          for line = (offset-to-line start line-map)
          for head-cst = (when (concrete-syntax-tree:consp cst)
                           (concrete-syntax-tree:first cst))
          for head = (when head-cst (concrete-syntax-tree:raw head-cst))
          do (progn
               ;; Manual tracking of IN-PACKAGE forms to update the
               ;; client's package context.
               (when (and head (eq head 'in-package))
                 (let* ((package-arg-cst (concrete-syntax-tree:second cst))
                        (pkg-name (when package-arg-cst
                                    (concrete-syntax-tree:raw package-arg-cst))))
                   (when pkg-name
                     (setf (client-package client)
                           (if (symbolp pkg-name)
                               (find-package pkg-name)
                               (find-package (string pkg-name)))))))

               ;; Create an appropriate analyzer instance based on the
               ;; form's head.
               (let ((analysis (if head
                                   (make-analyzer head)
                                   (make-instance 'analysis))))
                 (setf (analysis-start analysis) start)
                 (setf (analysis-end analysis) end)
                 (setf (analysis-line analysis) line)
                 ;; TODO: "Do we need both client and analysis package
                 ;;       slots? I dont think so."  The
                 ;;       `analysis-package` should reflect the
                 ;;       package AT THE TIME OF READING this form.
                 ;;       `client-package` is the current state of the
                 ;;       reader. They should be the same here.
                 ;;       Consolidate if truly redundant or clarify
                 ;;       purpose.
                 (setf (analysis-package analysis) (client-package client))
                 (setf (analysis-cst analysis) cst)

                 (push analysis analyses))))
        ;; Return analyses in the order they appeared in the file
        (nreverse analyses)))))

;;; Analyzes a single file: parses it and then performs deeper
;;; analysis on each form.
(defun analyze-file (file-path &optional (project nil))
  "Analyzes the Lisp source file at FILE-PATH.
   First, parses the file to get basic analysis for each form.  Then,
   performs type-specific deeper analysis (e.g., for DEFUN, DEFCLASS)
   on each form.  Returns a 'code-file' object containing all analyses
   for the file."
  ;; TODO: The `project` argument is declared ignored. If it's
  ;;       intended to link the file to a project context during
  ;;       analysis, this needs implementation.
  (declare (ignore project))
  (let (;; First, parse the file and create barebones analysis objects
        ;; for each form.
        (analyses (parse-file-with-eclector file-path))
        (code-file (make-instance 'code-file :path file-path)))
    ;; Perform deeper, type-specific analysis for each form.
    (dolist (analysis analyses)
      (push (analyze-cst (analysis-cst analysis) analysis)
            (file-analyses code-file)))
    ;; Ensure analyses are stored in source order in the code-file object
    (setf (file-analyses code-file) (nreverse (file-analyses code-file)))
    code-file))

;;; Generic function to serialize an analysis object, typically to a
;;; plist or JSON-like structure.
(defgeneric write-analysis (analysis filename &key)
  (:documentation "Serializes the ANALYSIS object into a property list.
FILENAME is the name of the source file for context.
Subclasses of 'analysis' should specialize this to include type-specific information."))

;;; Analyzes all specified file paths and groups them into a
;;; 'code-project' object.
(defun analyze-project% (file-paths &key (name "default-project"))
  "Analyzes a list of Lisp source files (FILE-PATHS) and returns a 'code-project' object.
   NAME provides a symbolic name for this project."
  ;; TODO: Consider parallelizing file analysis for large projects.
  (let ((project (make-instance 'code-project :name name)))
    (dolist (file file-paths)
      (push (analyze-file file project) (project-files project)))
    ;; Ensure files are in a consistent order, e.g., reverse of
    ;; processing or sorted by path.
    (setf (project-files project) (nreverse (project-files project)))
    project))

;;; Helper function to collect source file pathnames from an ASDF
;;; component.
(defun collect-source-files (component)
  "Recursively collects source file pathnames from an ASDF COMPONENT (system or module).
   Returns a list of pathnames."
  ;; TODO: This might not capture all types of source files defined in
  ;;       complex ASDF systems.  Review ASDF's component types and
  ;;       how pathnames are best extracted.
  (cond
    ((typep component 'asdf:source-file)
     (list (asdf:component-pathname component)))
    ((typep component 'asdf:module)
     (mapcan #'collect-source-files (asdf:component-children component)))
    (t nil)))

;;; Gathers all source files for a given ASDF system.
;;; Optionally includes files from dependency systems.
(defun all-system-files (system-name &key include-dependencies)
  "Returns a list of all Lisp source file pathnames for the ASDF system SYSTEM-NAME.
   If INCLUDE-DEPENDENCIES is true, recursively includes files from dependent systems."
  ;; TODO: This could be slow for systems with many dependencies if
  ;;       `include-dependencies` is true.  Consider options for
  ;;       caching or more efficient traversal if performance is an
  ;;       issue.

  ;; To avoid processing systems multiple times
  (let ((visited (make-hash-table :test #'equal)))
    (labels ((recurse (sys-name)
               (unless (gethash sys-name visited)
                 (setf (gethash sys-name visited) t)
                 (let* ((system (asdf:find-system sys-name nil))
                        (files (if system (collect-source-files system) nil)))
                   (if (and system include-dependencies)
                       (append files
                               (mapcan #'recurse (asdf:system-depends-on system)))
                       files)))))
      (recurse system-name))))

;;; Loads an ASDF system (if not already loaded) and then analyzes its
;;; source files.
(defun load-system-and-analyze (system-name source-dir &optional file include-dependencies)
  "Loads the ASDF system SYSTEM-NAME found in SOURCE-DIR (if not already loaded),
   then analyzes its source files.
   If FILE is provided, only that file is analyzed (must be part of the system).
   If INCLUDE-DEPENDENCIES is true, files from dependent systems are also analyzed.
   Returns a 'code-project' object."
  ;; TODO: Error handling for ASDF operations (system not found, load
  ;; errors).
  (let* ((absolute-dir (uiop:ensure-directory-pathname (truename source-dir)))
         (asd-file (merge-pathnames
                    ;; TODO: More robust way to create "name.asd"
                    (concatenate 'string system-name ".asd")
                    absolute-dir)))

    (unless (probe-file asd-file)
      (error "ASD file not found: ~A" asd-file))

    ;; Ensure the source directory is in ASDF's central registry
    (unless (member absolute-dir
                    asdf:*central-registry*
                    :test #'equalp
                    :key #'uiop:ensure-directory-pathname)
      (push absolute-dir asdf:*central-registry*))

    ;; Only load the system if it's not already loaded.
    (unless (asdf:component-loaded-p system-name)
      (asdf:load-system system-name))

    (let ((files-to-analyze
            (if file
                (list (truename file))
                (let ((sys-files (all-system-files
                                  system-name
                                  :include-dependencies include-dependencies)))
                  (unless sys-files
                    (warn "System ~A (or its dependencies) defines no source files directly that were found."
                          system-name))
                  sys-files))))
      (analyze-project% files-to-analyze :name system-name))))

;;; Stores the analysis resu lts of a project into a persistent store
;;; (cl-naive-store).
(defun store-project (project)
  "Persists the analysis results from the PROJECT object into a cl-naive-store.
   Each top-level form's analysis is stored as a separate document."
  ;; TODO: Ensure `init-project-store` is defined and correctly sets up the store.
  ;; TODO: Consider batching writes or other optimizations for large projects.
  (let ((collection (init-project (project-name project))))
    (dolist (file (project-files project))
      (let* ((filename (namestring (file-path file)))
             ;; Analyses should already be sorted by `analyze-file`
             (analyses (file-analyses file)))
        (dolist (analysis analyses)
          (cl-naive-store:persist-document
           collection
           (write-analysis analysis filename)))))))

;;; Main entry point to analyze an ASDF system and store its definitions.
(defun analyze-project (system-name source-dir &optional file)
  "High-level function to load, analyze, and store definitions for an ASDF project.
   SYSTEM-NAME is the name of the system.
   SOURCE-DIR is the directory containing the .asd file.
   FILE, if provided, specifies a single file within the system to analyze.
   Returns the 'code-project' object."
  ;; TODO: Add option to control inclusion of dependencies for analysis.
  (let* ((project (load-system-and-analyze system-name source-dir file nil)))
    (store-project project)
    project))

;;;; These exist for testing only at this stage.

;;; Parses a Lisp code string using Eclector and generates initial analysis
;;; objects for each top-level form in the string.
(defun parse-string-with-eclector (code-string &key (package (find-package :cl-user)))
  "Parses the Lisp CODE-STRING using Eclector.
   Returns a list of 'analysis' objects, each corresponding to a top-level form.
   PACKAGE specifies the initial package context for parsing."
  (let* ((analyses '())
         (client (make-instance 'analyzer-client :package package))
         (eclector.reader:*client* client)
         ;; Precompute offset-to-line mapping for the string
         (line-map (offset-to-line-map code-string)))
    (with-input-from-string (raw-stream code-string)
      (let* ((tracking-stream (make-instance 'tracking-stream :underlying raw-stream))
             (*readtable* (copy-readtable nil)))

        (setf (client-package client) package) ; Ensure client package is set

        (loop
          for start = (tracking-stream-position tracking-stream)
          for cst = (handler-case
                        (eclector.concrete-syntax-tree:read tracking-stream nil nil)
                      (eclector.reader:unknown-character-name (c)
                        (format *error-output*
                                "String reader error (unknown char name) at position ~A: ~A~%"
                                (tracking-stream-position tracking-stream) c)
                        (read-char tracking-stream nil nil)
                        nil)
                      (end-of-file () nil)
                      (error (e)
                        (format *error-output*
                                "Generic string reader error at position ~A: ~A~%"
                                (tracking-stream-position tracking-stream) e)
                        nil))
          while cst
          for end = (tracking-stream-position tracking-stream)
          for line = (offset-to-line start line-map)
          for head-cst = (when (concrete-syntax-tree:consp cst)
                           (concrete-syntax-tree:first cst))
          for head = (when head-cst (concrete-syntax-tree:raw head-cst))
          do (progn
               (when (and head (eq head 'in-package))
                 (let* ((package-arg-cst (concrete-syntax-tree:second cst))
                        (pkg-name (when package-arg-cst
                                    (concrete-syntax-tree:raw package-arg-cst))))
                   (when pkg-name
                     (setf (client-package client)
                           (if (symbolp pkg-name)
                               (find-package pkg-name)
                               (find-package (string pkg-name)))))))

               (let ((analysis (if head
                                   (make-analyzer head)
                                   (make-instance 'analysis))))
                 (setf (analysis-start analysis) start)
                 (setf (analysis-end analysis) end)
                 (setf (analysis-line analysis) line)
                 (setf (analysis-package analysis) (client-package client))
                 (setf (analysis-cst analysis) cst)
                 (push analysis analyses))))
        (nreverse analyses)))))

;;; Analyzes a string of Lisp code.
(defun analyze-string (code-string &key (package (find-package :cl-user)))
  "Analyzes the Lisp CODE-STRING.
   First, parses the string to get basic analysis for each form.
   Then, performs type-specific deeper analysis on each form.
   Returns a list of 'analysis' objects for the forms in the string.
   PACKAGE specifies the initial package context for parsing."
  (let ((analyses (parse-string-with-eclector code-string :package package))
        (results '()))
    (dolist (analysis analyses)
      (push (analyze-cst (analysis-cst analysis) analysis) results))
    (nreverse results)))

