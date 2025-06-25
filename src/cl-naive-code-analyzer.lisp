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
  ((path :accessor file-path :initarg :path ; The file system path to the code file.
         :documentation "The absolute path to the source file.")
   (analyses :accessor file-analyses :initform nil ; A list of analysis objects for forms in this file.
             :documentation "A list of 'analysis' objects, each representing a top-level form found in the file.")))

;;; Represents a collection of code files, typically a Lisp system or project.
(defclass code-project ()
  ((name :accessor project-name :initarg :name :initform nil ; The name of the project.
         :documentation "A symbolic name for the project being analyzed (e.g., the ASDF system name).")
   (files :accessor project-files :initarg :files :initform nil ; A list of 'code-file' objects in the project.
          :documentation "A list of 'code-file' objects that constitute this project.")))

;;; Base class for storing analysis results of a Lisp form.
;;; Specific types of forms (defun, defclass, etc.) will have specialized subclasses.
(defclass analysis ()
  ((name :accessor analysis-name :initarg :name :initform nil
         :documentation "The name of the analyzed form (e.g., function name, class name).")
   (kind :accessor analysis-kind :initarg :kind :initform nil
         :documentation "The type of the form (e.g., 'defun, 'defclass).")
   (cst :accessor analysis-cst :initarg :form :initform nil
        :documentation "The Concrete Syntax Tree (CST) of the form.")
   (start :accessor analysis-start :initarg :start :initform nil
          :documentation "The starting character offset of the form in the source file.")
   (end :accessor analysis-end :initarg :end :initform nil
        :documentation "The ending character offset of the form in the source file.")
   (line :accessor analysis-line :initarg :line :initform nil
         :documentation "The starting line number of the form in the source file.")
   (package :accessor analysis-package :initarg :package :initform (find-package :cl)
            :documentation "The package in which the form was read.")
   (function-calls :accessor analysis-function-calls :initform nil
                   :documentation "A list of global functions called within this form.")
   (macro-calls :accessor analysis-macro-calls :initform nil
                :documentation "A list of global macros called within this form.")
   (variable-uses :accessor analysis-variable-uses :initform nil
                  :documentation "A list of global variables referenced within this form. TODO: Clarify if special vars only.")
   (local-function-calls :accessor analysis-local-function-calls :initform nil
                         :documentation "A list of locally defined functions (labels/flet) called. TODO: Implement.")
   (local-variable-uses :accessor analysis-local-variable-uses :initform nil
                        :documentation "A list of lexically bound variables referenced. TODO: Implement.")
   (lexical-definitions :accessor analysis-lexical-definitions :initform nil
                        :documentation "A list of symbols defined lexically within this form (e.g., parameters, let-bound vars).")
   (dynamic-definitions :accessor analysis-dynamic-definitions :initform nil
                        :documentation "A list of symbols defined dynamically (e.g., special variables declared). TODO: Implement.")
   (raw-body :accessor analysis-raw-body :initform nil
             :documentation "The raw body of the form, typically a CST or list of CSTs. For definitions, this is the code after name, lambda-list, docstring.")))

;;; Custom Eclector client to hook into the reading process.
;;; Used to capture CSTs and their positions, and manage package context.
(defclass analyzer-client (eclector.concrete-syntax-tree:cst-client)
  ((forms :initform '() :accessor client-forms ; TODO: This slot seems unused. Verify and remove if so.
          :documentation "Accumulated forms (CSTs) read by the client. Potentially redundant if analyses are built directly.")
   (form-positions :initform (make-hash-table :test 'eq) :accessor client-form-positions ; TODO: This slot seems unused. Verify and remove.
                   :documentation "A hash table mapping CSTs to their start/end positions. Potentially redundant.")
   (current-position :initform nil :accessor client-current-position ; TODO: This slot seems unused. Verify and remove.
                     :documentation "The starting position of the current form being parsed.")
   (last-position :initform nil :accessor client-last-position ; TODO: This slot seems unused. Verify and remove.
                  :documentation "The ending position of the last form parsed.")
   (package :accessor client-package :initarg :package :initform (find-package :cl)
            :documentation "The current package context for the reader.")))

;;; Method to customize symbol interpretation during reading.
;;; Handles package qualifications and uninterned symbols.
(defmethod eclector.reader:interpret-symbol ((client analyzer-client)
                                             input-stream package-name
                                             symbol-name internp)
  "Handles symbol interpretation, respecting package context and interning rules.
   Ensures symbols are resolved or created in the correct package based on `client-package`."
  ;; TODO: Review logic for :current package handling, ensure it aligns with CL expectations.
  (cond
    ;; #:foo => make-symbol (uninterned symbol)
    ((null package-name)
     (make-symbol symbol-name))
    ;; Qualified or current package symbol
    (t
     (let ((pkg (cond
                  ((eq package-name :current) (or (client-package client)
                                                  (find-package :cl-user))) ;; Safer fallback if client-package is nil
                  ((eq package-name :keyword) (find-package :keyword))
                  (t (or (find-package package-name)
                         (error "No package named ~a" package-name))))))
       (multiple-value-bind (sym status) (find-symbol symbol-name pkg)
         (cond
           (status sym) ;; Symbol found in the package, use it.
           ;; If internp is true and not in CL package (to avoid interning new symbols there by mistake)
           ((and internp (not (eq pkg (find-package "COMMON-LISP"))))
            (intern symbol-name pkg))
           ;; Otherwise (not internp or in CL package and not found), create an uninterned symbol.
           ;; This typically happens for symbols that are not meant to be interned or are being defined.
           (t (make-symbol symbol-name))))))))

;;; Method to customize character name lookup during reading (e.g., #\Newline).
(defmethod eclector.reader:find-character ((client analyzer-client) (name string))
  "Finds a character by its name, extending standard lookup with #\\NUL."
  ;; TODO: Consider if other non-standard character names need support.
  (or (eclector.reader::find-standard-character name) ; Check standard character names first
      (when (string-equal name "NUL") ; Special case for #\NUL
        (code-char 0))
      (call-next-method))) ; Fallback to default behavior

#| ; This method was commented out in the original code.
   ; It seems intended to store form positions and forms, but this might be
   ; handled differently now.
   ; TODO: Evaluate if this functionality is needed or if current approach is sufficient.
(defmethod eclector.parse-result:make-expression-result ((client analyzer-client) result children source)
(setf (gethash result (client-form-positions client))
(cons (client-current-position client)
(client-last-position client)))
(push result (client-forms client))
result)
|#

;;; Method to handle feature expressions during conditional compilation (#+/#-).
;;; Currently, it prints the expression and returns NIL (feature not present).
(defmethod eclector.reader:evaluate-feature-expression
    ((client analyzer-client) expression)
  "Handles read-time feature expressions (#+/#-). Currently treats all features as false.
   Outputs the feature expression to *error-output* for debugging."
  ;; TODO: Implement proper feature expression evaluation if needed for accurate analysis
  ;;       of conditionally compiled code. This might involve querying the current system's features.
  (format *error-output* "Feature expression: ~S~%" expression)
  nil) ; Default to feature not being present

;;; Method to handle read-time evaluation (#. expression).
;;; Currently, it prints the expression and returns a placeholder :TO-EXPAND.
(defmethod eclector.reader:evaluate-expression ((client analyzer-client) expression)
  "Handles read-time evaluation (#.). Currently wraps the expression and prints it.
   Outputs the expression to *error-output* for debugging."
  ;; TODO: Implement actual read-time evaluation if the results are critical for analysis.
  ;;       This can be complex and have side effects. For static analysis, it's often skipped or approximated.
  (format *error-output* "Read-time eval: ~S~%" expression)
  `(:to-expand ,expression)) ; Return a placeholder indicating it was a read-time eval

;;; Parses a Lisp file using Eclector and generates initial analysis objects for each top-level form.
(defun parse-file-with-eclector (file-path)
  "Parses the Lisp source file at FILE-PATH using Eclector.
   Returns a list of 'analysis' objects, each corresponding to a top-level form.
   Tracks file positions and package changes (IN-PACKAGE)."
  ;; TODO: Enhance error handling for file I/O and parsing issues.
  (let* ((analyses '())
         (client (make-instance 'analyzer-client))
         (eclector.reader:*client* client) ; Set the global Eclector client for this thread
         (file-contents (alexandria:read-file-into-string file-path))
         (line-map (offset-to-line-map file-contents))) ; Precompute offset-to-line mapping
    (with-open-file (raw-stream file-path :direction :input :external-format :utf-8)
      (let* ((tracking-stream (make-instance 'tracking-stream :underlying raw-stream))
             (*readtable* (copy-readtable nil))) ; Use a fresh readtable for each file

        ;; Reset package to CL at the start of parsing each file
        (setf (client-package client) (find-package :cl))

        (loop
          for start = (tracking-stream-position tracking-stream)
          for cst = (handler-case
                        (eclector.concrete-syntax-tree:read tracking-stream nil nil) ; Read one form
                      (eclector.reader:unknown-character-name (c)
                        ;; Handle unknown character names gracefully
                        (format *error-output* "Reader error (unknown char name) at position ~A in ~S: ~A~%"
                                (tracking-stream-position tracking-stream) file-path c)
                        (read-char tracking-stream nil nil) ; Consume the problematic part if possible
                        nil) ; Skip this form
                      (end-of-file () nil) ; Stop at EOF
                      (error (e) ; Catch other generic reader errors
                             (format *error-output* "Generic reader error at position ~A in ~S: ~A~%"
                                     (tracking-stream-position tracking-stream) file-path e)
                             nil))) ; Skip this form
          while cst ; Continue as long as forms are read
          for end = (tracking-stream-position tracking-stream)
          for line = (offset-to-line start line-map)
          for head-cst = (when (concrete-syntax-tree:consp cst) (concrete-syntax-tree:first cst))
          for head = (when head-cst (concrete-syntax-tree:raw head-cst))
          do (progn
               ;; Manual tracking of IN-PACKAGE forms to update the client's package context.
               (when (and head (eq head 'in-package))
                 (let* ((package-arg-cst (concrete-syntax-tree:second cst))
                        (pkg-name (when package-arg-cst (concrete-syntax-tree:raw package-arg-cst))))
                   (when pkg-name
                     (setf (client-package client)
                           (if (symbolp pkg-name)
                               (find-package pkg-name)
                               (find-package (string pkg-name)))))))

               ;; Create an appropriate analyzer instance based on the form's head.
               (let ((analysis (if head (make-analyzer head) (make-instance 'analysis))))
                 (setf (analysis-start analysis) start)
                 (setf (analysis-end analysis) end)
                 (setf (analysis-line analysis) line)
                 ;; TODO: "Do we need both client and analysis package slots? I dont think so."
                 ;;       The `analysis-package` should reflect the package AT THE TIME OF READING this form.
                 ;;       `client-package` is the current state of the reader. They should be the same here.
                 ;;       Consolidate if truly redundant or clarify purpose.
                 (setf (analysis-package analysis) (client-package client))
                 (setf (analysis-cst analysis) cst)

                 ;; These client slots seem for a different purpose, perhaps originally for make-expression-result.
                 ;; (setf (client-current-position client) start)
                 ;; (setf (client-last-position client) end)

                 (push analysis analyses))))
        (nreverse analyses))))) ; Return analyses in the order they appeared in the file

;;; Analyzes a single file: parses it and then performs deeper analysis on each form.
(defun analyze-file (file-path &optional (project nil))
  "Analyzes the Lisp source file at FILE-PATH.
   First, parses the file to get basic analysis for each form.
   Then, performs type-specific deeper analysis (e.g., for DEFUN, DEFCLASS) on each form.
   Returns a 'code-file' object containing all analyses for the file."
  ;; TODO: The `project` argument is declared ignored. If it's intended to link
  ;;       the file to a project context during analysis, this needs implementation.
  (declare (ignore project))
  (let (;; First, parse the file and create barebones analysis objects for each form.
        (analyses (parse-file-with-eclector file-path))
        (code-file (make-instance 'code-file :path file-path)))
    ;; Perform deeper, type-specific analysis for each form.
    (dolist (analysis analyses)
      (push (analyze-cst (analysis-cst analysis) analysis) ; `analyze-cst` populates the existing `analysis` object
            (file-analyses code-file)))
    ;; Ensure analyses are stored in source order in the code-file object
    (setf (file-analyses code-file) (nreverse (file-analyses code-file)))
    code-file))

;;; Generic function to serialize an analysis object, typically to a plist or JSON-like structure.
(defgeneric write-analysis (analysis filename &key)
  (:documentation "Serializes the ANALYSIS object into a property list.
                   FILENAME is the name of the source file for context.
                   Subclasses of 'analysis' should specialize this to include type-specific information."))

;;; Default method for WRITE-ANALYSIS. Serializes generic slots common to all analysis types.
;;; Subclasses specialize this method to add slots like docstrings or parameters where applicable.
(defmethod write-analysis ((a analysis) filename &key)
  "Default method to serialize an 'analysis' object.
   Includes common properties like name, package, kind, position, code, and calls."
  ;; TODO: "Some time or another we need to figure out how to get
  ;;        the raw code using start and end so that we dont use the
  ;;        read in code! That would make it more usefull when giving it to an AI because then the AI"
  ;;        This is a good point. Storing the raw text segment would be more robust than re-serializing the CST.
  ;;        It would require having the file content available here or passing start/end to `normalize-reader-macros`.
  `(,@`(:name , (analysis-name a)
        :package ,(if (packagep (analysis-package a))
                      (package-name (analysis-package a))
                      (analysis-package a)) ; Should always be a package object from client.
        :filename ,filename
        :kind ,(analysis-kind a)
        :line ,(analysis-line a)
        :start ,(analysis-start a)
        :end ,(analysis-end a)
        ;; Serialize the CST back to a string representation.
        ;; `normalize-reader-macros` attempts to make this more canonical.
        :code ,(format nil "~S"
                       (normalize-reader-macros ; TODO: Ensure this function is defined and works as expected.
                        (concrete-syntax-tree:raw ; Get Lisp data from CST before printing
                         (analysis-cst a))))
        ;; TODO: `export-symbol` needs to be defined. It should probably convert symbols to strings with package prefixes.
        :function-calls ,(mapcar #'export-symbol (analysis-function-calls a))
        :macro-calls ,(mapcar #'export-symbol (analysis-macro-calls a))
        :variable-uses ,(mapcar #'export-symbol (analysis-variable-uses a))
        :lexical-definitions ,(mapcar #'export-symbol (analysis-lexical-definitions a))
        :dynamic-definitions ,(mapcar #'export-symbol (analysis-dynamic-definitions a)))))

;;; Specialized WRITE-ANALYSIS methods for different definition types.
;;; These add specific information like parameters, docstrings, slots, etc.

(defmethod write-analysis ((a defun-analysis) filename &key)
  "Serializes a 'defun-analysis' object, including lambda info, parameters, and docstring."
  `(,@(call-next-method) ; Include generic analysis properties
    ,@(when (analysis-lambda-info a)
        `(:lambda-info ,(analysis-lambda-info a)))
    ,@(when (analysis-parameters a)
        `(:parameters ,(mapcar #'export-symbol (analysis-parameters a)))) ; Ensure params are exported
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))))

(defmethod write-analysis ((a defmethod-analysis) filename &key)
  "Serializes a 'defmethod-analysis' object, including parameters and docstring."
  `(,@(call-next-method)
    ,@(when (analysis-parameters a)
        `(:parameters ,(mapcar #'export-symbol (analysis-parameters a))))
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))) )

(defmethod write-analysis ((a defmacro-analysis) filename &key)
  "Serializes a 'defmacro-analysis' object, including parameters and docstring."
  `(,@(call-next-method)
    ,@(when (analysis-parameters a)
        `(:parameters ,(mapcar #'export-symbol (analysis-parameters a))))
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))) )

(defmethod write-analysis ((a defgeneric-analysis) filename &key)
  "Serializes a 'defgeneric-analysis' object, including parameters and docstring."
  `(,@(call-next-method)
    ,@(when (analysis-parameters a)
        `(:parameters ,(mapcar #'export-symbol (analysis-parameters a))))
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))) )

(defmethod write-analysis ((a defsetf-analysis) filename &key)
  "Serializes a 'defsetf-analysis' object, including parameters (if applicable) and docstring."
  ;; TODO: Parameters for defsetf need careful handling based on short/long form.
  `(,@(call-next-method)
    ,@(when (analysis-parameters a)
        `(:parameters ,(mapcar #'export-symbol (analysis-parameters a))))
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))) )

(defmethod write-analysis ((a deftype-analysis) filename &key)
  "Serializes a 'deftype-analysis' object, including parameters and docstring."
  `(,@(call-next-method)
    ,@(when (analysis-parameters a)
        `(:parameters ,(mapcar #'export-symbol (analysis-parameters a))))
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))) )

;; This was a duplicate of the defclass-analysis method below it, but simpler.
;; Removing this simpler one to avoid conflict, assuming the more detailed one is intended.
;; (defmethod write-analysis ((a defclass-analysis) filename &key)
;;   `(,@(call-next-method)
;;     ,@(when (analysis-docstring a)
;;         `(:docstring ,(analysis-docstring a)))) )

(defmethod write-analysis ((a defstruct-analysis) filename &key)
  "Serializes a 'defstruct-analysis' object, including docstring and slots."
  ;; TODO: Add :slots serialization if `analysis-slots` is populated for defstruct.
  `(,@(call-next-method)
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))
    ,@(when (analysis-slots a) ; Assuming analysis-slots exists and is populated
        `(:slots ,(mapcar #'export-symbol (analysis-slots a))))))

(defmethod write-analysis ((a defparameter-analysis) filename &key)
  "Serializes a 'defparameter-analysis' object (used for defparameter, defvar, defconstant), including docstring."
  `(,@(call-next-method)
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))) )

(defmethod write-analysis ((a define-condition-analysis) filename &key)
  "Serializes a 'define-condition-analysis' object, including docstring, superclasses, and slots."
  ;; TODO: Add :superclasses and :slots serialization.
  `(,@(call-next-method)
    ,@(when (analysis-docstring a)
        `(:docstring ,(analysis-docstring a)))
    ,@(when (slot-exists-p a 'analysis-superclasses) ; Check if superclasses slot exists
        `(:superclasses ,(mapcar #'export-symbol (analysis-superclasses a))))
    ,@(when (slot-exists-p a 'analysis-slots) ; Check if slots slot exists
        `(:slots ,(mapcar #'export-symbol (analysis-slots a))))))


(defmethod write-analysis ((a defpackage-analysis) filename &key)
  "Serializes a 'defpackage-analysis' object, including all package options like nicknames, uses, exports, etc."
  `(,@(call-next-method)
    ,@(when (analysis-docstring a) `(:docstring ,(analysis-docstring a)))
    ,@(when (analysis-nicknames a) `(:nicknames ,(mapcar #'string (analysis-nicknames a))))
    ,@(when (analysis-uses a) `(:uses ,(mapcar #'package-name (analysis-uses a)))) ; Assuming uses are package objects or names
    ,@(when (analysis-exports a) `(:exports ,(mapcar #'export-symbol (analysis-exports a))))
    ,@(when (analysis-shadows a) `(:shadows ,(mapcar #'export-symbol (analysis-shadows a))))
    ,@(when (analysis-shadowing-imports a) `(:shadowing-imports ,(analysis-shadowing-imports a))) ; Needs proper serialization
    ,@(when (analysis-imports a) `(:imports ,(analysis-imports a))) ; Needs proper serialization
    ,@(when (analysis-interns a) `(:interns ,(mapcar #'export-symbol (analysis-interns a))))
    ,@(when (analysis-other-options a) `(:other-options ,(analysis-other-options a)))))

;;; Specialized method for DEFCLASS to include superclasses and slots.
;;; Note: There was a potential conflict with a simpler defclass method above. This one is more detailed.
(defmethod write-analysis ((a defclass-analysis) filename &key)
  "Serializes a 'defclass-analysis' object, including docstring, superclasses, and slots."
  ;; TODO: Ensure `serialize-slot` is defined and properly serializes slot definitions.
  `(,@(call-next-method a filename) ; Call specific method for defclass, not the general 'analysis' one if overridden
    ,@(when (analysis-docstring a) `(:docstring ,(analysis-docstring a)))
    :superclasses ,(mapcar #'export-symbol (analysis-superclasses a))
    :slots ,(mapcar #'serialize-slot (analysis-slots a)))) ; `serialize-slot` needs to be defined

;;; Analyzes all specified file paths and groups them into a 'code-project' object.
(defun analyze-project (file-paths &key (name "default-project"))
  "Analyzes a list of Lisp source files (FILE-PATHS) and returns a 'code-project' object.
   NAME provides a symbolic name for this project."
  ;; TODO: Consider parallelizing file analysis for large projects.
  (let ((project (make-instance 'code-project :name name)))
    (dolist (file file-paths)
      (push (analyze-file file project) (project-files project)))
    ;; Ensure files are in a consistent order, e.g., reverse of processing or sorted by path.
    (setf (project-files project) (nreverse (project-files project)))
    project))

;;; Helper function to collect source file pathnames from an ASDF component.
(defun collect-source-files (component)
  "Recursively collects source file pathnames from an ASDF COMPONENT (system or module).
   Returns a list of pathnames."
  ;; TODO: This might not capture all types of source files defined in complex ASDF systems.
  ;;       Review ASDF's component types and how pathnames are best extracted.
  (cond
    ((typep component 'asdf:source-file)
     (list (asdf:component-pathname component)))
    ((typep component 'asdf:module)
     (mapcan #'collect-source-files (asdf:component-children component)))
    (t nil))) ; Ignore other component types like static-file, doc-file, etc.

;;; Gathers all source files for a given ASDF system.
;;; Optionally includes files from dependency systems.
(defun all-system-files (system-name &key include-dependencies)
  "Returns a list of all Lisp source file pathnames for the ASDF system SYSTEM-NAME.
   If INCLUDE-DEPENDENCIES is true, recursively includes files from dependent systems."
  ;; TODO: This could be slow for systems with many dependencies if `include-dependencies` is true.
  ;;       Consider options for caching or more efficient traversal if performance is an issue.
  (let ((visited (make-hash-table :test #'equal))) ; To avoid processing systems multiple times
    (labels ((recurse (sys-name)
               (unless (gethash sys-name visited)
                 (setf (gethash sys-name visited) t)
                 (let* ((system (asdf:find-system sys-name nil)) ; Find system, don't error if not found initially
                        (files (if system (collect-source-files system) nil)))
                   (if (and system include-dependencies)
                       (append files
                               (mapcan #'recurse (asdf:system-depends-on system)))
                       files)))))
      (recurse system-name))))

;;; Loads an ASDF system (if not already loaded) and then analyzes its source files.
(defun load-system-and-analyze (system-name source-dir &optional file include-dependencies)
  "Loads the ASDF system SYSTEM-NAME found in SOURCE-DIR (if not already loaded),
   then analyzes its source files.
   If FILE is provided, only that file is analyzed (must be part of the system).
   If INCLUDE-DEPENDENCIES is true, files from dependent systems are also analyzed.
   Returns a 'code-project' object."
  ;; TODO: Error handling for ASDF operations (system not found, load errors).
  (let* ((absolute-dir (uiop:ensure-directory-pathname (truename source-dir)))
         (asd-file (merge-pathnames
                    (concatenate 'string system-name ".asd") ; More robust way to create "name.asd"
                    absolute-dir)))

    (unless (probe-file asd-file)
      (error "ASD file not found: ~A" asd-file))

    ;; Ensure the source directory is in ASDF's central registry
    (unless (member absolute-dir asdf:*central-registry* :test #'equalp :key #'uiop:ensure-directory-pathname)
      (push absolute-dir asdf:*central-registry*))

    ;; Only load the system if it's not already loaded.
    (unless (asdf:component-loaded-p system-name) ; Check by name directly
      (asdf:load-system system-name))

    (let ((files-to-analyze
            (if file
                (list (truename file)) ; Analyze a single specified file
                (let ((sys-files (all-system-files
                                  system-name
                                  :include-dependencies include-dependencies)))
                  (unless sys-files
                    (warn "System ~A (or its dependencies) defines no source files directly that were found."
                          system-name))
                  sys-files))))
      (analyze-project files-to-analyze :name system-name))))

;;; Stores the analysis results of a project into a persistent store (cl-naive-store).
(defun store-project (project)
  "Persists the analysis results from the PROJECT object into a cl-naive-store.
   Each top-level form's analysis is stored as a separate document."
  ;; TODO: Ensure `init-project-store` is defined and correctly sets up the store.
  ;; TODO: Consider batching writes or other optimizations for large projects.
  (let ((collection (init-project-store (project-name project))))
    (dolist (file (project-files project))
      (let* ((filename (namestring (file-path file)))
             ;; Analyses should already be sorted by `analyze-file`
             (analyses (file-analyses file)))
        (dolist (analysis analyses)
          (cl-naive-store:persist-document
           collection
           (write-analysis analysis filename)))))))

;;; Main entry point to analyze an ASDF system and store its definitions.
(defun index-project-definitions (system-name source-dir &optional file)
  "High-level function to load, analyze, and store definitions for an ASDF project.
   SYSTEM-NAME is the name of the system.
   SOURCE-DIR is the directory containing the .asd file.
   FILE, if provided, specifies a single file within the system to analyze.
   Returns the 'code-project' object."
  ;; TODO: Add option to control inclusion of dependencies for analysis.
  (let* ((project (load-system-and-analyze system-name source-dir file nil))) ; include-dependencies defaults to nil
    (store-project project)
    project))

