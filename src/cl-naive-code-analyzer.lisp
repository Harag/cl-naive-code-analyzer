(in-package :cl-naive-code-analyzer)

(defclass code-file ()
  ((path :accessor file-path :initarg :path)
   (analyses :accessor file-analyses :initform nil)))

(defclass code-project ()
  ((name :accessor project-name :initarg :name :initform nil)
   (files :accessor project-files :initarg :files :initform nil)))

(defclass analysis ()
  ((name :accessor analysis-name :initarg :name :initform nil)
   (kind :accessor analysis-kind :initarg :kind :initform nil)
   (cst :accessor analysis-cst :initarg :form :initform nil)
   (start :accessor analysis-start :initarg :start :initform nil)
   (end :accessor analysis-end :initarg :end :initform nil)
   (line :accessor analysis-line :initarg :line :initform nil)
   (package :accessor analysis-package :initarg :package :initform (find-package :cl))
   (function-calls :accessor analysis-function-calls :initform nil)
   (macro-calls :accessor analysis-macro-calls :initform nil)
   (variable-uses :accessor analysis-variable-uses :initform nil)
   (local-function-calls :accessor analysis-local-function-calls :initform nil)
   (local-variable-uses :accessor analysis-local-variable-uses :initform nil)
   (lexical-definitions :accessor analysis-lexical-definitions :initform nil)
   (dynamic-definitions :accessor analysis-dynamic-definitions :initform nil)
   (parameters :accessor analysis-parameters :initform nil)
   (docstring :accessor analysis-docstring :initform nil)
   (raw-body :accessor analysis-raw-body :initform nil)))

(defclass analyzer-client (eclector.concrete-syntax-tree:cst-client)
  ((forms :initform '() :accessor client-forms)
   (form-positions :initform (make-hash-table :test 'eq) :accessor client-form-positions)
   (current-position :initform nil :accessor client-current-position)
   (last-position :initform nil :accessor client-last-position)
   (package :accessor client-package :initarg :package :initform (find-package :cl))))

(defmethod eclector.reader:interpret-symbol ((client analyzer-client)
                                             input-stream package-name
                                             symbol-name internp)
  (cond
    ;; #:foo => make-symbol
    ((null package-name)
     (make-symbol symbol-name))

    (t
     (let ((pkg (cond
                  ((eq package-name :current) (or (client-package client)
                                                  (find-package :cl-user))) ;; safer fallback
                  ((eq package-name :keyword) (find-package :keyword))
                  (t (or (find-package package-name)
                         (error "No package named ~a" package-name))))))
       (multiple-value-bind (sym status) (find-symbol symbol-name pkg)
         (cond
           (status sym) ;; found: use it!
           ((and internp (not (eq pkg (find-package "COMMON-LISP"))))
            (intern symbol-name pkg)) ;; only intern if not common-lisp!
           (t (make-symbol symbol-name))))))))

(defmethod eclector.reader:find-character ((client analyzer-client) (name string))
  (or (eclector.reader::find-standard-character name)
      (when (string-equal name "NUL")
        (code-char 0))
      (call-next-method)))

#|
(defmethod eclector.parse-result:make-expression-result ((client analyzer-client) result children source)
(setf (gethash result (client-form-positions client))
(cons (client-current-position client)
(client-last-position client)))
(push result (client-forms client))
result)
|#

(defmethod eclector.reader:evaluate-feature-expression
    ((client analyzer-client) expression)
  (format *error-output* "Feature expression: ~S~%" expression)
  nil)

(defmethod eclector.reader:evaluate-expression ((client analyzer-client) expression)
  (format *error-output* "Read-time eval: ~S~%" expression)
  `(:to-expand ,expression))

(defun parse-file-with-eclector (file-path)
  (let* ((analyses '())
         (client (make-instance 'analyzer-client))
         (eclector.reader:*client* client)
         (file-contents (alexandria:read-file-into-string file-path))
         (line-map (offset-to-line-map file-contents)))
    (with-open-file (raw-stream file-path :direction :input :external-format :utf-8)

      (let* ((tracking (make-instance 'tracking-stream :underlying raw-stream))
             (*readtable* (copy-readtable nil)))

        ;;reset package to cl at start of page
        (setf (client-package client)
              (find-package :cl))
        (loop
          for start = (tracking-stream-position tracking)
          for cst = (handler-case
                        (eclector.concrete-syntax-tree:read tracking nil nil)
                      (eclector.reader:unknown-character-name (c)
                        ;; Safely print the error
                        (format *error-output* "Reader error at position ~A: ~A~%"
                                (tracking-stream-position tracking)
                                c)
                        (read-char nil nil)
                        nil))
          while cst
          for end = (tracking-stream-position tracking)
          for line = (offset-to-line start line-map)
          for head = (concrete-syntax-tree:raw
                      (concrete-syntax-tree:first cst))
          do (progn

               ;;Manually trying to track package usage to resolve
               ;;symbol pacakages elsewhere.
               (when (eq head
                         'in-package)
                 (let ((pkg-name (concrete-syntax-tree:raw
                                  (concrete-syntax-tree:second cst))))
                   (setf (client-package client)
                         (if (symbolp pkg-name)
                             (find-package pkg-name)
                             (find-package (string pkg-name))))))

               (let ((analysis (make-analyzer head)))

                 (setf (analysis-start analysis) start)
                 (setf (analysis-end analysis) end)
                 (setf (analysis-line analysis) line)
                 ;;TODO: Do we need both client and analysis package
                 ;;slots? I dont think so.
                 (setf (analysis-package analysis) (client-package client))
                 (setf (analysis-cst analysis) cst)

                 (setf (client-current-position client) start)
                 (setf (client-last-position client) end)

                 (push analysis analyses))))
        (nreverse analyses)))))

(defun analyze-file (file-path &optional (project nil))
  (declare (ignore project))
  (let (;;First we read the file and create the barebones analysis for
        ;;each form.
        (analyses (parse-file-with-eclector file-path))
        (code-file (make-instance 'code-file :path file-path)))
    (dolist (analysis analyses)
      ;;Do type specific deeper analysis
      (push (analyze-cst (analysis-cst analysis) analysis)
            (file-analyses code-file)))
    code-file))

(defgeneric write-analysis (analysis filename &key))

;;TODO: Need to do specializations per analysis class type at the
;;moment we have stuff that are in an analysis that is not valid for
;;all analysis.
(defmethod write-analysis ((a analysis) filename  &key)
  `(,@`(:name , (analysis-name a)
        :package ,(if (packagep (analysis-package a))
                      (package-name (analysis-package a))
                      (analysis-package a))
        :filename ,filename
        :kind ,(analysis-kind a)
        :line ,(analysis-line a)
        :start ,(analysis-start a)
        :end ,(analysis-end a)
        ;;TODO: Some time or another we need to figure out how to get
        ;;the raw code using start and end so that we dont use the
        ;;read in code! That would make it more usefull when giving it to an AI because then the AI
        :code ,(format nil "~S"
                       (normalize-reader-macros
                        (concrete-syntax-tree:raw
                         (analysis-cst a))))
        :function-calls ,(mapcar #'export-symbol (analysis-function-calls a))
        :macro-calls ,(mapcar #'export-symbol (analysis-macro-calls a))
        :variable-uses ,(mapcar #'export-symbol (analysis-variable-uses a))
        :lexical-definitions ,(mapcar #'export-symbol (analysis-lexical-definitions a))
        :dynamic-definitions ,(mapcar #'export-symbol (analysis-dynamic-definitions a)))
       ;; ✅ :parameters if any
       ,@(when (analysis-parameters a)
           `(:parameters ,(analysis-parameters a)))
       ;; ✅ :docstring if any
       ,@(when (analysis-docstring a)
           `(:docstring ,(analysis-docstring a)))
       #|
       ;; ✅ defpackage-specific slots if any
       ,@(when (typep a 'defpackage-analysis)
       (append
       (when (analysis-nicknames a)
       `(:nicknames ,(analysis-nicknames a)))
       (when (analysis-uses a)
       `(:uses ,(analysis-uses a)))
       (when (analysis-exports a)
       `(:exports ,(analysis-exports a)))
       (when (analysis-shadows a)
       `(:shadows ,(analysis-shadows a)))
       (when (analysis-shadowing-imports a)
       `(:shadowing-imports ,(analysis-shadowing-imports a)))
       (when (analysis-imports a)
       `(:imports ,(analysis-imports a)))
       (when (analysis-interns a)
       `(:interns ,(analysis-interns a)))
       (when (analysis-other-options a)
       `(:other-options ,(analysis-other-options a)))))
       |#))

(defmethod write-analysis ((a defun-analysis) filename &key)
  `(,@(call-next-method)
    ,@(when (analysis-lambda-info a)
        `(:lambda-info ,(analysis-lambda-info a)))))

#|
(defmethod write-analysis ((a defclass-analysis) filename &key)
`(,@(call-next-method a filename)
:superclasses ,(mapcar #'export-symbol (analysis-superclasses a))
:slots ,(mapcar #'serialize-slot (analysis-slots a))))
|#

(defun analyze-project (file-paths &key (name "default-project"))
  (let ((project (make-instance 'code-project :name name)))
    (dolist (file file-paths)
      (push (analyze-file file project) (project-files project)))
    project))

(defun collect-source-files (component)
  (cond
    ((typep component 'asdf:source-file)
     (list (asdf:component-pathname component)))
    ((typep component 'asdf:module)
     (mapcan #'collect-source-files (asdf:component-children component)))
    (t nil)))

(defun all-system-files (system-name &key include-dependencies)
  (let ((visited (make-hash-table :test #'equal)))
    (labels ((recurse (sys)
               (unless (gethash sys visited)
                 (setf (gethash sys visited) t)
                 (let* ((system (asdf:find-system sys))
                        (files (collect-source-files system)))
                   (if include-dependencies
                       (append files
                               (mapcan #'recurse (asdf:system-depends-on system)))
                       files)))))
      (recurse system-name))))

(defun load-system-and-analyze (system-name source-dir &optional file include-dependencies)
  (let* ((absolute-dir (uiop:ensure-directory-pathname (truename source-dir)))
         (asd-file (merge-pathnames
                    (format nil "~A.asd" system-name)
                    absolute-dir)))

    (unless (probe-file asd-file)
      (error "ASD file not found: ~A" asd-file))

    (unless (member absolute-dir asdf:*central-registry* :test #'equal)
      (push absolute-dir asdf:*central-registry*))

    ;; Only load if not already loaded
    (unless (asdf:component-loaded-p
             (asdf:find-system system-name))
      (asdf:load-system system-name))

    (let ((files (if file
                     (list (truename file))
                     (let ((files (all-system-files
                                   system-name
                                   :include-dependencies
                                   include-dependencies)))
                       (unless files
                         (warn "System ~A defines no source files directly."
                               system-name))
                       files))))
      (analyze-project files :name system-name))))

(defun store-project (project)
  (let ((collection (init-project-store (project-name project))))
    (dolist (file (project-files project))

      (let* ((filename (namestring (file-path file)))
             (unsorted-analyses (file-analyses file))
             (analyses (sort unsorted-analyses #'<
                             :key (lambda (a)
                                    (analysis-start a)))))
        (dolist (analysis analyses)
          (cl-naive-store:persist-document
           collection
           (write-analysis analysis
                           filename)))))))

(defun index-project-definitions (system-name source-dir &optional file)
  "Analyze and store project definitions using cl-naive-store."
  (let* ((project (load-system-and-analyze system-name source-dir file)))
    (store-project project)
    project))

