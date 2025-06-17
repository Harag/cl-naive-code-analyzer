(in-package :cl-naive-code-analyzer)

(defclass analysis ()
  ((name :accessor analysis-name :initform nil)
   (fn-calls :accessor analysis-fn-calls :initform nil)
   (macro-calls :accessor analysis-macro-calls :initform nil)
   (variable-uses :accessor analysis-variable-uses :initform nil)
   (local-function-calls :accessor analysis-local-function-calls :initform nil)
   (local-variable-uses :accessor analysis-local-variable-uses :initform nil)
   (lexical-definitions :accessor analysis-lexical-definitions :initform nil)
   (dynamic-definitions :accessor analysis-dynamic-definitions :initform nil)
   (parameters :accessor analysis-parameters :initform nil)
   (docstring :accessor analysis-docstring :initform nil)
   (raw-body :accessor analysis-raw-body :initform nil)))

(defclass analyzer-client (eclector.parse-result:parse-result-client)
  ((forms :initform '() :accessor client-forms)
   (form-positions :initform (make-hash-table :test 'eq) :accessor client-form-positions)
   (current-position :initform nil :accessor client-current-position)
   (last-position :initform nil :accessor client-last-position)
   (package :accessor client-package :initarg :package :initform (find-package :cl))))

(defmethod eclector.reader:interpret-symbol ((client analyzer-client)
                                             input-stream package-name symbol-name internp)
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

(defmethod eclector.parse-result:make-expression-result ((client analyzer-client) result children source)
  (setf (gethash result (client-form-positions client))
        (cons (client-current-position client)
              (client-last-position client)))
  (push result (client-forms client))
  result)

(defmethod eclector.reader:evaluate-feature-expression
    ((client analyzer-client) expression)
  (format *error-output* "Feature expression: ~S~%" expression)
  nil)

(defmethod eclector.reader:evaluate-expression ((client analyzer-client) expression)
  (format *error-output* "Read-time eval: ~S~%" expression)
  `(:to-expand ,expression))

(defun parse-file-with-eclector (file-path)
  (let* ((forms '())
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
          for form = (handler-case
                         (eclector.reader:read tracking nil nil)
                       (eclector.reader:unknown-character-name (c)
                         ;; Safely print the error
                         (format *error-output* "Reader error at position ~A: ~A~%"
                                 (tracking-stream-position tracking)
                                 c)
                         (read-char nil nil)
                         nil))
          while form
          for end = (tracking-stream-position tracking)
          for line = (offset-to-line start line-map)
          do (progn
               (when (eq (first form) 'in-package)
                 (let ((pkg-name (second form)))
                   (setf (client-package client)
                         (if (symbolp pkg-name)
                             (find-package pkg-name)
                             (find-package (string pkg-name))))))

               (setf (client-current-position client) start)
               (setf (client-last-position client) end)
               (push (list :form form
                           :start start
                           :end end
                           :line line
                           :package (client-package client))
                     forms))))
      (nreverse forms))))

(defun parse-file-forms (file-path)
  (mapcar #'(lambda (entry)
              (getf entry :form))
          (parse-file-with-eclector file-path)))

(defun walk-analysis-hook (form env analysis)
  (cond
    ((symbolp form)
     (cond ((member form env :test #'equal)
            (pushnew form (analysis-local-variable-uses analysis) :test #'equal))
           (t
            (pushnew form (analysis-variable-uses analysis) :test #'equal))))
    ((and (consp form)
          (symbolp (car form)))
     (let ((head (car form)))
       (cond
         ((macro-function head)
          (pushnew head (analysis-macro-calls analysis) :test #'equal))
         ((member head env :test #'equal)
          (pushnew head (analysis-local-function-calls analysis) :test #'equal))
         (t
          (pushnew head (analysis-fn-calls analysis) :test #'equal))))))
  nil)

(defun walk-form (form args analysis)
  (format t "~&[WALK-FORM] Root form: ~S~%" form)
  ;; Build a lexenv with the known local variables
  (let ((env (portable-sb-walker::make-lexenv :vars args)))
    (portable-sb-walker:walk-form
     form
     env
     (lambda (subform context walker-env)
       (format t "~&[WALK] Subform: ~S | Context: ~S~%" subform context)

       ;; Detect real function or macro calls:
       (when (and (consp subform)
                  (symbolp (car subform))
                  (or (macro-function (car subform))
                      (fboundp (car subform))))
         (let ((head (car subform)))
           (format t "~&[WALK] Call Head: ~S~%" head)
           (cond
             ((macro-function head)
              (pushnew head (analysis-macro-calls analysis) :test #'equal))
             (t
              (pushnew head (analysis-fn-calls analysis) :test #'equal)))))

       ;; Detect variable usage (local or global)
       (when (symbolp subform)
         (cond
           ((member subform (portable-sb-walker::lexenv-vars walker-env) :test #'eq)
            (pushnew subform (analysis-local-variable-uses analysis) :test #'equal))
           (t
            (pushnew subform (analysis-variable-uses analysis) :test #'equal))))

                                        ; Optional: do user-defined hooks if needed:
       (walk-analysis-hook subform args analysis)))))

(defun extract-calls-and-variables (form env-bindings lexical-defs)
  "Walk FORM with ENV-BINDINGS for scoping, record LEXICAL-DEFS for output."
  (let ((analysis (make-instance 'analysis)))
    ;; Save explicit lexical definitions for reporting
    (when lexical-defs
      (setf (analysis-lexical-definitions analysis) (copy-list lexical-defs)))
    ;; Walk with env bindings for scoping only
    (walk-form form env-bindings analysis)
    ;; Return facts
    (values (analysis-fn-calls analysis)
            (analysis-macro-calls analysis)
            (analysis-variable-uses analysis)
            (analysis-lexical-definitions analysis)
            (analysis-dynamic-definitions analysis))))

(defclass defun-analysis (analysis) ())
(defclass defclass-analysis (analysis)
  ((slots :accessor analysis-slots :initform nil)
   (superclasses :accessor analysis-superclasses :initform nil)))
(defclass defparameter-analysis (analysis) ())
(defclass defmacro-analysis (analysis) ())

(defclass code-form ()
  ((name :accessor form-name :initarg :name)
   (kind :accessor form-kind :initarg :kind)
   (form :accessor form-form :initarg :form)
   (analysis :accessor form-analysis :initarg :analysis)))

(defclass code-file ()
  ((path :accessor file-path :initarg :path)
   (forms :accessor file-forms :initform nil)))

(defclass code-project ()
  ((name :accessor project-name :initarg :name)
   (files :accessor project-files :initform nil)))

(defgeneric merge-analysis (a b))

(defmethod merge-analysis ((a defun-analysis) (b defun-analysis))
  (setf (analysis-fn-calls a)
        (union (analysis-fn-calls a)
               (analysis-fn-calls b)
               :test #'equal))
  (setf (analysis-macro-calls a)
        (union (analysis-macro-calls a)
               (analysis-macro-calls b)
               :test #'equal))
  (setf (analysis-variable-uses a)
        (union (analysis-variable-uses a)
               (analysis-variable-uses b)
               :test #'equal))
  (setf (analysis-lexical-definitions a)
        (union (analysis-lexical-definitions a)
               (analysis-lexical-definitions b)
               :test #'equal))
  (setf (analysis-dynamic-definitions a)
        (union (analysis-dynamic-definitions a)
               (analysis-dynamic-definitions b)
               :test #'equal))
  (setf (analysis-docstring a)
        (or (analysis-docstring a)
            (analysis-docstring b)))
  a)

(defmethod merge-analysis ((a defclass-analysis) (b defclass-analysis))
  (call-next-method)
  (setf (analysis-slots a)
        (append (analysis-slots a) (analysis-slots b)))
  (setf (analysis-superclasses a)
        (union (analysis-superclasses a)
               (analysis-superclasses b) :test #'equal))
  a)

(defmethod merge-analysis ((a defparameter-analysis) (b defparameter-analysis))
  (call-next-method)
  a)

(defmethod merge-analysis ((a defmacro-analysis) (b defmacro-analysis))
  (call-next-method)
  a)

(defmacro with-analysis ((var type) &body body)
  `(let ((,var (make-instance ',type)))
     ,@body
     ,var))

(defgeneric write-analysis (analysis filename name kind definition code &key line start end package))

(defmethod write-analysis ((a analysis) filename name kind definition code &key line start end package)
  `(,@`(:name ,(or (getf (slot-value a 'name) :name) name)

              :package ,(if (packagep package)
                            (package-name package)
                            package)
              :filename ,filename
              :kind ,kind
              :definition ,definition
              :line ,line
              :start ,start
              :end ,end
              ;; 📌 Prefer raw-body if set, fallback to plain code
              :code ,(format nil "~S"
                             (normalize-reader-macros
                              (or (ignore-errors (slot-value a 'raw-body)) code)))
              :function-calls ,(mapcar #'export-symbol (analysis-fn-calls a))
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
              `(:other-options ,(analysis-other-options a)))))))

(defun parse-lambda-list+metadata (lambda-list)
  "Parse LAMBDA-LIST into:
   (values symbols param-metadata)

   param-metadata preserves :name, :default, :specializer, :role etc."
  (let ((mode :required)
        (result '())
        (bound-vars '()))
    (labels ((record-param (var &key default specializer)
               (let ((sym-info (export-symbol var))
                     (entry '()))
                 (setf entry sym-info)
                 (when default
                   (setf entry (append entry `(:default ,default))))
                 (when specializer
                   (let ((spec-info
                           (if (and (consp specializer) (eq (car specializer) 'eql))
                               ;; EQL specializer: record operator and value
                               `(:name "EQL" :package "common-lisp"
                                 :value ,(second specializer))
                               ;; normal type
                               (export-symbol specializer))))
                     (setf entry (append entry `(:specializer ,spec-info)))))
                 (push entry result)
                 (push var bound-vars))))
      (loop for item in lambda-list do
            (cond
              ;; Mode switches:
              ((member item '(&optional &key &rest &body &aux &whole))
               (setf mode item))

              ;; &whole: consume the next symbol ONLY
              ((eq mode '&whole)
               (record-param item)
               ;; after &whole, back to normal
               (setf mode :required))

              ;; &rest / &body: bind exactly one param
              ((or (eq mode '&rest) (eq mode '&body))
               (record-param item)
               (setf mode :done))

              ;; &aux: bindings are (var [initform])
              ((eq mode '&aux)
               (etypecase item
                 (symbol (record-param item))
                 (list (record-param (first item)
                                     :default (second item)))))

              ;; &optional / &key: (var [default])
              ((or (eq mode '&optional) (eq mode '&key))
               (etypecase item
                 (symbol (record-param item))
                 (list (record-param (first item)
                                     :default (second item)))))

              ;; default: either plain symbol or (var type)
              (t
               (etypecase item
                 (symbol (record-param item))
                 (list (record-param (first item)
                                     :specializer (second item)))))))
      (values (nreverse bound-vars)
              (nreverse result)))))

(defun serialize-slot (slot)
  (let ((copy (copy-list slot)))
    (let* ((initform (getf copy :initform))
           (kind nil)
           (raw nil)
           (analyzed nil))

      (cond
        ((and (consp initform) (eq (car initform) 'quote))
         (setf kind :quoted-symbol
               raw (format nil "~S" initform)
               initform (cadr initform)))

        ((and (consp initform) (eq (car initform) 'function))
         (setf kind :function
               raw (format nil "~S" initform)
               initform (export-symbol (cadr initform))))

        ((and (consp initform) (eq (car initform) 'lambda))
         (setf kind :lambda
               raw (format nil "~S" initform))
         (multiple-value-bind (fn macro vars)
             ;; NEW: updated API: env = nil, defs = nil
             (extract-calls-and-variables initform nil nil)
           (setf analyzed `((:function-calls ,(mapcar #'export-symbol fn))
                            (:macro-calls ,(mapcar #'export-symbol macro))
                            (:variable-uses ,(mapcar #'export-symbol vars))))))

        ((symbolp initform)
         (setf kind :symbol
               raw (format nil "~S" initform)
               initform (export-symbol initform)))

        ((or (numberp initform) (stringp initform) (keywordp initform))
         (setf kind :literal
               raw (format nil "~S" initform)))

        ((listp initform)
         ;; Treat general form like (1+ 42)
         (setf kind :computed-form
               raw (format nil "~S" initform))
         (multiple-value-bind (fn macro vars)
             ;; NEW: updated API: env = nil, defs = nil
             (extract-calls-and-variables initform nil nil)
           (setf analyzed `((:function-calls ,(mapcar #'export-symbol fn))
                            (:macro-calls ,(mapcar #'export-symbol macro))
                            (:variable-uses ,(mapcar #'export-symbol vars))))))

        ((functionp initform)
         (multiple-value-bind (expr name) (function-lambda-expression initform)
           (cond
             (name
              (setf kind :function
                    raw (format nil "~S" initform)
                    initform (export-symbol name)))
             (expr
              (setf kind :lambda
                    raw (format nil "~S" expr))
              (multiple-value-bind (fn macro vars)
                  ;; NEW: updated API: env = nil, defs = nil
                  (extract-calls-and-variables expr nil nil)
                (setf analyzed `((:function-calls ,(mapcar #'export-symbol fn))
                                 (:macro-calls ,(mapcar #'export-symbol macro))
                                 (:variable-uses ,(mapcar #'export-symbol vars)))))))
           (unless kind
             (setf kind :function
                   raw (format nil "~S" initform)))))

        (t
         (setf kind :unknown
               raw (format nil "~S" initform))))

      ;; Replace initform slot with a rich object
      (setf (getf copy :initform)
            `(:kind ,kind :raw ,raw :value ,initform :analyzed ,analyzed)))
    copy))

(defmethod write-analysis ((a defclass-analysis) filename name kind definition code
                           &key line start end package)
  (declare (ignorable line start end))
  `(,@(call-next-method a filename name kind definition code :line line
                                                             :start start
                                                             :end end
                                                             :package package)
    :superclasses ,(mapcar #'export-symbol (analysis-superclasses a))
    :slots ,(mapcar #'serialize-slot (analysis-slots a))))

(defun fallback-form-analyzer (form &key file metadata project)
  (declare (ignore file metadata project))
  (with-analysis (analysis defun-analysis)
    ;; If the form looks like (foo ...) then foo is the "name"
    (when (and (consp form)
               (symbolp (first form)))
      (setf (analysis-name analysis)
            (export-symbol (first form))))
    ;; Extract calls as usual:
    (multiple-value-bind (fn macro var lex dyn)
        (extract-calls-and-variables form nil nil)
      (setf (analysis-fn-calls analysis) fn
            (analysis-macro-calls analysis) macro
            (analysis-variable-uses analysis) var
            (analysis-lexical-definitions analysis) lex
            (analysis-dynamic-definitions analysis) dyn))
    analysis))

(defun analyze-form-dispatch (form file metadata project)
  (let* ((type (car form))
         (analyzer (gethash type *form-analyzers*)))

    (if analyzer
        (funcall analyzer form :file file :metadata metadata :project project)
        ;; fallback attempt
        (fallback-form-analyzer form :file file :metadata metadata :project project))))

(defun analyze-file (file-path &optional (project nil))
  (let ((forms-with-meta (parse-file-with-eclector file-path))
        (code-file (make-instance 'code-file :path file-path)))
    (dolist (entry forms-with-meta)
      (let* ((form (getf entry :form))
             (start (getf entry :start))
             (end (getf entry :end))
             (line (getf entry :line))
             (package (getf entry :package))
             (form-name (safe-normalize-name form))
             (raw-kind (car form))
             (name-part (string-downcase (symbol-name raw-kind)))
             (package-part (string-downcase (package-name (symbol-package raw-kind))))
             (setf-method-p (and (eq raw-kind 'defmethod)
                                 (consp (second form))
                                 (eq (car (second form)) 'setf)))
             (kind `(:name ,name-part :package ,package-part ,@(when setf-method-p '(:setf t))))
             (definition-tag (form-is-definition-p form))
             (metadata `(:start ,start :end ,end :line ,line :source-path ,file-path
                         :definition ,definition-tag))
             (analysis (analyze-form-dispatch form file-path metadata project)))

        (when analysis
          (push (make-instance 'code-form
                               :name (or form-name (format nil "anonymous-form-~A" line))
                               :kind kind
                               :form (list :form form
                                           :start start
                                           :end end
                                           :line line
                                           :package package)
                               :analysis analysis)
                (file-forms code-file)))))
    code-file))

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
             (unsorted-forms (file-forms file))
             (forms (sort unsorted-forms #'<
                          :key (lambda (f)
                                 (getf (form-form f) :start)))))
        (dolist (form forms)
          (let* ((analysis (form-analysis form))
                 (meta (form-form form))
                 (start (getf meta :start))
                 (end (getf meta :end))
                 (line (getf meta :line))
                 (definition (getf meta :definition))
                 (package (getf meta :package)))
            (cl-naive-store:persist-document
             collection
             (write-analysis analysis
                             filename
                             (form-name form)
                             (form-kind form)
                             definition
                             (getf meta :form)
                             :line line
                             :start start
                             :end end
                             :package package))))))))

(defun index-project-definitions (system-name source-dir &optional file)
  "Analyze and store project definitions using cl-naive-store."
  (let* ((project (load-system-and-analyze system-name source-dir file)))
    (store-project project)
    project))

