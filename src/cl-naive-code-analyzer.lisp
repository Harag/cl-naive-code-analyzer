(in-package :cl-naive-code-analyzer)

(defparameter *form-analyzers* (make-hash-table))

(defclass analysis ()
  ((fn-calls :accessor analysis-fn-calls :initform nil)
   (macro-calls :accessor analysis-macro-calls :initform nil)
   (variable-uses :accessor analysis-variable-uses :initform nil)
   (local-function-calls :accessor analysis-local-function-calls :initform nil)
   (local-variable-uses :accessor analysis-local-variable-uses :initform nil)
   (lexical-definitions :accessor analysis-lexical-definitions :initform nil)
   (dynamic-definitions :accessor analysis-dynamic-definitions :initform nil)
   (docstring :accessor analysis-docstring :initform nil)))

(defclass analyzer-client (eclector.parse-result:parse-result-client)
  ((forms :initform '() :accessor client-forms)
   (form-positions :initform (make-hash-table :test 'eq) :accessor client-form-positions)
   (current-position :initform nil :accessor client-current-position)
   (last-position :initform nil :accessor client-last-position)))

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

               (setf (client-current-position client) start)
               (setf (client-last-position client) end)
               (push (list :form form :start start :end end :line line) forms)))))
    (nreverse forms)))

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

(defun walk-form (form env analysis)
  (portable-sb-walker:walk-form
   form
   nil
   (lambda (subform context walker-env)
     ;; Track function calls
     (when (and (consp subform)
                (eq context :call)
                (symbolp (car subform)))
       (let ((head (car subform)))
         (cond
           ((member head (portable-sb-walker::lexenv-vars walker-env) :test #'eq)
            (pushnew head (analysis-local-function-calls analysis) :test #'equal))
           ((macro-function head)
            (pushnew head (analysis-macro-calls analysis) :test #'equal))
           (t
            (pushnew head (analysis-fn-calls analysis) :test #'equal)))))

     ;; Track variable uses
     (when (symbolp subform)
       (cond
         ((member subform (portable-sb-walker::lexenv-vars walker-env) :test #'eq)
          (pushnew subform (analysis-local-variable-uses analysis) :test #'equal))
         (t
          (pushnew subform (analysis-variable-uses analysis) :test #'equal))))

     ;; Continue walking
     (walk-analysis-hook subform env analysis))))

(defun extract-calls-and-variables (body args)
  (let ((analysis (make-instance 'analysis)))
    (when args
      (setf (analysis-lexical-definitions analysis) (copy-list args)))
    (walk-form body args analysis)
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

(defgeneric write-analysis (analysis filename name kind definition code &key line start end))

(defmethod write-analysis ((a analysis) filename name kind definition code &key line start end)
  `(:name ,name
    :filename ,filename
    :kind ,kind
    :definition ,definition
    :line ,line
    :start ,start
    :end ,end
    :code ,(format nil "~S" (normalize-reader-macros code))
    :function-calls ,(mapcar #'export-symbol (analysis-fn-calls a))
    :macro-calls ,(mapcar #'export-symbol (analysis-macro-calls a))
    :variable-uses ,(mapcar #'export-symbol (analysis-variable-uses a))
    :lexical-definitions ,(mapcar #'export-symbol (analysis-lexical-definitions a))
    :dynamic-definitions ,(mapcar #'export-symbol (analysis-dynamic-definitions a))
    :docstring ,(analysis-docstring a)))

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
             (extract-calls-and-variables initform nil)
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
             (extract-calls-and-variables initform nil)
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
                  (extract-calls-and-variables expr nil)
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
                           &key line start end)
  (declare (ignorable line start end))
  `(,@(call-next-method a filename name kind definition code :line line :start start :end end)
    :superclasses ,(mapcar #'export-symbol (analysis-superclasses a))
    :slots ,(mapcar #'serialize-slot (analysis-slots a))))

(defun fallback-form-analyzer (form &key file metadata project)
  (declare (ignore file metadata project))
  (with-analysis (analysis defun-analysis)
    (when (and (consp form)
               (symbolp (car form)))
      (let (
            (maybe-args nil)
            (body (cddr form))) ;; skip head and second

        ;; Check if the second element is a *plausible* lambda list
        ;; (list of symbols or typed args)
        (let ((candidate (second form)))
          (when (and (consp candidate)
                     (every (lambda (item)
                              (or (symbolp item)
                                  (and (consp item)
                                       (symbolp (first item))))) ; handle (&optional x)
                            candidate))
            (setf maybe-args candidate)
            (setf body (cddr form))))

        ;; Only analyze body if we have plausible args
        (when maybe-args
          (let ((normalized-args (flatten-typed-lambda-list maybe-args)))
            (multiple-value-bind (fn-calls macro-calls var-uses lex-defs dyn-defs)
                (extract-calls-and-variables body normalized-args)
              (setf (analysis-fn-calls analysis) fn-calls
                    (analysis-macro-calls analysis) macro-calls
                    (analysis-variable-uses analysis) var-uses
                    (analysis-lexical-definitions analysis) lex-defs
                    (analysis-dynamic-definitions analysis) dyn-defs)))))

      analysis)))

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
                               :form (list :form form :start start :end end :line line)
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
                 (definition (getf meta :definition)))
            (cl-naive-store:persist-document
             collection
             (cl-naive-store:make-document
              :store (store collection)
              :collection collection
              :document-type "code-definition"
              :elements (write-analysis analysis
                                        filename
                                        (form-name form)
                                        (form-kind form)
                                        definition
                                        (getf meta :form)
                                        :line line
                                        :start start
                                        :end end)))))))))

(defun index-project-definitions (system-name source-dir &optional file)
  "Analyze and store project definitions using cl-naive-store."
  (let* ((project (load-system-and-analyze system-name source-dir file)))
    (store-project project)
    project))

