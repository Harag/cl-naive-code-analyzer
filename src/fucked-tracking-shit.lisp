(in-package :cl-naive-code-index)

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

(defclass call-tree-client (eclector.parse-result:parse-result-client)
  ((forms :initform '() :accessor client-forms)
   (form-positions :initform (make-hash-table :test 'eq) :accessor client-form-positions)
   (current-position :initform nil :accessor client-current-position)
   (last-position :initform nil :accessor client-last-position)))

(defmethod eclector.parse-result:make-expression-result ((client call-tree-client) result children source)
  (setf (gethash result (client-form-positions client))
        (cons (client-current-position client)
              (client-last-position client)))
  (push result (client-forms client))
  result)

(defmethod eclector.reader:evaluate-expression ((client call-tree-client) expression)
  (format *error-output* "Read-time eval: ~S~%" expression)
  `(:to-expand ,expression))

(defun parse-file-with-eclector (file-path)
  "Parse FILE-PATH using Eclector and return a list of parsed forms with metadata."
  (with-open-file (raw file-path :direction :input :external-format :utf-8)
    (let* ((stream (make-instance 'tracking-stream :underlying raw))
           (client (make-instance 'call-tree-client))
           (eclector.reader:*client* client)
           (forms '())
           (file-contents (alexandria:read-file-into-string file-path))
           (line-map (offset-to-line-map file-contents)))

      (format t "File: ~S~%" file-path)

      (loop for form = (eclector.reader:read stream nil nil)
            while form
            do
            (let* ((start (or (tracking-stream-last-form-start stream) 0))
                   (end   (or (tracking-stream-last-form-end stream)
                              (tracking-stream-position stream)))
                   (line (offset-to-line start line-map))
                   (chunks (copy-list (tracking-stream-chunk-boundaries stream))))

              (push (list :form form
                          :start start
                          :end end
                          :line line
                          :chunk-boundaries chunks)
                    forms))

            ;; Clear tracking state
            (setf (tracking-stream-paren-depth stream) 0)
            (setf (tracking-stream-last-form-start stream) nil)
            (setf (tracking-stream-last-form-end stream) nil)
            (setf (tracking-stream-chunk-boundaries stream) nil)
            (setf (tracking-stream-tracking-active stream) nil))

      (nreverse forms))))

(defun parse-file-with-eclector_ (file-path)
  (let* ((forms '())
         (client (make-instance 'call-tree-client))
         (eclector.reader:*client* client)
         (file-contents (alexandria:read-file-into-string file-path))
         (line-map (offset-to-line-map file-contents)))
    (with-open-file (raw file-path :direction :input :external-format :utf-8)
      (let* ((stream (make-instance 'tracking-stream :underlying raw))
             (*readtable* (copy-readtable nil)))

        (loop
          for pre-form-position = (tracking-stream-position stream)
          for form = (eclector.reader:read stream nil nil)
          while form
          do
          (let* ((form-start pre-form-position)
                 (form-end (tracking-stream-position stream))
                 (line (offset-to-line form-start line-map)))
            (push (list :form form
                        :pre-start pre-form-position
                        :start form-start
                        :end form-end
                        :line line
                        :chunk-boundaries
                        (tracking-stream-chunk-boundaries stream))
                  forms)))))
    (nreverse forms)))

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

(defun register-form-analyzer (type fn)
  (setf (gethash type *form-analyzers*) fn))

(defmacro with-analysis ((var type) &body body)
  `(let ((,var (make-instance ',type)))
     ,@body
     ,var))

(defun normalize-reader-macros (form)
  (cond
    ((consp form)
     (let ((head (car form)))
       (cond
         ((eq head 'eclector.reader:quasiquote)
          `(quasiquote ,(normalize-reader-macros (second form))))
         ((eq head 'eclector.reader:unquote)
          `(unquote ,(normalize-reader-macros (second form))))
         ((eq head 'eclector.reader:unquote-splicing)
          `(unquote-splicing ,(normalize-reader-macros (second form))))
         (t
          (cons (normalize-reader-macros (car form))
                (normalize-reader-macros (cdr form)))))))
    (t form)))

(defgeneric write-analysis (analysis filename name kind definition code
                            &key line start end file-comments comments))

(defparameter *source-file-cache* nil)

(defun get-source-file (filename)
  (or (gethash filename *source-file-cache*)
      (setf (gethash filename *source-file-cache*)
            (alexandria:read-file-into-string filename))))

(defun extract-source-snippet (filename start end)
  (subseq (get-source-file filename) start end))

(defmethod write-analysis ((a analysis) filename name kind definition code
                           &key line start end file-comments comments)
  ;;(break "~S ~S~% ~S" start end code)
  `(:name ,name
    :filename ,filename
    :kind ,kind
    :definition ,definition
    :line ,line
    :start ,start
    :end ,end
    :file-comments ,file-comments
    :comments ,comments
    :code ,(extract-source-snippet filename start end)
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

(defmethod write-analysis ((a defclass-analysis) filename name kind definition code &key line start end file-comments comments)
  (declare (ignorable line start end))
  `(,@(call-next-method a filename name kind definition code :line line :start start :end end :file-comments file-comments :comments comments)
    :superclasses ,(mapcar #'export-symbol (analysis-superclasses a))
    :slots ,(mapcar #'serialize-slot (analysis-slots a))))

(defun analyze-form-dispatch (form file metadata project)
  (let* ((type (car form))
         (analyzer (gethash type *form-analyzers*)))

    (when analyzer
      (funcall analyzer  form :file file :metadata metadata :project project))))

(defun safe-normalize-name (form)
  (when (and (consp form)
             (symbolp (second form)))
    (normalize-name (second form))))

(defun form-is-definition-p (form)
  (let ((head (car form)))
    (cond
      ((member head '(defun defmethod defmacro defclass defstruct deftype defparameter defvar defconstant define-condition))
       :cl)
      ((and (eq head 'defmethod)
            (consp (second form))
            (eq (car (second form)) 'setf))
       :cl)
      ((and (symbolp head)
            (macro-function head))
       ;; Best effort guess — this is a macro and might define something
       :possible-user)
      (t nil))))

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
             (kind `(:name ,name-part :package ,package-part
                           ,@(when setf-method-p '(:setf t))))
             (definition-tag (form-is-definition-p form))
             (metadata `(:start ,start
                         :end ,end
                         :line ,line
                         :source-path ,file-path
                         :definition ,definition-tag))
             (analysis (analyze-form-dispatch form file-path metadata project)))

        ;;(break "analyze-file entry ~S ~S" entry start)

        (when analysis
          (push (make-instance 'code-form
                               :name (or form-name (format nil "anonymous-form-~A" line))
                               :kind kind
                               :form (list :form form
                                           :start start
                                           :end end
                                           :line line
                                           :chunk-boundaries (getf entry :chunk-boundaries))
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

    ;; ✅ Only load if not already loaded
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

(defun string-prefix-p (prefix string &key (start1 0) (start2 0) (end1 nil) (end2 nil))
  "Checks if PREFIX is a prefix of STRING."
  (let ((prefix-length (- (or end1 (length prefix)) start1))
        (string-length (- (or end2 (length string)) start2)))
    (and (<= prefix-length string-length)
         (string= prefix string
                  :start1 start1 :end1 end1
                  :start2 start2 :end2 (+ start2 prefix-length)))))

#|
(split-interstitial-comments ";;;; Some code to test with

;; A global variable")

(string-prefix-p ";" ";; A global variable")

(count #\; ";;;; A global variable")
|#

(defun split-interstitial-comments (text)
  "Splits comments between forms into :file-comments and :comments blocks."
  (let ((lines (uiop:split-string text :separator "\n"))
        (file-comments '())
        (form-comments '()))
    ;; (break "~S" text)
    (dolist (line lines)
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        ;;(break "~S~%~S" trimmed lines)
        (cond
          ;; file-level comment (;;; or more)
          ((and (string-prefix-p ";" trimmed)
                (>= (count #\; trimmed) 3))
           (break "fuck")
           (push line file-comments))
          ;; form-level comment (;; only)
          ((and (string-prefix-p ";;" trimmed)
                (<= (count #\; trimmed) 2))
           (push line form-comments)))))

    (when (> (length file-comments) 0)
      (break "~S" file-comments))

    (values
     (when file-comments (format nil "~{~A~^~%~}" (nreverse file-comments)))
     (when form-comments (format nil "~{~A~^~%~}" (nreverse form-comments))))))

(defun extract-region (filename start end)
  (subseq (get-source-file filename) start end))

(defun extract-form-comments (chunk-boundaries current-start)
  "Find :comment chunks immediately before CURRENT-START."
  (let ((comments nil))
    (dolist (chunk (reverse chunk-boundaries)) ; oldest first
      (destructuring-bind (type chunk-start chunk-end) chunk
        (when (and (eq type :comment)
                   (= chunk-end current-start))
          (push (list :start chunk-start :end chunk-end) comments))))
    comments))

(defun extract-interstitial-chunks (chunks last-end form-start filename)
  "Extract source from all :comment chunks between last form and current form."
  (let ((snippets '()))
    ;; (format t "chunks: ~S~%last-end: ~S~%form-start: ~S~%" chunks last-end form-start)
    (dolist (chunk (reverse chunks))  ; Most recent first
      (destructuring-bind (type chunk-start chunk-end) chunk
        (when (and (eq type :comment)
                   (<= last-end chunk-start)
                   (<= chunk-end form-start))

          ;; (break "help me GTP is a black hole that just sucks the life out of you with endlessly going around and around with the same issues over and over and over ...")
          (push (extract-source-snippet filename chunk-start chunk-end) snippets))))
    (apply #'concatenate 'string (nreverse snippets))))

(defun store-project (project)
  (let ((*source-file-cache* (make-hash-table :test 'equal))
        (collection (init-project-store (project-name project))))
    (dolist (file (project-files project))

      (let* ((filename (namestring (file-path file)))
             (unsorted-forms (file-forms file))
             (forms (sort unsorted-forms #'<
                          :key (lambda (f)
                                 (getf (form-form f) :start))))
             (last-end 0))

        (dolist (form forms)

          (let* ((analysis (form-analysis form))
                 (meta (form-form form))
                 (start (getf meta :start))
                 (end (getf meta :end))
                 (line (getf meta :line))
                 (definition (getf meta :definition))

                 ;; Use tracked chunk boundaries
                 (chunks (getf meta :chunk-boundaries))

                 (interstitial (extract-interstitial-chunks
                                chunks last-end start filename)))

            (multiple-value-bind (file-comments form-comments)
                (split-interstitial-comments interstitial)

              (setf last-end end)
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
                                          :end end
                                          :file-comments file-comments
                                          :comments form-comments))))))))))

(defun store-project_ (project)
  (let ((*source-file-cache* (make-hash-table :test 'equal))
        (collection (init-project-store (project-name project))))
    (dolist (file (project-files project))

      (let* ((filename (namestring (file-path file)))
             (unsorted-forms (file-forms file))
             (forms (sort unsorted-forms #'<
                          :key (lambda (f)
                                 (getf (form-form f) :start))))
             (last-end 0))

        (dolist (form forms)
          (let* ((analysis (form-analysis form))
                 (meta (form-form form))
                 (start (getf meta :start))
                 (end (getf meta :end))
                 (line (getf meta :line))
                 (definition (getf meta :definition))
                 (comment-block (extract-region filename last-end start)))

            (multiple-value-bind (file-comments form-comments)
                (split-interstitial-comments comment-block)

              (setf last-end end)
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
                                          :end end
                                          :file-comments file-comments
                                          :comments form-comments))))))))))

(defun index-project-definitions (system-name source-dir &optional file)
  "Analyze and store project definitions using cl-naive-store."
  (let* (
         (project (load-system-and-analyze system-name source-dir file)))
    (store-project project)
    project))

(defparameter *form-analyzers* (make-hash-table))

;;; analyzers.lisp
(register-form-analyzer
 'defun
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defun-analysis)
     (let* ((args (third form))
            (body (cdddr form)))
       (when (and (consp body) (stringp (first body)))
         (setf (analysis-docstring analysis) (first body)
               body (rest body)))
       (multiple-value-bind (fn-calls macro-calls var-uses lex-defs dyn-defs)
           (extract-calls-and-variables body args)
         (setf (analysis-fn-calls analysis) fn-calls
               (analysis-macro-calls analysis) macro-calls
               (analysis-variable-uses analysis) var-uses
               (analysis-lexical-definitions analysis) lex-defs
               (analysis-dynamic-definitions analysis) dyn-defs))))))

(defclass defmethod-analysis (analysis) ())

(register-form-analyzer
 'defstruct
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defclass-analysis)
     (destructuring-bind (defstruct name &rest options) form
       (declare (ignore defstruct name))
       ;; Extract constructor lambda-list if present
       (let* ((constructor-spec (find-if (lambda (x)
                                           (and (consp x)
                                                (eq (first x) :constructor)))
                                         options))
              (ctor-lambda-list (when (and (consp constructor-spec)
                                           (consp (second constructor-spec)))
                                  (second constructor-spec))))
         (when ctor-lambda-list
           (setf (analysis-lexical-definitions analysis) ctor-lambda-list)))
       ;; Analyze slots
       (let ((slot-specs (remove-if-not #'listp options)))
         (dolist (slot slot-specs)
           (let* ((slot-name (car slot))
                  (details (cdr slot))
                  (initform (cond
                              ((and (consp details)
                                    (null (cdr details)))
                               (car details))
                              ((and (evenp (length details))
                                    (keywordp (car details)))
                               (getf details :initial-value))
                              (t nil))))
             (when initform
               (multiple-value-bind (fn-calls macro-calls var-uses)
                   (extract-calls-and-variables initform nil)
                 (push `(:slot ,slot-name :initform ,initform
                         :function-calls ,(mapcar #'export-symbol fn-calls)
                         :macro-calls ,(mapcar #'export-symbol macro-calls)
                         :variable-uses ,(mapcar #'export-symbol var-uses))
                       (analysis-slots analysis))
                 (setf (analysis-fn-calls analysis)
                       (union (analysis-fn-calls analysis) fn-calls :test #'equal))
                 (setf (analysis-macro-calls analysis)
                       (union (analysis-macro-calls analysis) macro-calls :test #'equal))
                 (setf (analysis-variable-uses analysis)
                       (union (analysis-variable-uses analysis) var-uses :test #'equal)))))))
       analysis))))

(register-form-analyzer
 'defmethod
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defun-analysis)
     (destructuring-bind (defmethod name lambda-list &rest body) form
       (declare (ignore defmethod))

       (when (and (consp name) (eq (car name) 'setf))
         (setf name (list :setf (second name))) ;; Preserve as (:setf foo))
         (when (and (consp body)
                    (stringp (first body))
                    (rest body))
           (setf (analysis-docstring analysis) (first body))
           (setf body (rest body)))
         (let ((normalized-args (flatten-typed-lambda-list lambda-list)))
           (multiple-value-bind (fn-calls macro-calls var-uses lex-defs dyn-defs)
               (extract-calls-and-variables body normalized-args)
             (setf (analysis-fn-calls analysis) fn-calls
                   (analysis-macro-calls analysis) macro-calls
                   (analysis-variable-uses analysis) var-uses
                   (analysis-lexical-definitions analysis) lex-defs
                   (analysis-dynamic-definitions analysis) dyn-defs))))))))

(register-form-analyzer
 'defclass
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defclass-analysis)
     (let ((supers (ensure-list (third form)))
           (slots (fourth form)))
       (setf (analysis-superclasses analysis) (normalize-symbol-list supers))
       (dolist (slot slots)
         (let ((initform (getf (cdr slot) :initform)))
           (when initform
             (multiple-value-bind (fn-calls macro-calls var-uses)
                 (extract-calls-and-variables initform nil)
               (push `(:slot ,(car slot) :initform ,initform
                       :function-calls ,(mapcar #'export-symbol fn-calls)
                       :macro-calls ,(mapcar #'export-symbol macro-calls)
                       :variable-uses ,(mapcar #'export-symbol var-uses))
                     (analysis-slots analysis))
               (setf (analysis-fn-calls analysis)
                     (union (analysis-fn-calls analysis) fn-calls :test #'equal))
               (setf (analysis-macro-calls analysis)
                     (union (analysis-macro-calls analysis) macro-calls :test #'equal))
               (setf (analysis-variable-uses analysis)
                     (union (analysis-variable-uses analysis) var-uses :test #'equal))))))))))

(register-form-analyzer
 'defparameter
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defparameter-analysis)
     (let ((initform (third form)))
       (when initform
         (multiple-value-bind (fn-calls macro-calls var-uses)
             (extract-calls-and-variables initform nil)
           (setf (analysis-fn-calls analysis) fn-calls
                 (analysis-macro-calls analysis) macro-calls
                 (analysis-variable-uses analysis) var-uses)))))))

(register-form-analyzer
 'defvar
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defparameter-analysis)
     (let ((initform (third form)))
       (when initform
         (multiple-value-bind (fn-calls macro-calls var-uses)
             (extract-calls-and-variables initform nil)
           (setf (analysis-fn-calls analysis) fn-calls
                 (analysis-macro-calls analysis) macro-calls
                 (analysis-variable-uses analysis) var-uses)))))))

(register-form-analyzer
 'defconstant
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defparameter-analysis)
     (let ((initform (third form)))
       (when initform
         (multiple-value-bind (fn-calls macro-calls var-uses)
             (extract-calls-and-variables initform nil)
           (setf (analysis-fn-calls analysis) fn-calls
                 (analysis-macro-calls analysis) macro-calls
                 (analysis-variable-uses analysis) var-uses)))))))

(defclass defgeneric-analysis (analysis) ())

(register-form-analyzer
 'defgeneric
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defgeneric-analysis)
     ;; Optional docstring check, similar to defun
     (let ((body (cdddr form)))
       (when (and (consp body) (stringp (first body)))
         (setf (analysis-docstring analysis) (first body))))
     analysis)))

(defclass define-condition-analysis (defclass-analysis) ())

(register-form-analyzer
 'define-condition
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis define-condition-analysis)
     (let ((supers (ensure-list (third form)))
           (slots (fourth form)))
       (setf (analysis-superclasses analysis) (normalize-symbol-list supers))
       (dolist (slot slots)
         (let ((initform (getf (cdr slot) :initform)))
           (when initform
             (multiple-value-bind (fn-calls macro-calls var-uses)
                 (extract-calls-and-variables initform nil)
               (push `(:slot ,(car slot) :initform ,initform
                       :function-calls ,(mapcar #'export-symbol fn-calls)
                       :macro-calls ,(mapcar #'export-symbol macro-calls)
                       :variable-uses ,(mapcar #'export-symbol var-uses))

                     (analysis-slots analysis))
               (setf (analysis-fn-calls analysis)
                     (union (analysis-fn-calls analysis)
                            fn-calls
                            :test #'equal))
               (setf (analysis-macro-calls analysis)
                     (union (analysis-macro-calls analysis)
                            macro-calls
                            :test #'equal))
               (setf (analysis-variable-uses analysis)
                     (union (analysis-variable-uses analysis)
                            var-uses
                            :test #'equal))))))))))

(register-form-analyzer
 'defmacro
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defmacro-analysis)
     (let* ((args (third form))
            (body (cdddr form)))
       (when (and (consp body) (stringp (first body)))
         (setf (analysis-docstring analysis) (first body)
               body (rest body)))
       (multiple-value-bind (fn-calls macro-calls var-uses lex-defs dyn-defs)
           (extract-calls-and-variables body args)
         (setf (analysis-fn-calls analysis) fn-calls
               (analysis-macro-calls analysis) macro-calls
               (analysis-variable-uses analysis) var-uses
               (analysis-lexical-definitions analysis) lex-defs
               (analysis-dynamic-definitions analysis) dyn-defs))))))

(defclass deftype-analysis (analysis) ())

(register-form-analyzer
 'deftype
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis deftype-analysis)
     (let* ((args (third form))
            (body (cdddr form)))
       (multiple-value-bind (fn-calls macro-calls vars)
           (extract-calls-and-variables body args)
         (setf (analysis-fn-calls analysis) fn-calls
               (analysis-macro-calls analysis) macro-calls
               (analysis-variable-uses analysis) vars))))))

(defclass defsetf-analysis (analysis) ())

(register-form-analyzer
 'defsetf
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defsetf-analysis)
     ;; Structure: (defsetf name lambda-list &body body)
     (let ((args (third form))
           (body (cdddr form)))
       (multiple-value-bind (fn-calls macro-calls vars)
           (extract-calls-and-variables body args)
         (setf (analysis-fn-calls analysis) fn-calls
               (analysis-macro-calls analysis) macro-calls
               (analysis-variable-uses analysis) vars))))))

(defclass define-symbol-macro-analysis (analysis) ())

(register-form-analyzer
 'define-symbol-macro
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis define-symbol-macro-analysis)
     (let ((expansion (third form)))
       (when expansion
         (multiple-value-bind (fn-calls macro-calls vars)
             (extract-calls-and-variables expansion nil)
           (setf (analysis-fn-calls analysis) fn-calls
                 (analysis-macro-calls analysis) macro-calls
                 (analysis-variable-uses analysis) vars)))))))

(defclass defpackage-analysis (analysis)
  ((package-name :accessor analysis-package-name :initform nil)
   (exports :accessor analysis-exports :initform nil)
   (uses :accessor analysis-uses :initform nil)))

(register-form-analyzer
 'defpackage
 (lambda (form &key file metadata project)
   (declare (ignore file metadata project))
   (with-analysis (analysis defpackage-analysis)
     (destructuring-bind (defpackage name &rest options) form
       (declare (ignore defpackage))
       (setf (analysis-package-name analysis) name)
       (dolist (option options)
         (case (car option)
           (:use (setf (analysis-uses analysis) (cdr option)))
           (:export (setf (analysis-exports analysis) (cdr option)))))))))

;;(index-project-definitions "test-code" "/home/phil/source/naive/cl-naive-code-index/tests/test-code/")

;;(index-project-definitions "cl-naive-store.naive-core" "/home/phil/source/naive/cl-naive-store/")

;;;(index-project-definitions "insite" "/home/phil/source/acquire/insite-product/insite/")

(prin1-to-string  '(symbol))
