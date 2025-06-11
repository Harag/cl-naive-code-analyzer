(in-package :cl-naive-code-analyzer)

(setf lparallel:*kernel* (lparallel:make-kernel 2))

(defparameter *multiverse* nil)

(defparameter *universe* nil)

(defun init-universe ()
  (unless *multiverse*
    (setf *multiverse*
          (make-instance
           'multiverse
           :name "code-index-multiverse"
           :location "~/code-index-multiverse/" ;Setting the location on disk.
           :universe-class 'universe)))
  (unless *universe*
    (setf *universe* (add-multiverse-element
                      *multiverse*
                      (make-instance
                       'universe
                       :name "code-index-universe"
                       :multiverse *multiverse*
                       :location "~/code-index-multiverse/code-index-universe/" ;Setting the location on disk.
                       :store-class 'document-store)))
    ;;Persist store definition with its document type and collection.
    (cl-naive-store:persist *multiverse* :definitions-only-p t)))

(defparameter *definition-document-type*
  '(:document-type (:name "code-definition"
                    :label "Code Definition"
                    :elements
                    ((:element
                      (:name :name
                       :label "Name"
                       :concrete-type :string
                       :key-p t))
                     (:element
                      (:name :package
                       :label "Package"
                       :concrete-type :string))
                     (:element
                      (:name :filename
                       :label "Filename"
                       :concrete-type :string))
                     (:element
                      (:name :type
                       :label "Top-Level Type"
                       :concrete-type :symbol))
                     (:element
                      (:name :kind
                       :label "Kind"
                       :concrete-type :symbol))
                     (:element
                      (:name :docstring
                       :label "Docstring"
                       :concrete-type :string))
                     (:element
                      (:name :code
                       :label "Code"
                       :concrete-type :string))
                     (:element
                      (:name :function-calls
                       :label "Function Calls"
                       :concrete-type :list))
                     (:element
                      (:name :macro-calls
                       :label "Macro Calls"
                       :concrete-type :list))
                     (:element
                      (:name :variables
                       :label "Referenced Vars"
                       :concrete-type :list))
                     (:element
                      (:name :lexical-bindings
                       :label "Lexical Bindings"
                       :concrete-type :list))
                     (:element
                      (:name :dynamic-bindings
                       :label "Dynamic Bindings"
                       :concrete-type :list))))))

(defun symbol-info (sym)
  (when sym
    `(:name ,(symbol-name sym)
      :package ,(package-name (symbol-package sym))
      :internal-p ,(not (eq sym (find-symbol (symbol-name sym)
                                             (symbol-package sym)))))))

(defun structured-binding (binding)
  "Convert a binding (X TYPE) or symbol X into a plist with :name, :package, and optional :type info (also symbol info)."
  (cond
    ((symbolp binding)
     (symbol-info binding)) ; just the variable name
    ((and (consp binding) (symbolp (first binding)))
     (let* ((var (first binding))
            (type (second binding))
            (var-info (symbol-info var))
            (type-info (when (symbolp type)
                         `(:type ,(symbol-info type)))))
       (append var-info type-info)))
    (t
     (error "Invalid binding form: ~S" binding))))

(defun store-definition (collection def)
  (persist-document
   collection
   (make-document
    :store (store collection)
    :collection collection
    :document-type "code-definition"
    :elements
    (append
     (list
      :name (string (getf def :name))
      :package (getf def :package)
      :filename (getf def :filename)
      :type (getf def :type)
      :kind (getf def :kind)
      :docstring (getf def :docstring)
      :code (prin1-to-string (getf def :code))
      :function-calls (mapcar #'symbol-info (getf def :function-calls))
      :macro-calls (mapcar #'symbol-info (getf def :macro-calls))
      :variables (mapcar #'symbol-info (getf def :variables))
      :lexical-bindings (mapcar #'structured-binding (getf def :lexical-bindings))
      :dynamic-bindings (mapcar #'symbol-info (getf def :dynamic-bindings))
      :line (getf def :line)
      :offset (getf def :offset)
      :end-offset (getf def :end-offset))
     (when (member (getf def :kind) '(:class :struct))
       (list :slots (getf def :slots)))
     (when (eq (getf def :kind) :class)
       (list :superclasses (getf def :superclasses)))))))

(defun init-project-store (project-name)
  (when (or (not *multiverse*) (not *universe*))
    (init-universe))

  (let* ((store  (cl-naive-store:get-multiverse-element
                  :store *universe* project-name))

         (collection (and store (cl-naive-store:get-multiverse-element
                                 :collection store project-name))))

    (unless store
      (setf store (add-multiverse-element
                   *universe*
                   (make-instance
                    (store-class *universe*)
                    :name project-name
                    :collection-class
                    'cl-naive-store.naive-documents:document-collection)))
      (persist store
               :definitions-only-p t))

    (unless collection
      (let ((document-type (load-from-definition
                            store
                            :document-type
                            *definition-document-type*
                            :with-children-p t)))

        (setf collection
              (cl-naive-store:add-multiverse-element
               store
               (make-instance (collection-class store)
                              :name project-name
                              :document-type document-type
                              ;; Not specifying the keys to show
                              ;; that they are retrieved from the document-type

                              ;; Specifying the elements to set up indexes for.
                              :indexes '((:name)))))

        (persist collection
                 :definitions-only-p t)))
    collection))

(defun query-index (collection test)
  (query-data collection :query (lambda (document)
                                  (funcall test document))))

(defun lookup-index (collection definition-name)
  (index-lookup-values
   collection
   (list (list :name definition-name))))
