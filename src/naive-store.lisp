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
                       :store-class 'store)))
    ;;Persist store definition
    (cl-naive-store:persist *multiverse* :definitions-only-p t)))

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

(defun init-project-store (project-name)
  (when (or (not *multiverse*) (not *universe*))
    (init-universe))

  (let* ((store  (cl-naive-store:get-multiverse-element
                  :store *universe* project-name))

         (collection (and store (cl-naive-store:get-multiverse-element
                                 :collection store "code-definitions"))))

    (unless store
      (setf store (add-multiverse-element
                   *universe*
                   (make-instance
                    (store-class *universe*)
                    :name project-name
                    :collection-class
                    'cl-naive-store.naive-indexed:indexed-collection)))
      (persist store
               :definitions-only-p t))

    (unless collection

      (setf collection
            (cl-naive-store:add-multiverse-element
             store
             (make-instance (collection-class store)
                            :name "code-definitions"
                            :indexes '((:name)
                                       (:file-name)
                                       (:name :file-name)))))
      (persist collection
               :definitions-only-p t))
    collection))

(defun lookup-index (collection definition-name)
  (index-lookup-values
   collection
   (list (list :name definition-name))))
