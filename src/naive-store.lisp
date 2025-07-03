;;; naive-store.lisp
;;;
;;; This file handles the setup and interaction with `cl-naive-store`
;;; for persisting and querying code analysis data. It defines how
;;; universes, stores, and collections are initialized for storing
;;; information about code projects and their definitions.

(in-package :cl-naive-code-analyzer)

(defparameter *location* "~/")
(defparameter *multiverse-name* "code-multiverse")
(defparameter *universe-name* "code-universe")
(defparameter *collection-name* "code-definitions")

(defun multiverse-location ()
  (concatenate 'string *location* *multiverse-name* "/"))

(defun universe-location ()
  (concatenate 'string *location* *multiverse-name*
               "/" *universe-name* "/"))

;; Initialize the lparallel kernel for potential parallel processing.
;; The number of workers is set to 2.

;; TODO: Make the number of workers configurable or determine dynamically.
(setf lparallel:*kernel* (lparallel:make-kernel 2))

;; Global variable to hold the multiverse instance.
;; The multiverse is the top-level container in cl-naive-store.
(defparameter *multiverse* nil
  "The global multiverse instance for storing code analysis data.")

;; Global variable to hold the main universe instance within the
;; multiverse. A universe typically corresponds to a specific domain
;; or application.
(defparameter *universe* nil
  "The global universe for code indexing within the *multiverse*.")

;; Initializes the multiverse and the main universe if they haven't
;; been already. Sets up locations on disk for persistence.
(defun init-naive-store ()
  "Initializes the *multiverse* and *universe* for storing code analysis.
   Creates them on disk if they don't exist in memory.
"
  ;; Initialize the multiverse if it's not already set.
  (unless *multiverse*
    (setf *multiverse*
          (make-instance
           'cl-naive-store:multiverse
           :name *multiverse-name*
           :location (multiverse-location)
           :universe-class 'cl-naive-store:universe)))

  ;; Initialize the universe if it's not already set.
  (unless *universe*
    (setf *universe* (add-multiverse-element
                      *multiverse*
                      (make-instance
                       'cl-naive-store:universe
                       :name *universe-name*
                       :multiverse *multiverse*
                       :location (universe-location)
                       :store-class 'cl-naive-store:store)))
    ;; Persist the multiverse definition (which includes the universe definition).
    (cl-naive-store:persist *multiverse* :definitions-only-p t)))

;; Loads and/or initializes store.
;; If projects are supplied only those will be loaded.
(defun load-naive-store (&optional projects)
  ;; Ensure multiverse and universe are initialized
  (init-naive-store)

  ;;If no projects are supplied we load the whole universe.
  (if (not projects)
      (cl-naive-store:load-from-definitions
       *multiverse*
       :universe :with-children-p t :with-data-p t)
      (let ((*universe* (or *universe*
                            (add-multiverse-element
                             *universe*
                             (add-multiverse-element
                              *multiverse*
                              (make-instance
                               'cl-naive-store:universe
                               :name *universe-name*
                               :multiverse *multiverse*
                               :location (universe-location)
                               :store-class 'cl-naive-store:store))))))

        (dolist (project-name projects)
          (let* ((store (cl-naive-store:get-multiverse-element
                         :store *universe* project-name))
                 (collection (and store (cl-naive-store:get-multiverse-element
                                         :collection store *collection-name*))))

            ;;If it does not exist create and persist
            (unless store
              (setf store (add-multiverse-element
                           *universe*
                           (make-instance
                            (store-class *universe*)
                            :name project-name
                            ;; Use indexed-collection for efficient lookups.
                            :collection-class
                            'cl-naive-store.naive-indexed:indexed-collection)))
              (persist store))
            (unless collection
              (setf collection (cl-naive-store:add-multiverse-element
                                store
                                (make-instance
                                 (collection-class store)
                                 :name *collection-name*
                                 :indexes '(;; Index by definition name.
                                            (:name)
                                            ;; Index by file name.
                                            (:file-name)
                                            ;; Index by definition
                                            ;; name and file name
                                            (:name :file-name))))))

            (unless (cl-naive-store:documents collection)
              (cl-naive-store:load-data collection)))))))

(defun load-project (project-name)
  "Initializes or loads the necessary store and collection for the given PROJECT name.
   Returns the 'code-definitions' collection for the project.
   Assumes project data has already been indexed and stored."
  ;; Ensure the universe is initialized.
  (load-naive-store (list project-name))

  ;; Attempt to retrieve an existing store for the project.
  (cl-naive-store:get-multiverse-element
   :collection (cl-naive-store:get-multiverse-element
                :store *universe* project-name)
   *collection-name*))

;; Helper function to create a plist with information about a symbol.
(defun symbol-info (sym)
  "Returns a plist containing the name, package name, and internal status of SYM.
   Returns NIL if SYM is NIL."
  ;; TODO: Consider how to handle symbols that might not have a
  ;;       package (e.g., gensyms not yet interned).  Currently,
  ;;       `symbol-package` on a fresh gensym might be an issue.
  (when sym
    `(:name ,(symbol-name sym)
      :package ,(if (symbol-package sym)
                    (package-name (symbol-package sym))
                    nil)
      ;; Check if the symbol is internal (not directly accessible via
      ;; find-symbol in its home package).  This usually means it's
      ;; uninterned or shadowed.
      :internal-p ,(if (symbol-package sym)
                       (not (eq sym (find-symbol (symbol-name sym)
                                                 (symbol-package sym))))
                       t))))

;; Converts a binding (which can be a symbol or a list like (VAR
;; TYPE)) into a structured plist containing name, package, and
;; optional type information.
(defun structured-binding (binding)
  "Convert a binding (e.g., X, (X TYPE)) into a plist with :name, :package,
   and optional :type information. Type information itself is also structured via `symbol-info`."
  ;; TODO: This function is used in `serialize-slot` (from utils.lisp,
  ;;       presumably).  Ensure its output format is consistent with
  ;;       expectations there.
  (cond
    ((symbolp binding)
     ;; If just a symbol, get its info.
     (symbol-info binding))
    ;; If (VAR TYPE) or (VAR)
    ((and (consp binding) (symbolp (first binding)))
     (let* ((var (first binding))
            ;; Type is optional
            (type (when (consp (rest binding)) (second binding)))
            (var-info (symbol-info var))
            (type-info
              ;; Process type only if it's a symbol
              (when (and type (symbolp type))
                `(:type ,(symbol-info type)))))
       (append var-info type-info)))
    (t
     (error "Invalid binding form: ~S. Expected a symbol or (symbol [type-symbol])."
            binding))))

;; Looks up definitions by name in the given collection.

;; TODO: This function seems to be a specific wrapper around
;;       `index-lookup-values`.  Consider if its name clearly reflects
;;       its specific use case (lookup by :name).
(defun lookup-index (collection definition-name)
  "Looks up documents in COLLECTION where the :name field matches DEFINITION-NAME.
   Uses the pre-defined index on :name for efficient retrieval."
  ;; `index-lookup-values` is specific to
  ;; `cl-naive-store.naive-indexed` collections. It expects a list of
  ;; query lists, where each inner list is a key-value pair for the
  ;; index.
  (index-lookup-values
   collection
   (list (list :name definition-name))))
