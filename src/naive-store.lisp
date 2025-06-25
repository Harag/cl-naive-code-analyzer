;;; naive-store.lisp
;;;
;;; This file handles the setup and interaction with `cl-naive-store`
;;; for persisting and querying code analysis data. It defines how
;;; universes, stores, and collections are initialized for storing
;;; information about code projects and their definitions.
;;;
;;; TODO: Review the hardcoded paths like "~/code-index-multiverse/" and consider
;;;       making them configurable or more platform-independent.
;;; TODO: Ensure thread safety if the lparallel kernel implies concurrent access
;;;       to the store, although `cl-naive-store` itself might handle this.

(in-package :cl-naive-code-analyzer)

;; Initialize the lparallel kernel for potential parallel processing.
;; The number of workers is set to 2.
;; TODO: Make the number of workers configurable or determine dynamically.
(setf lparallel:*kernel* (lparallel:make-kernel 2))

;; Global variable to hold the multiverse instance.
;; The multiverse is the top-level container in cl-naive-store.
(defparameter *multiverse* nil
  "The global multiverse instance for storing code analysis data.")

;; Global variable to hold the main universe instance within the multiverse.
;; A universe typically corresponds to a specific domain or application.
(defparameter *universe* nil
  "The global universe for code indexing within the *multiverse*.")

;; Initializes the multiverse and the main universe if they haven't been already.
;; Sets up locations on disk for persistence.
(defun init-universe ()
  "Initializes the *multiverse* and *universe* for storing code analysis.
   Creates them on disk if they don't exist in memory.
   The multiverse is named 'code-index-multiverse' and located at '~/code-index-multiverse/'.
   The universe is named 'code-index-universe' and located within the multiverse's path."
  ;; Initialize the multiverse if it's not already set.
  (unless *multiverse*
    (setf *multiverse*
          (make-instance
           'multiverse ; Assuming 'multiverse' is a class provided by cl-naive-store or defined elsewhere.
           :name "code-index-multiverse"
           :location "~/code-index-multiverse/" ; TODO: Make this path configurable.
           :universe-class 'universe))) ; Specifies the class for universes within this multiverse.
  ;; Initialize the universe if it's not already set.
  (unless *universe*
    (setf *universe* (add-multiverse-element
                      *multiverse*
                      (make-instance
                       'cl-naive-store:universe ; Explicitly using cl-naive-store:universe.
                       :name "code-index-universe"
                       :multiverse *multiverse*
                       :location "~/code-index-multiverse/code-index-universe/" ; TODO: Make this path configurable.
                       :store-class 'cl-naive-store:store))) ; Specifies the default class for stores.
    ;; Persist the multiverse definition (which includes the universe definition).
    (cl-naive-store:persist *multiverse* :definitions-only-p t)))

;; Helper function to create a plist with information about a symbol.
(defun symbol-info (sym)
  "Returns a plist containing the name, package name, and internal status of SYM.
   Returns NIL if SYM is NIL."
  ;; TODO: Consider how to handle symbols that might not have a package (e.g., gensyms not yet interned).
  ;;       Currently, `symbol-package` on a fresh gensym might be an issue.
  (when sym
    `(:name ,(symbol-name sym)
      :package ,(if (symbol-package sym) ; Handle case where symbol might not have a package
                    (package-name (symbol-package sym))
                    nil)
      ;; Check if the symbol is internal (not directly accessible via find-symbol in its home package).
      ;; This usually means it's uninterned or shadowed.
      :internal-p ,(if (symbol-package sym)
                       (not (eq sym (find-symbol (symbol-name sym)
                                                 (symbol-package sym))))
                       t)))) ; If no package, consider it internal/unreachable by default.

;; Converts a binding (which can be a symbol or a list like (VAR TYPE))
;; into a structured plist containing name, package, and optional type information.
(defun structured-binding (binding)
  "Convert a binding (e.g., X, (X TYPE)) into a plist with :name, :package,
   and optional :type information. Type information itself is also structured via `symbol-info`."
  ;; TODO: This function is used in `serialize-slot` (from utils.lisp, presumably).
  ;;       Ensure its output format is consistent with expectations there.
  (cond
    ((symbolp binding)
     (symbol-info binding)) ; If just a symbol, get its info.
    ((and (consp binding) (symbolp (first binding))) ; If (VAR TYPE) or (VAR)
     (let* ((var (first binding))
            (type (when (consp (rest binding)) (second binding))) ; Type is optional
            (var-info (symbol-info var))
            (type-info (when (and type (symbolp type)) ; Process type only if it's a symbol
                         `(:type ,(symbol-info type)))))
       (append var-info type-info)))
    (t
     (error "Invalid binding form: ~S. Expected a symbol or (symbol [type-symbol])." binding))))

;; Initializes or retrieves a store for a given project name within the main universe.
;; Also ensures a collection named "code-definitions" exists within that store,
;; configured with indexes for querying.
(defun init-project-store (project-name)
  "Initializes or retrieves a store for PROJECT-NAME within the global *universe*.
   Ensures a collection 'code-definitions' exists with appropriate indexes.
   Returns the 'code-definitions' collection object."
  ;; Ensure the universe is initialized.
  (when (or (not *multiverse*) (not *universe*))
    (init-universe))

  ;; Attempt to retrieve an existing store for the project.
  (let* ((store (cl-naive-store:get-multiverse-element
                 *universe* project-name :element-type :store)) ; Specify element-type for clarity

         ;; Attempt to retrieve the "code-definitions" collection from the store if the store exists.
         (collection (when store
                       (cl-naive-store:get-multiverse-element
                        store "code-definitions" :element-type :collection))))

    ;; If the store doesn't exist, create it.
    (unless store
      (setf store (add-multiverse-element
                   *universe*
                   (make-instance
                    (store-class *universe*) ; Use the default store class from the universe.
                    :name project-name
                    ;; Use indexed-collection for efficient lookups.
                    :collection-class 'cl-naive-store.naive-indexed:indexed-collection)))
      ;; Persist the new store definition.
      (persist store :definitions-only-p t))

    ;; If the collection doesn't exist within the store, create it.
    (unless collection
      (setf collection
            (cl-naive-store:add-multiverse-element
             store
             (make-instance (collection-class store) ; Use the store's collection class.
                            :name "code-definitions"
                            ;; Define indexes for common query patterns.
                            :indexes '((:name) ; Index by definition name.
                                       (:file-name) ; Index by file name.
                                       (:name :file-name))))) ; Composite index.
      ;; Persist the new collection definition.
      (persist collection :definitions-only-p t))
    ;; Return the collection, whether retrieved or newly created.
    collection))

;; Looks up definitions by name in the given collection.
;; TODO: This function seems to be a specific wrapper around `index-lookup-values`.
;;       Consider if its name clearly reflects its specific use case (lookup by :name).
(defun lookup-index (collection definition-name)
  "Looks up documents in COLLECTION where the :name field matches DEFINITION-NAME.
   Uses the pre-defined index on :name for efficient retrieval."
  ;; `index-lookup-values` is specific to `cl-naive-store.naive-indexed` collections.
  ;; It expects a list of query lists, where each inner list is a key-value pair for the index.
  (index-lookup-values
   collection
   (list (list :name definition-name)))) ; Query for documents where :name equals definition-name.
