;;; query.lisp
;;;
;;; This file provides functions and macros for querying the code
;;; analysis data stored using `cl-naive-store`. It includes
;;; mechanisms for defining named queries, initializing project stores
;;; for querying, and performing queries with filtering, sorting, and
;;; limits.

;;; TODO: The `init-project` function here seems to duplicate some
;;;       functionality of `init-project-store` from
;;;       `naive-store.lisp` but with a focus on loading existing
;;;       definitions. Clarify their roles and potentially merge.

;;; TODO: Review error handling, especially for cases where stores or
;;;       collections might not be found or correctly initialized.

;;; TODO: Consider making the location `"~/code-index-multiverse/"`
;;; configurable.

(in-package :cl-naive-code-analyzer)

;; A hash table to store globally registered, named queries.
;; Queries are typically lambda functions that take a definition (document)
;; and return true if it matches the query criteria.
(defparameter *registered-queries* (make-hash-table :test #'equal)
  "A hash table storing named queries (keywords) and their corresponding lambda functions.")

;; Initializes a project's store and collection for querying.
;; If the multiverse or universe is not set up, it initializes them.

;; It attempts to load store and collection definitions from disk if
;; not already in memory.

(defun load-store (project)
  "Initializes or loads the necessary store and collection for the given PROJECT name.
   Returns the 'code-definitions' collection for the project.
   Assumes project data has already been indexed and stored."
  (let ((store nil) ; Renamed to avoid conflict with cl-naive-store:store class
        (collection nil))
    ;; Ensure multiverse and universe are initialized (uses global
    ;; *multiverse* and *universe*)
    (unless *multiverse*
      (setf *multiverse*
            (make-instance
             'multiverse
             :name *multiverse-name*
             ;; TODO: Make configurable
             :location (multiverse-location)
             :universe-class 'cl-naive-store:universe)))
    (unless *universe*
      (setf *universe*
            (add-multiverse-element
             *multiverse*
             (make-instance
              'cl-naive-store:universe
              :name *universe-name*
              :multiverse *multiverse*
              ;; TODO: Make configurable
              :location (universe-location)))))

    ;; Get or load the project-specific store
    (setf store (cl-naive-store:get-multiverse-element
                 :store *universe* project))
    (unless store
      (setf store (cl-naive-store:load-from-definition-file
                   *universe*
                   :store
                   project)))

    ;; Get or load the 'code-definitions' collection from the store
    (when store
      (setf collection (cl-naive-store:get-multiverse-element
                        :collection store *collection-name*))

      (unless collection
        (setf collection (cl-naive-store:load-from-definition-file
                          store
                          *collection-name*
                          :collection
                          :with-data-p t))))
    ;; Return the collection, or ERROR if store/collection couldn't be
    ;; loaded/found.
    (if (cl-naive-store:documents collection)
        collection
        (error "Project definitions not found."))))

;; Macro to define and register a named query.
;; NAME is a keyword, and LAMBDA is the query function.
(defmacro defquery (name lambda)
  "Defines a named query. NAME should be a keyword.
   LAMBDA is a function that takes a definition document and returns true if it matches."
  ;; TODO: Consider adding docstrings or metadata to registered
  ;; queries.
  `(setf (gethash ,name *registered-queries*) ,lambda))

;; Retrieves project store objects.

;; If PROJECTS (a list of project names) is provided, it
;; gets/initializes stores for those.

;; Otherwise, it returns all stores in the current *universe*.
(defun get-project-stores (&optional projects)
  "Retrieves store objects for the specified PROJECTS (list of names).
   If PROJECTS is NIL, returns all stores in the current *universe*.
   Uses `init-project` to ensure stores are loaded if not already in memory."
  ;; TODO: `init-project` returns a collection, not a store. This
  ;;       needs correction.  It should probably call
  ;;       `cl-naive-store:get-multiverse-element` for the store, and
  ;;       `init-project` might be misnamed or misused here if the
  ;;       goal is to get store objects.  Assuming `init-project` is
  ;;       intended to ensure the project's data is loaded and returns
  ;;       the collection, this function needs to be rethought if it's
  ;;       meant to return *store* objects.  For now, proceeding with
  ;;       the assumption that it aims to get collections via
  ;;       init-project.
  (if projects
      (loop for project-name in projects
            collect
            ;; This part is problematic: init-project returns a
            ;; collection.  We need the store object itself if the
            ;; goal is to iterate stores.  Correcting to get the
            ;; store, then ensure its collection is loaded.
            (let ((store-obj (cl-naive-store:get-multiverse-element
                              :store
                              *universe* project-name)))
              (unless store-obj
                ;; If store isn't even in memory, `init-project` might
                ;; try to load its definition. However,
                ;; `init-project` as written is more about loading the
                ;; *collection*. This logic needs to be robust for
                ;; cases where store definition exists but not
                ;; collection.
                (init-project project-name)
                (setf store-obj
                      (cl-naive-store:get-multiverse-element
                       :store
                       *universe* project-name)))
              (unless
                  (cl-naive-store:get-multiverse-element
                   store-obj
                   "code-definitions"
                                        ; Get all store objects directly

                   ;; Main function to query stored analysis data.  Allows specifying
                   ;; projects, a query function/keyword, sorting, filtering, and
                   ;; limiting results.
                   (defun query-analyzer (query &key projects sort filter limit)
                     "Run QUERY over stored analysis data across specified PROJECTS.
   QUERY can be a keyword (for a registered query) or a lambda function.
   FILTER is a predicate (keyword or lambda) to further narrow results.
   SORT is a key function for sorting results.
   LIMIT restricts the number of results returned."
                     ;; TODO: Improve error message for invalid query/filter types.

                     ;; TODO: Ensure `get-project-stores` correctly
                     ;; returns a list of store objects.
                     (let* ((stores (get-project-stores projects))
                            (final-query
                              (cond ((functionp query) query)
                                    ((keywordp query)
                                     (gethash query *registered-queries*))
                                    (t (error "Invalid query type: ~A. Must be a function or registered keyword."
                                              query))))
                            (results
                              (loop for store in stores
                                    when store
                                    append
                                    (let ((collection
                                            (cl-naive-store:get-multiverse-element
                                             :collection
                                             store "code-definitions")))
                                      (when collection
                                        (cl-naive-store:query-data
                                         collection
                                         :query
                                         (lambda (doc)
                                           (funcall final-query doc)))))))
                            (processed-results results))

                       ;; Apply sorting if specified
                       (when sort
                         (setf processed-results
                               (sort processed-results #'< :key sort)))

                       ;; Apply filtering if specified
                       (when filter
                         (let ((filter-fn
                                 (cond ((functionp filter)
                                        filter)
                                       ((keywordp filter)
                                        (gethash filter *registered-queries*))
                                       (t (error "Invalid filter type: ~A. Must be a function or registered keyword." filter)))))
                           (setf processed-results
                                 (remove-if-not filter-fn processed-results))))

                       ;; Apply limit if specified
                       (when limit
                         (setf processed-results
                               (subseq processed-results
                                       0
                                       (min limit (length processed-results)))))
                       processed-results))))))))

;; Helper function to find a symbol (represented as
;; a plist with :name and :package) within a list
;; of such symbol plists.
(defun match-symbol (symbol-name symbol-package symbol-list)
  "Searches SYMBOL-LIST for a symbol matching SYMBOL-NAME and SYMBOL-PACKAGE.
   Returns the matching symbol plist or NIL."
  ;; TODO: Consider case sensitivity for
  ;; symbol/package names if `equalp` is too broad
  ;; or narrow.
  (loop for symbol-pair in symbol-list
        when (and (equalp (getf symbol-pair :name) symbol-name)
                  (equalp (getf symbol-pair :package) symbol-package))
        return symbol-pair))

;; Factory function to create a query that finds
;; definitions calling a specific symbol.
(defun make-callers-of-query (symbol-name symbol-package)
  "Returns a lambda query function that finds definitions calling the function
   specified by SYMBOL-NAME and SYMBOL-PACKAGE."
  (lambda (definition)
    ;; Assumes :function-calls contains plists from `export-symbol`
    (loop for call in (getf definition :function-calls)
          when (and (equalp (getf call :name) symbol-name)
                    (equalp (getf call :package) symbol-package))
          return call)))

;; Factory function to create a query that finds
;; definitions using a specific symbol either as a
;; function call, macro call, or variable use.
(defun make-uses-symbol-query (symbol-name symbol-package)
  "Returns a lambda query function that finds definitions that use the symbol
   (specified by SYMBOL-NAME and SYMBOL-PACKAGE) in function calls, macro calls, or variable uses."
  (lambda (definition)
    (or (match-symbol symbol-name symbol-package
                      (getf definition :function-calls))
        (match-symbol symbol-name symbol-package
                      (getf definition :macro-calls))
        (match-symbol symbol-name symbol-package
                      (getf definition :variable-uses)))))

;; Factory function to create a query that finds
;; all definitions in a specific file.
(defun make-functions-in-file-query (pathname)
  "Returns a lambda query function that finds all definitions within the specified PATHNAME."
  ;; TODO: Ensure pathname comparison is robust
  ;; (e.g., using `uiop:pathname-equal`).
  (let ((target-namestring (if (pathnamep pathname)
                               (namestring pathname)
                               pathname)))
    (lambda (definition)
      (equalp (getf definition :filename) target-namestring))))

;;;; === Built-in Queries ===
;;; These are examples of how to define reusable queries.

;; Query to find all function-like definitions.
(defquery :all-functions
    (lambda (definition)
      ;; Compares the :name sub-property of the :kind property of the definition.

      ;; :kind is of the form  `(:name 'defun ...)
      (let ((kind-val (getf definition :kind)))
        (member (if (consp kind-val) (getf kind-val :name) kind-val)
                '(defun defmethod lambda function)
                :test #'string-equal))))

;; Query to find all macro definitions.
(defquery :macros
    (lambda (definition)
      ;; Similar to :all-functions, adjusting for
      ;; :kind structure.
      (let ((kind-val (getf definition :kind)))
        (string-equal (if (consp kind-val)
                          (getf kind-val :name)
                          kind-val)
                      "defmacro"))))
#|
;; === Analysis that is deeper than a simple query ===
;; Example of a more complex analysis: finding uncalled functions.

;; Finds functions that are defined but not called
;; by any other analyzed function.

;; TODO: This is a simplified check. It doesn't account for calls via APPLY, funcall,
;;       indirect calls, calls from outside the analyzed projects, or entry points.
(defun uncalled-functions (&optional projects)
"Identifies functions defined in PROJECTS that do not appear in the :function-calls
list of any other definition within the same set of PROJECTS.
Returns a list of definition plists for such uncalled functions."
(let ((functions (query-analyzer :all-functions :projects projects)))
(loop for func-def in functions
;; Check if this function is called by any other definition.
unless (query-analyzer
(make-callers-of-query (getf func-def :name) (getf func-def :package))
:projects projects)
collect func-def)))
|#

;; Example usage comments (kept for context, but should be actual tests or examples elsewhere).
;; (uncalled-functions '("test-code"))
;; (uncalled-functions '("insite"))
;; (query-analyzer :all-functions :projects '("test-code"))
;; (load-project "test-code")
;; (break "~S" *universe*)

