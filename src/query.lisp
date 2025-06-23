(in-package :cl-naive-code-analyzer)

(defparameter *registered-queries* (make-hash-table :test #'equal))

(defun init-project (project)
  (let ((store)
        (collection))
    (unless *multiverse*
      (setf *multiverse*
            (make-instance
             'multiverse
             :name "code-index-multiverse"
             :location "~/code-index-multiverse/"
             :universe-class 'cl-naive-store:universe)))
    (unless *universe*
      (setf *universe* (add-multiverse-element
                        *multiverse*
                        (make-instance
                         'cl-naive-store:universe
                         :name "code-index-universe"
                         :multiverse *multiverse*
                         :location "~/code-index-multiverse/code-index-universe/"))))

    (setf store (cl-naive-store:get-multiverse-element
                 :store *universe* project))
    (unless store
      (setf store (cl-naive-store:load-from-definition-file
                   *universe*
                   :store
                   project)))

    (setf collection (cl-naive-store:get-multiverse-element
                      :collection store "code-definitions"))
    (unless collection
      (setf collection (cl-naive-store:load-from-definition-file
                        store
                        :collection
                        "code-definitions"
                        :with-data-p t)))

    collection))

(defmacro defquery (name lambda)
  `(setf (gethash ,name *registered-queries*) ,lambda))

(defun get-project-stores (&optional projects)
  (if projects
      (loop for project in projects
            collect
            (let ((store (cl-naive-store:get-multiverse-element
                          :store *universe* project)))
              (if store
                  store
                  (init-project project))))
      (cl-naive-store:stores *universe*)))

(defun query-analyzer (query &key projects sort filter limit)
  "Run QUERY over stored analysis across PROJECTS.

QUERY can be a keyword (for built-in query), or a lambda.

FILTER is a predicate to narrow results,can be a keyword (for built-in
query), or a lambda.

SORT is a key function.

LIMIT reduces the result length."

  (let* ((stores (get-project-stores projects))
         (final-query (cond ((functionp query) query)
                            ((keywordp query)
                             (gethash query *registered-queries*))
                            (t (error "Invalid query type: ~A" query))))
         (results (loop for store in stores
                        append
                        (cl-naive-store:query-data
                         store
                         :collection-name "code-definitions"
                         :query (lambda (doc)
                                  (funcall final-query doc)))))
         final)

    (if sort
        (setf final (sort results #'< :key sort))
        (setf final results))

    (when filter
      (setf final (remove-if-not (cond ((functionp filter)
                                        filter)
                                       ((keywordp filter)
                                        (gethash filter *registered-queries*))
                                       (t (error "Invalid filter type: ~A" filter)))
                                 final)))

    (when limit
      (setf final (subseq final 0 (min limit (length final)))))
    final))

(defun match-symbol (symbol-name symbol-package symbol-list)
  (loop for symbol-pair in symbol-list
        when (and (equalp (getf symbol-pair :name) symbol-name)
                  (equalp (getf symbol-pair :package) symbol-package))
        return symbol-pair))

(defun make-callers-of-query (symbol-name symbol-package)
  (lambda (definition)
    (loop for call in (getf definition :function-calls)
          when (and (equalp (getf call :name) symbol-name)
                    (equalp (getf call :package) symbol-package))
          return call)))

(defun make-uses-symbol-query (symbol-name symbol-package)
  (lambda (definition)
    (or (match-symbol symbol-name symbol-package (getf definition :function-calls))
        (match-symbol symbol-name symbol-package (getf definition :macro-calls))
        (match-symbol symbol-name symbol-package (getf definition :variable-uses)))))

(defun make-functions-in-file-query (pathname)
  (lambda (definition)
    (equalp (getf definition :filename)
            (if (pathnamep pathname)
                (namestring pathname)
                pathname))))

;;;; === Built-in Queries ===

(defquery :all-functions
    (lambda (definition)
      (member (getf (getf definition :kind) :name)
              '("defun" "defmethod" "lambda" "setf-macrolet")
              :test #'equalp)))

(defquery :macros
    (lambda (definition)
      (equalp (getf (getf definition :kind) :name) "defmacro")))

;;;; === Analysis that is deeper than a simple query ===

(defun uncalled-functions (&optional projects)
  (let ((functions (query-analyzer :all-functions :projects projects)))
    (loop for function in functions
          unless (query-analyzer
                  (lambda (definition)
                    (match-symbol (getf function :name) (getf function :package)
                                  (getf definition :function-calls)))
                  :projects projects)
          collect function)))

;;(uncalled-functions '("test-code"))

;;(uncalled-functions '("insite"))

;;(query-analyzer :all-functions :projects '("test-code"))

;;(init-project "test-code")
;;(break "~S" *universe*)
#|
(with-open-file (s "~/Downloads/test-output.lisp"
:direction :output :if-exists :supersede)
(let ((foo "foo")
(writer (make-instance 'naive-impl::safe-writer))
(hash (make-hash-table :size 2)))
(naive-impl::safe-write
(list :name foo
:info "abc def"
:vector #(a b c)
:hash hash
:type-of (type-of writer))
writer s)))
|#
