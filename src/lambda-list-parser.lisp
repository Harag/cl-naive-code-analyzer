;;; lambda-list-parser.lisp
;;;
;;; Functions for parsing lambda list CSTs from Eclector.
;;; This aims to replace the usage of alexandria:parse-ordinary-lambda-list
;;; and provide more direct CST-based analysis of lambda lists.

(in-package :cl-naive-code-analyzer)

(defun parse-lambda-list-cst (lambda-list-cst &key (context :ordinary))
  "Parses a lambda list CST into a structured representation.
   Returns a plist with keys like :required, :optionals, :keys, etc.
   Each value will be a list of parameter detail plists.
   CONTEXT can be :ordinary, :generic-function-ordinary, :specialized, :macro, :deftype
   to guide parsing nuances (e.g. specializers, &whole, &body)."
  (let ((required-params '())
        (optional-params '())
        (rest-param nil)
        (key-params '())
        (aux-params '())
        (whole-param nil)
        (env-param nil)
        (body-param nil)
        (allow-other-keys-p nil)
        (current-section :required)
        (cst-nodes (cst:listify lambda-list-cst)))

    (loop for current-node in cst-nodes
          for raw-node = (concrete-syntax-tree:raw current-node)
          do (cond
               ((and (member context '(:macro)) (eq raw-node '&whole))
                (setf current-section :whole))
               ((and (member context '(:macro)) (eq raw-node '&environment))
                (setf current-section :environment))
               ((eq raw-node '&optional)
                (setf current-section :optional))
               ((eq raw-node '&rest)
                (setf current-section :rest))
               ((and (member context '(:macro)) (eq raw-node '&body))
                (setf current-section :body))
               ((eq raw-node '&key)
                (setf current-section :key))
               ((eq raw-node '&allow-other-keys)
                (setf allow-other-keys-p t)
                (setf current-section :key)) ; Continue expecting keys or &aux
               ((eq raw-node '&aux)
                (setf current-section :aux))
               (t ; This is a parameter
                (case current-section
                  (:whole
                   (setf whole-param (parse-simple-parameter-cst current-node :whole))
                   (setf current-section :required)) ; Next is ordinary part
                  (:environment
                   (setf env-param (parse-simple-parameter-cst current-node :environment))
                   (setf current-section :required)) ; Next is ordinary part
                  (:required
                   (push (parse-parameter-cst current-node current-section context) required-params))
                  (:optional
                   (push (parse-parameter-cst current-node current-section context) optional-params))
                  (:rest
                   (setf rest-param (parse-simple-parameter-cst current-node :rest))
                   (setf current-section :after-rest)) ; Only one &rest
                  (:body
                   (setf body-param (parse-simple-parameter-cst current-node :body))
                   (setf current-section :after-body)) ; Only one &body
                  (:key
                   (push (parse-parameter-cst current-node current-section context) key-params))
                  (:aux
                   (push (parse-parameter-cst current-node current-section context) aux-params))
                  (:after-rest nil) ; Waiting for &key or &aux
                  (:after-body nil) ; Waiting for &aux in macros
                  ))))
    (list :whole whole-param
          :environment env-param
          :required (nreverse required-params)
          :optionals (nreverse optional-params)
          :rest rest-param
          :body body-param
          :keys (nreverse key-params)
          :allow-other-keys allow-other-keys-p
          :aux (nreverse aux-params))))

(defun parse-simple-parameter-cst (cst kind)
  "Parses a simple parameter CST (e.g., for &whole, &environment, &rest, &body vars)
   which is expected to be an ATOM-CST representing the variable name."
  (unless (typep cst 'concrete-syntax-tree:atom-cst)
    (warn "Expected an ATOM-CST for ~A parameter, got ~S" kind cst)
    ;; Potentially return a special value or try to extract name if it's a list by mistake
    (return-from parse-simple-parameter-cst
      (list :name (concrete-syntax-tree:raw cst) :kind kind :error "Expected ATOM-CST")))
  (list :name (concrete-syntax-tree:raw cst) :kind kind))

(defun parse-parameter-cst (cst section context)
  "Parses an individual parameter CST based on the current lambda list section and context.
   Returns a parameter-detail plist."
  (let ((raw (concrete-syntax-tree:raw cst)))
    (cond
      ;; Required parameters
      ((eq section :required)
       (cond
         ;; Specialized parameter like (VAR TYPE) in :specialized context (defmethod)
         ((and (eq context :specialized) (typep cst 'concrete-syntax-tree:cons-cst))
          (let ((children (cst:listify cst)))
            (unless (= (length children) 2)
              (warn "Malformed specialized parameter: ~S. Expected (NAME TYPE)." raw)
              (return-from parse-parameter-cst
                (list :name raw :kind :required :error "Malformed specialized parameter")))
            (list :name (concrete-syntax-tree:raw (first children))
                  :kind :required
                  :type-specifier (concrete-syntax-tree:raw (second children)))))
         ;; Destructuring list in :macro or :deftype context (or potentially :ordinary if allowed)
         ((and (member context '(:macro :deftype :ordinary)) ; :ordinary might allow destructuring too
               (typep cst 'concrete-syntax-tree:cons-cst))
          (list :name raw ; Keep the raw list as the "name" for the top-level destructure
                :kind :required
                :destructured t
                ;; Recursively parse the destructuring pattern itself as a lambda list.
                ;; Use :ordinary context for the sub-list, as macro keywords like &whole aren't nested.
                :sub-parameters (parse-lambda-list-cst cst :context :ordinary)))
         ;; Simple required parameter (ATOM-CST)
         ((typep cst 'concrete-syntax-tree:atom-cst)
          (list :name raw :kind :required))
         (t
          (warn "Unexpected CST form for required parameter: ~S" raw)
          (list :name raw :kind :required :error "Unexpected CST form"))))

      ;; Optional parameters: ATOM-CST or CONS-CST (name init-form supplied-p-name)
      ((eq section :optional)
       (if (typep cst 'concrete-syntax-tree:atom-cst)
           (list :name raw :kind :optional :default-value nil :supplied-p-variable nil)
           (let ((children (cst:listify cst)))
             (list :name (concrete-syntax-tree:raw (first children))
                   :kind :optional
                   ;; Store the raw CST for default value and supplied-p for potential later analysis/stringification
                   :default-value-cst (second children)
                   :default-value (when (second children) (concrete-syntax-tree:raw (second children)))
                   :supplied-p-variable-cst (third children)
                   :supplied-p-variable (when (third children) (concrete-syntax-tree:raw (third children)))))))

      ;; Key parameters: ATOM-CST or CONS-CST (name-or-alias init-form supplied-p-name)
      ((eq section :key)
       (if (typep cst 'concrete-syntax-tree:atom-cst)
           ;; Simple keyword parameter: (VAR), keyword is generated from VAR
           (list :name raw :kind :key :keyword (alexandria:make-keyword raw)
                 :default-value nil :supplied-p-variable nil)
           ;; Complex keyword parameter: ((KW VAR) INIT S-P) or (VAR INIT S-P)
           (let ((children (cst:listify cst)))
             (let ((name-part-cst (first children))
                   (init-form-cst (second children))
                   (supplied-p-cst (third children)))
               (if (typep name-part-cst 'concrete-syntax-tree:cons-cst)
                   ;; ((KW VAR) INIT S-P)
                   (let* ((kw-var-children (cst:listify name-part-cst))
                          (keyword-cst (first kw-var-children))
                          (var-name-cst (second kw-var-children)))
                     (list :name (concrete-syntax-tree:raw var-name-cst)
                           :kind :key
                           :keyword-cst keyword-cst
                           :keyword (concrete-syntax-tree:raw keyword-cst)
                           :default-value-cst init-form-cst
                           :default-value (when init-form-cst (concrete-syntax-tree:raw init-form-cst))
                           :supplied-p-variable-cst supplied-p-cst
                           :supplied-p-variable (when supplied-p-cst (concrete-syntax-tree:raw supplied-p-cst))))
                   ;; (VAR INIT S-P)
                   (list :name (concrete-syntax-tree:raw name-part-cst)
                         :kind :key
                         :keyword (alexandria:make-keyword (concrete-syntax-tree:raw name-part-cst))
                         :default-value-cst init-form-cst
                         :default-value (when init-form-cst (concrete-syntax-tree:raw init-form-cst))
                         :supplied-p-variable-cst supplied-p-cst
                         :supplied-p-variable (when supplied-p-cst (concrete-syntax-tree:raw supplied-p-cst))))))))

      ;; Aux parameters: ATOM-CST or CONS-CST (name init-form)
      ((eq section :aux)
       (if (typep cst 'concrete-syntax-tree:atom-cst)
           (list :name raw :kind :aux :default-value nil)
           (let ((children (cst:listify cst)))
             (list :name (concrete-syntax-tree:raw (first children))
                   :kind :aux
                   :default-value-cst (second children)
                   :default-value (when (second children) (concrete-syntax-tree:raw (second children)))))))
      (t
       (error "Unknown parameter CST ~S in section ~S for context ~S" cst section context)))))

;; No need for normalize-section-to-kind as kinds are now directly assigned.

;;; Notes and Future Improvements:
;;; - Contexts handled:
;;;   - :ordinary (defun, general destructuring)
;;;   - :specialized (defmethod required parameters)
;;;   - :macro (defmacro, handles &whole, &environment, &body)
;;;   - :deftype (deftype, similar to ordinary but might have specific validation later)
;;;   - :generic-function-ordinary (defgeneric, like ordinary but no specializers in its direct lambda list)
;;;
;;; - Destructuring:
;;;   - Basic support added by recursively calling parse-lambda-list-cst.
;;;   - The :name of a destructured parameter is the raw list itself.
;;;   - :sub-parameters will contain the parsed structure of the destructuring pattern.
;;;
;;; - Parameter Detail Fields:
;;;   - Added :default-value-cst, :supplied-p-variable-cst, :keyword-cst to store the
;;;     original CSTs for initforms, s-p vars, and keywords. This can be useful if the
;;;     analyzer needs to, for example, stringify the exact initform as it appeared in source.
;;;
;;; - Testing:
;;;   - Requires extensive testing with various valid and potentially malformed lambda lists
;;;     for each context to ensure robustness.
;;;
;;; - Error Handling:
;;;   - Added some basic warnings for malformed structures (e.g., specialized parameter).
;;;   - More robust error handling or recovery might be needed depending on requirements.

"Lambda list parser functions updated with context handling and destructuring."
