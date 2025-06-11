(in-package :portable-sb-walker)

(defmacro m (&rest cases)
  `(labels (,@(loop for (case . rest) in cases
                    collect
                    (list* (intern (format nil "WALK-~a" case))
                           ()
                           rest))
            (parse (token)
              (tagbody
                 (go body)
               fallthrough
                 (throw 'fallthrough token)
               body
                 (let ((x (catch 'fallthrough
                            (cond
                              ((not (symbolp token))
                               (walk token))
                              ,@(loop for (case) in cases
                                      collect `((or
                                                 ,@(loop for case in (if (consp case)
                                                                         case
                                                                         (list case))
                                                         collect `(string-equal token ',case)))
                                                (collect token)
                                                (,(intern (format nil "WALK-~a" case)))))
                              (t
                               (go fallthrough)))
                            :end)))
                   (unless (eq x :end)
                     (setf token x)
                     (go body))))))
     (loop
       (parse (next)))))

(defun walk-loop (form context env)
  (destructuring-bind (loop &rest clauses) form
    (relist*
     form loop
     (block nil
       (error "Unknown loop keyword ~a"
              (catch 'fallthrough
                (let ((result))
                  (labels ((next ()
                             (if clauses
                                 (pop clauses)
                                 (return (nreverse result))))
                           (collect (x)
                             (push x result))
                           (walk (form)
                             (collect (walk-form-internal form context env)))
                           (type ()
                             (case (car clauses)
                               ((fixnum float t)
                                (collect (next)))
                               (of-type
                                (collect (next))
                                (collect (next)))))
                           (var ()
                             (collect (next))
                             (type)))
                    (m
                     (as
                      (walk-for))
                     (for
                      (var)
                      (m
                       (and
                        (walk-for))
                       (=
                        (walk (next))
                        (m
                         (then
                          (walk (next)))))
                       ((from downfrom upfrom below above to upto downto by)
                        (walk (next)))
                       ((in on)
                        (walk (next))
                        (m
                         (by
                          (walk (next)))))
                       (across
                        (walk (next)))
                       (being
                        (m
                         ((each the)
                          (m
                           ((hash-key hash-keys hash-value hash-values)
                            (m ((in of) (walk (next)))))))
                         ((using)
                          (collect (next))
                          (m
                           ((hash-key hash-keys hash-value hash-values)
                            (m ((in of) (walk (next)))))))))))
                     (with
                      (var)
                      (m
                       (and
                        (walk-with))
                       (=
                        (walk (next)))))
                     ((if when unless) (walk (next)))
                     ((else and end))
                     ((collect collecting append appending nconc nconcing)
                      (walk (next))
                      (m
                       (into
                        (collect (next)))))
                     ((count counting sum summing
                             maximize maximizing minimize minimizing)
                      (walk (next))
                      (m
                       (into
                        (collect (next))
                        (type))))
                     ((while until repeat)
                      (walk (next)))
                     ((always never thereis)
                      (walk (next)))
                     ((do initially finally))
                     (return (walk (next)))
                     (it))))))))))
