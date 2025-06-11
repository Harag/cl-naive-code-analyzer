(in-package :cl-naive-code-analyzer)

(defun export-symbol (sym)
  (when (symbolp sym)
    `(:name ,(string-downcase (symbol-name sym))

            :package ,(string-downcase (package-name (symbol-package sym))))))

(defun normalize-name (x)
  (string-downcase (if (consp x) (symbol-name (car x)) (symbol-name x))))

(defun normalize-symbol-list (lst)
  (mapcar #'normalize-name lst))

(defun ensure-list (x)
  (cond
    ((null x) '())
    ((listp x) x)
    (t (list x))))

(defun offset-to-line-map (text)
  (let* ((length (length text))
         (line-map (make-array length :element-type 'unsigned-byte :initial-element 1))
         (line 1))
    (loop for i from 0 below length
          do (progn
               (setf (aref line-map i) line)
               (when (char= (char text i) #\Newline)
                 (incf line))))
    line-map))

(defun offset-to-line (offset line-map)
  (aref line-map offset))

(defun flatten-typed-lambda-list (lambda-list)
  "Extract symbols from lambda-list that may include type specifiers."
  (mapcar (lambda (item)
            (if (symbolp item)
                item
                (first item)))  ; handles (x string)
          lambda-list))

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
          (loop for tail on form
                collect (normalize-reader-macros (car tail))
                while (consp tail)
                finally (when tail
                          (rplacd (last form) (normalize-reader-macros tail))))))))
    (t form)))

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
(defclass tracking-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((underlying :initarg :underlying :accessor tracking-stream-underlying)
   (position :initform 0 :accessor tracking-stream-position)
   (last-form-start :initform nil :accessor tracking-stream-last-form-start)
   (last-form-end :initform nil :accessor tracking-stream-last-form-end)

   (last-char :initform nil :accessor tracking-stream-last-char)))

(defmethod trivial-gray-streams:stream-read-char ((s tracking-stream))
  (let ((char (read-char (tracking-stream-underlying s) nil :eof)))
    (unless (eq char :eof)
      ;; If this is the first paren in a new form, record it
      (when (and (char= char #\()
                 (null (tracking-stream-last-form-start s)))
        (setf (tracking-stream-last-form-start s)
              (tracking-stream-position s)))

      ;; If it’s a closing paren, always update last-form-end
      (when (char= char #\))
        (setf (tracking-stream-last-form-end s)
              (1+ (tracking-stream-position s))))  ; position after)

      (incf (tracking-stream-position s))
      (setf (tracking-stream-last-char s) char))
    char))

(defmethod trivial-gray-streams:stream-unread-char ((stream tracking-stream) character)
  (decf (tracking-stream-position stream))
  (unread-char character (tracking-stream-underlying stream)))

(defmethod trivial-gray-streams:stream-peek-char ((stream tracking-stream))
  (peek-char nil (tracking-stream-underlying stream) nil nil))

(defmethod trivial-gray-streams:stream-listen ((stream tracking-stream))
  (listen (tracking-stream-underlying stream)))

(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream tracking-stream))
  (let ((char (read-char-no-hang (tracking-stream-underlying stream) nil :eof)))
    (unless (eq char :eof)
      (incf (tracking-stream-position stream)))
    char))

(defmethod trivial-gray-streams:stream-line-column ((stream tracking-stream))
  nil)

(defmethod trivial-gray-streams:stream-clear-input ((stream tracking-stream))
  nil)
