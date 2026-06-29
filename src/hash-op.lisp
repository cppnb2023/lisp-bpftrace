(defpackage :hash-op
  (:use :cl :generic :do-varient :memoize)
  (:export #:with-hash-bindings #:with-hash-let #:mul-gethash
           #:hash-lambda #:to-hash-binding))

(in-package :hash-op)

(defmacro with-hash-bindings ((&rest bindings) hash-table &body body)
  (let ((hash-sym (gensym "hash")))
    `(let ((,hash-sym (the hash-table ,hash-table)))
       (symbol-macrolet
           ,(loop for (v k nil) in bindings
                  collect `(,v (gethash ,k ,hash-sym)))
         ,@(loop for (v nil default) in bindings
                 when default
                   collect `(aunless2 ,v (setf ,v ,default)))
         (declare (hash-table ,hash-sym))
         ,@body))))

(defmacro with-hash-let ((&rest bindings) hash-table &body body)
  (let ((hash-sym (gensym "hash")))
    `(let* ((,hash-sym (the hash-table ,hash-table))
            ,@(loop for (v k default) in bindings
                    if default
                      collect `(,v (aunless2 (gethash ,k ,hash-sym) ,default))
                    else
                      collect `(,v (gethash ,k ,hash-sym))))
       (declare (hash-table ,hash-sym))
       ,@body)))

(defmacro mul-gethash ((&rest keys) hash)
  (destructuring-bind (first &rest rest) keys
    (if rest
        `(mul-gethash ,rest (gethash ,first ,hash))
        `(gethash ,first ,hash))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-hash-binding (obj)
    (cond
      ((symbolp obj)
       (list obj (intern (symbol-name obj) :keyword)))
      ((listp obj) obj)
      (t (error 'simple-error
                :format-control "unknown type ~a"
                :format-arguments (list (type-of obj)))))))

(defmacro hash-lambda (parameters &body body)
  (with-symbols (hash)
    `(lambda (,hash)
       (declare (hash-table ,hash))
       (with-hash-let
           ,(mapcar #'to-hash-binding parameters)
           ,hash
         ,@body))))
