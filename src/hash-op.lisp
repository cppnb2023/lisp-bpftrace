(defpackage :hash-op
  (:use :cl :generic :do-varient :memoize)
  (:export #:with-hash-let #:hash-lambda))

(in-package :hash-op)

(defmacro with-hash-let ((&rest bindings) hash-table &body body)
  (with-symbols (hash-sym)
    `(let* ((,hash-sym (the hash-table ,hash-table))
            ,@(loop for (v k default) in (mapcar #'to-key-binding bindings)
                    collect `(,v (gethash ,k ,hash-sym ,default))))
       (declare (hash-table ,hash-sym))
       ,@body)))

(defmacro hash-lambda (parameters &body body)
  (with-symbols (hash)
    `(lambda (,hash)
       (declare (hash-table ,hash))
       (with-hash-let ,parameters ,hash
         ,@body))))
