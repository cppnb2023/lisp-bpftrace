(defpackage :memoize
  (:use :cl :generic)
  (:export #:last-memoize #:with-last-memoize-let #:defun-last-mem))

(in-package :memoize)

(defparameter *last-memoize-hash* (make-hash-table :test #'equal))

(defstruct (last-memoize-cache (:conc-name lmc-))
  key cache)

(defun last-memoize (idx-sym fn &rest parameters)
  (let ((cache (gethash idx-sym *last-memoize-hash*))
        (expr  (cons fn parameters)))
    (unless cache
      (setf cache (setf (gethash idx-sym *last-memoize-hash*)
                        (make-last-memoize-cache))))
    (if (equal (lmc-key cache) expr)
        (values (lmc-cache cache) t)
        (prog1
            (values (setf (lmc-cache cache)
                          (apply fn parameters))
                    nil)
          (setf (lmc-key cache) expr)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-last-memoize (obj &optional env)
    (let ((obj (macroexpand obj env)))
      (cond
        ((symbolp obj) obj)
        ((listp obj)
         (destructuring-bind (fn . args) obj
           `(last-memoize ',(gensym) #',fn ,@args)))))))


(defmacro with-last-memoize-let ((&rest bindings) &body body &environment env)
  `(let ,(loop for (var expr) in bindings
               collect `(,var ,(to-last-memoize expr env)))
     ,@body))

