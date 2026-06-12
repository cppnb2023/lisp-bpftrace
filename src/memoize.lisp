(defpackage :memoize
  (:use :cl :generic)
  (:export #:last-memoize #:with-last-memoize))

(in-package :memoize)

(defparameter *last-memoize-hash* (make-hash-table :test #'equal))

(defstruct (last-memoize-cache (:conc-name lmc-))
  key cache)

(defun last-memoize (idx-sym fn &rest parameter)
  (let ((cache (gethash idx-sym *last-memoize-hash*))
        (expr  (cons fn parameter)))
    (unless cache
      (setf cache (setf (gethash idx-sym *last-memoize-hash*)
                        (make-last-memoize-cache))))
    (if (equal (lmc-key cache) expr)
        (lmc-cache cache)
        (prog1
            (setf (lmc-cache cache)
                  (apply fn parameter))
          (setf (lmc-key cache) expr)))))

(defmacro with-last-memoize ((&rest bindings) &body body &environment env)
  `(let ,(loop for (var expr idx-sym) in bindings
               collect (if expr
                           (destructuring-bind (fn . args) (macroexpand expr env)
                             `(,var (last-memoize ',(gensym) #',fn ,@args)))
                           `(,var nil)))
     ,@body))
