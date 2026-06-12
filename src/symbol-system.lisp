(defpackage :symbol-system
  (:use :cl :generic)
  (:export #:define-symbol-system))

(in-package :symbol-system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun format-intern (fmt &rest args)
    (intern (string-upcase (apply #'format nil fmt args)))))

(defmacro define-symbol-system (name)
  (let ((expander (format-intern "define-~a-expander" name))
        (expansion (format-intern "get-~a-expansion" name))
        (get-expander (format-intern "symbol-~a-expander" name))
        (prop (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defparameter ,prop (make-hash-table :test #'equal))

       (defun ,get-expander (sym)
         (gethash sym ,prop))

       (defun (setf ,get-expander) (new-lambda sym)
         (declare (function new-lambda))
         (setf (gethash sym ,prop) new-lambda))

       (defmacro ,expander (name parameters &body body)
         `(eval-when (:compile-toplevel :load-toplevel :execute)
            (handler-case (setf (,',get-expander ',name)
                                (lambda-env ,parameters ,@body))
              (error (c)
                (error 'simple-error
                       :format-control "define ~s expander failed~%  ~s"
                       :format-arguments (list ',name c))))))

       (defun ,expansion (codes &optional env)
         (if (listp codes)
             (destructuring-bind (name . args) codes
               (aif2 (,get-expander name)
                     (values (handler-case (funcall it args env)
                               (error (c)
                                 (error 'simple-error
                                        :format-control "~s expansion failed~%  ~s"
                                        :format-arguments (list ',name c))))
                             t)
                     (aif2 (macroexpand codes)
                           (values (,expansion it env) t)
                           (values codes nil))))
             (values codes nil))))))
