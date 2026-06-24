(defpackage :symbol-system
  (:use :cl :generic)
  (:export #:define-symbol-system))

(in-package :symbol-system)

(defstruct expand-node
  fn (debug nil) type)

(define-condition redefine-warning (style-warning)
  ((older   :initarg :older   :reader older)
   (newer   :initarg :newer   :reader newer)
   (fn-name :initarg :fn-name :reader fn-name))
  (:report (lambda (c s)
             (with-slots (older newer fn-name) c
               (if (equal older newer)
                   (format s "redefining ~a in ~a" fn-name newer)
                   (format s "redefining ~a in ~a but it was previously defined to ~a"
                           fn-name newer older))))))

(defmacro with-debug ((cond codes &optional (name "")) &body body)
  (with-symbols (cond-sym result)
    `(let ((,cond-sym ,cond))
       (when ,cond-sym
         (format *debug-io* "~%~a enter:~% ~s~%" ,name ,codes))
       (let ((,result (progn ,@body)))
         (when ,cond-sym
           (format *debug-io* "~a result:~%~s~%" ,name ,result))
         ,result))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun format-intern (fmt &rest args)
    (intern (string-upcase (apply #'format nil fmt args)))))

(defmacro define-symbol-system (name)
  (let ((expander (format-intern "define-~a-expander" name))
        (expansion (format-intern "get-~a-expansion" name))
        (get-expander (format-intern "symbol-~a-expander" name))
        (trace (format-intern "trace-~a" name))
        (untrace (format-intern "untrace-~a" name))
        (prop (gensym)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defparameter ,prop (make-hash-table :test #'equal))

         (defun ,get-expander (sym)
           (awhen (gethash sym ,prop)
             (when (eq (expand-node-type it) ',expander)
               (expand-node-fn it))))

         (defun (setf ,get-expander) (new-lambda sym)
           (declare (function new-lambda))
           (awhen (gethash sym ,prop)
             (warn 'redefine-warning
                   :older (expand-node-type it)
                   :newer ',expander
                   :fn-name sym))
           (setf (gethash sym ,prop)
                 (make-expand-node :fn new-lambda :type ',expander)))

         (defun ,expansion (codes)
           (if (listp codes)
               (destructuring-bind (name . args) codes
                 (aif (,get-expander name)
                      (values 
                       (with-debug ((expand-node-debug (gethash name ,prop))
                                    codes ',expansion)
                         (handler-case (funcall it args)
                           (error (c)
                             (error 'simple-error
                                    :format-control "~s symbol expansion failed~%  ~a"
                                    :format-arguments (list codes c)))))
                       t)
                      (values codes nil)))
               (values codes nil)))

         (defun ,trace (sym)
           (aif (gethash sym ,prop)
                (setf (expand-node-debug it) t)))

         (defun ,untrace (sym)
           (aif (gethash sym ,prop)
                (setf (expand-node-debug it) nil))))

       (defmacro ,expander (name parameters &body body)
         (with-symbols (outer-args)
           `(eval-when (:load-toplevel :execute)
              (handler-case (setf (,',get-expander ',name)
                                  (lambda (,outer-args)
                                    (destructuring-bind ,parameters ,outer-args
                                      ,@body)))
                (error (c)
                  (error 'simple-error
                         :format-control "define ~s symbol expander failed~%  ~a"
                         :format-arguments (list ',name c))))))))))
