(defpackage :dsl
  (:use :cl :base-tools)
  (:export #:define-dsl-expander #:get-dsl-expansion #:make-binary-operator
           #:make-trans-operator :make-untrans-operator #:define-dsl-operator))

(in-package :dsl)

(define-symbol-system dsl)

(defun dsl-expand (language body &optional env)
  (get-dsl-expansion (cons language body) env))

(defun make-binary-operator (expand-func single-fmt op)
  (lambda-env (&rest parameters &environment env)
    (cond
      ((singlep parameters)
       (format nil single-fmt (car parameters)))
      (t
       (do-complex ((:format fmt)) ((:list p parameters))
         (:start (fmt "("))
         (:do (:stage (:first (fmt "~a" (funcall expand-func p env)))
                      (:main  (fmt " ~a ~a" op (funcall expand-func p env)))))
         (:finally (fmt ")")))))))

(defun make-trans-operator (expand-func op)
  (lambda-env (&rest parameters &environment env)
    (do-complex ((:format fmt)) ((:window (a b) parameters))
      (:start (fmt "("))
      (:do (:stage (:first (fmt "~a ~a ~a"
                                (funcall expand-func a env)
                                op
                                (funcall expand-func b env)))
                   (:main  (fmt " && ~a ~a ~a"
                                (funcall expand-func a env)
                                op
                                (funcall expand-func b env)))))
      (:finally (fmt ")")))))

(defun make-untrans-operator (expand-func op)
  (lambda-env (&rest parameters &environment env)
    (do-complex ((:format fmt)) ((:on p1 parameters))
      (:start (fmt "("))
      (:do (do-complex () ((:on p2 parameters))
             (:do (fmt "~a ~a ~a"
                       (funcall expand-func (car p1) env)
                       op
                       (funcall expand-func (car p2) env)))))
      (:finally (fmt ")")))))

(defmacro define-dsl-operator (symbol-expand expand-func &body body)
  (with-parse-body ((trans :transitivity-compare)
                    (untrans :untransitivity-compare)
                    (binary :binary-compute-operator))
                   body
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(loop for (name . args) in trans
               collect `(setf (,symbol-expand ,name)
                              (make-trans-operator #',expand-func ,@args)))
       ,@(loop for (name . args) in untrans
               collect `(setf (,symbol-expand ,name)
                              (make-untrans-operator #',expand-func ,@args)))
       ,@(loop for (name . args) in binary
               collect `(setf (,symbol-expand ,name)
                              (make-binary-operator #',expand-func ,@args))))))
