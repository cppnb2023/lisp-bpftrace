(defpackage :dsl
  (:use :cl :base-tools)
  (:export #:define-dsl-expander #:get-dsl-expansion #:make-binary-operator
           #:make-trans-operator :make-untrans-operator #:define-dsl-operator
           #:generate-string))

(in-package :dsl)

(define-symbol-system dsl)

(defun dsl-expand (language body)
  (get-dsl-expansion (cons language body)))

(defun make-binary-operator (expand-func single-fmt op)
  (lambda (&rest parameters)
    (cond
      ((singlep parameters)
       (format nil single-fmt (car parameters)))
      (t
       (do-complex ((:format fmt)) ((:list p parameters))
         (:start (fmt "("))
         (:do (:stage (:first (fmt "~a" (funcall expand-func p)))
                      (:main  (fmt " ~a ~a" op (funcall expand-func p)))))
         (:finally (fmt ")")))))))

(defun make-trans-operator (expand-func op)
  (lambda (&rest parameters)
    (do-complex ((:format fmt)) ((:window (a b) parameters))
      (:start (fmt "("))
      (:do (:stage (:first (fmt "~a ~a ~a"
                                (funcall expand-func a)
                                op
                                (funcall expand-func b)))
                   (:main  (fmt " && ~a ~a ~a"
                                (funcall expand-func a)
                                op
                                (funcall expand-func b)))))
      (:finally (fmt ")")))))

(defun make-untrans-operator (expand-func op)
  (lambda (&rest parameters)
    (do-complex ((:format fmt)) ((:on p1 parameters))
      (:start (fmt "("))
      (:do (do-complex () ((:on p2 parameters))
             (:do (fmt "~a ~a ~a"
                       (funcall expand-func (car p1))
                       op
                       (funcall expand-func (car p2))))))
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

(defun generate-string (string depth)
  (declare (string string))
  (declare (fixnum depth))
  (if (= depth 0)
      string
      (generate-string
       (do-complex ((:format fmt)) ((:across c string))
         (:do (fmt (cond
                     ((char= c #\") "\\\"")
                     ((char= c #\\) "\\\\")
                     (t (string c))))))
       (1- depth))))
