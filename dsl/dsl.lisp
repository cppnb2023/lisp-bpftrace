(defpackage :dsl
  (:use :cl :base-tools)
  (:export #:define-dsl-expander #:get-dsl-expansion #:dsl-expr-fmt
           #:make-binary-operator #:make-trans-operator :make-untrans-operator
           #:define-dsl-operator))

(in-package :dsl)

(define-symbol-system dsl)

(defun dsl-expand (language body &optional env)
  (get-dsl-expansion (cons language body) env))

(define-dc-acc-expander dsl-expr-fmt (&rest parameters)
  (let ((symbols (loop repeat (length parameters)
                       collect (gensym))))
    `(:bind  ,(loop for s in symbols
                    collect `(,s (make-string-output-stream)))
      :init  ,(loop for s in symbols
                    collect `(write-char #\( ,s))
      :end   ,(loop for s in symbols
                    collect `(write-char #\) ,s))
      :macro ,(loop for p in parameters
                    for s in symbols
                    collect `(,p (fmt &rest args)
                                 `(format ,',s ,fmt ,@args)))
      :opt   ((declare (stream ,@symbols)))
      :res   ,(loop for s in symbols
                    collect `(get-output-stream-string ,s)))))

(defun make-binary-operator (expand-func single-fmt op)
  (lambda-env (&rest parameters &environment env)
    (cond
      ((singlep parameters)
       (format nil single-fmt (car parameters)))
      (t
       (do-complex ((dsl-expr-fmt fmt)) ((:list p parameters))
         (:do (:stage (:first (fmt "~a" (funcall expand-func p env)))
                      (:main  (fmt " ~a ~a" op (funcall expand-func p env))))))))))

(defun make-trans-operator (expand-func op)
  (lambda-env (&rest parameters &environment env)
    (do-complex ((dsl-expr-fmt fmt)) ((:window (a b) parameters))
      (:do (:stage (:first (fmt "~a ~a ~a"
                                (funcall expand-func a env)
                                op
                                (funcall expand-func b env)))
                   (:main  (fmt " && ~a ~a ~a"
                                (funcall expand-func a env)
                                op
                                (funcall expand-func b env))))))))

(defun make-untrans-operator (expand-func op)
  (lambda-env (&rest parameters &environment env)
    (do-complex ((dsl-expr-fmt fmt)) ((:on p1 parameters))
      (:do (do-complex () ((:on p2 parameters))
             (:do (fmt "~a ~a ~a"
                       (funcall expand-func (car p1) env)
                       op
                       (funcall expand-func (car p2) env))))))))

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
