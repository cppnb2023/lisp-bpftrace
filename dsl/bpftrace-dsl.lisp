(defpackage :bpftrace-dsl
  (:use :cl :base-tools :dsl)
  (:export #:bpftrace-expand #:bpftrace-gensym #:with-bpftrace-expand))

(in-package :bpftrace-dsl)

(define-symbol-system bpftrace)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-dsl-symbol (symbol)
    (string-downcase (substitute #\_ #\- (string symbol))))

  (defun bpftrace-expand (code &optional env)
    (cond
      ((typep code 'string)
       (format nil "~s" code))
      ((typep code 'number)
       (format nil "~a" code))
      ((typep code 'symbol)
       (aif2 (macroexpand code)
             (bpftrace-expand it env)
             (to-dsl-symbol code)))
      ((typep code 'list)
       (get-bpftrace-expansion code env))
      (t (error (format nil "unknown type ~a" (type-of code)))))))

(define-dsl-expander :bpftrace (&body body)
  (bpftrace-expand body))

(let ((counter 0))
  (defun bpftrace-gensym (&optional (preffix 'b))
    (string-downcase (substitute #\_ #\- (format nil "$~a~8,'0X" preffix counter)))))

(defmacro with-bpftrace-expand ((&rest vars) &body body)
  `(let ,(loop for v in vars collect `(,v (bpftrace-expand ,v)))
     ,@body))

(define-bpftrace-expander :set (var val)
  (with-bpftrace-expand (var val)
    (format nil "~a = ~a" var val)))

(define-bpftrace-expander :progn (&body body)
  (do-complex ((:format fmt)) ((:list code body))
    (:do (fmt "~s"
              (let ((code (bpftrace-expand code)))
                (if (or-char= (array-last code) #\{ #\} #\;)
                    (concatenate 'string code ";")
                    code))))))

(define-bpftrace-expander :let ((&rest bindings) &body body)
  (bpftrace-expand
   `(:progn
      ,@(loop for (var val) in bindings
              collect `(:set ,var ,val))
      ,@body)))

(define-dsl-operator symbol-bpftrace-expander bpftrace-expand
  (:binary-compute-operator
   (:+ "~a"   "+")
   (:- "-~a"  "-")
   (:* "~a"   "*")
   (:/ "1/~a" "/")
   (:logior "~a"   "|")
   (:logand "~a"   "&")
   (:logxor "~~~a" "^"))

  (:transitivity-compare
   (:=  "==")
   (:<  "<")
   (:>  ">")
   (:<= "<=")
   (:>= ">="))

  (:untransitivity-compare
   (:/= "!=")))

