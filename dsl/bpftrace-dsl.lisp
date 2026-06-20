(defpackage :bpftrace-dsl
  (:use :cl :base-tools :dsl)
  (:export #:with-bpftrace-expand #:bpftrace-expand #:bpftrace-gensym))

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
             (cond
               ((eq code nil) "null")
               ((eq code t)   "true")
               (t (to-dsl-symbol code)))))
      ((typep code 'list)
       (get-bpftrace-expansion code env))
      (t (error (format nil "unknown type ~a" (type-of code)))))))

(define-dsl-expander :bpftrace (&body body)
  (bpftrace-expand body))

(defmacro with-bpftrace-expand ((&rest vars) &body body)
  `(let ,(loop for v in vars collect `(,v (bpftrace-expand ,v)))
     ,@body))

(defparameter *bpftrace-gensym-counter* 0)

(defun bpftrace-gensym (&optional global-p (preffix "b"))
  (intern (format nil "~:[$~;@~]~a~8,'0X" global-p preffix
                  (incf *bpftrace-gensym-counter*))
          :keyword))

(define-bpftrace-expander :set (var val)
  (with-bpftrace-expand (var val)
    (format nil "~a = ~a" var val)))

(define-bpftrace-expander :progn (&body body)
  (do-complex ((:format fmt)) ((:list code body))
    (:start (fmt "{"))
    (:do (fmt "~a"
              (let ((code (bpftrace-expand code)))
                (if (or-char= (array-last code) #\{ #\} #\;)
                    code
                    (concatenate 'string code ";")))))
    (:finally (fmt "}"))))

(define-bpftrace-expander :let ((&rest bindings) &body body)
  (bpftrace-expand
   `(:progn
      ,@(loop for (var val) in bindings
              collect `(:set ,var ,val))
      ,@body)))

(define-bpftrace-expander :if (cond then &optional else)
  (with-bpftrace-expand (cond then)
    (with-output-to-string (stream)
      (format stream "if (~a) ~a" cond then)
      (when else
        (format stream "else ~a" (bpftrace-expand else))))))

(define-bpftrace-expander :define-probe (probe cond &body body)
  (with-bpftrace-expand (probe cond)
    (format nil "~a/~a/~a" probe cond
            (bpftrace-expand `(:progn ,@body)))))

(define-bpftrace-expander :gethash (key hash)
  (with-bpftrace-expand (key hash)
    (format nil "~a[~a]" key hash)))

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

