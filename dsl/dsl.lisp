(defpackage :dsl
  (:use :cl :base-tools)
  (:export #:defun-dsl #:get-dsl-expansion #:make-binary-operator
           #:make-trans-operator :make-untrans-operator #:define-dsl-operator
           #:generate-string #:*dsl-usually-op* #:define-dsl-usually-op))

(in-package :dsl)

(define-symbol-system dsl)

(set-dispatch-macro-character
 #\# #\"
 (lambda (stream char1 char2)
   (declare (ignorable char1 char2))
   (with-output-to-string (s)
     (format s "\"")
     (loop for c = (read-char stream)
           until (char= c #\")
           do (if (char= c #\\)
                  (format s "~c~c" #\\ (read-char stream))
                  (format s "~c" c)))
     (format s "\""))))

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

(defun make-trans-compare (op)
  (lambda (&rest parameters)
    (do-complex ((:format fmt)) ((:window (a b) parameters))
      (:start (fmt "("))
      (:do (:stage (:first (fmt "~a ~a ~a" a op b))
                   (:main  (fmt " && ~a ~a ~a" a op b))))
      (:finally (fmt ")")))))

(defun make-untrans-compare (op)
  (lambda (&rest parameters)
    (do-complex ((:format fmt)) ((:on a parameters))
      (:start (fmt "("))
      (:do (do-complex () ((:on b (cdr a)))
             (:do (:stage (:first (fmt "~a ~a ~a" (car a) op (car b)))
                          (:main  (fmt " && ~a ~a ~a" (car a) op (car b)))))))
      (:finally (fmt ")")))))

(defun make-binary-solve (op single-fmt)
  (lambda (&rest parameters)
    (cond
      ((null parameters)
       (error 'simple-error
              :format-control "dsl ~s expand failed: nothing parameter"
              :format-arguments (list op)))
      ((singlep parameters)
       (format nil single-fmt (car parameters)))
      (t (do-complex ((:format fmt)) ((:list a parameters))
           (:start (fmt "("))
           (:do (:stage (:first (fmt "~a" a))
                        (:main  (fmt " ~a ~a" op a))))
           (:finally (fmt ")")))))))

(defparameter *dsl-usually-op*
  `((:+ ,(make-binary-solve "+" "~a"))
    (:- ,(make-binary-solve "-" "-~a"))
    (:* ,(make-binary-solve "*" "~a"))
    (:/ ,(make-binary-solve "/" "1.0/~a"))
    (:logior ,(make-binary-solve "|" "~a"))
    (:logand ,(make-binary-solve "&" "~a"))
    (:logxor ,(make-binary-solve "^" "~~~a"))
    (:=  ,(make-trans-compare "=="))
    (:<  ,(make-trans-compare "<"))
    (:<= ,(make-trans-compare "<="))
    (:>  ,(make-trans-compare ">"))
    (:>= ,(make-trans-compare ">="))
    (:/= ,(make-untrans-compare "!="))
    (:and ,(make-binary-solve "&&" "~a"))
    (:or  ,(make-binary-solve "||" "~a"))))

(defmacro define-dsl-usually-op (symbol-system)
  `(eval-when (:load-toplevel :execute)
     (loop for (name lambda) in *dsl-usually-op*
           do (setf (symbol-system-function ,symbol-system name)
                    lambda))))
