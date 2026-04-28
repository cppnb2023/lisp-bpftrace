(defpackage :unit-test
  (:use :cl :generic)
  (:export :deftest :check))

(in-package :unit-test)

(defparameter *test-name* nil)

(defun report-result (result form)
  (format t "~:[FAIL~;PASS~] ~a: ... ~s~%" result *test-name* form)
  result)

(defmacro combine-result (&body body)
  (let ((result (gensym "result")))
    `(let ((,result t))
       ,@(loop for expr in body collect
               `(unless ,expr (setf ,result nil)))
       ,result)))

(defmacro check (&body body)
  `(combine-result
     ,@(loop for expr in body collect
             `(report-result ,expr ',expr))))

(defmacro deftest (fname args &body body)
  `(defun ,fname ,args
     (let ((*test-name* (append *test-name* (list ',fname))))
       ,@body)))

