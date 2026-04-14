(defpackage :rule
  (:use :cl :generic :bpftrace-dsl :monitor-template)
  (:export :rule :install-rule :uninstall-rule :solve :make-rule
			  :other))

(in-package :rule)

(defclass rule ()
  ((monitors :type list :accessor get-monitors :initarg :monitors)
	(rules :type list :accessor get-rules :initarg :rules)
	(hook-hash :initform (make-hash-table) :reader get-hook-hash)
	(other :type list :initform nil :accessor get-other)))

(defgeneric install-rule (rule))
(defgeneric uninstall-rule (rule))

(defmethod solve ((rule rule))
  (loop for v being the hash-values in (get-hook-hash rule) do
		  (funcall v rule)))

(defmacro make-rule ((&rest monitor-or-rules) &body default-rule)
  (let* ((tmp-sym (gensym "tmp"))
			(monitors (loop for mr in monitor-or-rules collect
								 (car mr)))
			(rules (loop for mr in monitor-or-rules collect
					 `(lambda ,@(aif (cdr mr) it default-rule)
						 (solve ,tmp-sym)))))
	 `(let (,tmp-sym)
		 (setf ,tmp-sym
				 (make-instance 'rule
									 :monitors (list ,@monitors)
									 :rules    (list ,@rules))))))

(defmethod install-rule ((rule rule))
  (loop for m in (get-monitors rule)
		  for r in (get-rules rule) do
		  (setf (gethash rule (get-hook-hash m)) r)))

(defmethod uninstall-rule (rule)
  (loop for m in (get-monitors rule) do
		  (remhash rule (get-hook-hash m))))
