(defpackage :monitor-base
  (:use :cl :monitor-template :generic :bpftrace-dsl)
  (:export :monitor-base :get-hook-hash :get-member
			  :make-monitor :generate-bpftrace-code :solve
			  :read-information :with-monitor-base-bind :add-monitors
			  :exec-monitors :defmonitor ::tracepoint))

(in-package :monitor-base)

(defclass monitor-base (monitor-template)
  ((probe :type string :reader get-probe :initarg :probe)
	(member-hash :initform (make-hash-table) :reader get-member-hash)
	(hook-hash   :initform (make-hash-table) :reader get-hook-hash)))

(defmethod generate-bpftrace-code ((monitor monitor-base))
  (bpftrace-code
	(:printf (get-idx monitor)
				:str "comm"
				:u32 "ppid" :u32 "pid" :u32 "tid"  :u64 "nsecs")))

(defmethod get-member ((monitor monitor-base) keyword)
  (gethash keyword (get-member-hash monitor)))

(defmethod read-information ((monitor monitor-base) plist)
  (plist-into-hash (get-member-hash monitor) plist))

(defmethod solve ((monitor monitor-base))
  (loop for v being the hash-values in (get-hook-hash monitor) do
		  (funcall v monitor)))

(defmacro defmonitor (class-name parents members &body body)
  (let ((tmp-sym (gensym "tmp"))
		  (monitor-sym (gensym "monitor")))
	 `(progn
		 (defclass ,class-name ,(if parents parents (list 'monitor-base))
			(,@members))

		 (defun ,(intern (format nil "~aP" class-name))
			  (,tmp-sym)
			(typep ,tmp-sym ',class-name))

		 (defmethod generate-bpftrace-code ((,monitor-sym ,class-name))
			(bpftrace-code
			 (macrolet ((:printf (&rest kvlist)
							  `(bpftrace-printf (get-idx ,',monitor-sym) ,@kvlist)))
				,@(cdr (find :bpftrace body :key #'car))))))))

(defmacro make-monitor (class-name probe)
  `(macrolet ((:filter (probe filter)
					 `(format nil "~a /~a/" ,probe ,filter))
				  (:kprobe (kernel-func)
					 `(format nil "kprobe:~a" ,kernel-func))
				  (:tracepoint (type event)
					 `(format nil "tracepoint:~a:~a" ,type ,event))
				  (:not-in (var &body rest) `(bpftrace-not-in ,var ,@rest))
				  (:in (var &body rest) `(bpftrace-in ,var ,@rest))
				  (:bstr (string)
					 `(format nil "\"~a\"" ,string)))
	  (make-instance ,class-name :probe ,probe)))

