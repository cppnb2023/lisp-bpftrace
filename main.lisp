(defpackage :main
  (:use :cl :generic :monitor-base :process-information :bpftrace-dsl :monitor-template
		  :rule))

(in-package :main)

(defparameter *my-monitor* (make-monitor 'monitor-base
													  (:filter (:tracepoint "syscalls" "sys_enter_openat")
																  (:not-in "comm" (:bstr "bpftrace") (:bstr "sbcl")
																			  (:bstr "valkey-server")))))
(defparameter *my-monitor2* (make-monitor 'monitor-base
														(:tracepoint "syscalls" "sys_enter_execve")))

(defparameter *my-rule*
  (macrolet ((gethash1 (key rule)
					`(gethash ,key (first (get-other ,rule))))
				 (gethash2 (key rule)
					`(gethash ,key (second (get-other ,rule)))))
	 (make-rule ((list (make-hash-table :test #'equal)
							 (make-hash-table :test #'equal))
					 (*my-monitor* (monitor)
										(with-monitor (monitor)
										  (let ((comm (:get-member :comm)))
											 (when (gethash2 comm *my-rule*)
														(format t "~a即打开文件，又执行命令~%" comm))
											 (setf (gethash1 comm *my-rule*) 1))))
					 (*my-monitor2* (monitor)
										 (with-monitor (monitor)
											(let ((comm (:get-member :comm)))
											  (when (gethash1 comm *my-rule*)
														 (format t "~a即打开文件，又执行命令~%" comm))
											  (setf (gethash2 comm *my-rule*) 1)))))
					(monitor))))

(defun main ()
  (install-rule *my-rule*)
  (with-open-file (stream "/tmp/a.bt" :direction :output :if-exists :supersede)
  	 (add-monitors stream *my-monitor* *my-monitor2*))
  (exec-monitors "/tmp/a.bt")
  )

(main)
