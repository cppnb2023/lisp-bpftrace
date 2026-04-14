(defpackage :main
  (:use :cl :generic :monitor-base :process-information :bpftrace-dsl :monitor-template
		  :rule))

(in-package :main)

(defparameter *my-monitor* (make-monitor 'monitor-base (:tracepoint "syscalls" "sys_enter_execve")))
(defparameter *my-rule*
  (make-rule ((*my-monitor*))
				 (monitor)
				 (with-monitor (monitor)
					(format t "ppid:~a pid: ~a~%" (get-member :ppid) (get-member :pid)))))

(defun main ()
  (install-rule *my-rule*)
  (with-open-file (stream "/tmp/a.bt" :direction :output :if-exists :supersede)
	 (add-monitors stream *my-monitor*))
  (exec-monitors "/tmp/a.bt"))

(main)
