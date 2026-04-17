(defpackage :monitor-template
  (:use :cl #:generic #:bpftrace-dsl)
  (:import-from :sb-ext :run-program :process-output)
  (:export :monitor-template :get-probe :get-hook-hash :get-member
			  :solve :write-monitor :generate-bpftrace-code
			  :read-information :get-idx :add-monitors :exec-monitors
			  :with-monitor))

(in-package :monitor-template)

(defclass monitor-template ()
  ((idx :type fixnum :reader get-idx)))

(defparameter *idx* 0)
(defparameter *monitor-hash* (make-hash-table))

(defgeneric get-probe (monitor))
(defgeneric get-hook-hash (monitor))
(defgeneric get-member (monitor key))
(defgeneric solve (monitor))

(defgeneric write-monitor (monitor stream))
(defgeneric generate-bpftrace-code (monitor))
(defgeneric read-information (monitor plist))

(defmethod initialize-instance :after ((monitor monitor-template) &key)
  (setf (slot-value monitor 'idx) (incf *idx*)))

(defun monitorp (var)
  (typep var 'monitor-template))

(defun push-monitor (monitor)
  (if (monitorp monitor)
      (setf (gethash (get-idx monitor) *monitor-hash*) monitor) nil))

(defun add-monitors (stream &rest monitors)
  (dolist (monitor monitors)
	 (push-monitor monitor)
	 (write-monitor monitor stream)))

(defmethod write-monitor ((monitor monitor-template) stream)
  (with-write-bpftrace (stream)
    (:probe
     (get-probe monitor)
     (generate-bpftrace-code monitor))))

(defun solve-infomation (stream)
  (do ((plist (read stream) (read stream))) (nil)
	 (aif2 (gethash (getf plist :hash) *monitor-hash*)
			 (progn
				(read-information it plist)
				(solve it))
			 (error (format nil "read error: not cognizance ~a" idx)))))

(defun exec-monitors (file)
  (forever
	(let* ((process (run-program "/usr/bin/bpftrace" (list file) :wait nil :output :stream))
			 (output (process-output process)))
	  (format t "~a~%" (read-line output))
	  (solve-infomation output))))

(defmacro with-monitor ((monitor) &body body)
  (let ((monitor-sym (gensym "monitor")))
	 `(let ((,monitor-sym ,monitor))
		 (macrolet ((:get-member (keyword)
						  `(get-member ,',monitor-sym ,keyword)))
			,@body))))
