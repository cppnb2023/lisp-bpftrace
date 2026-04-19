(defpackage :process-information
  (:use :cl :base-tools)
  (:export :process-dir :process-cmdline :process-status))

(in-package :process-information)

(defun process-dir (pid)
  (the string (format nil "/proc/~a/" pid)))

(defun process-cmdline (pid-or-path)
  (when (numberp pid-or-path)
    (setf pid-or-path (strcat (process-dir pid-or-path) "cmdline")))
  (with-open-file (in pid-or-path :direction :input)
    (read-line in)))

(defun process-status (pid-or-path)
  (when (numberp pid-or-path)
    (setf pid-or-path (strcat (process-dir pid-or-path) "status")))
  (let ((strings nil))
    (handler-case 
        (with-open-file (in pid-or-path :direction :input)
          (loop do (push (read-line in) strings)))
      (end-of-file (c)
        (return-from process-status (nreverse strings)))
      (file-error (c)
        (return-from process-status (nreverse strings))))))

