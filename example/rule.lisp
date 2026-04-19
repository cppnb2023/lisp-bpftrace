;;下文是一个即可以检测打开文件，又能检测命令执行的规则
(defpackage :example-rule
  (:use :cl :generic :monitor-base :process-information :bpftrace-dsl :monitor-template
        :rule))

(in-package :example-rule)

(defparameter *my-monitor*
  (make-monitor 'monitor-base
                (:filter (:tracepoint "syscalls" "sys_enter_openat")
                         (:not-in "comm" (:bstr "bpftrace") (:bstr "sbcl")
                                  (:bstr "valkey-server")))))
(defparameter *my-monitor2*
  (make-monitor 'monitor-base
                (:tracepoint "syscalls" "sys_enter_execve")))

(defparameter *my-rule*
  (make-rule ((*my-monitor* (monitor)
                            (with-monitor (monitor)
                              (let ((comm (:get-member :comm)))
                                (when (get-member *my-rule* :execve)
                                  (format t "~a即打开文件，又执行命令~%" comm))
                                (setf (get-member *my-rule* :openat) t))))
              (*my-monitor2* (monitor)
                             (with-monitor (monitor)
                               (let ((comm (:get-member :comm)))
                                 (when (get-member *my-rule* :openat)
                                   (format t "~a即打开文件，又执行命令~%" comm))
                                 (setf (get-member *my-rule* :execve) t)))))
             (monitor)))

