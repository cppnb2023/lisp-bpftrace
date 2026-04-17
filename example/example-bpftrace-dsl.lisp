(defpackage :example-bpftrace-dsl
  (:use :cl :generic :monitor-base :process-information :bpftrace-dsl :monitor-template))

(in-package :example-bpftrace-dsl)

(format t "~a"
		  (bpftrace-code
			(:probe ;;探针定义，通常不需要自己写，这里为演示
			 "t:syscalls:sys_enter_execve"
			 (:if (:= "username" (:bstr "root"))
					(:progn
					  (:printf 1 :str "comm" :u32 "pid") ;;打印哈希值和命令名，进程id
					  )))))
