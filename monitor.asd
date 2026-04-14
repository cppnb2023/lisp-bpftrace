(defsystem :monitor
 :description ""
 :components
 ((:file "generic")
  (:file "root/bpftrace-dsl")
  (:file "root/monitor-template")
  (:file "root/monitor-base")
  (:file "user/misc")
  (:file "root/rule")
  (:file "main")))
