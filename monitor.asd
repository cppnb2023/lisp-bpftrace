(defsystem :monitor
 :description ""
 :components
 ((:file "src/generic")
  (:file "src/do-varient"
         :depends-on ("src/generic"))
  (:file "src/base-tools"
         :depends-on ("src/generic"
                      "src/do-varient"))
  (:file "root/bpftrace-dsl"
         :depends-on ("src/base-tools"))
  (:file "root/monitor-template"
         :depends-on ("root/bpftrace-dsl"
                      "src/base-tools"))
  (:file "root/monitor-base"
         :depends-on ("root/monitor-template"
                      "root/bpftrace-dsl"
                      "src/base-tools"))
  (:file "user/misc"
         :depends-on ("src/base-tools"))
  (:file "root/rule"
         :depends-on ("root/monitor-template"
                      "root/monitor-base"
                      "src/base-tools"))
  (:file "main"
         :depends-on ("root/monitor-template"
                      "root/monitor-base"
                      "root/rule"
                      "src/base-tools"))))
