(defsystem :monitor
    :description ""
    :components
    ((:file "src/generic")
     (:file "src/parse-code")
     (:file "src/do-varient"
            :depends-on ("src/generic"
                         "src/parse-code"))
     (:file "src/hash-op"
            :depends-on ("src/generic"
                         "src/do-varient"))
     (:file "src/queue/queue-template")
     (:file "src/queue/circular-queue"
            :depends-on ("src/queue/queue-template"
                         "src/do-varient"))
     (:file "src/base-tools"
            :depends-on ("src/generic"
                         "src/parse-code"
                         "src/do-varient"
                         "src/queue/circular-queue"
                         "src/hash-op"))
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
     (:file "root/rule/rule"
            :depends-on ("root/monitor-template"
                         "root/monitor-base"
                         "src/base-tools"))
     (:file "main"
            :depends-on ("root/monitor-template"
                         "root/monitor-base"
                         "root/rule/rule"
                         "src/base-tools"))))
