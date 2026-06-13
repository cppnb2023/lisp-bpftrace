(defsystem :dsl
  :description ""
  :depends-on (:base-tools)
  :components
  ((:file "dsl")
   (:file "bpftrace-dsl" :depends-on ("dsl"))))

