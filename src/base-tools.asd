(defsystem :base-tools
  :description ""
  :components
  ((:file "generic")
   (:file "setf-varient")
   (:file "parse-code")
   (:file "event-time"    :depends-on ("generic"))
   (:file "symbol-system" :depends-on ("generic"))
   (:file "memoize"       :depends-on ("generic"))
   (:file "hash-op"       :depends-on ("generic" "do-varient" "memoize"))
   (:file "do-varient"
    :depends-on ("generic" "parse-code" "setf-varient" "symbol-system"))
   (:file "base-tools"
    :depends-on ("generic"
                 "setf-varient"
                 "event-time"
                 "parse-code"
                 "do-varient"
                 "hash-op"
                 "symbol-system"
                 "memoize"))))

