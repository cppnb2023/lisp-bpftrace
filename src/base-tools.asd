(defsystem :base-tools
  :description ""
  :components
  ((:file "generic")
   (:file "double-list")
   (:file "setf-varient")
   (:file "parse-code")
   (:file "void"          :depends-on ("generic"))
   (:file "heap"          :depends-on ("generic"))
   (:file "event-time"    :depends-on ("generic"))
   (:file "symbol-system" :depends-on ("generic"))
   (:file "memoize"       :depends-on ("generic"))
   (:file "hash-op"       :depends-on ("generic" "do-varient" "memoize"))
   (:file "do-varient"
    :depends-on ("generic" "parse-code" "setf-varient" "symbol-system"))
   (:file "base-tools"
    :depends-on ("generic"
                 "void"
                 "setf-varient"
                 "heap"
                 "event-time"
                 "parse-code"
                 "do-varient"
                 "hash-op"
                 "symbol-system"
                 "memoize"))))

