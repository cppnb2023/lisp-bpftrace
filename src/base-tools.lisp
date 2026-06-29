(defpackage :base-tools
  (:use :cl :generic :do-varient :hash-op :setf-varient :parse-code :symbol-system
        :event-time :memoize :void)
  (:export
   ;;generic
   #:aif #:awhen #:acond #:aif2 #:awhen2 #:aunless2 #:it #:self #:last1
   #:singlep #:array-last #:or= #:or/= #:or-char= #:or-char/= #:or-eq #:strcat
   #:forever #:ensure #:with-stream-format #:with-collect
   #:with-wrappers #:with-symbols #:make-slice #:range
   #:best-position #:with-opt-slots #:with-compare #:with-plist-let
   ;;setf-varient
   #:logicf #:mvpsetq #:mvpsetf #:append-setf #:append1-setf
   ;;heap
   #:heapify! #:build-heap! #:sort-heap!
   ;;event-time
   #:et-data #:et-seconds #:et-mseconds #:event-time #:newer #:older #:make-event-time
   ;;do-varient
   #:do-complex #:define-dc-acc-expander #:define-dc-style-expander
   ;;hash-op
   #:with-hash-bindings #:with-hash-let #:mul-gethash #:hash-lambda #:to-hash-binding
   ;;parse-code
   #:with-collect-codes #:with-parse-body
   ;;symbol-system
   #:define-symbol-system #:symbol-system-function #:symbol-system-macro
   ;;memoize
   #:last-memoize #:with-last-memoize
   ;;void
   :+void+ :vif :vwhen :vunless :vwhen-void :avif :avwhen :avwhen-void
   :vife :avife))

(in-package :base-tools)

