(defpackage :base-tools
  (:use :cl :generic :do-varient :hash-op :setf-varient :parse-code :symbol-system)
  (:export
   ;;generic
   :aif :awhen :aif2 :awhen2 :aunless2 :it :self :last1
   :singlep :array-last :or= :or/= :or-char= :or-char/= :or-eq :strcat
   :forever :ensure :with-ensure :with-stream-format
   :with-collect :with-wrappers :with-symbols :make-slice :range :best-position
   :with-opt-slots :with-compare :with-plist-let :with-plist-builder
   ;;setf-varient
   :logicf :mvpsetq :mvpsetf :append-setf :append1-setf
   ;;heap
   :heapify! :build-heap! :sort-heap!
   ;;event-time
   :et-info :et-seconds :et-mseconds :event-time :newer :older
   ;;do-varient
   :do-complex
   ;;hash-op
   :plist-into-hash :with-hash-bindings
   ;;parse-code
   :with-collect-codes :with-parse-body
   ;;symbol-system
   :define-symbol-system))

(in-package :base-tools)

