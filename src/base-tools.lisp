(defpackage :base-tools
  (:use :cl :generic :do-varient :hash-op)
  (:export
   ;;generic
   :aif :awhen :aunless :aif2 :awhen2 :aunless2 :it :last1
   :singlep :array-last :or= :or/= :or-char= :or-char/= :or-eq
   :strcat :forever :ensure :with-ensure :logicf :with-stream-format
   :with-collect :with-wrappers :mvpsetq :mvpsetf :make-accessor
   :accessor :with-symbols :make-slice :range :best-position :with-opt-slots
   :with-compare
   ;;heap
   :heapify! :build-heap! :sort-heap!
   ;;event-time
   :et-info :et-seconds :et-mseconds :event-time :newer :older
   ;;do-varient
   :do-complex
   ;;hash-op
   :plist-into-hash :with-hash-bindings))

(in-package :base-tools)

