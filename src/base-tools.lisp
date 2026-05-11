(defpackage :base-tools
  (:use :cl :generic :do-varient :circular-queue :hash-op)
  (:export
   ;;generic
   :aif :awhen :aunless :aif2 :awhen2 :aunless2 :it :last1
   :singlep :array-last :or= :or/= :or-char= :or-char/= :or-eq
   :strcat :forever :ensure :with-ensure :logicf :with-stream-format
   :with-collect :with-wrappers :mvpsetq :mvpsetf :make-accessor
   :accessor :with-most :with-symbols :get-most-accessor :make-slice
   :range :best-position :with-opt-slots
   ;;do-varient
   :do-complex
   ;;circular-queue
   :circular-queue :make-circular-queue :queue-empty-p
   :queue-full-p :queue-enqueue :queue-dequeue :queue-peek
   :queue-size :queue-coerce :iterator
   ;;hash-op
   :plist-into-hash :with-hash-bindings))

(in-package :base-tools)

