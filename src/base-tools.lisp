(defpackage :base-tools
  (:use :cl :generic :do-varient :circular-queue :hash-op)
  (:export
   ;;generic
   :aif :awhen :aunless :aif2 :awhen2 :aunless2 :it :last1
   :singlep :array-last :or= :and= :or/=
   :and/= :or-char= :or-char/= :and-char= :and-char/= :strcat
   :ensure-symbol :forever  :plist-into-hash :ensure-integer :logior-setf
   :ensure-logior-setf :with-wrappers :mvsetq :mvsetf :mvpsetf
   :make-accessor :accessor :with-most :with-symbols :get-most-accessor
   ;;do-varient
   :do-stage :do-stage* :do-list-stage :do-times-stage :do-plist-stage
   :do-stage-format :do-stage-format* :do-list-stage-format
   :do-times-stage-format :do-plist-stage-format :do-window-stage
   :do-window-stage-format :do-circular-stage :do-circular-stage-collect
   :do-complex
   ;;circular-queue
   :circular-queue :make-circular-queue :queue-empty-p
   :queue-full-p :queue-enqueue :queue-dequeue :queue-peek
   :queue-size :queue-coerce :iterator
   ;;hash-op
   :plist-into-hash :with-hash-bindings))

(in-package :base-tools)

