(defpackage :base-tools
  (:use :cl :generic :do-varient)
  (:export
   ;;generic
   :aif :awhen :aunless :aif2 :awhen2 :aunless2 :it :last1
   :singlep :array-last :or= :and= :or/=
   :and/= :or-char= :or-char/= :and-char= :and-char/= :strcat
   :ensure-symbol :forever  :plist-into-hash
   ;;do-varient
   :do-stage :do-stage* :do-list-stage :do-times-stage :do-plist-stage
   :do-stage-format :do-stage-format* :do-list-stage-format
   :do-times-stage-format :do-plist-stage-format :do-tuple-stage
   :do-tuple-stage-format))
