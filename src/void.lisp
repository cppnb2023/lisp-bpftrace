(defpackage :void
  (:use :cl :generic)
  (:export :+void+ :vif :vwhen :vunless :vwhen-void :avif :avwhen :avwhen-void
           :vife :avife))

(in-package :void)

(defparameter +void+ '#.(gensym "void"))

(defmacro vif (cond then &optional else (void '+void+))
  (with-symbols (win)
    `(let ((,win ,cond))
       (if (eq ,win +void+)
           ,void
           (if ,win ,then ,else)))))

(defmacro vwhen (cond &body body)
  `(vif ,cond (progn ,@body) nil +void+))

(defmacro vunless (cond &body body)
  `(vif ,cond nil (progn ,@body) +void+))

(defmacro vwhen-void (cond &body body)
  `(vif ,cond nil nil (progn ,@body)))

(defmacro vife (cond then &optional else)
  (with-symbols (win)
    `(let ((,win ,cond))
       (if (and ,win (not (eq ,win +void+)))
           ,then ,else))))

(defmacro avif (cond then &optional else (void '+void+))
  `(let ((it ,cond))
     (if (eq it +void+)
         ,void
         (if it ,then ,else))))

(defmacro avwhen (cond &body body)
  `(avif ,cond (progn ,@body) nil +void+))

(defmacro avwhen-void (cond &body body)
  `(avif ,cond nil nil (progn ,@body)))

(defmacro avife (cond then &optional else)
  `(let ((it ,cond))
     (if (and it (not (eq it +void+)))
         ,then ,else)))
