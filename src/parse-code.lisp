(defpackage :parse-code
  (:use :cl)
  (:export :with-collect-codes))

(in-package :parse-code)

(defmacro with-collect-codes (bindings &body body)
  (let ((let-bindings nil)
        (flet-bindings nil))
    (dolist (bind bindings)
      (destructuring-bind (sym val) bind
        (push val let-bindings)
        (push `(,sym (&rest codes)
                     (setf ,val (append ,val codes)))
              flet-bindings)))
    (setf let-bindings (nreverse let-bindings))
    (setf flet-bindings (nreverse flet-bindings))
    `(let ,let-bindings
       (flet ,flet-bindings
         ,@body))))

