(defpackage :parse-code
  (:use :cl)
  (:export :with-collect-codes :with-parse-body))

(in-package :parse-code)

(defmacro with-collect-codes ((&rest bindings) &body body)
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

(defun find-only% (item list &rest args)
  (if (> (apply #'count item list args) 1)
      (error (format nil "~s occurs more than once~%" item))
      (apply #'find item list args)))

(defmacro with-parse-body ((&rest bindings) body-code &body body)
  (let ((body-sym (gensym "body"))
        (keys (mapcar #'second bindings)))
    `(let ((,body-sym ,body-code))
       (loop for (k . nil) in ,body-code
             do (unless (find k ',keys)
                  (error (format nil "WITH-PARSE-BODY: unknown ~s" k))))
       (let ,(loop for (v k nil) in bindings
                   collect `(,v (rest (find-only% ,k ,body-sym :key #'first))))
         ,@(loop for (v nil default) in bindings
                 when default
                   collect `(unless ,v (setf ,v ,default)))
         ,@body))))
