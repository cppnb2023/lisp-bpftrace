(defpackage :hash-op
  (:use :cl :generic :do-varient)
  (:export :with-hash-bindings :plist-into-hash :mul-gethash))

(in-package :hash-op)

(defmacro with-hash-bindings ((&rest bindings) hash-table &body body)
  (let ((hash-sym (gensym "hash")))
    `(let ((,hash-sym ,hash-table))
       (symbol-macrolet
           ,(loop for (v k nil) in bindings
                  collect `(,v (gethash ,k ,hash-sym)))
         ,@(loop for (v nil default) in bindings
                 when default
                   collect `(aunless2 ,v (setf ,v ,default)))
         ,@body))))

(defmacro with-hash-let ((&rest bindings) hash-table &body body)
  (let ((hash-sym (gensym "hash")))
    `(let* ((,hash-sym ,hash-table)
            ,@(loop for (v k default) in bindings
                    if default
                      collect `(,v (aunless2 (gethash ,k ,hash-sym) ,default))
                    else
                      collect `(,v (gethash ,k ,hash-sym))))
       ,@body)))

;;(defun plist-into-hash (hash plist)
;;  "将plist写入hash-table中"
;;  (when (= (mod (length plist) 2) 1)
;;    (error "plist have odd elements"))
;;  (do-complex () ((:plist k v plist))
;;    (:do (setf (gethash k hash) v))))

(defmacro mul-gethash ((&rest keys) hash)
  (destructuring-bind (first &rest rest) keys
    (if rest
        `(mul-gethash ,rest (gethash ,first ,hash))
        `(gethash ,first ,hash))))
