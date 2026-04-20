(defpackage :hash-op
  (:use :cl :generic :do-varient)
  (:export :with-hash-bindings :plist-into-hash))

(in-package :hash-op)

(defmacro with-hash-bindings (bindings hash-table &body body)
  (let ((hash-sym (gensym "hash")))
    `(let ((,hash-sym ,hash-table))
       (symbol-macrolet
           ,(loop for (v k) in bindings collect
                  `(,v (gethash ,k ,hash-sym)))
         ,@body))))

(defmacro with-hash-let (bindings hash-table &body body)
  (let ((hash-sym (gensym "hash")))
    `(let* ((,hash-sym ,hash-table)
            ,@(loop for (v k) in bindings collect
                    `(,v (gethash ,k ,hash-sym))))
       ,@body)))

(defun plist-into-hash (hash plist)
  "将plist写入hash-table中"
  (when (= (mod (length plist) 2) 1)
    (error "plist have odd elements"))
  (do-plist-stage (k v plist)
    (:main (setf (gethash k hash) v))))

