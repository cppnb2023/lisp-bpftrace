(defpackage :rule
  (:use :cl :base-tools :bpftrace-dsl :monitor-template)
  (:export :rule :install-rule :uninstall-rule :solve :make-rule
           :get-other :with-rule))

(in-package :rule)

(defclass rule ()
  ((monitors :type list :accessor get-monitors :initarg :monitors)
   (rules :type list :accessor get-rules :initarg :rules)
   (hook-hash :initform (make-hash-table) :reader get-hook-hash)
   (member-hash :initform (make-hash-table :test #'equal))))

(defgeneric install-rule (rule))
(defgeneric uninstall-rule (rule))

(defmethod solve ((rule rule))
  (loop for v being the hash-values in (get-hook-hash rule) do
        (funcall v rule)))

(defmacro with-rule ((rule) &body body)
  `(macrolet ((:get-other ()
                `(get-other ,',rule)))
     ,@body))

(defmethod get-member ((rule rule) key)
  (gethash key (slot-value rule 'member-hash)))

(defmethod (setf get-member) (val (rule rule) key)
  (setf (gethash key (slot-value rule 'member-hash)) val))

;;语法格式：
;;(make-rule ((monitor-or-rule1 rule1) (monitor-or-rule2 rule2) ...) default-rule)
;;rule1 rule2 ... 和 default-rule格式一样，第一个是参数列表，只有一个参数用于传当前的monitor-or-rule，其余为函数体
;;rule1 rule2 ...可以理解为特化的钩子，default-rule是通用规则
;;示例请看example/rule.lisp
(defmacro make-rule ((&rest monitor-or-rules) &body default-rule)
  (unless default-rule (error "必须有默认规则"))
  (let* ((tmp-sym (gensym "tmp"))
         (monitors (loop for mr in monitor-or-rules collect
                         (car mr)))
         (rules (loop for mr in monitor-or-rules collect
                      `(lambda ,@(aif (cdr mr) it default-rule)
                         (solve ,tmp-sym)))))
    `(let (,tmp-sym)
       (setf ,tmp-sym
             (make-instance 'rule
                            :monitors (list ,@monitors)
                            :rules    (list ,@rules))))))

(defmethod install-rule ((rule rule))
  (loop for m in (get-monitors rule)
        for r in (get-rules rule) do
        (setf (gethash rule (get-hook-hash m)) r)))

(defmethod uninstall-rule (rule)
  (loop for m in (get-monitors rule) do
        (remhash rule (get-hook-hash m))))
