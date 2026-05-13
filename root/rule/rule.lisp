(defpackage :rule
  (:use :cl :base-tools :bpftrace-dsl :monitor-template)
  (:export :rule :install-rule :uninstall-rule :solve :make-rule
           :install-trigger :uninstall-trigger))

(in-package :rule)

(defclass rule ()
  ((triggers :initform (make-hash-table :test #'equal))
   (hook-hash :initform (make-hash-table) :reader get-hook-hash)
   (member-hash :initform (make-hash-table :test #'equal))))

(defgeneric install-rule (rule))
(defgeneric uninstall-rule (rule))

(defmethod solve ((rule rule))
  (loop for v being the hash-values in (get-hook-hash rule) do
        (funcall v rule)))

(defmethod get-member ((rule rule) key)
  (gethash key (slot-value rule 'member-hash)))

(defmethod (setf get-member) (val (rule rule) key)
  (setf (gethash key (slot-value rule 'member-hash)) val))

(defmethod install-trigger ((rule rule) new-trigger func)
  (with-slots (triggers) rule
    (setf (gethash new-trigger triggers) func)))

(defmethod uninstall-trigger ((rule rule) trigger)
  (with-slots (triggers) rule
    (remhash trigger triggers)))

;;语法格式：
;;(make-rule ((monitor-or-rule1 rule1) (monitor-or-rule2 rule2) ...) default-rule)
;;rule1 rule2 ... 和 default-rule格式一样，第一个是参数列表，只有一个参数用于传当前的monitor-or-rule，其余为函数体
;;rule1 rule2 ...可以理解为特化的钩子，default-rule是通用规则
;;示例请看example/rule.lisp
(defmacro make-rule ((&rest triggers) &body default-rule)
  (unless default-rule (error "必须有默认规则"))
  (let* ((tmp-sym (gensym "tmp")))
    `(let (,tmp-sym)
       (setf ,tmp-sym (make-instance 'rule))
       ,@(loop for (tr . rules) in triggers collect
               `(install-trigger ,tmp-sym ,tr (lambda ,@rules)))
       ,tmp-sym)))

(defmethod install-rule ((rule rule))
  (with-slots (triggers) rule
    (loop for tr being the hash-keys in triggers using (hash-value r)
          do (setf (gethash rule (get-hook-hash tr)) r))))

(defmethod uninstall-rule ((rule rule))
  (with-slots (triggers) rule
    (loop for tr being the hash-keys in triggers
          do (remhash rule (get-hook-hash tr)))))
