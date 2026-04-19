(defpackage :do-varient
  (:use :cl :generic)
  (:export :do-stage :do-stage* :do-list-stage :do-times-stage :do-plist-stage
           :do-stage-format :do-stage-format* :do-list-stage-format
           :do-times-stage-format :do-plist-stage-format :do-tuple-stage
           :do-tuple-stage-format))

(in-package :do-varient)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun check-stages (body)
    "检查是否有非法阶段，用于含stage的循环流"
    (dolist (code body)
      (unless (or-eq (car code) :main :first :end)
        (error "不可解析keyword ~a" (car code)))))

  (defun parse-stages (body)
    "解析提取每个阶段代码，用于含stage的循环流"
    (let ((first (cdr (find :first body :key #'car)))
          (main  (cdr (find :main body :key #'car)))
          (end   (cdr (find :end body :key #'car))))
      (values first main end)))

  (defun make-stage-code (first main firstp-sym)
    "生成阶段代码"
    (if first
        `(if ,firstp-sym
             (progn
               ,@first
               (setf ,firstp-sym nil))
             (progn
               ,@main))
        `(progn ,@main))))

(defmacro do-stage (binds cond-res &body body)
  "和do一样但可以分first, main, end阶段, 具体操作example/do-varient.lisp"
  (check-stages body)
  (let ((firstp-sym (gensym "firstp")))
    (multiple-value-bind (first main end) (parse-stages body)
      `(progn
         (do (,@(when first (list `(,firstp-sym t)))
                ,@binds)
             ,cond-res
           ,(make-stage-code first main firstp-sym))
         ,@end))))

(defmacro do-stage* (binds cond-res &body body)
  "和do*一样但可以分first, main, end阶段, 具体操作example/do-varient.lisp"
  (check-stages body)
  (let ((firstp-sym (gensym "firstp")))
    (multiple-value-bind (first main end) (parse-stages body)
      `(progn
         (do* (,@(when first (list `(,firstp-sym t)))
                 ,@binds)
              ,cond-res
           ,(make-stage-code first main firstp-sym))
         ,@end))))

(defmacro do-list-stage ((var list &optional result) &body body)
  "分阶段的dolist, 具体操作example/do-varient.lisp"
  (let ((list-sym (gensym "list")))
    `(do-stage* ((,list-sym ,list (cdr ,list-sym))
                 (,var (car ,list-sym) (car ,list-sym)))
       ((not ,list-sym) ,result)
       ,@body)))

(defmacro do-mapcar ((element list) &body body)
  "类似(loop for element in list collect ...)"
  (let ((res-sym  (gensym "res")))
    `(let ((,res-sym nil))
       (dolist (,element ,list)
         (push (progn ,@body) ,res-sym))
       (nreverse ,res-sym))))

(defmacro do-times-stage ((var times &optional result) &body body)
  "分阶段的dotimes, 具体操作example/do-varient.lisp"
  (let ((times-sym (gensym "times")))
    `(do-stage ((,times-sym ,times)
                (,var 0 (1+ ,var)))
       ((= ,var ,times-sym) ,result)
       ,@body)))

(defmacro do-plist-stage ((key val plist &optional result) &body body)
  "分阶段式遍历plist, 具体操作example/do-varient.lisp"
  (let ((plist-sym (gensym "plist")))
    `(do-stage* ((,plist-sym ,plist (cddr ,plist-sym))
                 (,key (car ,plist-sym)  (car ,plist-sym))
                 (,val (cadr ,plist-sym) (cadr ,plist-sym)))
       ((not ,plist-sym) ,result)
       ,@body)))

(defmacro do-tuple-stage ((elements list &optional result) &body body)
  "分阶段, 滑动窗口式遍历list, 具体操作example/do-varient.lisp"
  (check-stages body)
  (let ((list-sym (gensym "list"))
        (tmp-sym  (gensym "tmp"))
        (firstp-sym (gensym "firstp"))
        (loop-sym (gensym "LOOP")))
    (multiple-value-bind (first main end) (parse-stages body)
      `(let ((,list-sym ,list)
             (,tmp-sym nil)
             ,@(do-mapcar (ele elements) `(,ele nil))
             (,firstp-sym t))
         (when (nthcdr ,(1- (length elements)) ,list-sym)
           (tagbody
              ,loop-sym
              (setf ,tmp-sym ,list-sym)
              ,@(do-mapcar (ele elements)
                  `(setf ,ele     (car ,tmp-sym)
                         ,tmp-sym (cdr ,tmp-sym)))
              ,(make-stage-code first main firstp-sym)
              (setf ,list-sym (cdr ,list-sym))
              (when ,tmp-sym
                (go ,loop-sym))
              ,@end))
         ,result))))

(defmacro do-stage-format (binds cond &body body)
  "分阶段式构造字符串, 语法像do, 具体操作example/do-varient.lisp"
  (let ((sstream (gensym "stream")))
    `(with-stream-format (,sstream)
       (do-stage ,binds ,cond ,@body))))

(defmacro do-stage-format* (binds cond &body body)
  "分阶段式构造字符串, 语法像do*, 具体操作example/do-varient.lisp"
  (let ((sstream (gensym "stream")))
    `(with-stream-format (,sstream)
       (do-stage* ,binds ,cond ,@body))))

(defmacro do-list-stage-format ((var list) &body body)
  "分阶段遍历list并且构造字符串, 具体操作example/do-varient.lisp"
  (let ((list-sym (gensym "list")))
    `(do-stage-format* ((,list-sym ,list (cdr ,list-sym))
                        (,var (car ,list-sym) (car ,list-sym)))
       ((not ,list-sym))
       ,@body)))

(defmacro do-times-stage-format ((var times) &body body)
  "分阶段dotimes并且构造字符串, 具体操作example/do-varient.lisp"
  (let ((times-sym (gensym "times")))
    `(do-stage-format ((,times-sym ,times)
                       (,var 0 (1+ ,var)))
       ((= ,var ,times-sym))
       ,@body)))

(defmacro do-plist-stage-format ((key val plist &optional result) &body body)
  "像do-plist-stage, 但可以构造字符串, 具体操作example/do-varient.lisp"
  (let ((plist-sym (gensym "plist")))
    `(do-stage-format* ((,plist-sym ,plist (cddr ,plist-sym))
                        (,key (car ,plist-sym)  (car ,plist-sym))
                        (,val (cadr ,plist-sym) (cadr ,plist-sym)))
       ((not ,plist-sym) ,result)
       ,@body)))

(defmacro do-tuple-stage-format ((elements list) &body body)
  "像do-tuple-stage, 但可以构造字符串, 具体操作example/do-varient.lisp"
  (let ((sstream (gensym "stream")))
    `(with-stream-format (,sstream)
       (do-tuple-stage (,elements ,list)
         ,@body))))
