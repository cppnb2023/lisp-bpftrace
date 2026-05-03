(defpackage :do-varient
  (:use :cl :generic :parse-code)
  (:export :do-stage :do-stage* :do-list-stage :do-times-stage :do-plist-stage
           :do-stage-format :do-stage-format* :do-list-stage-format
           :do-times-stage-format :do-plist-stage-format :do-window-stage
           :do-window-stage-format :do-circular-stage :do-circular-stage-collect
           :do-complex))

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

(defmacro do-mapcar-stage ((element list) &body body)
  "类似(loop for element in list collect ...)"
  `(with-collect ()
     (do-list-stage (,element ,list)
       ,@body)))

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

(defmacro do-window-stage ((elements list &optional result) &body body)
  "分阶段, 滑动窗口式遍历list, 具体操作example/do-varient.lisp"
  (check-stages body)
  (let ((list-sym (gensym "list"))
        (tmp-sym  (gensym "tmp"))
        (firstp-sym (gensym "firstp"))
        (loop-sym (gensym "LOOP")))
    (multiple-value-bind (first main end) (parse-stages body)
      `(let ((,list-sym ,list)
             (,tmp-sym nil)
             ,@(do-mapcar-stage (ele elements)
                 (:main (:collect `(,ele nil))))
             (,firstp-sym t))
         (tagbody
            ,loop-sym
            (setf ,tmp-sym ,list-sym)
            ,@(do-mapcar-stage (ele elements)
                (:first
                 (:collect
                  `(setf ,ele (car ,tmp-sym))))
                (:main
                 (:collect
                  `(setf ,tmp-sym (cdr ,tmp-sym)
                         ,ele (car ,tmp-sym))))
                (:end
                 (:collect 
                 `(progn
                    (when ,tmp-sym
                      ,(make-stage-code first main firstp-sym)
                      (setf ,list-sym (cdr ,list-sym))
                      (go ,loop-sym))))))
            ,@end)
         ,result))))

(defmacro do-stage-format (binds cond &body body)
  "分阶段式构造字符串, 语法像do, 具体操作example/do-varient.lisp"
  `(with-stream-format ()
     (do-stage ,binds ,cond ,@body)))

(defmacro do-stage-format* (binds cond &body body)
  "分阶段式构造字符串, 语法像do*, 具体操作example/do-varient.lisp"
  `(with-stream-format ()
     (do-stage* ,binds ,cond ,@body)))

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

(defmacro do-window-stage-format ((elements list) &body body)
  "像do-window-stage, 但可以构造字符串, 具体操作example/do-varient.lisp"
  `(with-stream-format ()
     (do-window-stage (,elements ,list)
       ,@body)))

(defmacro do-circular-stage ((iter beg end limit) &body body)
  "环形遍历"
  (let ((end-sym (gensym  "end"))
        (limit-sym (gensym  "capacity")))
    `(do-stage ((,iter ,beg (mod (1+ ,iter) ,limit-sym))
                (,end-sym ,end)
                (,limit-sym ,limit))
       ((= ,iter ,end-sym))
       ,@body)))

(defmacro do-circular-stage-collect ((iter beg end limit) &body body)
  `(with-collect ()
     (do-circular-stage (,iter ,beg ,end ,limit)
       ,@body)))

;;测试阶段 1.1
(defmacro do-complex ((&rest accumulation) (&rest styles) &body body)
  (check-stages body)
  (let ((loop-sym (gensym "loop"))
        (firstp-sym (gensym "firstp")))
    (macrolet ((with-parse ((destruct-lst lst gensyms) &body body)
                 `(destructuring-bind ,destruct-lst ,lst
                    (let ,(loop for sym in gensyms collect
                                `(,sym (gensym ,(string sym))))
                      ,@body))))
      (with-collect-codes ((:bind  bindings)     (:beg   beg-codes)
                           (:judge conditions)   (:next  next-codes)
                           (:wrap  wrappers)     (:macro macro-bindings)
                           (:res   result-codes) (:init  initial-codes))
        (dolist (style styles)
          (destructuring-bind (style-name &rest parameters) style
            (ecase style-name
              (:times
               (with-parse ((iter limit) parameters (limit-sym))
                 (:bind  `(,limit-sym ,limit) `(,iter 0))
                 (:judge `(= ,iter ,limit-sym))
                 (:next  `(setf ,iter (1+ ,iter)))))
              (:list
               (with-parse ((iter list) parameters (list-sym))
                 (:bind  `(,list-sym ,list))
                 (:next  `(setf ,list-sym (cdr ,list-sym)))
                 (if (listp iter)
                     (progn
                       (:wrap  `(destructuring-bind ,iter (car ,list-sym)))
                       (:judge `(not ,list-sym)))
                     (progn 
                       (:bind iter)
                       (:beg   `(setf ,iter (car ,list-sym)))
                       (:judge `(not ,list-sym))))))
              (:on
               (with-parse ((iter list) parameters ())
                 (:bind  `(,iter ,list))
                 (:next  `(setf ,iter (cdr ,iter)))
                 (:judge `(not ,iter))))
              (:plist
               (with-parse ((key val plist) parameters (plist-sym))
                 (:bind  `(,plist-sym ,plist) key val)
                 (:beg   `(setf ,key (car  ,plist-sym))
                         `(setf ,val (cadr ,plist-sym)))
                 (:judge `(not (cdr ,plist-sym)))
                 (:next  `(setf ,plist-sym (cddr ,plist-sym)))))
              (:window
               (with-parse ((elements list) parameters (list-sym tmp-sym))
                 (:bind `(,list-sym ,list) tmp-sym)
                 (:beg  `(setf ,tmp-sym ,list-sym))
                 (do-list-stage (e elements)
                   (:first (:bind e) (:beg `(setf ,e (car ,tmp-sym))))
                   (:main  (:bind e) (:beg `(setf ,tmp-sym (cdr ,tmp-sym)
                                                  ,e (car ,tmp-sym)))))
                 (:judge `(not ,tmp-sym))
                 (:next  `(setf ,list-sym (cdr ,list-sym)))))
              (:tuple
               (with-parse ((elements list) parameters (list-sym))
                 (:bind `(,list-sym ,list))
                 (do-list-stage (e elements)
                   (:first (:bind e) (:beg `(setf ,e (car ,list-sym))))
                   (:main  (:bind e) (:beg `(setf ,list-sym (cdr ,list-sym)
                                                  ,e (car ,list-sym))))
                   (:end   (:judge `(not ,list-sym))))
                 (:next `(setf ,list-sym (cdr ,list-sym)))))
              (:circular
               (with-parse ((iter beg end limit) parameters (end-sym limit-sym))
                 (:bind  `(,end-sym ,end) `(,limit-sym ,limit) `(,iter ,beg))
                 (:next  `(setf ,iter (mod (1+ ,iter) ,limit-sym)))
                 (:judge `(= ,iter ,end-sym))))
              (:do*
               (destructuring-bind (parameters &optional cond) parameters
                 (dolist (p parameters)
                   (with-parse ((var &optional init step) p ())
                     (:bind `(,var ,init))
                     (when step
                       (:next `(setf ,var ,step)))))
                 (with-parse ((&rest cond) cond ())
                   (when cond
                     (:judge cond)))))
              (:mvdo*
               (destructuring-bind (parameters &optional cond) parameters
                 (dolist (p parameters)
                   (with-parse ((vars &optional init step) p ())
                     (dolist (v vars)
                       (:bind v))
                     (:init `(mvsetq ,vars ,init))
                     (when step
                       (:next `(mvsetq ,vars ,step)))))
                 (with-parse ((&rest cond) cond ())
                   (when cond
                     (:judge cond))))))))
        (dolist (acc accumulation)
          (destructuring-bind (type &rest parameters) acc
            (ecase type
              (:collect
               (with-parse ((mname) parameters (collect-sym))
                 (:bind  collect-sym)
                 (:macro `(,mname (e) `(push ,e ,',collect-sym)))
                 (:res   `(nreverse ,collect-sym))))
              (:format
               (with-parse ((mname) parameters (stream))
                 (:bind  `(,stream (make-string-output-stream)))
                 (:macro `(,mname (str &body parameters)
                                  `(format ,',stream ,str ,@parameters)))
                 (:res   `(get-output-stream-string ,stream))))
              (:append
               (with-parse ((mname) parameters (append-sym))
                 (:bind  append-sym)
                 (:macro `(,mname (list)
                                  `(setf ,',append-sym (append ,',append-sym ,list))))
                 (:res  append-sym))))))
        (multiple-value-bind (first main end) (parse-stages body)
          (when first (:bind `(,firstp-sym t)))
          `(let* ,bindings
             ,@initial-codes
             (macrolet ,macro-bindings
               (block nil
                 (tagbody
                    ,loop-sym
                    ,@beg-codes
                    (unless (or ,@conditions)
                      (with-wrappers ,wrappers
                        ,(make-stage-code first main firstp-sym))
                      ,@next-codes
                      (go ,loop-sym))
                    ,@end
                    (return (values ,@result-codes)))))))))))
