(defpackage :generic
  (:use :cl)
  (:export #:aif #:awhen #:aif2 #:awhen2 #:aunless2 #:it #:self #:last1
           #:singlep #:array-last #:or= #:or/= #:or-char= #:or-char/= #:or-eq #:strcat
           #:forever #:ensure #:with-stream-format #:with-collect
           #:with-wrappers #:with-symbols #:make-slice #:range
           #:best-position #:with-opt-slots #:with-compare #:with-plist-let))

(in-package :generic)

(defmacro aif (cond then &optional else)
  "Anaphoric if，用it保存cond返回值"
  `(let ((it ,cond))
     (if it ,then ,else)))

(defmacro awhen (cond &body then)
  "aif的when变体"
  `(aif ,cond (progn ,@then) nil))

(defmacro aif2 (cond then &optional else)
  "Anaphoric if但可以进行多值判断，适用于hash"
  (let ((win-sym (gensym "win")))
    `(multiple-value-bind (it ,win-sym) ,cond
       (if ,win-sym ,then ,else))))

(defmacro awhen2 (cond &body then)
  "aif2的when变体"
  `(aif2 ,cond (progn ,@then) nil))

(defmacro aunless2 (cond &body else)
  `(aif2 ,cond nil (progn (declare (ignorable it)) ,@else)))

(defmacro self (expr)
  `(let ((self nil))
     (setf self ,expr)))

(defun array-last (array)
  "获取数组最后一个元素"
  (aref array (1- (length array))))

(defun (setf array-last) (value array)
  (setf (aref array (1- (length array))) value))

(defun last1 (list)
  "获取链表最后一个元素"
  (car (last list)))

(defun singlep (list)
  "判断链表是否只有一个元素"
  (and (consp list) (not (cdr list))))

(defmacro defmultiple-compare-macro (mname compare-func combine)
  "生成多种使用combine组合compare-func判断的宏"
  `(defmacro ,mname (var &body exprs)
     (let ((var-sym (gensym "var")))
       `(let ((,var-sym ,var))
          (,',combine ,@(loop for expr in exprs collect
                              `(funcall ,',compare-func ,var-sym ,expr)))))))

(defmultiple-compare-macro or=   #'=   or)
(defmultiple-compare-macro or/=  #'/=  or)

(defmultiple-compare-macro or-char=   #'char=   or)
(defmultiple-compare-macro or-char/=  #'char/=  or)

(defmultiple-compare-macro or-eq  #'eq or)

(defun strcat (&rest strings)
  "连接多个字符串"
  (with-output-to-string (stream)
    (dolist (str strings)
      (write-string str stream))))

(defmacro forever (&body body)
  "死循环"
  `(do () (nil)
     ,@body))

(defun ensure (type var default)
  (if (typep var type) var default))

(defmacro with-stream-format ((&optional (stream-sym (gensym "sstream"))) &body body)
  "使用:format将多个格式化字符串拼接返回"
  `(with-output-to-string (,stream-sym)
     (macrolet ((:format (string &body args)
                  (append (list 'format ',stream-sym string) args)))
       ,@body)))

(defmacro with-collect ((&optional (lst (gensym "lst"))) &body body)
  `(let ((,lst nil))
     (macrolet ((:collect (element)
                  (list 'push element ',lst)))
       ,@body
       (nreverse ,lst))))

(defmacro with-wrappers (wrappers &body body)
  (reduce #'(lambda (a b)
              (append a (list b)))
          wrappers
          :initial-value `(progn ,@body)
          :from-end t))

(defmacro with-symbols ((&rest symbols) &body body)
  `(let ,(loop for sym in symbols collect
               `(,sym (gensym (string ',sym))))
     ,@body))

(defun make-slice (array &optional start end)
  (setf start (if start start 0))
  (setf end   (if end end (length array)))
  (make-array (- end start)
              :displaced-to array
              :displaced-index-offset start))

(defmacro range (&key gt ge lt le)
  (when (and gt ge)
    (error "have :GT and :GE at once"))
  (when (and lt le)
    (error "hava :LT and :LE at once"))
  (let ((condition nil)
        (arg-sym (gensym "arg")))
    (when gt (push `(>  ,arg-sym ,gt) condition))
    (when ge (push `(>= ,arg-sym ,ge) condition))
    (when lt (push `(<  ,arg-sym ,lt) condition))
    (when le (push `(<= ,arg-sym ,le) condition))
    `(lambda (,arg-sym)
       (and ,@condition))))

(defun best-position (array test &key start end specify)
  (if specify
      (progn
        (loop with res = (car specify)
              for i in (cdr specify)
              do (unless (funcall test (aref array res) (aref array i))
                   (setf res i))
              finally (return res)))
      (progn
        (unless start (setf start 0))
        (unless end   (setf end   (length array)))
        (loop with res = start
              for i from (1+ start) below end
              do (unless (funcall test (aref array res) (aref array i))
                   (setf res i))
              finally (return res)))))

(defmacro with-opt-slots (opt-slots object &body body)
  (with-symbols (obj-sym)
    `(symbol-macrolet
         ,(loop for slot in opt-slots
                collect (cond
                          ((listp slot)
                           `(,(second slot)
                             (the ,(first slot) (slot-value ,obj-sym ',(second slot)))))
                          ((symbolp slot) `(,slot (slot-value ,obj-sym ',slot)))))
       (let ((,obj-sym ,object))
         ,@body))))

(defmacro with-compare ((&rest exprs) &body body)
  (let ((symbols (loop for nil in exprs collect (gensym))))
    `(let ,(mapcar #'list symbols exprs)
       (cond
         ,@(mapcar #'(lambda (code)
                       (destructuring-bind (first &rest rest) code
                         (if (eq first t)
                             `(t ,@rest)
                             `((funcall ,first ,@symbols) ,@rest))))
                   body)))))

(defun single-level-p (lst)
  (and (listp lst) (loop for code in lst always (atom code))))

(defun tree-reduce (tree-lst func)
  (labels ((tree-reduce% (lst)
             (if (single-level-p lst)
                 (funcall func lst)
                 (loop for code in lst
                       if (atom code)
                         collect code into result
                       else
                         collect (tree-reduce% code) into result
                       finally (return (funcall func result))))))
    (tree-reduce% tree-lst)))

(defmacro with-plist-let ((&rest bindings) plist &body body)
  (with-symbols (plist-sym)
    `(let ((,plist-sym ,plist))
       (let ,(loop for (v k) in bindings
                   collect `(,v (getf ,plist-sym ,k)))
         ,@body))))
