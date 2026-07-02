(defpackage :generic
  (:use :cl)
  (:export #:aif #:awhen #:acond #:aif2 #:awhen2 #:aunless2 #:it #:self #:last1
           #:singlep #:array-last #:or= #:or/= #:or-char= #:or-char/= #:or-eq
           #:forever #:ensure #:with-stream-format #:with-collect
           #:with-wrappers #:with-symbols #:with-opt-slots #:with-compare
           #:with-plist-let #:with-debug #:to-key-binding))

(in-package :generic)

(defmacro with-symbols ((&rest symbols) &body body)
  `(let ,(loop for sym in symbols collect
               `(,sym (gensym (string ',sym))))
     ,@body))

(defmacro aif (cond then &optional else)
  "Anaphoric if，用it保存cond返回值"
  `(let ((it ,cond))
     (if it ,then ,else)))

(defmacro awhen (cond &body then)
  "aif的when变体"
  `(aif ,cond (progn ,@then) nil))

(defmacro acond (&rest body)
  (destructuring-bind (first . rest) body
    (destructuring-bind (cond . codes) first
      `(aif ,cond
            (progn ,@codes)
            ,(if rest `(acond ,@rest) nil)))))

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

(defmacro ensure (var default &optional type)
  (with-symbols (var-sym)
    `(let ((,var-sym ,var))
       ,(if type
            `(if (typep ,var-sym ,type) ,var-sym ,default)
            `(if ,var-sym ,var-sym ,default)))))

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

(defmacro with-plist-let ((&rest bindings) plist &body body)
  (with-symbols (plist-sym)
    `(let ((,plist-sym ,plist))
       (let ,(loop for (v k) in bindings
                   collect `(,v (getf ,plist-sym ,k)))
         ,@body))))

(defmacro with-debug ((cond &optional print-codes) &body body)
  (with-symbols (cond-sym codes-sym result-sym)
    `(let ((,cond-sym ,cond))
       (if ,cond-sym
           (let ((,codes-sym ,(if print-codes print-codes `(quote (progn ,@body)))))
             (format *debug-io* "~%enter: ~s~%" ,codes-sym)
             (let ((,result-sym (progn ,@body)))
               (format *debug-io* "result: ~a~%"  ,result-sym)))))))

(defun to-key-binding (obj)
  (etypecase obj
    (symbol `(,obj ,(intern (symbol-name obj) :keyword) nil))
    (list
     (destructuring-bind (bind &optional default) obj
       (etypecase bind
         (symbol `(,bind ,(intern (symbol-name bind) :keyword) ,default))
         (list `(,(first bind) ,(second bind) ,default)))))))
