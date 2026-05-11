;;; 通用工具包，提供一些便捷的宏和函数
(defpackage :generic
  (:use :cl)
  (:export :aif :awhen :aunless :aif2 :awhen2 :aunless2 :it :last1
           :singlep :array-last :or= :or/= :or-char= :or-char/= :or-eq
           :strcat :forever :ensure :with-ensure :logicf :with-stream-format
           :with-collect :with-wrappers :mvpsetq :mvpsetf :make-accessor
           :accessor :with-most :with-symbols :get-most-accessor :make-slice
           :range :best-position :with-opt-slots))

(in-package :generic)

(defmacro aif (cond then else)
  "Anaphoric if，用it保存cond返回值"
  `(let ((it ,cond))
     (if it ,then ,else)))

(defmacro awhen (cond &body then)
  "aif的when变体"
  `(aif ,cond (progn ,@then) nil))

(defmacro aunless (cond &body else)
  "这个用处不大"
  `(aif ,cond nil (progn ,@else)))

(defmacro aif2 (cond then else)
  "Anaphoric if但可以进行多值判断，适用于hash"
  (let ((win-sym (gensym "win")))
    `(multiple-value-bind (it ,win-sym) ,cond
       (if ,win-sym ,then ,else))))

(defmacro awhen2 (cond &body then)
  "aif2的when变体"
  `(aif2 ,cond (progn ,@then) nil))

(defmacro aunless2 (cond &body else)
  `(aif2 ,cond nil (progn ,@else)))

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

(defmacro logicf (op place num)
  (let* ((meth (multiple-value-list (get-setf-expansion place)))
         (tmp  (third meth))
         (optimize-place (fifth meth))
         (op-func
          (ecase op
            (:ior #'logior) (:xor #'logxor)
            (:and #'logand)))
         (bindings (mapcar #'list
                           (append (first  meth) tmp)
                           (append (second meth)
                                   (list `(funcall ,op-func ,optimize-place ,num))))))
    `(let* ,bindings
       ,(fourth meth))))

(defun ensure (type var default)
  (if (typep var type) var default))

(defmacro with-ensure ((type place default) (reader writer) &body body)
  (let* ((meth (multiple-value-list (get-setf-expansion place)))
         (tmp  (first (third meth)))
         (bindings (mapcar #'list (first meth) (second meth)))
         (optimize-place (fifth meth))
         (read-sym  (gensym "read")))
    `(let (,@bindings
           ,read-sym)
       (flet ((,reader () ,read-sym)
              (,writer (,tmp)
                (setf ,read-sym ,(fourth meth))))
         (setf ,read-sym ,optimize-place)
         (unless (typep ,read-sym ',type)
           (,writer ,default))
         ,@body))))

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

(defmacro mvpsetq (vars values)
  (let ((bindings
         (loop for v in vars collect
               (gensym (string v)))))
    `(multiple-value-bind ,bindings ,values
       ,@(loop for v in vars
               for b in bindings collect
               `(setq ,v ,b)))))

(defmacro mvpsetf (vars values)
  (let ((bindings
         (loop for v in vars collect
               (gensym (string v)))))
    `(multiple-value-bind ,bindings ,values
       ,@(loop for v in vars
               for b in bindings collect
               `(setf ,v ,b)))))

(defmacro make-accessor (place &optional optimize initial-val)
  (let* ((meth (multiple-value-list (get-setf-expansion place)))
         (val-sym (gensym "val"))
         (opt-place (fifth meth)))
    `(let ,(mapcar #'list (first meth) (second meth))
       ,(if optimize
            `(let ((,val-sym ,initial-val))
               (lambda (&optional ensure write)
                 (declare (boolean ensure))
                 (if ensure
                     (setf ,opt-place write
                           ,val-sym write)
                     ,val-sym)))
            `(lambda (&optional ensure write)
               (declare (boolean ensure))
               (if ensure
                   (setf ,opt-place write)
                   ,opt-place))))))

(defun accessor (lambda-func)
  (funcall lambda-func nil nil))

(defun (setf accessor) (new-val lambda-func)
  (funcall lambda-func t new-val))

;;测试
(defmacro with-most ((op &rest places)
                           (most-val-sym most-expr-sym &optional (preffix 'arg))
                     &body body)
  (let* ((meths (mapcar #'(lambda (p)
                            (declare (ignorable p))
                            (multiple-value-list (get-setf-expansion p)))
                        places))
         (tmps  (mapcar #'(lambda (p) (declare (ignorable p)) (gensym)) places))
         (opt-places (mapcar #'fifth meths)))
    (labels ((make-parameter-acc (place)
               (loop for parameter in place
                     for i = 0 then (1+ i)
                     collect
                     `(,(intern (format nil "~s~s" preffix i))
                        ,(if (= i 0) `',parameter parameter))))
             (make-symbol-macro-bindings (place val)
               `(symbol-macrolet ((,most-val-sym ,val)
                                  (,most-expr-sym ,place)
                                  ,@(when (listp place)
                                      (make-parameter-acc place)))
                  ,@body))
             (most (place tmps)
               (destructuring-bind (first second &rest rest) tmps
                 (destructuring-bind (p-first p-second &rest p-rest) place
                   (if rest
                       `(if (funcall ,op ,first ,second)
                            ,(most (cons p-first p-rest)  (cons first  rest))
                            ,(most (cons p-second p-rest) (cons second rest)))
                       `(if (funcall ,op ,first ,second)
                            ,(make-symbol-macro-bindings p-first  first)
                            ,(make-symbol-macro-bindings p-second second)))))))
      `(let ,(mapcar #'list
                     (mapcan #'first  meths)
                     (mapcan #'second meths))
         (let ,(mapcar #'list tmps opt-places)
           ,(cond
              ((null places) `(progn ,@body))
              ((singlep places)
               (make-symbol-macro-bindings (first opt-places) (first tmps)))
              (t (most opt-places tmps))))))))

(defmacro with-symbols ((&rest symbols) &body body)
  `(let ,(loop for sym in symbols collect
               `(,sym (gensym (string ',sym))))
     ,@body))

(defmacro get-most-accessor (op &body places)
  (with-symbols (val-sym most-sym)
    `(with-most (,op ,@places) (,val-sym ,most-sym)
       (make-accessor ,most-sym t ,val-sym))))

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
         ,(mapcar #'(lambda (slot)
                      (cond
                        ((listp slot)
                         `(,(second slot)
                            (the ,(first slot)
                                 (slot-value ,obj-sym ',(second slot)))))
                        ((symbolp slot)
                         `(,slot
                            (slot-value ,obj-sym ',slot)))))
                  opt-slots)
       (let ((,obj-sym ,object))
         ,@body))))
