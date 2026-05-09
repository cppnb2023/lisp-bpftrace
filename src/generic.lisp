;;; 通用工具包，提供一些便捷的宏和函数
(defpackage :generic
  (:use :cl)
  (:export :aif :awhen :aunless :aif2 :awhen2 :aunless2 :it :last1
           :singlep :array-last :or= :or/= :or-char= :or-char/= :or-eq
           :strcat :ensure-symbol :forever :with-stream-format
           :ensure-integer :logior-setf :ensure-logior-setf
           :with-collect :with-wrappers :mvsetq :mvsetf :mvpsetf
           :make-accessor :accessor :with-most :with-symbols
           :get-most-accessor :valid-index :best-index))

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

(defun ensure-symbol (var symbol)
  "确保返回符号，var不是符号返回symbol"
  (if (symbolp var) var symbol))

(defun ensure-integer (var number)
  (if (integerp var) var number))

(defmacro logior-setf (var num)
  `(setf ,var (logior ,var ,num)))

(defmacro ensure-logior-setf (var num)
  `(setf ,var (logior (ensure-integer ,var 0) ,num)))

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

(defmacro mvsetq (vars values)
  (let ((bindings
         (loop for v in vars collect
               (gensym (string v)))))
    `(multiple-value-bind ,bindings ,values
       ,@(loop for v in vars
               for b in bindings collect
               `(setq ,v ,b)))))

(defmacro mvsetf (vars values)
  (let ((bindings
         (loop for v in vars collect
               (gensym (string v)))))
    `(multiple-value-bind ,bindings ,values
       ,@(loop for v in vars
               for b in bindings collect
               `(setf ,v ,b)))))

(defmacro mvpsetf (vars values)
  (let ((bindings
         (loop for v in vars collect
               (gensym (string v)))))
    `(multiple-value-bind ,bindings ,values
       (psetf ,@(loop for v in vars
                      for b in bindings append
                      `(,v ,b))))))

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
                            (multiple-value-list (get-setf-expansion p)))
                        places))
         (tmps  (mapcar #'(lambda (p) (gensym)) places))
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

(defun valid-index (length &rest idx-list)
  (loop for idx in idx-list
        when (< idx length)
        collect idx))

(defun best-index (array compare idx-list)
  (cond
    ((null idx-list) nil)
    ((singlep idx-list) (car idx-list))
    (t (destructuring-bind (first &rest rest) idx-list
         (loop with res = first
               for idx in rest
               do (unless (funcall compare (aref array res) (aref array idx))
                    (setf res idx))
               finally (return res))))))

