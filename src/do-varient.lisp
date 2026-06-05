(defpackage :do-varient
  (:use :cl :generic :parse-code :setf-varient :symbol-system)
  (:export :do-complex))

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

(defmacro do-mapcar-stage ((element list) &body body)
  "类似(loop for element in list collect ...)"
  `(with-collect ()
     (do-list-stage (,element ,list)
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

(define-symbol-system dc-acc)
(define-symbol-system dc-style)
(define-symbol-system dc-wrapper)

(defmacro do-complex% (wrapper (&rest accumulations) (&rest styles) &body body &environment env)
  (let ((codes nil)) 
    (dolist (acc accumulations)
      (do-plist-stage (k v (get-dc-acc-expansion acc env))
        (:main (append-setf (getf codes k) v))))
    (dolist (sty styles)
      (do-plist-stage (k v (get-dc-style-expansion sty env))
        (:main (append-setf (getf codes k) v))))
    (get-dc-wrapper-expansion (list wrapper codes body))))

(defmacro do-complex ((&rest accumulations) (&rest styles) &body body)
  `(do-complex% :standard-wrapper ,accumulations ,styles ,@body))

(define-dc-wrapper-expander :standard-wrapper ((plist body))
  (with-symbols (loop first-p)
    (with-plist-let ((bindings      :bind)  (beg-codes      :beg)
                     (conditions    :judge) (next-codes     :next)
                     (result-codes  :res)   (macro-bindings :macro)
                     (initial-codes :init))
                    plist
      (with-parse-body ((do :do) (finally :finally)) body
        `(macrolet ((:stage (&body body)
                      (with-parse-body ((first :first) (main :main)) body
                        `(if ,',first-p (progn ,@first) (progn ,@main)))))
           (macrolet ,macro-bindings
             (let (,@bindings
                   (,first-p t))
               (declare (ignorable ,first-p))
               ,@initial-codes
               (tagbody
                  ,loop
                  ,@beg-codes
                  (unless (or ,@conditions)
                    ,@do
                    (setf ,first-p nil)
                    ,@next-codes
                    (go ,loop)))
               ,@finally
               (values ,@result-codes))))))))

(define-dc-acc-expander :collect ((&rest symbols))
  (let ((lists (loop repeat (length symbols) collect (gensym "list"))))
    (with-plist-builder ((bindings :bind)   (macro-bindings :macro)
                         (result-codes :res))
      (setf bindings lists)
      (setf macro-bindings
            (loop for sym in symbols
                  for l   in lists
                  collect `(,sym (element) `(push ,element ,',l))))
      (setf result-codes
            (loop for l in lists
                  collect `(nreverse ,l))))))

(define-dc-acc-expander :append ((&rest symbols))
  (let ((lists (loop repeat (length symbols) collect (gensym "list"))))
    (with-plist-builder ((bindings :bind) (macro-bindings :macro)
                         (result-codes :res))
      (setf bindings lists)
      (setf macro-bindings
            (loop for sym in symbols
                  for l   in lists
                  collect `(,sym (list) `(append-setf ,',l ,list))))
      (setf result-codes lists))))

(define-dc-acc-expander :format ((&rest symbols))
  (let ((streams (loop repeat (length symbols) collect (gensym "stream"))))
    (with-plist-builder ((bindings :bind) (macro-bindings :macro)
                         (result-codes :res))
      (setf bindings (loop for s in streams
                           collect `(,s (make-string-output-stream))))
      (setf macro-bindings
            (loop for sym in symbols
                  for s in streams
                  collect `(,sym (fmt-str &rest args)
                                 `(format ,',s ,fmt-str ,@args))))
      (setf result-codes (loop for s in streams
                               collect `(get-output-stream-string ,s))))))

(define-dc-style-expander :list ((var list &key (by #'cdr)))
  (with-symbols (list-sym)
    `(:bind  ((,var nil)
              (,list-sym ,list))
      :beg   ((setf ,var (car ,list-sym)))
      :judge ((null ,list-sym))
      :next  ((setf ,list-sym (funcall ,by ,list-sym))))))

(define-dc-style-expander :plist ((k v plist))
  (with-symbols (plist-sym)
    `(:bind  ((,plist-sym ,plist) ,k ,v)
      :beg   ((setf ,k (car ,plist-sym))
              (setf ,v (cadr ,plist-sym)))
      :judge ((null ,plist-sym))
      :next ((setf ,plist-sym (cddr ,plist-sym))))))

(define-dc-style-expander :times ((i limit))
  (with-symbols (limit-sym)
    `(:bind  ((,limit-sym ,limit)
              (,i 0))
      :judge ((= ,limit-sym ,i))
      :next  ((setf ,i (1+ ,i))))))

