(defpackage :do-varient
  (:use :cl :generic :parse-code :setf-varient :symbol-system)
  (:export #:do-complex #:define-dc-acc-expander #:define-dc-style-expander))

(in-package :do-varient)

(define-symbol-system dc-acc)
(define-symbol-system dc-style)
(define-symbol-system dc-wrapper)

(defmacro do-plist ((k v plist) &body body)
  (with-symbols (plist-sym )
    `(loop for ,plist-sym = ,plist
             then (cddr   ,plist-sym)
           for ,k = (car  ,plist-sym)
           for ,v = (cadr ,plist-sym)
           while ,plist-sym
           do (progn ,@body))))

(defmacro do-complex% (wrapper (&rest accumulations) (&rest styles) &body body &environment env)
  (let ((codes nil)) 
    (dolist (acc accumulations)
      (do-plist (k v (get-dc-acc-expansion acc env))
        (append-setf (getf codes k) v)))
    (dolist (sty styles)
      (do-plist (k v (get-dc-style-expansion sty env))
        (append-setf (getf codes k) v)))
    (get-dc-wrapper-expansion (list wrapper codes body))))

(defmacro do-complex ((&rest accumulations) (&rest styles) &body body)
  `(do-complex% :standard-wrapper ,accumulations ,styles ,@body))

(define-dc-wrapper-expander :standard-wrapper (plist body)
  (with-symbols (loop first-p)
    (with-plist-let ((bindings       :bind)    (beg-codes     :begin)
                     (end-codes      :end)     (conditions    :judge)
                     (next-codes     :next)    (result-codes  :result)
                     (macro-bindings :macro)   (initial-codes :init)
                     (wrappers       :wrapper) (optimize      :optimize))
                    plist
      (with-parse-body ((start :start) (do :do) (finally :finally)) body
        `(macrolet ((:stage (&body body)
                      (with-parse-body ((first :first) (main :main)) body
                        `(if ,',first-p (progn ,@first) (progn ,@main)))))
           (macrolet ,macro-bindings
             (let (,@bindings
                   (,first-p t))
               ,@optimize
               (declare (ignorable ,first-p))
               ,@initial-codes
               ,@start
               (tagbody
                  ,loop
                  ,@beg-codes
                  (unless (or ,@conditions)
                    (with-wrappers ,wrappers
                      ,@do)
                    (setf ,first-p nil)
                    ,@next-codes
                    (go ,loop)))
               ,@end-codes
               ,@finally
               (values ,@result-codes))))))))

(define-dc-acc-expander :collect (&rest symbols)
  (let ((lists (loop repeat (length symbols) collect (gensym "list"))))
    `(:bind  ,lists
      :macro ,(loop for sym in symbols
                    for l   in lists
                    collect `(,sym (element) `(push ,element ,',l)))
      :result   ,(loop for l in lists
                    collect `(nreverse ,l))
      :optimize   ((declare (list ,@lists))))))

(define-dc-acc-expander :append (&rest symbols)
  (let ((lists (loop repeat (length symbols) collect (gensym "list"))))
    `(:bind  ,lists
      :macro ,(loop for sym in symbols
                    for l   in lists
                    collect `(,sym (list) `(append-setf ,',l ,list)))
      :result   ,lists
      :optimize   ((declare (list ,@lists))))))

(define-dc-acc-expander :format (&rest symbols)
  (let ((streams (loop repeat (length symbols) collect (gensym "stream"))))
      `(:bind  ,(loop for s in streams
                      collect `(,s (make-string-output-stream)))
        :macro ,(loop for sym in symbols
                      for s in streams
                      collect `(,sym (fmt-str &rest args)
                                     `(format ,',s ,fmt-str ,@args)))
        :result   ,(loop for s in streams
                      collect `(get-output-stream-string ,s))
        :optimize  ((declare (stream ,@streams))))))

(define-dc-style-expander :list (var list)
  (with-symbols (list-sym)
    `(:bind  ((,var nil)
              (,list-sym (the list ,list)))
      :begin   ((setf ,var (car ,list-sym)))
      :judge ((null ,list-sym))
      :next  ((setf ,list-sym (cdr ,list-sym)))
      :optimize ((declare (list ,list-sym))))))

(define-dc-style-expander :plist (k v plist)
  (with-symbols (plist-sym)
    `(:bind  ((,plist-sym (the list ,plist)) ,k ,v)
      :begin   ((setf ,k (car ,plist-sym))
              (setf ,v (cadr ,plist-sym)))
      :judge ((null ,plist-sym))
      :next ((setf ,plist-sym (cddr ,plist-sym)))
      :optimize ((declare (list ,plist-sym))))))

(define-dc-style-expander :times (i limit)
  (with-symbols (limit-sym)
    `(:bind  ((,limit-sym (the integer ,limit)) (,i 0))
      :judge ((= ,limit-sym ,i))
      :next  ((setf ,i (1+ ,i)))
      :optimize ((declare (integer ,limit-sym ,i))))))

(define-dc-style-expander :window (vars list)
  (with-symbols (list-sym tmp-sym)
    `(:bind    ((,list-sym (the list ,list)) (,tmp-sym nil) ,@vars)
      :begin   ((setf ,tmp-sym ,list-sym)
                ,@(loop for v in vars
                        for expr = `(setf ,v (car ,tmp-sym))
                          then     `(setf ,tmp-sym (cdr ,tmp-sym)
                                          ,v (car ,tmp-sym))
                        collect expr))
      :judge ((null ,tmp-sym))
      :next  ((setf ,list-sym (cdr ,list-sym)))
      :optimize ((declare (list ,list-sym ,tmp-sym))))))

(define-dc-style-expander :on (var-sym list)
  `(:bind  ((,var-sym (the list ,list)))
    :judge ((null ,var-sym))
    :next  ((setf ,var-sym (cdr ,var-sym)))
    :optimize ((declare (list ,var-sym)))))
