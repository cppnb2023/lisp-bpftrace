(defpackage :do-varient
  (:use :cl :generic :parse-code :setf-varient :symbol-system)
  (:export #:do-complex #:defun-dc-acc #:defun-dc-style #:defun-dc-wrapper))

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

(defmacro do-complex% (wrapper (&rest accumulations) (&rest styles) &body body)
  (let ((codes nil)) 
    (dolist (acc accumulations)
      (do-plist (k v (apply-dc-acc (car acc) (cdr acc)))
        (append-setf (getf codes k) v)))
    (dolist (sty styles)
      (do-plist (k v (apply-dc-style (car sty) (cdr sty)))
        (append-setf (getf codes k) v)))
    (funcall-dc-wrapper wrapper codes body)))

(defmacro do-complex ((&rest accumulations) (&rest styles) &body body)
  `(do-complex% :standard-wrapper ,accumulations ,styles ,@body))

(defun-dc-wrapper :standard-wrapper (plist body)
  (with-symbols (loop first-p)
    (with-plist-let ((bindings       :bind)    (beg-codes     :begin)
                     (end-codes      :end)     (conditions    :judge)
                     (next-codes     :next)    (result-codes  :result)
                     (macro-bindings :macro)   (initial-codes :init)
                     (wrappers       :wrapper) (optimize      :optimize)
                     (predo         :predo))
                    plist
      (with-parse-body ((start :start) (do :do) (finally :finally)) body
        `(macrolet ((:stage (&body body)
                      (with-parse-body ((first :first) (main :main)) body
                        `(if ,',first-p (progn ,@first) (progn ,@main)))))
           (macrolet ,macro-bindings
             (let* (,@bindings
                    (,first-p t))
               ,@optimize
               (declare (ignorable ,first-p))
               ,@initial-codes
               ,@start
               (tagbody
                  ,loop
                  ,@beg-codes
                  (unless (or ,@conditions)
                    ,@predo
                    (with-wrappers ,wrappers
                      ,@do)
                    (setf ,first-p nil)
                    ,@next-codes
                    (go ,loop)))
               ,@end-codes
               ,@finally
               (values ,@result-codes))))))))

(defun-dc-acc :collect (&rest symbols)
  (let ((lists (loop repeat (length symbols) collect (gensym "list"))))
    `(:bind  ,lists
      :macro ,(loop for sym in symbols
                    for l   in lists
                    collect `(,sym (&rest element)
                                   `(progn
                                      ,@(loop for e in element
                                              collect `(push ,e ,',l)))))
      :result   ,(loop for l in lists
                       collect `(nreverse ,l))
      :optimize   ((declare (list ,@lists))))))

(defun-dc-acc :append (&rest symbols)
  (let ((lists (loop repeat (length symbols) collect (gensym "list"))))
    `(:bind  ,lists
      :macro ,(loop for sym in symbols
                    for l   in lists
                    collect `(,sym (&rest lists)
                                   `(progn
                                      ,@(loop for list in lists
                                              collect `(append-setf ,',l ,list)))))
      :result   ,lists
      :optimize   ((declare (list ,@lists))))))

(defun-dc-acc :format (&rest symbols)
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

(defun-dc-style :list (var list)
  (if (listp var)
      (with-symbols (list-sym tmp)
        `(:bind  ((,list-sym (the list ,list))
                  (,tmp nil))
          :begin ((setf ,tmp (the list (car ,list-sym))))
          :judge ((null ,list-sym))
          :next  ((setf ,list-sym (cdr ,list-sym)))
          :optimize ((declare (list ,list-sym ,tmp)))
          :wrapper ((destructuring-bind ,var ,tmp))))
      (with-symbols (list-sym)
        `(:bind  ((,var nil)
                  (,list-sym (the list ,list)))
          :begin   ((setf ,var (car ,list-sym)))
          :judge ((null ,list-sym))
          :next  ((setf ,list-sym (cdr ,list-sym)))
          :optimize ((declare (list ,list-sym)))))))

(defun-dc-style :plist (k v plist)
  (with-symbols (plist-sym)
    `(:bind  ((,plist-sym (the list ,plist)) ,k ,v)
      :begin   ((setf ,k (car ,plist-sym))
              (setf ,v (cadr ,plist-sym)))
      :judge ((null ,plist-sym))
      :next ((setf ,plist-sym (cddr ,plist-sym)))
      :optimize ((declare (list ,plist-sym))))))

(defun-dc-style :times (i limit)
  (with-symbols (limit-sym)
    `(:bind  ((,limit-sym (the integer ,limit)) (,i 0))
      :judge ((= ,limit-sym ,i))
      :next  ((setf ,i (1+ ,i)))
      :optimize ((declare (integer ,limit-sym ,i))))))

(defun-dc-style :window (vars list)
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

(defun-dc-style :on (var-sym list)
  `(:bind  ((,var-sym (the list ,list)))
    :judge ((null ,var-sym))
    :next  ((setf ,var-sym (cdr ,var-sym)))
    :optimize ((declare (list ,var-sym)))))

(defun-dc-style :across (var-sym array)
  (with-symbols (iter-sym array-sym limit-sym)
    `(:bind  ((,iter-sym 0) (,var-sym nil) (,array-sym (the array ,array))
              (,limit-sym (length ,array-sym)))
      :judge ((= ,limit-sym ,iter-sym))
      :predo ((setf ,var-sym (aref ,array-sym ,iter-sym)))
      :next  ((setf ,iter-sym (1+ ,iter-sym)))
      :optimize ((declare (array ,array-sym))
                 (declare (fixnum ,iter-sym))))))
