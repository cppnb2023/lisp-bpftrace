;;; 通用工具包，提供一些便捷的宏和函数
(defpackage :generic
  (:use :cl)
  (:export :aif :awhen :aunless :aif2 :awhen2 :aunless2 :it :read-more-into
           :read-into-slots :array-last :or= :and= :or/= :and/= :or-char=
           :or-char/= :and-char= :and-char/= :strcat :ensure-symbol :forever
           :do-stage :do-stage* :do-list-stage :do-times-stage :do-plist-stage
           :do-stage-format :do-stage-format* :do-list-stage-format
           :do-times-stage-format :do-plist-stage-format
			  :plist-into-hash))

(in-package :generic)

(defmacro aif (cond then else)
  `(let ((it ,cond))
     (if it ,then ,else)))

(defmacro awhen (cond &body then)
  `(aif ,cond (progn ,@then) nil))

(defmacro aunless (cond &body else)
  `(aif ,cond nil (progn ,@else)))

(defmacro aif2 (cond then else)
  (let ((win-sym (gensym "win")))
    `(multiple-value-bind (it ,win-sym) ,cond
       (if ,win-sym ,then ,else))))

(defmacro awhen2 (cond &body then)
  `(aif2 ,cond (progn ,@then) nil))

(defmacro aunless2 (cond &body else)
  `(aif2 ,cond nil (progn ,@else)))

(defun array-last (array)
  (aref array (1- (length array))))

(defun (setf array-last) (value array)
  (setf (aref array (1- (length array))) value))

(defmacro defmultiple-compare-macro (mname compare-func combine)
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
  (with-output-to-string (stream)
    (dolist (str strings)
      (write-string str stream))))

(defmacro forever (&body body)
  `(do () (nil)
     ,@body))

(defun ensure-symbol (var symbol)
  (if (symbolp var) var symbol))

;;;流程控制宏
(defmacro do-stage (binds cond-res &body body)
  (loop for code in body do
		  (unless (or-eq (car code) :main :first :end)
			 (error "不可解析keyword ~a" (car code))))
  (let ((firstp-sym (gensym "firstp"))
		  (first (cdr (find :first body :key #'car)))
		  (main  (cdr (find :main body :key #'car)))
		  (end   (cdr (find :end body :key #'car))))
	 `(progn
		 (do (,@(when first (list `(,firstp-sym t)))
				  ,@binds)
			  ,cond-res
			,(if first
				  `(if ,firstp-sym
						 (progn
							(setf ,firstp-sym nil)
							,@first)
						 (progn ,@main))
				  `(progn ,@main)))
		 ,@end)))

(defmacro do-stage* (binds cond-res &body body)
  (loop for code in body do
		  (unless (or-eq (car code) :main :first :end)
			 (error "不可解析keyword ~a" (car code))))
  (let ((firstp-sym (gensym "firstp"))
		  (first (cdr (find :first body :key #'car)))
		  (main  (cdr (find :main body :key #'car)))
		  (end   (cdr (find :end body :key #'car))))
	 `(progn
		 (do* (,@(when first (list `(,firstp-sym t)))
					,@binds)
				,cond-res
			,(if first
				  `(if ,firstp-sym
						 (progn
							(setf ,firstp-sym nil)
							,@first)
						 (progn ,@main))
				  `(progn ,@main)))
		 ,@end)))

(defmacro do-list-stage ((var list &optional result) &body body)
  (let ((list-sym (gensym "list")))
	 `(do-stage* ((,list-sym ,list (cdr ,list-sym))
					  (,var (car ,list-sym) (car ,list-sym)))
		 ((not ,list-sym) ,result)
		 ,@body)))

(defmacro do-times-stage ((var times &optional result) &body body)
  (let ((times-sym (gensym "times")))
	 `(do-stage ((,times-sym ,times)
					 (,var 0 (1+ ,var)))
		 ((= ,var ,times-sym) ,result)
		 ,@body)))

(defmacro do-plist-stage ((key val plist &optional result) &body body)
  (let ((plist-sym (gensym "plist")))
	 `(do-stage* ((,plist-sym ,plist (cddr ,plist-sym))
					  (,key (car ,plist-sym)  (car ,plist-sym))
					  (,val (cadr ,plist-sym) (cadr ,plist-sym)))
		 ((not ,plist-sym) ,result)
		 ,@body)))

(defmacro do-stage-format (binds cond &body body)
  (let ((sstream (gensym "stream")))
	 `(with-output-to-string (,sstream)
		 (macrolet ((:format (string &body args)
						  (append (list 'format ',sstream string) args)))
			(do-stage ,binds ,cond ,@body)))))

(defmacro do-stage-format* (binds cond &body body)
  (let ((sstream (gensym "stream")))
	 `(with-output-to-string (,sstream)
		 (macrolet ((:format (string &body args)
						  (append (list 'format ',sstream string) args)))
			(do-stage* ,binds ,cond ,@body)))))

(defmacro do-list-stage-format ((var list) &body body)
  (let ((list-sym (gensym "list")))
	 `(do-stage-format* ((,list-sym ,list (cdr ,list-sym))
								(,var (car ,list-sym) (car ,list-sym)))
		 ((not ,list-sym))
		 ,@body)))

(defmacro do-times-stage-format ((var times) &body body)
  (let ((times-sym (gensym "times")))
	 `(do-stage-format ((,times-sym ,times)
							  (,var 0 (1+ ,var)))
		 ((= ,var ,times-sym))
		 ,@body)))

(defmacro do-plist-stage-format ((key val plist &optional result) &body body)
  (let ((plist-sym (gensym "plist")))
	 `(do-stage-format* ((,plist-sym ,plist (cddr ,plist-sym))
					  (,key (car ,plist-sym)  (car ,plist-sym))
					  (,val (cadr ,plist-sym) (cadr ,plist-sym)))
		 ((not ,plist-sym) ,result)
		 ,@body)))

;;;流程控制宏 END

(defun plist-into-hash (hash plist)
  (when (= (mod (length plist) 2) 1)
	 (error "plist have odd elements"))
  (do-plist-stage (k v plist)
	 (:main (setf (gethash k hash) v))))

