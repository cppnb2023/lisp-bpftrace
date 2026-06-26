(defpackage :symbol-system
  (:use :cl :generic)
  (:export #:define-symbol-system #:symbol-system-function #:symbol-system-macro))

(in-package :symbol-system)

(define-condition redefine-warning (style-warning)
  ((system-name :initarg :system-name)
   (func-name :initarg :func-name)
   (older :initarg :older)
   (newer :initarg :newer))
  (:report (lambda (c s)
             (with-slots (system-name func-name older newer) c
               (if (eq older newer)
                   (format s "redefining ~s in ~a ~a"
                           func-name system-name newer)
                   (format s "redefining ~s in ~a ~a but previous defined in ~a"
                           func-name system-name newer older))))))

(defstruct function-node
  fn (debug nil) type)

(defstruct symbol-system
  name (hash (make-hash-table)))

(defun symbol-system-function (symbol-system name)
  (declare (symbol-system symbol-system))
  (with-slots (hash) symbol-system
    (aif (gethash name hash)
         (when (equal (function-node-type it) :function)
           (function-node-fn it)))))

(defun (setf symbol-system-function) (new-lambda symbol-system name)
  (declare (symbol-system symbol-system))
  (declare (function new-lambda))
  (with-slots (hash) symbol-system
    (awhen (gethash name hash)
      (warn 'redefine-warning
            :system-name (symbol-system-name symbol-system)
            :func-name   name
            :older (function-node-type it)
            :newer :function))
    (setf (gethash name hash)
          (make-function-node :fn new-lambda :type :function)))
  new-lambda)

(defun symbol-system-macro (symbol-system name)
  (declare (symbol-system symbol-system))
  (with-slots (hash) symbol-system
    (aif (gethash name hash)
         (when (equal (function-node-type it) :macro)
           (function-node-fn it)))))

(defun (setf symbol-system-macro) (new-lambda symbol-system name)
  (declare (symbol-system symbol-system))
  (declare (function new-lambda))
  (with-slots (hash) symbol-system
    (awhen (gethash name hash)
      (warn 'redefine-warning
            :system-name (symbol-system-name symbol-system)
            :func-name   name
            :older (function-node-type it)
            :newer :macro))
    (setf (gethash name hash)
          (make-function-node :fn new-lambda :type :macro)))
  new-lambda)

(defmacro with-symbol-system-handler ((system expand-name codes) &body body)
  `(handler-case (progn ,@body)
     (error (c)
       (error 'simple-error
              :format-control   "~a ~s failed~%~s~%~a"
              :format-arguments (list (symbol-system-name ,system)
                                      ,expand-name ,codes c)))))

(defun symbol-system-macroexpand-1 (symbol-system codes &optional env)
  (declare (symbol-system symbol-system))
  (with-symbol-system-handler (symbol-system 'symbol-system-macroexpand-1 codes)
    (aif (symbol-system-macro symbol-system (car codes))
         (values (funcall it codes env) t)
         (values codes nil))))

(defun symbol-system-macroexpand (symbol-system codes &optional env)
  (labels ((-> (codes env)
             (declare (optimize (speed 3)))
             (aif2 (symbol-system-macroexpand-1 symbol-system codes env)
                   (-> it env)
                   codes)))
    (aif2 (symbol-system-macroexpand-1 symbol-system codes env)
          (values (-> it env) t)
          (values codes nil))))

(defun symbol-system-eval (symbol-system codes &optional (transform-fn #'identity))
  (cond
    ((stringp codes) (funcall transform-fn codes))
    ((symbolp codes) (funcall transform-fn codes))
    ((integerp codes) (funcall transform-fn codes))
    ((listp codes)
     (destructuring-bind (name . args) codes
       (with-symbol-system-handler (symbol-system 'symbol-system-eval codes)
         (acond
          ((symbol-system-function symbol-system name)
           (apply it (mapcar #'(lambda (codes)
                                 (symbol-system-eval symbol-system codes transform-fn))
                             args)))
          ((symbol-system-macro symbol-system name)
           (symbol-system-eval symbol-system (funcall it codes nil) transform-fn))
          (t (error 'simple-error
                    :format-control "~a is not defined"
                    :format-arguments (list name)))))))))

(defun remove-environment (lambda-list)
  (labels ((-> (lst &optional result)
             (declare (optimize (speed 3)))
             (if lst
                 (if (equal '&environment (car lst))
                     (-> (cddr lst) result)
                     (-> (cdr lst)  (cons (car lst) result)))
                 result)))
    (nreverse (-> lambda-list))))

(defmacro lambda-env (lambda-list &body body)
  (with-symbols (args env)
    `(lambda (,args ,env)
       (declare (ignorable ,env))
       ,(aif (member '&environment lambda-list)
             `(destructuring-bind ,(remove-environment lambda-list) (cdr ,args)
                (let ((,(second it) ,env))
                  ,@body))
             `(destructuring-bind ,lambda-list (cdr ,args)
                ,@body)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun format-intern (fmt &rest args)
    (intern (string-upcase (apply #'format nil fmt args)))))

(defmacro define-symbol-system
    (name &key defun defmacro funcall apply eval macroexpand-1 macroexpand)
  (let* ((concat-name  (remove #\* (string name)))
         (defun-sym    (ensure defun    (format-intern "defun-~a" concat-name)))
         (defmacro-sym (ensure defmacro (format-intern "defmacro-~a" concat-name)))
         (funcall-sym  (ensure funcall  (format-intern "funcall-~a" concat-name)))
         (apply-sym    (ensure apply    (format-intern "apply-~a" concat-name)))
         (eval-sym     (ensure eval     (format-intern "eval-~a" concat-name)))
         (macroexpand-1-sym (ensure macroexpand-1
                                    (format-intern "macroexpand-1-~a" concat-name)))
         (macroexpand-sym   (ensure macroexpand
                                    (format-intern "macroexpand-~a" concat-name))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defparameter ,name (make-symbol-system :name ',name)))

       (defmacro ,defun-sym (name lambda-list &body body)
         `(eval-when (:load-toplevel :execute)
            (setf (symbol-system-function ,',name ',name)
                  (lambda ,lambda-list ,@body))))

       (defmacro ,defmacro-sym (name lambda-list &body body)
         `(eval-when (:load-toplevel :execute)
            (setf (symbol-system-macro ,',name ',name)
                  (lambda-env ,lambda-list ,@body))))

       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defun ,macroexpand-1-sym (form &optional env)
           (symbol-system-macroexpand-1 ,name form env))

         (defun ,macroexpand-sym (form &optional env)
           (symbol-system-macroexpand ,name form env))

         (defun ,funcall-sym (symbol &rest parameters)
           (with-symbol-system-handler
               (,name ',funcall-sym (list* ',funcall-sym symbol parameters))
             (aif (symbol-system-function ,name symbol)
                  (apply it parameters)
                  (error 'simple-error
                         :format-control "~s: ~s not defined"
                         :format-arguments (list ',funcall-sym symbol)))))

         (defun ,apply-sym (symbol &rest parameters)
           (with-symbol-system-handler
               (,name ',apply-sym (list* ',funcall-sym symbol parameters))
             (aif (symbol-system-function ,name symbol)
                  (apply it (reduce #'cons parameters :from-end t))
                  (error 'simple-error
                         :format-control "~s: ~s not defined"
                         :format-arguments (list ',apply-sym symbol)))))

         (defun ,eval-sym (expr &optional (transform-fn #'identity))
           (symbol-system-eval ,name expr transform-fn))))))
