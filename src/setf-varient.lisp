(defpackage :setf-varient
  (:use :cl)
  (:export #:logicf #:mvpsetq #:mvpsetf #:append-setf #:append1-setf
           #:union-setf #:union1-setf #:delete-setf))

(in-package :setf-varient)

(defmacro logicf (op place num &environment env)
  (let* ((meth (multiple-value-list (get-setf-expansion place env)))
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

(define-setf-expander ensure (type place default &environment env)
  (let* ((type-sym (gensym))
         (default-sym (gensym))
         (place-val (gensym))
         (meth (multiple-value-list (get-setf-expansion place env)))
         (tmps (append (first meth)
                       (list type-sym default-sym place-val)))
         (vals (append (second meth)
                       (list type default (fifth meth)))))
    (values tmps vals (third meth)
            (fourth meth)
            `(if (typep ,place-val ,type-sym)
                 ,place-val ,default-sym))))

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

(defmacro define-complex-setf (name function)
  `(defmacro ,name (place obj &environment env)
     (let ((meth (multiple-value-list (get-setf-expansion place env))))
       `(let ,(mapcar #'list (first meth) (second meth))
          (let ((,(first (third meth)) (funcall ,',function ,(fifth meth) ,obj)))
            ,(fourth meth))))))

(define-complex-setf append-setf #'append)
(define-complex-setf union-setf #'union)
(define-complex-setf delete-setf #'delete)

(defmacro append1-setf (place element)
  `(append-setf ,place (list ,element)))

(defmacro union1-setf (place element)
  `(union-setf ,place (list ,element)))

