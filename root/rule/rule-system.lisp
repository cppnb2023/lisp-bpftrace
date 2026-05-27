(defpackage :rule-system
  (:use :cl :base-tools))

(in-package :rule-system)

(defclass rule-node ()
  ((outputs :initform (make-array 10 :initial-element nil))
   (result  :initform (make-hash-table) :accessor get-result)
   (hook :initform nil)))

(defmethod remove-output ((element rule-node) (rule-node rule-node))
  (with-slots (outputs) rule-node
    (map-into outputs #'(lambda (x) (delete element x)) outputs)))

(defmethod install-output ((element rule-node) priority (rule-node rule-node))
  (remove-output element rule-node)
  (with-slots (outputs) rule-node
    (unless priority (setf priority (length outputs)))
    (unless (aref outputs priority)
      (setf (aref outputs priority) (make-array 5 :fill-pointer 0 :adjustable t)))
    (vector-push-extend element outputs 10)))

(defmethod dispatch ((rule-node rule-node))
  (loop for array across outputs
        do (loop for out in array
                 do (with-slots (hook) out
                      (funcall hook rule-node)))))

(defun make-rule (inputs func &optional priority)
  (let ((tmp (make-instance 'rule-node)))
    (dolist (in inputs)
      (install-output tmp priority in))
    (with-slots (hook) tmp
      (setf hook func))))

(defmacro rule-style-expander (symbol)
  (get symbol 'rule-style-symbol-table))

(defmacro define-rule-style-expander (name parameters &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (rule-style-expander ',name)
           (lambda ,parameters ,@body))))

(defun get-rule-style-expansion (style &environment env)
  (destructuring-bind (name . args) style
    (aif (rule-style-expander name)
         (apply it args)
         (let ((result (macroexpand style env)))
           (if (equal style result)
               (error (format nil "rule style ~a not defined" name))
               (get-rule-style-expansion result env))))))

(defmacro make-rule-complex (&rest codes)
  (with-parse-body ((style :style) (body :do)) codes
    (with-collect-codes ((vars :vars) (vals :vals)
                         (preexec :preexec) (cond :cond)
                         (clear :clear) (inputs :inputs))
      ;;TODO
      `(make-rule ',inputs
                  (let ,(mapcar #'list vars vals)
                    (lambda (rule-arg0)
                      ,@preexec
                      (when (and ,@cond)
                        ,@body
                        ,@clear)))
                  ))))
