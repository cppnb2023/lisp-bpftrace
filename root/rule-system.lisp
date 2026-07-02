(defpackage :rule-system
  (:use :cl :base-tools)
  (:export :present-event :rule-call :define-rule))

(in-package :rule-system)

(defparameter *pid-world* (make-hash-table))
(defparameter *change-pid* nil)

(defstruct rule-node
  prev fn next)

(defparameter *rule-hash* (make-hash-table))
(defparameter *rule-top* nil)

(defun dfs-call (rule-node pid)
  (with-slots (fn next) rule-node
    (when (funcall fn (gethash pid *pid-world*))
      (loop for k in next
            do (awhen2 (gethash k *rule-hash*)
                 (dfs-call it pid))))))

(defun present-event (pid plist)
  (flet ((-> (plist hash)
           (do-complex () ((:plist k v plist))
             (:do (setf (gethash k hash) v)))))
    (aif (gethash pid *pid-world*)
         (-> plist it)
         (setf (gethash pid *pid-world*)
               (let ((hash (make-hash-table)))
                 (-> plist hash)
                 hash)))
    (union1-setf *change-pid* pid)))

(defun rule-call ()
  (loop for pid in *change-pid*
        do (loop for top in *rule-top*
                 do (dfs-call (gethash top *rule-hash*) pid)))
  (setf *change-pid* nil))

(defun define-rule% (name parenets lambda)
  (aif2 (gethash name *rule-hash*)
        (progn
          (loop for p in (rule-node-prev it)
                do (delete-setf (rule-node-next (gethash p *rule-hash*)) name))
          (loop for p in parenets
                do (union1-setf (rule-node-next (gethash p *rule-hash*)) name))
          (setf (rule-node-fn (gethash name *rule-hash*)) lambda)
          (setf (rule-node-prev (gethash name *rule-hash*)) parenets)
          (when parenets
            (delete-setf *rule-top* name)))
        (progn
          (loop for p in parenets
                do (union1-setf (rule-node-next (gethash p *rule-hash*)) name))
          (setf (gethash name *rule-hash*)
                (make-rule-node :prev parenets :fn lambda))))
  (unless parenets
    (union1-setf *rule-top* name)))

(defmacro define-rule (name parameters lambda)
  `(progn
     (define-rule% ',name ',parameters ,lambda)
     ',name))

