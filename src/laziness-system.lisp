(defpackage :laziness-system
  (:use :cl :base-tools))

;;(in-package :laziness-system)

(defstruct (laziness-system (:conc-name lsys-))
  (hash (make-hash-table)))

(defstruct cell
  data next prev update-fn)

(defun lazy-gethash (lsys key)
  (declare (laziness-system lsys))
  (gethash key (lsys-hash lsys)))

(defun (setf lazy-gethash) (new lsys key)
  (declare (laziness-system lsys))
  (declare (cell new))
  (setf (gethash key (lsys-hash lsys)) new))

(defun lazy-value (lsys key)
  (declare (laziness-system lsys))
  (awhen2 (lazy-gethash lsys key)
    (cell-data it)))

(defun (setf lazy-value) (new lsys key)
  (declare (laziness-system lsys))
  (aif2 (lazy-gethash lsys key)
        (progn
          (setf (cell-data it) new)
          (loop for n in (cell-next it)
                do (setf (lazy-value it)
                         (funcall (cell-update-fn it) lsys))))
        (setf (lazy-gethash lsys key)
              (make-cell :data new))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-key-binding (obj)
    (etypecase obj
      (list obj)
      (symbol `(,obj ,(intern (symbol-name obj) :keyword))))))

(defmacro with-lazy-let ((&rest bindings) laziness-system &body body)
  (with-symbols (lsys-sym)
    `(let ((,lsys-sym ,laziness-system))
       (let ,(loop for (v k) in (mapcar #'to-key-binding bindings)
                   collect `(,v (lazy-value ,k)))
         ,@body))))

(defmacro lazy-lambda (parameters &body body)
  (with-symbols (lsys)
    `(lambda (,lsys)
       (with-lazy-let ,parameters ,lsys
         ,@body))))


