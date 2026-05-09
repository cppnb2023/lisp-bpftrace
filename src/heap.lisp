(defpackage :heap
  (:use :cl :generic)
  (:export :heapify! :build-heap!))

(in-package :heap)

(defmacro left (i)
  `(1+ (* ,i 2)))

(defmacro right (i)
  `(* (1+ ,i) 2))

(defmacro root (i)
  `(truncate (1- ,i) 2))

(defun heapify! (array compare idx &key start end)
  (labels ((heapify (array idx)
             (let ((largest nil))
               (setf largest (best-index array compare
                                         (valid-index (length array) idx (left idx)
                                                      (right idx))))
               (if (= largest idx)
                   array
                   (progn
                     (rotatef (aref array largest) (aref array idx))
                     (heapify array largest))))))
    (heapify (make-slice array start end) idx)))

(defun build-heap! (array compare &key start end)
  (labels ((build-heap (array compare)
             (loop for i from (root (length array)) downto 0
                   do (format t "~s~%"
                              (heapify! array compare i :start start :end end)))
             array))
    (build-heap (make-slice array start end) compare)))

