(defpackage :heap
  (:use :cl :generic)
  (:export #:heapify! #:build-heap! #:sort-heap!))

(in-package :heap)

(defmacro left (i)
  `(1+ (* ,i 2)))

(defmacro right (i)
  `(* (1+ ,i) 2))

(defmacro root (i)
  `(truncate (1- ,i) 2))

(defun best-leaf (array test i)
  (let ((largest
         (best-position array test
                        :specify
                        (remove-if-not (range :ge 0 :lt (length array))
                                       (list i (left i) (right i))))))
    (when (and largest (not (= largest i))) largest)))

(defun heapify! (array test idx &key start end)
  (declare (array array))
  (declare ((unsigned-byte 64) idx))
  (labels ((heapify (array idx)
             (aif (best-leaf array test idx)
                  (progn
                    (rotatef (aref array idx) (aref array it))
                    (heapify array it))
                  array)))
    (heapify (make-slice array start end) idx)))

(defun build-heap! (array test &key start end)
  (declare (array array))
  (labels ((build-heap (array test)
             (loop for i from (root (length array)) downto 0
                   do (heapify! array test i :start start :end end))
             array))
    (build-heap (make-slice array start end) test)))

(defun sort-heap! (array test &key start end)
  (declare (array array))
  (unless start (setf start 0))
  (unless end   (setf end   (length array)))
  (loop for i from (1- end) downto start
        do (progn
             (rotatef (aref array start) (aref array i))
             (heapify! array test start :start start :end i)))
  (make-slice array start end))

