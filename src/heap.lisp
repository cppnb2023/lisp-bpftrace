(defpackage :heap
  (:use :cl :generic)
  (:export
   #:make-heap #:heap-insert #:heap-pop #:heap-peek #:heap-size
   #:heap-empty-p #:with-heap-limit))

(in-package :heap)

;;AI编码START

(defclass heap ()
  ((array :initform nil)
   (test  :initarg :test :initform #'<)))

(defmethod initialize-instance :after ((heap heap) &key (size 10))
  (with-slots (array) heap
    (setf array (make-array size :fill-pointer 0))))

(defun sift-up (array index test)
  "Bubble up the element at INDEX until heap property is restored."
  (loop while (> index 0)
        for parent = (floor (1- index) 2)
        when (funcall test (aref array index) (aref array parent))
          do (rotatef (aref array index) (aref array parent))
             (setf index parent)
        else return nil))

(defun sift-down (array index test)
  "Bubble down the element at INDEX until heap property is restored."
  (let ((len (length array)))
    (loop while (< index len)
          for left = (1+ (* 2 index))
          for right = (1+ left)
          for smallest = (cond ((and (< left len)
                                     (funcall test (aref array left) (aref array index)))
                                left)
                               (t index))
          do (when (and (< right len)
                        (funcall test (aref array right) (aref array smallest)))
               (setf smallest right))
          when (= smallest index)
            do (return nil)
          else do (rotatef (aref array index) (aref array smallest))
                  (setf index smallest))))

(defun make-heap (&key (test #'<) (size 10))
  "Create a new heap with optional test comparator and initial capacity."
  (make-instance 'heap :test test :size size))

(defmethod heap-size ((heap heap))
  (with-slots (array) heap
    (length array)))

(defmethod heap-empty-p ((heap heap))
  (zerop (heap-size heap)))

(defmethod heap-peek ((heap heap))
  "Return the top element without removing it. Signals error if empty."
  (with-slots (array) heap
    (if (zerop (length array))
        (error "Heap is empty")
        (aref array 0))))

(defmethod heap-insert ((heap heap) item)
  "Insert an item into the heap."
  (with-slots (array test) heap
    (vector-push-extend item array)
    (sift-up array (1- (length array)) test)))

(defmethod heap-pop ((heap heap))
  "Remove and return the top element. Signals error if empty."
  (with-slots (array test) heap
    (when (zerop (length array))
      (error "Heap is empty"))
    (let ((result (aref array 0)))
      (setf (aref array 0) (vector-pop array))
      (unless (zerop (length array))
        (sift-down array 0 test))
      result)))

;;AI编码END

(defun heap-insert-limit (heap item limit)
  (if (< (heap-size heap) limit)
      (heap-insert heap item)
      (prog1
          (loop while (>= (heap-size heap) limit)
                collect (heap-pop heap))
        (heap-insert heap item))))

(defmacro with-heap-limit ((heap limit) &body body)
  (with-symbols (heap-sym limit-sym)
    `(let ((,heap-sym ,heap) (,limit-sym (the fixnum ,limit)))
       (declare (fixnum ,limit-sym))
       (macrolet ((:insert (item)
                    `(heap-insert-limit ,',heap-sym ,item ,',limit-sym))
                  (:pop     () `(heap-pop ,',heap-sym))
                  (:size    () `(heap-size ,',heap-sym))
                  (:empty-p () `(heap-empty-p ,',heap-sym))
                  (:peek    () `(heap-peek ,',heap-sym)))
         ,@body
         ,heap-sym))))

