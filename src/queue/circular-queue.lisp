(defpackage :circular-queue
  (:use :cl :queue-template :do-varient)
  (:export :circular-queue :make-circular-queue :queue-empty-p
           :queue-full-p :queue-enqueue :queue-dequeue :queue-peek
           :queue-size :queue-coerce :iterator))

(in-package :circular-queue)

(defclass circular-queue ()
  ((array :initform nil)
   (capacity :type (unsigned-byte 32) :initarg :capacity
             :initform (error "必须分配空间"))
   (beg :type (unsigned-byte 32) :initform 0)
   (end :type (unsigned-byte 32) :initform 0)))

(defmacro next-pos (var limit)
  `(mod (1+ ,var) ,limit))

(defmethod initialize-instance :after ((circular-queue circular-queue) &key)
  (with-slots (array capacity) circular-queue
    (setf array (make-array capacity))))

(defun make-circular-queue (capacity)
  (make-instance 'circular-queue :capacity (1+ capacity)))

(defmethod queue-empty-p ((queue circular-queue))
  (with-slots (beg end) queue (= beg end)))

(defmethod queue-full-p ((queue circular-queue))
  (with-slots (beg end capacity) queue
    (= (next-pos end capacity) beg)))

(defmethod queue-enqueue ((queue circular-queue) element)
  (when (queue-full-p queue)
    (error 'queue-full :queue queue))
  (with-slots (array beg end capacity) queue
    (setf (aref array end) element
          end (next-pos end capacity))))

(defmethod queue-dequeue ((queue circular-queue))
  (when (queue-empty-p queue)
    (error 'queue-empty :queue queue))
  (with-slots (array beg capacity) queue
    (prog1
        (aref array beg)
      (setf (aref array beg) nil
            beg (next-pos beg capacity)))))

(defmethod queue-peek ((queue circular-queue))
  (if (queue-empty-p queue)
      (error 'queue-empty :queue queue)
      (with-slots (array beg) queue
        (aref array beg))))

(defmethod queue-size ((queue circular-queue))
  (with-slots (beg end capacity) queue
    (mod (- end beg) capacity)))

(defmethod queue-coerce ((queue circular-queue) type)
  (with-slots (array beg end capacity) queue
    (ecase type
      (list
       (do-complex ((:collect :clt))
           ((:circular i beg end capacity))
         (:main (:clt (aref array i)))))
      (array
       (let ((array (make-array (1- capacity))))
         (do-complex ()
             ((:circular i beg end capacity)
              (:do* ((j 0 (1+ j)))))
           (:main (setf (aref array j) (aref array i)))))))))

(defmethod iterator ((circular circular-queue))
  (with-slots (array beg end capacity) circular
    (let ((iter beg))
      (lambda ()
        (if (= iter end)
            (values nil nil)
            (values 
             (prog1
                 (aref array iter)
               (setf iter (next-pos iter capacity)))
             t))))))

