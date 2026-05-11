(defpackage :circular-queue
  (:use :cl :queue-template :do-varient :generic)
  (:export :circular-queue :make-circular-queue :queue-empty-p
           :queue-full-p :queue-enqueue :queue-dequeue :queue-peek
           :queue-size :queue-coerce :iterator))

(in-package :circular-queue)

(defclass circular-queue ()
  ((array :type simple-array)
   (capacity :type (unsigned-byte 32) :initarg :capacity
             :initform *queue-default-size*)
   (beg :type (unsigned-byte 32) :initform 0)
   (end :type (unsigned-byte 32) :initform 0)))

(defmacro with-circular-queue ((circular-queue) &body body)
  `(with-opt-slots ((simple-array array) ((unsigned-byte 32) capacity)
                    ((unsigned-byte 32) beg) ((unsigned-byte 32) end))
       ,circular-queue
     ,@body))

(defmacro next-pos (var limit)
  `(mod (1+ ,var) ,limit))

(defmethod initialize-instance :after ((circular-queue circular-queue) &key)
  (with-circular-queue (circular-queue)
    (setf array (make-array capacity))))

(defun make-circular-queue (capacity)
  (make-instance 'circular-queue :capacity (1+ capacity)))

(defmethod queue-empty-p ((queue circular-queue))
  (with-circular-queue (queue) (= beg end)))

(defmethod queue-full-p ((queue circular-queue))
  (with-circular-queue (queue)
    (= (next-pos end capacity) beg)))

(defmethod queue-enqueue ((queue circular-queue) element)
  (when (queue-full-p queue)
    (error 'queue-full :queue queue))
  (with-circular-queue (queue)
    (setf (aref array end) element
          end (next-pos end capacity))))

(defmethod queue-dequeue ((queue circular-queue))
  (when (queue-empty-p queue)
    (error 'queue-empty :queue queue))
  (with-circular-queue (queue)
    (prog1
        (aref array beg)
      (setf (aref array beg) nil
            beg (next-pos beg capacity)))))

(defmethod queue-peek ((queue circular-queue))
  (if (queue-empty-p queue)
      (error 'queue-empty :queue queue)
      (with-circular-queue (queue)
        (aref array beg))))

(defmethod queue-size ((queue circular-queue))
  (with-circular-queue (queue)
    (mod (- end beg) capacity)))

(defmethod queue-coerce ((queue circular-queue) type)
  (with-circular-queue (queue)
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
  (with-circular-queue (circular)
    (let ((iter beg))
      (lambda ()
        (if (= iter end)
            (values nil nil)
            (values 
             (prog1
                 (aref array iter)
               (setf iter (next-pos iter capacity)))
             t))))))

