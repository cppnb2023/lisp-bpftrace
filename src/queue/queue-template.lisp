(defpackage :queue-template
  (:use :cl)
  (:export :queue-enqueue :queue-dequeue :queue-peek :queue-coerce
           :queue-size :queue-empty-p :queue-empty :queue-full))

(in-package :queue-template)

(defgeneric queue-enqueue (queue element)
  (:documentation "队列入队，返回值不明"))
(defgeneric queue-dequeue (queue)
  (:documentation "队列出队，返回值不明"))
(defgeneric queue-peek (queue)
  (:documentation "查看队列头部，返回值为头部元素"))
(defgeneric queue-coerce (queue type)
  (:documentation "将队列强制转换为其他类型，支持'list或'array"))
(defgeneric queue-size (queue)
  (:documentation "返回当前队列容量"))
(defgeneric queue-empty-p (queue)
  (:documentation "查看队列是否为空，为空返回t"))
(defgeneric queue-full-p (queue)
  (:documentation
   "查看队列是否已满，已满返回t
    理论无限长的队列应永远返回nil"))

(define-condition queue-empty (error)
  ((queue :initarg :queue :reader queue-empty-queue))
  (:report
   (lambda (condition stream)
     (format stream "队列~s为空~%"
             (queue-empty-queue condition)))))

(define-condition queue-full (error)
  ((queue :initarg :queue :reader queue-full-queue))
  (:report
   (lambda (condition stream)
     (format stream "队列~s已满~%"
             (queue-full-queue condition)))))
