(defpackage :event-time
  (:use :cl :generic)
  (:export #:et-data #:et-seconds #:et-mseconds #:event-time #:newer #:older
           #:make-event-time))

(in-package :event-time)

(defstruct (event-time (:conc-name et-))
  data
  seconds
  mseconds)

(defun newer (event1 event2)
  (declare (event-time event1 event2))
  (with-compare ((et-seconds event1) (et-seconds event2))
    (#'< t)
    (#'> nil)
    (t (< (et-mseconds event1)
          (et-mseconds event2)))))

(defun older (event1 event2)
  (declare (event-time event1 event2))
  (with-compare ((et-seconds event1) (et-seconds event2))
    (#'> t)
    (#'< nil)
    (t (> (et-mseconds event1)
          (et-mseconds event2)))))

