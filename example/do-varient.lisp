(defpackage :example-do-varient
  (:use :base-tools))

(in-package :example-do-varient)

(do-stage ((i 0 (1+ i)))
  ((= i 3))
  (:first (format t "first: ~a~%" i))
  (:main  (format t "main: ~a~%" i))
  (:end   (format t "end~%")))
;;输出
;; first: 0
;; main: 1
;; main: 2
;; end
;;改写do-times-stage
(do-times-stage (i 3)
  (:first (format t "first: ~a~%" i))
  (:main  (format t "main: ~a~%" i))
  (:end   (format t "end~%")))
;; first, main, end根据需求选择性写
(do-times-stage (i 3)
  (:main (format t "main: ~a~%" i)))
;; 上面这个和直接用dotimes没区别
;; do-list-stage也是同理, 不介绍了
;; do-plist-stage用来遍历plist
(do-plist-stage (k v '(:a 1 :b 2 :c 3))
  (:first (format t "~a: ~a~%" k v))
  (:main  (format t "~a~%" v)))
;;输出
;; a 1
;; 2
;; 3

(do-tuple-stage ((first second) '(1 2 3))
  (:main (format t "~a ~a~%" first second)))
;;输出
;; 1 2
;; 2 3

;;以do-*-*-format几乎和上边用法一样,
;;只是可以批量拼接字符串
;;举一个简单例子
(format t "~a~%"
        (do-times-stage-format (i 10)
          (:first (:format "(~a" i))
          (:main  (:format " ~a" i))
          (:end   (:format ")"))))
;;输出
;;(1 2 3 4 5 6 7 8 9)
