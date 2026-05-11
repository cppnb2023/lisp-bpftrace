(defpackage :example-do-varient
  (:use :base-tools))

(in-package :example-do-varient)

(format t "~s~%" (macroexpand-1
                  '(do-complex ((:collect :clt))
                    ((:list e '(1 2 3 4)))
                    (:main (:clt (1+ e))))))

(format t "~s~%" (do-complex ((:collect :clt))
                     ((:list e '(1 2 3 4)))
                   (:main (:clt (1+ e)))))

(format t "~s~%" (macroexpand-1
                  '(do-complex ((:collect :clt))
                    ((:list e '(1 2 3 4)) (:times i 5))
                    (:main (:clt (+ i e))))))

(format t "~s~%" (do-complex ((:collect :clt))
                     ((:list e '(1 2 3 4)) (:times i 5))
                   (:main (:clt (+ i e)))))

(multiple-value-bind (lst str)
    (do-complex ((:collect :clt) (:format :fmt)) 
        ((:plist k v '(:a 1 :b 2 :c 3 :d 4 :e 5))
         (:circular i 4 3 5)
         (:tuple (a b) '(1 2 3))
         (:do* idx 0 (1+ idx)))
      (:main (:clt (list v (list a b) idx)) (:fmt "~a~a" k i)))
  (format t "~s ~s~%" lst str))

(format t "~s~%"
        (do-complex ((:append :apd))
            ((:list a '((1 2) (3 4))))
          (:main (:apd a))))
