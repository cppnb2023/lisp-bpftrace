(defpackage :test-generic
  (:use :cl :generic :unit-test))

(in-package :test-generic)

(deftest test-branch ()
  (check (= (aif 0 it it) 0)
         (not (aif nil 1 it))
         (= (awhen 1 it) 1)
         (not (awhen nil 1))))

(deftest test-last ()
  (check (= (array-last #(1 2 114514)) 114514)
         (eq (last1 '(1 2 :key)) :key)))

(deftest test-misc ()
  (check (singlep '(1))
         (not (singlep '(2 3)))
         (string= (strcat "test" "123a") "test123a")
         (eq (ensure-symbol :a :b) :a)
         (eq (ensure-symbol '(1 2) :b) :b)
         (= (ensure-integer 1 2) 1)
         (= (ensure-integer #(1) 4) 4)
         (string= (with-stream-format () (:format "1234a~a" 1))
                  "1234a1")
         (equal (with-collect ()
                  (:collect 1)
                  (:collect '(2 3)))
                '(1 (2 3)))))

(deftest test-generic ()
  (check (test-branch)
         (test-last)
         (test-misc)))

(test-generic)
