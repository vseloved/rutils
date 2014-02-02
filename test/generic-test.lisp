;;;;; Test suite for RUTILSX GENERIC
;;;;; see LICENSE file for permissions


(cl:in-package #:rutilsx.test)
(named-readtables:in-readtable rutils-readtable)


(deftest donext ()
  (should print-to *standard-output* (format nil "1~%2~%")
          (donext (n '(1 2))
            (print n)))
  (should print-to *standard-output* (format nil "1~%2~%")
          (donext (n #(1 2))
            (print n)))
  (should print-to *standard-output* (format nil "1~%2~%")
          (donext (n #h(:foo 1 :bar 2))
            (print n))))

(deftest maptab ()
  (should be equalp #h()
          (maptab #'identity #h()))
  (should be equalp #h(1 3 3 5)
          (maptab #`(1+ %%) #h(1 2 3 4)))
  (should be equal '((1 . 3) (3 . 5))
          (maptab #`(1+ %%) '((1 . 2) (3 . 4)))))

(deftest seq ()
  (should be = 0 (2nd (seq '(1 2 3))))
  (should be = 2 (seq '(1 2 3) 1)))