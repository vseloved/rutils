;;;;; Test suite for RUTILSX GENERATORS
;;;;; see LICENSE file for permissions


(cl:in-package #:rutilsx.test)
(named-readtables:in-readtable rutils-readtable)


(defun gen-test1 (n)
  (dotimes (i n)
    (yield i)))

(defun gen-test2 (n)
  (dotimes (i n)
    (yield (- i))))

(deftest yield ()
  (flet ((gen-range ()
           (dolist (item (range 0 10))
             (yield item))))
    (should be = 2
            (let ((i 0))
              (doing (item (gen-range))
                (:+ i)
                (when (> i 2)
                  (return item)))))
    (should be equalp '((1 -1) (1 -2) (2 -1) (2 -2))
            (let (rez)
              (doing (x (gen-test1 2) rez)
                (doing (y (gen-test2 2))
                  (push (pair x y) rez)))))))
