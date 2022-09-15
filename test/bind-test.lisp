;;;;; Test suite for RUTILSX BIND
;;;;; see LICENSE file for permissions


(in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)

(defstruct dummy-struct-for-bind i j)
(defclass dummy-obj-for-bind ()
  ((k :initarg :k)
   (l :initarg :l)))

(deftest bind ()
  (should be equal '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
          (bind ((a 1)
                 (b c (values 2 3))
                 ((d e &rest f) '(4 5 13 14 15 16))
                 (((g :g) (h :h)) ? #h(:g 6 :h 7))
                 ((i rutils.bind::j) @ (make-dummy-struct-for-bind :i 8 :j 9))
                 (((k k) (l l)) @ (make 'dummy-obj-for-bind :k 10 :l 11))
                 (#(_ m) (funcall (lambda () #(0 12)))))
            (list* a b c d e g h i rutils.bind::j k l m f))))
