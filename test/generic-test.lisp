;;;;; Test suite for RUTILSX GENERIC
;;;;; see LICENSE file for permissions


(cl:in-package #:rutilsx.test)
(named-readtables:in-readtable rutils-readtable)


(deftest maptab ()
  (should be equalp #h()
          (maptab #'identity #h()))
  (should be equalp #h(1 3 3 5)
          (maptab #`(1+ %%) #h(1 2 3 4)))
  (should be equal '((1 . 3) (3 . 5))
          (maptab #`(1+ %%) '((1 . 2) (3 . 4)))))

(defstruct foo-struct bar)

(deftest ? ()
  (should be equalp #h(1 #h(2 4))
          (let ((ht #h(1 #h(2 3))))
            (:= (? ht 1 2) 4)
            ht))
  (should be equalp #h(1 #(2 4))
          (let ((ht #h(1 #(2 3))))
            (:= (? ht 1 1) 4)
            ht))
  (should be eql :baz
          (? (make-foo-struct :bar :baz) :bar)))
