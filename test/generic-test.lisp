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
