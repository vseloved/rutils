;;;;; Test suite for RUTILSX GENERIC
;;;;; see LICENSE file for permissions


(in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)

(deftest mapkv ()
  (should be equalp #h()
          (mapkv #'identity #h()))
  (should be equalp #h(1 3 3 5)
          (mapkv #`(1+ %%) #h(1 2 3 4)))
  (should be equal '((1 . 3) (3 . 5))
          (mapkv #`(1+ %%) '((1 . 2) (3 . 4)))))
