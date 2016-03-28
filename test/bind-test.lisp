;;;;; Test suite for RUTILSX BIND
;;;;; see LICENSE file for permissions


(cl:in-package #:rutilsx.test)
(named-readtables:in-readtable rutils-readtable)


(deftest bind ()
  (should be equal '(1 2 3 4 5 6 7 8 9)
          (bind ((a 1)
                 (b c (values 2 3))
                 ((d e &rest f) '(4 5 6 7 8 9)))
            (list* a b c d e f))))
