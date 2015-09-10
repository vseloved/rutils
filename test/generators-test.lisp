;;;;; Test suite for RUTILSX GENERIC
;;;;; see LICENSE file for permissions


(cl:in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)


(deftest yield ()
  (flet ((gen-range ()
           (dolist (item (range 0 10))
             (yield item))))
    (should be = 2
            (let ((i 0))
              (doing (item (gen-range))
                (:+ i)
                (when (> i 2)
                  (return item)))))))
