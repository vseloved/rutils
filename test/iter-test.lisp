;;;;; Test suite for RUTILSX ITER
;;;;; see LICENSE file for permissions


(cl:in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)

(deftest iter ()
  (should be equal '(#\a #\b #\c)
          (iter (:for c :in-string "abc")
            (:collect c))))
