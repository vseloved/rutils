;;;;; Test suite for RUTILS LIST
;;;;; see LICENSE file for permissions


(cl:in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)


(deftest slice ()
  (should be string= "foo"
          (slice "foo" 0))
  (should be string= "foo"
          (slice "foo" 0 3))
  (should be string= "f"
          (slice "foo" 0 1))
  (should be blankp
          (slice "foo" 3 3)))

(deftest vec ()
  (should be equalp #()
          (vec))
  (should be equalp #(1)
          (vec 1))
  (should be equalp #(1 "abc")
          (vec 1 "abc")))
