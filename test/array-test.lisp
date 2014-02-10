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
          (slice "foo" 2 3)))