;;;;; Test suite for RUTILSX THREADING
;;;;; see LICENSE file for permissions


(cl:in-package #:rutilsx.test)
(named-readtables:in-readtable rutils-readtable)


(deftest -> ()
  (should be string= "X"
          (-> "a b c d"
              (substitute #\x #\a %)
              string-upcase
              (split-sequence #\space %)
              first)))

(deftest ->> ()
  (should be equalp '(3 5 7 9)
          (->> (range 1 10)
               (remove-if #'oddp)
               (map 'list #`(+ 1 %)))))
