;;;;; Test suite for RUTILS TREE
;;;;; see LICENSE file for permissions


(cl:in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)


(deftest dotree ()
  (should be equal '(5 4 3 2 1)
          (let (rez)
            (dotree (el '(1 (2 3) (4 5)) rez)
              (push (first (mklist el)) rez)))))

(deftest doleaves ()
  (should be equal '(5 3)
          (let (rez)
            (doleaves (el '(1 (2 3) (4 5)) rez)
              (push el rez)))))

(deftest maptree ()
  (should be equal '(2 (3 4) (5 6))
          (maptree #'1+ '(1 (2 3) (4 5)))))

(deftest mapleaves ()
  (should be equal '(1 (2 4) (4 6))
          (mapleaves #'1+ '(1 (2 3) (4 5)))))

(deftest tree-size ()
  (should be = 0
          (tree-size '()))
  (should be = 1
          (tree-size '(:foo)))
  (should be = 2
          (tree-size '(:foo :bar)))
  (should be = 4
          (tree-size '(:foo (:bar :baz :foo))))
  (should be = 5
          (tree-size '(:foo (:bar :baz :foo) :bar))))

(deftest tree-depth ()
  (should be = 0
          (tree-depth ()))
  (should be = 1
          (tree-depth '(:foo)))
  (should be = 2
          (tree-depth '(:foo :bar)))
  (should be = 3
          (tree-depth '(:foo (:bar :baz :foo))))
  (should be = 3
          (tree-depth '(:foo (:bar :baz :foo) :bar))))
