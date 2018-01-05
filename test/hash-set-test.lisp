;;;;; Test suite for RUTILS HASH-TABLE
;;;;; see LICENSE file for permissions


(cl:in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)


(deftest hash-set ()
  (should be equalp #h()
          (hash-set))
  (should be equalp #h(equal :foo t :bar t)
          (hash-set 'equal :foo :bar)))

(deftest add# ()
  (let ((set (hash-set)))
    (should be equalp #h(:foo t)
            (progn (add# :foo set)
                   set))
    (should be equalp #h(:foo t)
            (progn (add# :foo set)
                   set))))

(deftest inter# ()
  (should be equalp #h(:foo t)
          (inter# (hash-set 'eql :foo)
                  (hash-set 'eql :foo :bar)))
  (should be equalp #h()
          (inter# (hash-set 'eql :foo)
                  (hash-set 'eql :bar))))

(deftest union# ()
  (should be equalp #h(:foo t :bar t)
          (union# (hash-set 'eql :foo)
                  (hash-set 'eql :foo :bar)))
  (should be equalp #h(:foo t :bar t)
          (union# (hash-set 'eql :foo)
                  (hash-set 'eql :bar))))

(deftest diff# ()
  (should be emptyp#
          (diff# (hash-set 'eql :foo)
                 (hash-set 'eql :foo :bar)))
  (should be equalp #h(:bar t)
          (diff# (hash-set 'eql :foo :bar)
                 (hash-set 'eql :foo)))
  (should be equalp #h(:foo t)
          (diff# (hash-set 'eql :foo)
                 (hash-set 'eql :bar))))

(deftest xor# ()
  (should be emptyp#
          (xor# (hash-set 'eql :foo)
                 (hash-set 'eql :foo)))
  (should be equalp #h(:bar t)
          (xor# (hash-set 'eql :foo)
                  (hash-set 'eql :foo :bar)))
  (should be equalp #h(:foo t :bar t)
          (xor# (hash-set 'eql :foo)
                  (hash-set 'eql :bar))))
