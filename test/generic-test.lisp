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

(defstruct foo-struct bar)

(deftest ? ()
  (should be equalp #h(1 #h(2 4))
          (let ((ht #h(1 #h(2 3))))
            (:= (? ht 1 2) 4)
            ht))
  (should be equalp #h(1 #(2 4))
          (let ((ht #h(1 #(2 3))))
            (:= (? ht 1 1) 4)
            ht))
  (should be eql :baz
          (? (make-foo-struct :bar :baz) 'bar)))

(defun not-eql (a b)
  (not (eql a b)))

(deftest copy ()
  (let* ((l (list 1 (list 2 3)))
         (c (copy l)))
    (should be not-eql l c)
    (should be not-eql (cdr l) (cdr c)))  ; copy-list copies sublists in SBCL
  (let* ((h #h(1 '(2)))
         (c (copy h)))
    (should be not-eql h c)
    (should be eql (get# 1 h) (get# 1 c)))
  (let* ((v #(1 (2 3)))
         (c (copy v)))
    (should be not-eql v c)
    (should be eql (aref v 1) (aref c 1)))
  (let* ((p (pair '(1) '(2)))
         (c (copy p)))
    (should be not-eql p c)
    (should be eql (lt p) (lt c))))
  
