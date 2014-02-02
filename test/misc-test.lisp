;;;;; Test suite for RUTILS MISC
;;;;; see LICENSE file for permissions


(in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)


(deftest xor ()
  (should be null
          (xor))
  (should be true
          (xor t))
  (should be null
          (xor nil))
  (should be true
          (xor t nil))
  (should be null
          (xor t nil t)))

(deftest less ()
  (should be true
          (less nil 0))
  (should be null
          (less 0 nil))
  (should be null
          (less 0 0))
  (should be true
          (less 0 1))
  (should be null
          (less 1 0)))

(deftest not-less ()
  (should be true
          (not-less nil 0))
  (should be null
          (not-less 0 nil))
  (should be true
          (not-less 0 0))
  (should be null
          (not-less 0 1))
  (should be true
          (not-less 1 0)))

(deftest more ()
  (should be true
          (more 0 nil))
  (should be null
          (more nil 0))
  (should be null
          (more 0 0))
  (should be null
          (more 0 1))
  (should be true
          (more 1 0)))

(deftest not-more ()
  (should be true
          (not-more nil 0))
  (should be null
          (not-more 0 nil))
  (should be true
          (not-more 0 0))
  (should be true
          (not-more 0 1))
  (should be null
          (not-more 1 0)))

(deftest named-lambda ()
  (should be fboundp
          (unwind-protect (progn (named-lambda foo ())
                                 'foo)
            (fmakunbound 'foo))))

(deftest re-setf ()
  (should be eql :bar
          (let ((foo 0))
            (re-setf foo :foo :bar))))

(deftest 2nd ()
  (should be null (2nd ()))
  (should be null (2nd (values 1)))
  (should be eql :bar (2nd (values :foo :bar))))

(deftest pcase ()
  (should be eql :ok
          (pcase '< 1
            (0 :below-one)
            (2 :ok)
            (otherwise (error "Oops"))))
  (should be null
          (pcase '< 1
            (0 :below-zero)
            (1 :one))))

(deftest pccase ()
  (should be eql :ok
          (pccase '< 1
            (0 :below-one)
            (2 :ok)))
  (should signal case-failure
          (pccase '< 1
            (0 :below-zero)
            (1 :one))))

(deftest pecase ()
  (should be eql :ok
          (pecase '< 1
            (0 :below-one)
            (2 :ok)))
  (should signal case-failure
          (pecase '< 1
            (0 :below-zero)
            (1 :one))))

(defun dcase-test (x)
  (dcase x
    ((:foo a b) (list a b))
    ((:bar &key a b) (list a b))
    (((:alt1 :alt2) a) a)
    ((t &rest rest) rest)))

(deftest dcase ()
  (should be equal '(1 2)
          (dcase-test '(:foo 1 2)))
  (should be equal '(1 2)
          (dcase-test '(:bar :a 1 :b 2)))
  (should be equal 1
          (dcase-test '(:alt1 1)))
  (should be equal 2
          (dcase-test '(:alt2 2)))
  (should be equal '(1 2 3)
          (dcase-test '(:baz 1 2 3))))

(defun dccase-test (x)
  (dccase x
    ((:foo a b) (list a b))
    ((:bar &key a b) (list a b))
    (((:alt1 :alt2) a) a)))

(deftest dccase ()
  (should be equal '(1 2)
          (dccase-test '(:foo 1 2)))
  (should be equal '(1 2)
          (dccase-test '(:bar :a 1 :b 2)))
  (should be equal 1
          (dccase-test '(:alt1 1)))
  (should be equal 2
          (dccase-test '(:alt2 2)))
  (should signal case-failure
          (dccase-test '(:baz 1 2 3))))

(defun decase-test (x)
  (decase x
    ((:foo a b) (list a b))
    ((:bar &key a b) (list a b))
    (((:alt1 :alt2) a) a)))

(deftest decase ()
  (should be equal '(1 2)
          (decase-test '(:foo 1 2)))
  (should be equal '(1 2)
          (decase-test '(:bar :a 1 :b 2)))
  (should be equal 1
          (decase-test '(:alt1 1)))
  (should be equal 2
          (decase-test '(:alt2 2)))
  (should signal case-failure
          (decase-test '(:baz 1 2 3))))

(deftest switch ()
  (should be true
          (switch ("foo")
            ("foo" :ok)
            (t t)))
  (should be eql :ok
          (switch ("foo" :key 'mkeyw)
            (:foo :ok)
            (t t)))
  (should be true
          (switch ("foo")
            ("foo" :ok)
            (otherwise t)))
  (should be eql :ok
          (switch ("foo" :test 'string=)
            ("foo" :ok)
            (otherwise t))))

(deftest cswitch ()
  (should signal case-failure
          (cswitch ("foo")
            ("fool" t)
            ("bar" t)))
  (should be true
          (cswitch ("foo" :test 'string=)
            ("foo" t)
            ("bar" nil))))

(deftest eswitch ()
  (should signal case-failure
          (eswitch ("foo")
            (:foo t)
            ("bar" t)))
  (should signal case-failure
          (eswitch ((list 1 2))
            ('(1 2) t)
            ("bar" t)))
  (should be true
          (eswitch ("foo" :test 'string=)
            ("foo" t)
            ("bar" nil))))

(deftest multiple-value-prog2 ()
  (should be equal 1 2
          (multiple-value-prog2
              (+ 1 2)
              (values 1 2)
            (+ 2 1))))

(defmacro once-only-aux-mac (x)
  (once-only (x)
    `(+ ,x ,x)))

(deftest once-only ()
  (should be = 4  ;; w/o once-only will return 6
          (let ((x 0))
            (+ (once-only-aux-mac (incf x))
               (incf x)))))
