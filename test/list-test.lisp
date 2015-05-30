;;;;; Test suite for RUTILS LIST
;;;;; see LICENSE file for permissions


(cl:in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)


(deftest last1 ()
  (should be null (last1 ()))
  (should be eql :foo (last1 '(:foo)))
  (should be eql :bar (last1 '(:foo :bar)))
  (should be eql :foo (last1 '(:foo :bar) 2)))

(deftest butlast2 ()
  (should be eql () :foo (butlast2 '(:foo)))
  (should be equal '(:foo) :bar (butlast2 '(:foo :bar)))
  (should be equal () '(:foo :bar) (butlast2 '(:foo :bar) 2)))

(deftest single ()
  (should be null (single ()))
  (should be true (single '(:foo)))
  (should be null (single '(:foo :bar))))

(deftest dyadic ()
  (should be null (dyadic ()))
  (should be null (dyadic '(:foo)))
  (should be true (dyadic '(:foo :bar)))
  (should be null (dyadic '(:foo :bar :baz))))

(deftest tryadic ()
  (should be null (tryadic ()))
  (should be null (tryadic '(:foo)))
  (should be null (tryadic '(:foo :bar)))
  (should be true (tryadic '(:foo :bar :baz)))
  (should be null (tryadic '(:foo :bar :baz :foo))))

(deftest ensure-list ()
  (should be listp (ensure-list :foo))
  (should be listp (ensure-list '(:foo))))

(deftest with-output-to-list ()
  (should be equal '(:foo :bar)
          (with-output-to-list (out)
            (push :foo out)
            (push :bar out))))

(deftest flatten ()
  (should be null
          (flatten ()))
  (should be equal '(:foo)
          (flatten '(:foo)))
  (should be equal '(:foo :bar)
          (flatten '(:foo :bar)))
  (should be equal '(:foo :bar)
          (flatten '((:foo) (:bar))))
  (should be equal '(:foo :bar :baz)
          (flatten '(:foo (:bar :baz))))
  (should be equal '(:foo :bar :baz)
          (flatten '(:foo (:bar (:baz))))))

(deftest interleave ()
  (should be null
          (interleave ()))
  (should be null
          (interleave () ()))
  (should be equal '()
          (interleave '(:foo) ()))
  (should be equal '(:foo :bar)
          (interleave '(:foo) '(:bar)))
  (should be equal '(:foo :bar)
          (interleave '(:foo :baz) '(:bar))))

(deftest interpose ()
  (should be null
          (interpose () ()))
  (should be null
          (interpose 1 ()))
  (should be equal '()
          (interpose '(:foo) ()))
  (should be equal '(:foo :baz :bar)
          (interpose :baz '(:foo :bar)))
  (should be equal '(:foo (:baz) :bar)
          (interpose '(:baz) '(:foo :bar)))
  (should be equal '(:foo :baz :bar :baz :qux)
          (interpose :baz '(:foo :bar :qux))))

(deftest take ()
  (should be null
          (take 1 ()))
  (should be equal '(:foo)
          (take 1 '(:foo)))
  (should be equal '(:foo :bar)
          (take 2 '(:foo :bar)))
  (should be equal '(:foo)
          (take 1 '(:foo :bar))))

(deftest plistp ()
  (should be null
          (plistp ()))
  (should be null
          (plistp '(:foo)))
  (should be true
          (plistp '(:foo :bar))))

(deftest alistp ()
  (should be null
          (alistp ()))
  (should be null
          (alistp '(:foo)))
  (should be true
          (alistp '((:foo . :bar))))
  (should be true
          (alistp '((:foo . :bar) (:baz . :foo)))))

(deftest alist-to-plist ()
  (should be null
          (alist-to-plist ()))
  (should be equal '(:foo 42 :bar 42)
          (alist-to-plist '((:foo . 42) (:bar . 42)))))

(deftest plist-to-alist ()
  (should be null
          (plist-to-alist ()))
  (should be equal '((:foo . 42) (:bar . 42))
          (plist-to-alist '(:foo 42 :bar 42))))

(deftest remove-from-plist ()
  (should be null
          (remove-from-plist '(:foo 42 :bar 42) :foo :bar))
  (should be equal '(:foo 42)
          (remove-from-plist '(:foo 42 :bar 42) :bar))
  (should be equal '(:foo 42 :bar 42)
          (remove-from-plist '(:foo 42 :bar 42) :baz)))

(deftest delete-from-plist ()
  (let ((plist (list :foo 42 :bar 42)))
    (should be eql plist
            (delete-from-plist plist :baz))
    (should be equal '(:foo 42)
            (delete-from-plist plist :bar))
    (should be null
            (delete-from-plist plist :foo))))

(deftest assoc1 ()
  (should be null
          (assoc1 :foo ()))
  (should be = 42
          (assoc1 :baz '((:foo . :bar) (:baz . 42)))))

(deftest doplist ()
  (should be equal '(42 :bar 41 :foo)
          (let (rez)
            (doplist (k v '(:foo 41 :bar 42) rez)
              (push k rez)
              (push v rez)))))

(deftest set-equal ()
  (should be true
          (set-equal () ()))
  (should be true
          (set-equal '(:foo :bar) '(:bar :foo)))
  (should be null
          (set-equal '(:foo :bar) '(:foo :bar :baz))))

(deftest zip ()
  (should be null
          (zip))
  (should be equal '((:foo :baz) (:bar 42))
          (zip '(:foo :bar) '(:baz 42)))
  (should be equal '((:foo :baz))
          (zip '(:foo :bar) '(:baz)))
  (should be null
          (zip '(:foo :bar) ())))

(deftest zip-with ()
  (should be null
          (zip-with 'identity))
  (should be equal '((:foo . :baz) (:bar . 42))
          (zip-with 'cons '(:foo :bar) '(:baz 42))))

(deftest zip* ()
  (should be null
          (zip*))
  (should be equal '((:foo :baz) (:bar 42))
          (zip* '(:foo :bar) '(:baz 42)))
  (should be equal '((:foo :baz) (:bar nil))
          (zip* '(:foo :bar) '(:baz)))
  (should be equal '((:foo nil) (:bar nil))
          (zip* '(:foo :bar) ())))

(deftest zip*-with ()
  (should be null
          (zip*-with 'identity))
  (should be equal '((:foo . 41) (:bar . 42) (:baz))
          (zip*-with 'cons '(:foo :bar :baz) '(41 42))))

(deftest maptimes ()
  (should be null
          (maptimes 0 #'1+))
  (should be equal '(1 2 3)
          (maptimes 3 #'1+)))

(deftest mapindex ()
  (should be null
          (mapindex #'+ ()))
  (should be equal '(0 2 4)
          (mapindex #'+ '(0 1 2))))

(deftest mapcanindex ()
  (should be null
          (mapcanindex #'+ ()))
  (should be equal '(0 0 1 1 2 2)
          (mapcanindex #'list '(0 1 2))))

(defun set-equal-with-equal (s1 s2)
  (set-equal s1 s2 :test 'equal))

(deftest permutations ()
  (should be null
          (permutations ()))
  (should be set-equal-with-equal
          '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
          (permutations '(1 2 3))))

(deftest range ()
  (should be null
          (range 0 0))
  (should be equal '(1 2 3)
          (range 1 4))
  (should be equal '(1 3)
          (range 1 4 :step 2)))

(deftest listcase ()
  (should be equal :alist
          (listcase '((:foo . :bar) (1 . 2))
            (alist :alist)
            (dlist :dlist)
            (t :list)))
  (should be equal :dlist
          (listcase '((:foo :bar) 1 2)
            (alist :alist)
            (dlist :dlist)
            (t :list)))
  (should be equal :list
          (listcase '(:foo :bar 1 2)
            (alist :alist)
            (dlist :dlist)
            (t :list))))

(deftest dcons ()
  (should be equal '((:foo) 42)
          (dcons :foo 42 '(())))
  (should be equal '((:foo :bar) 2 1)
          (dcons :foo 2 '((:bar) 1))))

(deftest dlistp ()
  (should be null
          (dlistp ()))
  (should be null
          (dlistp '((:foo . :bar))))
  (should be true
          (dlistp '((:foo) :bar))))
