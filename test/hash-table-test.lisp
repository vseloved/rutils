;;;;; Test suite for RUTILS HASH-TABLE
;;;;; see LICENSE file for permissions


(cl:in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)


(deftest copy-hash-table ()
  (should be equalp #h(equalp :foo :foo "bar" "bar")
          (copy-hash-table #h(equalp :foo :foo "bar" "bar"))))

(deftest merge-hash-tables ()
  (should be equalp #h(:foo 1)
          (merge-hash-tables #h(:foo 1)))
  (should be equalp #h(:foo 1 :bar 2)
          (merge-hash-tables #h(:foo 1) #h(:bar 2)))
  (should be equalp #h(:foo 1 :bar 2)
          (merge-hash-tables #h(:foo 1 :bar 1) #h(:bar 2))))

(deftest hash-table-keys ()
  (should be null
          (hash-table-keys #h()))
  (should be equal '(:foo :bar)
          (hash-table-keys #h(:foo 1 :bar 2))))

(deftest hash-table-vals ()
  (should be null
          (hash-table-vals #h()))
  (should be equal '(1 2)
          (hash-table-vals #h(:foo 1 :bar 2))))

(deftest hash-table-from-plist ()
  (should be equalp #h()
          (hash-table-from-plist '()))
  (should be equalp #h(:foo 1 :bar 2)
          (hash-table-from-plist '(:foo 1 :bar 2))))

(deftest hash-table-to-plist ()
  (should be null
          (hash-table-to-plist #h()))
  (should be equalp '(:foo 1 :bar 2)
          (hash-table-to-plist #h(:foo 1 :bar 2))))

(deftest hash-table-from-alist ()
  (should be equalp #h()
          (hash-table-from-alist '()))
  (should be equalp #h(:foo 1 :bar 2)
          (hash-table-from-alist '((:foo . 1) (:bar . 2)))))

(deftest hash-table-to-alist ()
  (should be null
          (hash-table-to-alist #h()))
  (should be equal '((:foo . 1) (:bar . 2))
          (hash-table-to-alist #h(:foo 1 :bar 2))))

(deftest print-hash-table ()
  (should print-to *standard-output* (fmt "#{~% } ")
          (print-hash-table #h()))
  (should print-to *standard-output*
          (format nil "#{~%  :FOO 1~% } ")
          (print-hash-table #h(:foo 1)))
  (should print-to *standard-output*
          (format nil "#{EQUAL~%  \"foo\" '(1)~%  \"bar\" NIL~% } ")
          (print-hash-table #h(equal "foo" '(1) "bar" nil))))

(deftest with-keys ()
  (should be equal (list :quux 42 nil)
          (with-keys ((foo :foo) (bar :bar) (baz "baz"))
              #h(equal :foo :quux "baz" 42)
            (list foo baz bar))))

(deftest dotable ()
  (should print-to *standard-output* (format nil "~%(1 2) ~%(3 4) ~%(5 6) ")
          (dotable (k v #h(1 2 3 4 5 6))
            (print (list k v)))))

(deftest hash-set ()
  (should be equalp #h()
          (hash-set nil))
  (should be equalp #h(:key t)
          (hash-set '(:key)))
  (should be equalp #h(:key t)
          (hash-set #(:key)))
  (should be equalp #h(equal "key" t)
          (hash-set '("key") :test 'equal)))
