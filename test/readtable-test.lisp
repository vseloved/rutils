;;;;; Test suite for RUTILS READTABLE
;;;;; see LICENSE file for permissions

(cl:in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)


(deftest |#v-reader| ()
  (should be equalp #()
          (eval (read-from-string "#v()")))
  (should be equalp #(:foo :bar :baz)
          (eval (read-from-string "#v(:foo :bar :baz)")))
  (should be equalp (make-array 3 :initial-contents '(1 2 3))
          (eval (read-from-string "#v((1+ 0) (1+ 1) (1+ 2))"))))

(deftest |#h-reader| ()
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "foo" ht) "bar")
    (should be equalp (make-hash-table)
            (eval (read-from-string "#h()")))
    (should be equalp (make-hash-table :test 'equal)
            (eval (read-from-string "#h(equal)")))
    (should be equalp ht
            (eval (read-from-string "#h(equal \"foo\" \"bar\")")))
    (should be equalp ht
            (eval (read-from-string
                   "#h(equal \"foo\" (coerce '(#\\b #\\a #\\r) 'string))")))))

(deftest |#{-reader| ()
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "foo" ht) "bar")
    (should be equalp (make-hash-table)
            (eval (read-from-string "#{}")))
    (should be equalp (make-hash-table :test 'equal)
            (eval (read-from-string "#{equal}")))
    (should be equalp ht
            (eval (read-from-string "#{equal \"foo\" \"bar\"}")))
    (should be = 17
            (hash-table-size (eval (read-from-string
                                    "#{1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0 9 0
                                     10 0 11 0 12 0 13 0 14 0 15 0 16 0 17 0}"))))
    (should be equalp ht
            (eval (read-from-string
                   "#{equal \"foo\" (coerce '(#\\b #\\a #\\r) 'string)}")))))

(deftest |#`-reader| ()
  (should be = 4
          (funcall (eval (read-from-string "#`(+ % %%)")) 2 2)))

(deftest |#/-reader| ()
  (should be string= "foo \"bar\""
          (eval (read-from-string "#/foo \"bar\"/#"))))
