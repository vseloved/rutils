;;;;; Test suite for RUTILS SEQUENCE
;;;;; see LICENSE file for permissions


(cl:in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)


(deftest split-sequence ()
  (should be null
          (split-sequence #\Space ()))
  (should be equal '("foo")
          (split-sequence #\Space "foo"))
  (should be equal '("foo" "bar")
          (split-sequence #\Space "foo bar"))
  (should be equal '("foo" "bar")
          (split-sequence #\Space "foo  bar"))
  (should be equal '("foo" "" "bar" "baz")
          (split-sequence #\Space "foo  bar baz" :remove-empty-subseqs nil))
  (should be equal '("foo" "bar" "baz")
          (split-sequence " " "foo bar baz" :test 'string=))
  (should be equal '("foo" "bar" "baz")
          (split-sequence '(#\Space #\-) "foo bar-baz"
                          :test-not #`(not (member %% %))))
  (should be equal '("foo")
          (split-sequence #\Space "foo bar baz" :count 1))
  (should be equal '("baz")
          (split-sequence #\Space "foo bar baz" :from-end t :count 1))
  (should be equal '("ar" "baz")
          (split-sequence #\Space "foo bar baz" :start 5))
  (should be equal '("foo" "b")
          (split-sequence #\Space "foo bar baz" :end 5))
  (should be equal '("foo" "bar" "baz")
          (split-sequence (char-code #\Space) "foo bar baz" :key 'char-code))
  (should be equal '((40 41) (43 44))
          (split-sequence 42 '(40 41 42 43 44))))

(deftest split-sequence-if ()
  (let ((space-char-p #`(char= #\Space %)))
    (should be null
            (split-sequence-if 'null ()))
    (should be equal '("foo")
            (split-sequence-if space-char-p "foo"))
    (should be equal '("foo" "bar")
            (split-sequence-if space-char-p "foo bar"))
    (should be equal '("foo" "bar")
            (split-sequence-if space-char-p "foo  bar"))
    (should be equal '("foo" "" "bar" "baz")
            (split-sequence-if space-char-p "foo  bar baz" :remove-empty-subseqs nil))
    (should be equal '("foo")
            (split-sequence-if space-char-p "foo bar baz" :count 1))
    (should be equal '("baz")
            (split-sequence-if space-char-p "foo bar baz" :from-end t :count 1))
    (should be equal '("ar" "baz")
            (split-sequence-if space-char-p "foo bar baz" :start 5))
    (should be equal '("foo" "b")
            (split-sequence-if space-char-p "foo bar baz" :end 5))
    (should be equal '("foo" "bar" "baz")
            (split-sequence-if #`(= % (char-code #\Space))
                               "foo bar baz" :key 'char-code))))

(deftest split-sequence-if-not ()
  (should be null
          (split-sequence-if-not 'null ()))
  (should be equal '("foo")
          (split-sequence-if-not 'alpha-char-p "foo"))
  (should be equal '("foo" "bar")
          (split-sequence-if-not 'alpha-char-p "foo bar"))
  (should be equal '("foo" "bar")
          (split-sequence-if-not 'alpha-char-p "foo  bar"))
  (should be equal '("foo" "" "bar" "baz")
          (split-sequence-if-not 'alpha-char-p "foo  bar baz" :remove-empty-subseqs nil))
  (should be equal '("foo")
          (split-sequence-if-not 'alpha-char-p "foo bar baz" :count 1))
  (should be equal '("baz")
          (split-sequence-if-not 'alpha-char-p "foo bar baz"
                                 :from-end t :count 1))
  (should be equal '("ar" "baz")
          (split-sequence-if-not 'alpha-char-p "foo bar baz" :start 5))
  (should be equal '("foo" "b")
          (split-sequence-if-not 'alpha-char-p "foo bar baz" :end 5))
  (should be equal '("foo" "bar" "baz")
          (split-sequence-if-not #`(<= (char-code #\a) % (char-code #\z))
                             "foo bar baz" :key 'char-code)))

(deftest partition-with ()
  (should be null
          (partition-with nil '(1 2 3)))
  (should be equal '((1)) '(1)
          (partition-with '(1) '(1 2 3)))
  (should be equal '((1) (2) (3)) '(1 2 3)
          (partition-with '(3 2 1) '(1 2 3)))
  (should be equal '((1) (2) (3)) '(1 2 3)
          (partition-with '(1 2 3) '(1 2 3) :keys-sorted t))
  (should be equal '((1) (2 3)) '(1 3)
          (partition-with '(1 3) '(1 2 3) :test '<=))
  (should be equal '(nil (2)) '(1 3)
          (partition-with '(1 3) '(1 2 3) :key '1+))
  (should be equal '((3) (1)) '(3 1)
          (partition-with '(1 3) '(1 2 3) :ordering '>))
  (should be equalp #(#(1) #(3)) '(1 3)
          (partition-with '(1 3) '(1 2 3) :result-type 'vector)))

(deftest doindex ()
  (should be null
          (let (rez)
            (doindex (idx val () rez)
              (push (+ idx val) rez))))
  (should be equal '(4 2 0)
          (let (rez)
            (doindex (idx val '(0 1 2) rez)
              (push (+ idx val) rez)))))

(defun uniform-stat-test (fn seq)
  (reduce 'min (let ((stats (loop :repeat 1000
                               :collect (funcall fn (copy-seq seq)))))
                 (mapcar #`(count % stats :test 'equal)
                         (permutations (coerce seq 'list))))))

(defun member-equal (elt set)
  (member elt set :test 'equal))

(deftest shuffle ()
  (should be set-equal '(:foo :bar :baz)
          (shuffle '(:foo :bar :baz)))
  (should be member-equal '((:foo :bar :baz :quux)
                            (:foo :bar :quux :baz))
          (shuffle '(:foo :bar :baz :quux) :start 2))
  (should be equal '(:foo :bar :baz)
          (shuffle '(:foo :bar :baz) :end 1))
  (should be > 125
          (uniform-stat-test 'shuffle '(1 2 3))))

(deftest nshuffle ()
  (should be rutil:set-equal '(:foo :bar :baz)
          (nshuffle (list :foo :bar :baz)))
  (should be member-equal '((:foo :bar :baz :quux)
                            (:foo :bar :quux :baz))
          (nshuffle (list :foo :bar :baz :quux) :start 2))
  (should be equal '(:foo :bar :baz)
          (nshuffle (list :foo :bar :baz) :end 1))
  (should be > 125
          (uniform-stat-test 'nshuffle (list 1 2 3))))

(deftest rotate ()
  (should be equal '(2 3 1)
          (rotate (list 1 2 3) 2))
  (should be equal '(3 1 2)
          (rotate (list 1 2 3) -2)))

(deftest equal-lengths ()
  (should signal simple-error
          (equal-lengths 1))
  (should be true
          (equal-lengths 1 '(:foo)))
  (should be null
          (equal-lengths 1 '(:foo) '(:bar :baz)))
  (should be null
          (equal-lengths '(:foo) #(:bar) 1 "baz")))

(deftest length= ()
  (should be true
          (length= '(:foo) 1))
  (should be null
          (length= '(:foo) 2)))

(deftest last-elt ()
  (should signal type-error
          (last-elt ()))
  (should be eql :baz
          (last-elt '(:foo :bar :baz)))
  (should be char= #\z
          (last-elt "foo bar baz")))

(deftest group ()
  (should be null
          (group 1 ()))
  (should be equal '((:foo) (:bar))
          (group 1 '(:foo :bar)))
  (should be equal '((:foo :bar))
          (group 2 '(:foo :bar)))
  (should be equal '((:foo :bar) (:baz))
          (group 2 '(:foo :bar :baz)))
  (should be equal '((:foo :bar) (:baz :foo))
          (group 2 '(:foo :bar :baz :foo)))
  (should be equalp '(#(:foo :bar) #(:baz))
          (group 2 #(:foo :bar :baz)))
  (should be null
          (group 2 #()))
  (should be equal '("fo" "ob" "ar" "ba" "z")
          (group 2 "foobarbaz")))

(deftest keep ()
  (should be equal '("bar" "baz")
          (keep #\b '("foo" "bar" "baz") :key ^(? % 0))))

(deftest sum ()
  (should be = 6
          (sum '1+ (range 0 3)))
  (should be = 1
          (sum '+ (range 0 3) '(1))))

(deftest product ()
  (should be = 120
          (product '1+ (range 0 5)))
  (should be = 6
          (product '+ (range 0 3) '(1 1 1))))
