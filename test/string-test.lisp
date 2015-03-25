;;;;; Test suite for RUTILS STRING
;;;;; see LICENSE file for permissions


(cl:in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)


(deftest strcat ()
  (should be null
          (strcat ()))
  (should be string= "foo"
          (strcat "foo"))
  (should be string= "foobar"
          (strcat "foo" "bar")))

(deftest strjoin ()
  (should be null
          (strjoin #\Space ()))
  (should be string= "foo bar"
          (strjoin #\Space '("foo" "bar")))
  (should be string= "foo bar"
          (strjoin " " '("foo" "bar"))))

(deftest blankp ()
  (should be null
          (blankp nil))
  (should be true
          (blankp ""))
  (should be null
          (blankp "foo")))

(deftest split-string ()
  (should be equal '("foo")
          (split-string "foo"))
  (should be equal '("foo" "bar")
          (split-string "foo bar")))

(deftest substr ()
  (should be string= "foo"
          (substr "foo" 0))
  (should be string= "oo"
          (substr "foo" 1))
  (should be string= "foo"
          (substr "foo" 0 3))
  (should be string= "fo"
          (substr "foo" 0 -1))
  (should be string= "o"
          (substr "foo" -1)))

(deftest starts-with ()
  (should be null
          (starts-with "foo" ""))
  (should be true
          (starts-with "" "foo"))
  (should be null
          (starts-with "foo" "fo"))
  (should be true
          (starts-with "foo" "foo"))
  (should be true
          (starts-with "foo" "foobar"))
  (should be true
          (starts-with "foo" "FOO" :test 'string-equal)))

(deftest ends-with ()
  (should be null
          (ends-with "foo" ""))
  (should be true
          (ends-with "" "foo"))
  (should be null
          (ends-with "foo" "fo"))
  (should be true
          (ends-with "foo" "foo"))
  (should be true
          (ends-with "bar" "foobar"))
  (should be true
          (ends-with "foo" "FOO" :test 'string-equal)))

(deftest cutsym ()
  (should be eq 'foo
          (cutsym 'foo 0))
  (should be eq 'oo
          (cutsym 'foo 1))
  (shoulg be eq 'foo
          (cutsym 'foo 0 3))
  (should be eq 'fo
          (cutysym 'foo 0 -1))
  (should be eq 'o
          (cutsym 'o)))

(deftest symbol-starts-with ()
  (should be null
          (symbol-starts-with 'foo '||))
  (should be true
          (symbol-starts-with '|| 'foo))
  (should be null
          (symbol-starts-with 'foo 'fo))
  (should be true
          (symbol-starts-with 'foo 'foo))
  (should be true
          (symbol-starts-with 'foo 'foobar))
  (should be true
          (symbol-starts-with "FOO" )))

(deftest symbol-ends-with ()
  (should be null
          (symbol-ends-with 'foo '||))
  (should be true
          (symbol-ends-with '|| 'foo))
  (should be null
          (symbol-ends-with 'foo 'fo))
  (should be true
          (symbol-ends-with 'foo 'foo))
  (should be true
          (symbol-ends-with 'bar 'foobar))
  (should be true
          (symbol-ends-with "BAR" 'foobar)))

(deftest dolines ()
  (should be equal '("world" "hello")
          (let ((file (fmt "/tmp/~A" (gensym)))
                rez)
            (with-out-file (out file)
              (format out "hello~%world~%"))
            (dolines (line file)
              (push line rez))
            (delete-file file)
            rez)))

(deftest with-out-file/read-file ()
  (should be string= "hello"
          (let ((file (fmt "/tmp/~A" (gensym))))
            (with-out-file (out file)
              (format out "hello"))
            (prog1 (read-file file)
              (delete-file file)))))

(deftest last-char ()
  (should be null
          (last-char ""))
  (should be char= #\o
          (last-char "foo")))

(deftest fmt ()
  (should be string= "foo"
          (fmt "~A" "foo")))
