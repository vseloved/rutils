;;;;; Test suite for RUTILS ANAPHORA
;;;;; see LICENSE file for permissions


(cl:in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)


(deftest if-it ()
  (should be equal 3
          (if-it (+ 1 2) it nil))
  (should be null
          (if-it nil it nil)))

(deftest when-it ()
  (should be equal 3
          (when-it (+ 1 2) it))
  (should be null
          (when-it nil 3)))

(deftest and-it ()
  (should be null
          (and-it))
  (should be equal 3
          (and-it (+ 1 2)))
  (should be equal 3
          (and-it (+ 1 2) it))
  (should be null
          (and-it (+ 1 2) (= it 2))))

(deftest dowhile-it ()
  (should print-to *standard-output* (format nil "~%T ~%T ")
          (let ((i 3))
            (dowhile-it (plusp (decf i))
              (print it)))))

(deftest cond-it ()
  (should be null
          (cond-it (nil it) (t nil)))
  (should be equal 1
          (cond-it (nil it) (1 it) (t t))))

(deftest if-let ()
  (should be equal 3
          (if-let (rez (+ 1 2)) rez nil))
  (should be null
          (if-let (rez nil) rez nil)))

(deftest when-let ()
  (should be equal 3
          (when-let (rez (+ 1 2)) rez))
  (should be null
          (when-let (rez nil) 3)))

(deftest and-let ()
  (should be = 3
          (and-let rez (+ 1 2) rez))
  (should be null
          (and-let rez (+ 1 2) (= rez 2))))

(deftest dowhile-let ()
  (should print-to *standard-output* (format nil "~%T ~%T ")
          (let ((i 3))
            (dowhile-let (rez (plusp (decf i)))
              (print rez)))))

(deftest cond-let ()
  (should be null
          (cond-let rez (nil rez) (t nil)))
  (should be equal 1
          (cond-let rez (nil rez) (1 rez) (t t))))
