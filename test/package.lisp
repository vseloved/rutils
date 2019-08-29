;;;;; RUTILS test package definition
;;;;; see LICENSE file for permissions


(cl:in-package :cl-user)

(defpackage #:rutils.test
  (:documentation "Test package for RUTILS.")
  (:use :common-lisp #:rtl #:should-test)
  (:export #:run-tests))

(cl:in-package #:rutils.test)

(defun run-tests ()
  (with-standard-io-syntax
    (dolist (p rutils::*all-packages*)
      (st:test :package p))))
