;;;;; RUTILS test package definition
;;;;; see LICENSE file for permissions


(cl:in-package :cl-user)

(defpackage #:rutils.test
  (:documentation "Test package for RUTILS.")
  (:use :common-lisp #:rutil #:should-test)
  (:export #:run-tests))

(cl:in-package #:rutils.test)

(defun run-tests ()
  (dolist (p rutils::*all-packages*)
    (st:test :package p)))