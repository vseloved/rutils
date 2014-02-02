;;;;; RUTILSX test package definition
;;;;; see LICENSE file for permissions


(cl:in-package :cl-user)

(defpackage #:rutilsx.test
  (:documentation "Test package for RUTILSX.")
  (:use :common-lisp #:rutilsx #:should-test)
  (:export #:run-tests))

(cl:in-package #:rutilsx.test)

(defun run-tests ()
  (dolist (p rutilsx::*all-packages*)
    (st:test :package p)))