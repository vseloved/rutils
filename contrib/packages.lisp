;;;;; RUTILSX package definitions
;;;;; see LICENSE file for permissions

(cl:in-package :cl-user)


(defpackage #:rutilsx.generators
  (:documentation "Python-like generators (yield) support.")
  (:use :common-lisp #:rtl)
  (:export #:doing
           #:force
           #:generated
           #:generated-item
           #:yield
           #:yield-to))

(defpackage #:rutilsx.readtable
  (:documentation "Additional reader syntax support.")
  (:use :common-lisp #:rtl)
  (:export #:rutilsx-readtable))

(defpackage #:rutilsx
  (:documentation "The whole set of utilities in one package.")
  (:use :common-lisp))
