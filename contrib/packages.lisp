;;;;; RUTILSX package definitions
;;;;; see LICENSE file for permissions

(cl:in-package :cl-user)


(defpackage #:rutilsx.generic
  (:use :common-lisp #:rutil)
  (:documentation "Generic access to pairs, sequences and tables.")
  (:export #:eq-test
           #:generic-elt
           #:keys
           #:kvs
           #:maptab
           #:pairs
           #:vals
           #:?))

(defpackage #:rutilsx.iter
  (:documentation "Iterate macro with keywords for clauses.")
  (:use :common-lisp #:rutil)
  (:export #:iter
           #:iter-version
           #:declare-variables
           #:defclause
           #:defclause-driver
           #:defclause-sequence
           #:defmacro-clause
           #:defmacro-driver
           #:display-iter-clauses))

(defpackage #:rutilsx.bind
  (:documentation "Unified extensible bind operator.")
  (:use :common-lisp #:rutil)
  (:export #:bind
           #:bind-dispatch))

(defpackage #:rutilsx.threading
  (:documentation "Clojure-like threading macros.")
  (:use :common-lisp #:rutil)
  (:export #:->
           #:->>))

(defpackage #:rutilsx
  (:documentation "The whole set of utilities in one package.")
  (:use :common-lisp))
