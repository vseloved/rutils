;;; RUTILS system definition
;;; see LICENSE file for permissions


(in-package :asdf)

(defsystem #:rutils
  :name "Reasonable utilities"
  :version "2.8.0"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "3-clause MIT licence"
  :description "A reasonable collection of basic utilities for syntactic
extension and basic data-structures hadling, developed over the years of
CL history by efforts of different individuals, and gathered under
the unbrella of a hierarchy of packages, which can be used
selectively on demand."
  :depends-on (#:named-readtables)
  :serial t
  :components
  ((:module #:core
            :serial t
            :components ((:file "packages")
                         (:file "readtable")
                         (:file "symbol")
                         (:file "syntax")
                         (:file "anaphoric")
                         (:file "misc")
                         (:file "list")
                         (:file "string")
                         (:file "hash-table")
                         (:file "tree")
                         (:file "iter")
                         (:file "sequence")
                         (:file "short")
                         (:file "rutils")))))

(defsystem #:rutilsx
  :name "Unreasonable utilities"
  :version "0.0.2"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "3-clause MIT licence"
  :description "Some crazy extensions to rutils."
  :depends-on (#:rutils #:named-readtables)
  :serial t
  :components
  ((:module #:contrib
            :serial t
            :components ((:file "packages")
                         (:file "readtable")))))
