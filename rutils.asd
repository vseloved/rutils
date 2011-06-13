;;; RUTILS system definition
;;; see LICENSE file for permissions


(in-package :asdf)

(defsystem #:rutils
  :name "Reasonable utilities"
  :version "0.3.2"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "3-clause MIT licence"
  :description "A reasonable collection of basic utilities for syntactic ~
extension and basic data-structures hadling, developed over the years of ~
CL history by efforts of different individuals, and gathered under ~
the unbrella of a hierarchy of packages, which can be used ~
selectively on demand.
Includes user-space CDR's (see http://cdr.eurolisp.org)"
  :depends-on (:closer-mop)
  :serial t
  :components ((:file "packages")
               (:file "impl")
               (:file "core")
               (:file "short")
               (:file "pkg")
               (:file "function")
               (:file "control")
               (:file "list")
               (:file "string")
               (:file "anaphoric")
               (:file "bind")
               (:file "iter")
               (:file "array")
               (:file "hash-table")
               (:file "genhash")
               (:file "seq")
               (:file "number")
               (:file "sequence")
               (:file "tree")
               #+:closer-mop (:file "object")
               (:file "condition")
               (:file "experimental")
               (:file "user")
               (:file "rutils")))

;; TODO: add test package

;;; end
