;;; RUTILS system definition
;;; see LICENSE file for permissions


(in-package :asdf-user)

(defsystem #:rutils
  :name "Radical utilities"
  :version (:read-file-line "version.txt" :at 0)
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "3-clause MIT licence"
  :description
  "A collection of basic utilities for syntactic
   extension and basic data structure handling,
   developed over the years of CL history by efforts of different individuals,
   and gathered under the unbrella of a hierarchy of packages
   which can be used on-demand."
  :depends-on (#:named-readtables #:closer-mop)
  :components
  ((:module #:core
    :serial t
    :components ((:file "packages")
                 (:file "readtable")
                 (:file "symbol")
                 (:file "misc")
                 (:file "anaphora")
                 (:file "list")
                 (:file "string")
                 (:file "hash-table")
                 (:file "hash-set")
                 (:file "tree")
                 (:file "array")
                 (:file "sequence")
                 (:file "pair")
                 (:file "kv")
                 (:file "generic")
                 (:file "bind")
                 (:file "iter")
                 (:file "abbr")
                 (:file "rutils")))))


(defmethod perform ((o asdf:test-op)
                    (s (eql (asdf:find-system '#:rutils))))
  (asdf:load-system '#:rutils-test)
  (funcall (read-from-string "rutils.test:run-tests")))
