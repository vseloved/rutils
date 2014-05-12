;;; RUTILS system definition
;;; see LICENSE file for permissions

(asdf:defsystem #:rutils
  :name "Radical utilities"
  :version "3.1.5";(:read-file-line "version.txt")
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "3-clause MIT licence"
  :description
  "A collection of basic utilities for syntactic
   extension and basic data structure hadling,
   developed over the years of CL history by efforts of different individuals,
   and gathered under the unbrella of a hierarchy of packages
   which can be used on-demand."
  :depends-on (#:named-readtables)
  :components
  ((:module #:core
            :serial t
            :components ((:file "packages")
                         (:file "readtable")
                         (:file "core")
                         (:file "misc")
                         (:file "anaphora")
                         (:file "list")
                         (:file "string")
                         (:file "hash-table")
                         (:file "tree")
                         (:file "array")
                         (:file "sequence")
                         (:file "pair")
                         (:file "abbr")
                         (:file "rutils")))))


(defmethod perform ((o asdf:test-op)
                    (s (eql (asdf:find-system '#:rutils))))
  (asdf:load-system '#:rutils-test)
  (funcall (read-from-string "rutils.test:run-tests")))
