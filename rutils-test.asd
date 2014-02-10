;;; RUTILS-TEST system definition
;;; see LICENSE file for permissions


(asdf:defsystem #:rutils-test
  :name "Radical utilities test suite."
  :version "1.1.0"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "3-clause MIT licence"
  :description
  "A test suite for RUTILS built with SHOULD-TEST library."
  :depends-on (#:rutils #:should-test)
  :components
  ((:module #:test
            :components ((:file "package")
                         (:file "core-test" :depends-on ("package"))
                         (:file "misc-test" :depends-on ("package"))
                         (:file "anaphora-test" :depends-on ("package"))
                         (:file "list-test" :depends-on ("package"))
                         (:file "string-test" :depends-on ("package"))
                         (:file "hash-table-test" :depends-on ("package"))
                         (:file "sequence-test" :depends-on ("package"))
                         (:file "tree-test" :depends-on ("package"))
                         (:file "pair-test" :depends-on ("package"))
                         (:file "array-test" :depends-on ("package"))))))
