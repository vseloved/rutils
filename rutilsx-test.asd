;;;;; RUTILSX-TEST system definition
;;;;; see LICENSE file for permissions


(asdf:defsystem #:rutilsx-test
  :name "Radical utilities contrib test suite."
  :version "1.0.0"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "3-clause MIT licence"
  :description
  "A test suite for RUTILSX built with SHOULD-TEST library."
  :depends-on (#:rutilsx #:should-test)
  :components
  ((:module #:test
            :components ((:file "packagex")
                         (:file "generic-test" :depends-on ("packagex"))
                         (:file "bind-test" :depends-on ("packagex"))
                         (:file "iter-test" :depends-on ("packagex"))))))
