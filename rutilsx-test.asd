;;;;; RUTILSX-TEST system definition
;;;;; see LICENSE file for permissions


(in-package :asdf-user)

(defsystem #:rutilsx-test
  :name "Radical utilities contrib test suite."
  :version (:read-file-line "version.txt" :at 1)
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "3-clause MIT licence"
  :description
  "A test suite for RUTILSX built with SHOULD-TEST library."
  :depends-on (#:rutilsx #:should-test)
  :components
  ((:module #:test
            :components ((:file "packagex")))))
