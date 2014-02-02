;;; RUTILS system definition
;;; see LICENSE file for permissions

(asdf:defsystem #:rutilsx
  :name "More radical utilities"
  :version "1.0.0";(:read-file-line "version.txt" 2)
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "3-clause MIT licence"
  :description
  "The most radical utilities."
  :depends-on (#:named-readtables #:rutils)
  :components
  ((:module #:contrib
            :serial t
            :components ((:file "packages")
                         (:file "generic")
                         (:file "bind")
                         (:file "iter")
                         (:file "rutilsx")))))


(defmethod perform ((o asdf:test-op)
                    (s (eql (asdf:find-system '#:rutilsx))))
  (asdf:load-system '#:rutilsx-test)
  (funcall (read-from-string "rutilsx.test:run-tests")))