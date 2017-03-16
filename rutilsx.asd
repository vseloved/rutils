;;; RUTILS system definition
;;; see LICENSE file for permissions

(asdf:defsystem #:rutilsx
  :name "More radical utilities"
  :version (:read-file-line "version.txt" :at 1)
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "3-clause MIT licence"
  :description
  "The most radical utilities."
  :depends-on (#:named-readtables #:rutils #:closer-mop)
  :components
  ((:module #:contrib
    :components ((:file "packages")
                 (:file "readtable" :depends-on ("packages"))
                 (:file "generic" :depends-on ("packages"))
                 (:file "bind" :depends-on ("packages"))
                 (:file "iter" :depends-on ("packages"))
                 (:file "threading" :depends-on ("packages"))
                 (:file "generators" :depends-on ("packages"))
                 (:file "rutilsx" :depends-on ("packages"))))))


(defmethod perform ((o asdf:test-op)
                    (s (eql (asdf:find-system '#:rutilsx))))
  (asdf:load-system '#:rutilsx-test)
  (funcall (read-from-string "rutilsx.test:run-tests")))
