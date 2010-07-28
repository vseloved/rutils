;;; RUTILS -everything- umbrella package
;;; see LICENSE file for permissions

(in-package #:rutils)

(rutils.core:eval-always
  (dolist (pkg '(#:rutils.user #:rutils.short #:rutils.ana/a
                 #:rutils.ana/let #:rutils.experimental))
    (rutils.core:export-exported-symbols pkg '#:rutils)))

;;; end