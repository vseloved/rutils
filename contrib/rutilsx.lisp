;;;;; Addition of symbols to RUTILSX
;;;;; see LICENSE file for permissions


(in-package #:rutilsx)

(rutils.core:eval-always
(defparameter *all-packages*
  '(#:rutilsx.iter #:rutilsx.generators #:rutilsx.readtable))

(dolist (p (cons '#:rtl *all-packages*))
  (rutils.core:re-export-symbols p '#:rutilsx))
)
