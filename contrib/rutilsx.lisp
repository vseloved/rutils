;;;;; Addition of symbols to RUTILSX
;;;;; see LICENSE file for permissions


(in-package #:rutilsx)

(rutils:eval-always
(defparameter *all-packages*
  '(#:rutilsx.generators #:rutilsx.readtable))

(dolist (p (cons '#:rtl *all-packages*))
  (rutils:re-export-symbols p '#:rutilsx))
)
