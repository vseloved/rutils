;;;;; Addition of symbols to RUTILS & RUTIL
;;;;; see LICENSE file for permissions


(in-package #:rutils)

(rutils.core:eval-always
(defparameter *all-packages*
  '(#:rutils.readtable #:rutils.core #:rutils.misc #:rutils.anaphora
    #:rutils.list #:rutils.string #:rutils.sequence
    #:rutils.hash-table #:rutils.tree #:rutils.pair #:rutils.array))

(dolist (p *all-packages*)
  (rutils.core:re-export-symbols p '#:rutils))
)


(in-package #:rutil)


(rutils.core:eval-always
(dolist (p '(#:rutils.abbr #:rutils))
  (rutils.core:re-export-symbols p '#:rutil))
)
