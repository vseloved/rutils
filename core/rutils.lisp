;;;;; Addition of symbols to RUTILS & RUTIL
;;;;; see LICENSE file for permissions


(in-package #:rutils)

(rutils.symbol:eval-always

  (defparameter *all-packages*
    '(#:rutils.readtable #:rutils.symbol #:rutils.misc
      #:rutils.generic #:rutils.bind #:rutils.anaphora #:rutils.iter
      #:rutils.pair #:rutils.list #:rutils.tree
      #:rutils.hash-table #:rutils.hash-set #:rutils.kv
      #:rutils.sequence #:rutils.array #:rutils.string))

  (dolist (p *all-packages*)
    (rutils.symbol:re-export-symbols p '#:rutils))
)


(in-package #:rtl)


(rutils.symbol:eval-always

  (dolist (p '(#:rutils.abbr #:rutils))
    (rutils.symbol:re-export-symbols p '#:rtl))
)
