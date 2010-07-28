;;; RUTILS USER and USR umbrella packages' definition
;;; see LICENSE file for permissions

(in-package #:reasonable-utilities.user)

(rutils.core:eval-always
  (dolist (pkg '(#:rutils.core #:rutils.function #:rutils.control #:rutils.pkg
                 #:rutils.list #:rutils.string #:rutils.sequence #:rutils.seq
                 #:rutils.ana/it #:rutils.ana/bind #:rutils.iter #:rutils.bind
                 #:rutils.array #:rutils.hash-table #:rutils.genhash
                 #:rutils.tree #:rutils.number #:rutils.condition
                 #+closer-mpo #:rutils.object))
    (rutils.core:export-exported-symbols pkg '#:reasonable-utilities.user)))


(in-package #:reasonable-utilities.usr)

(rutils.core:eval-always
 (rutils.core:export-exported-symbols '#:reasonable-utilities.user
                                      '#:reasonable-utilities.usr)
 (rutils.core:export-exported-symbols '#:reasonable-utilities.short
                                      '#:reasonable-utilities.usr))

;;; end