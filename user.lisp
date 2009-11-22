;;; RUTILS USER and USR umbrella packages' definition
;;; see LICENSE file for permissions

(in-package "REASONABLE-UTILITIES.USER")

(eval-always
  (dolist (pkg '("RUTILS.CORE" "RUTILS.FUNCTION" "RUTILS.CONTROL" "RUTILS.PKG"
                 "RUTILS.LIST" "RUTILS.STRING" "RUTILS.SEQUENCE" "RUTILS.SEQ"
                 "RUTILS.ANA/IT" "RUTILS.ANA/BIND" "RUTILS.ITER" "RUTILS.BIND"
                 "RUTILS.ARRAY" "RUTILS.HASH-TABLE" "RUTILS.GENHASH"
                 "RUTILS.TREE" "RUTILS.NUMBER" "RUTILS.CONDITION"))
    (export-exported-symbols pkg "REASONABLE-UTILITIES.USER")))


(in-package "REASONABLE-UTILITIES.USR")

(export-exported-symbols "REASONABLE-UTILITIES.USER"
                         "REASONABLE-UTILITIES.USR")
(export-exported-symbols "REASONABLE-UTILITIES.SHORT"
                         "REASONABLE-UTILITIES.USR")

;;; end