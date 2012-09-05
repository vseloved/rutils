;;; see LICENSE file for permissions

(in-package #:reasonable-utilities.short)

(declaim (optimize (speed 3) (space 1) (debug 0)))

(defmacro 2nd (form)
  "(NTH-VALUE 1 FORM)"
  `(nth-value 1 ,form))

(abbr defpar defparameter)
(abbr ds-bind destructuring-bind)
(abbr mv-bind multiple-value-bind)

(declaim (inline fmt))
(defun fmt (format-string &rest args)
  (apply #'format nil format-string args))

(abbr get# gethash (key hashtable &optional default))
(abbr set# sethash)
(abbr rem# remhash)
(abbr take# takehash)

(abbr ht-vals hash-table-vals)
(abbr ht-keys hash-table-keys)
(abbr ht->alist hash-table-to-alist)
(abbr ht->plist hash-table-to-plist)
(abbr alist->ht hash-table-from-alist)
(abbr plist->ht hash-table-from-plist)
(abbr merge-hts merge-hash-tables)
(abbr print-ht print-hash-table)

(abbr mkeyw  ensure-keyword)
(abbr mklist ensure-list)
(abbr mksym  ensure-symbol)

(abbr w/instr with-input-from-string)
(abbr w/outstr with-output-to-string)
(abbr w/uniqs with-gensyms)

(abbr fn named-lambda)

(abbr filter remove-if-not)
