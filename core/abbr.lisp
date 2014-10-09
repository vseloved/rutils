;;; see LICENSE file for permissions


(in-package #:rutils.abbr)

(declaim (inline in#))


(abbr defpar defparameter)

(abbr ds-bind destructuring-bind)
(abbr mv-bind multiple-value-bind)

(abbr get# gethash (key hashtable &optional default))
(abbr set# sethash)
(abbr rem# remhash)
(abbr take# takehash)
(defun in# (key hash-table)
  "Check if KEY is present in HASH-TABLE."
  (2nd (get# key hash-table)))

(abbr ht-count hash-table-count)
(abbr ht-keys hash-table-keys)
(abbr ht-vals hash-table-vals)
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

(abbr m1 macroexpand-1)

(abbr make make-instance)

(abbr sub subseq)

(abbr split split-sequence)
(abbr split-if split-sequence-if)
(abbr split-if-not split-sequence-if-not)

(abbr := psetf)
(abbr :+ incf)
(abbr :- decf)


(cl:in-package #:rutils.anaphora)

(abbr aand and-it)
(abbr acond cond-it)
(abbr adowhile dowhile-it)
(abbr aif if-it)
(abbr awhen when-it)

(abbr and-bind and-let)
(abbr cond-bind cond-let)
(abbr dowhile-bind dowhile-let)
(abbr if-bind if-let)
(abbr when-bind when-let)
