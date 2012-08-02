;;; see LICENSE file for permissions

(cl:in-package :cl-user)


(defpackage #:reasonable-utilities.readtable
  (:nicknames #:rutils.readtable)
  (:documentation "Readtable definition.")
  (:use :common-lisp #:named-readtables)
  (:export #:*print-literally*
           #:println
           #:rutils-readtable
           #:%
           #:%%))

(defpackage #:reasonable-utilities.symbol
  (:nicknames #:rutils.symbol)
  (:documentation "Symbol manipulation utilities.")
  (:use :common-lisp #:rutils.readtable)
  (:export #:abbr
           #:ensure-keyword
           #:ensure-symbol
           #:eval-always
           #:once-only
           #:package-symbols
           #:package-external-symbols
           #:re-export-symbols
           #:with-gensyms
           #:with-unique-names))

(defpackage #:reasonable-utilities.syntax
  (:nicknames #:rutils.syntax)
  (:documentation "Syntax extensions.")
  (:use :common-lisp #:rutils.readtable #:rutils.symbol)
  (:export #:bind
           #:bind-dispatch
           #:dcase
           #:dccase
           #:decase
           #:multiple-value-prog2
           #:pcase
           #:pccase
           #:pecase
           #:switch
           #:cswitch
           #:eswitch
           #:dotable))

(defpackage #:reasonable-utilities.anaphoric/a
  (:nicknames #:rutils.anaphoric/a #:rutils.ana/a)
  (:documentation "Anaphoric control constructs with a- prefix and
automatic binding of test to it.")
  (:use :common-lisp #:rutils.readtable #:rutils.symbol)
  (:export #:aand
           #:acond
           #:adowhile
           #:aif
           #:awhen
           #:it))

(defpackage #:reasonable-utilities.anaphoric/it
  (:nicknames #:rutils.anaphoric/it #:rutils.ana/it)
  (:documentation "Anaphoric control constructs with -it suffix and
automatic binding of test to it.")
  (:use :common-lisp #:rutils.readtable #:rutils.symbol)
  (:export #:and-it
           #:cond-it
           #:dowhile-it
           #:if-it
           #:when-it
           #:it))

(defpackage #:reasonable-utilities.anaphoric/let
  (:nicknames #:rutils.anaphoric/let #:rutils.ana/let)
  (:documentation "Anaphoric control constructs with -let suffix.")
  (:use :common-lisp #:rutils.readtable #:rutils.symbol)
  (:export #:and-let
           #:cond-let
           #:dowhile-let
           #:if-let
           #:when-let))

(defpackage #:reasonable-utilities.anaphoric/bind
  (:nicknames #:rutils.anaphoric/bind #:rutils.ana/bind)
  (:documentation "Anaphoric control constructs with -bind suffix.")
  (:use :common-lisp #:rutils.symbol #:rutils.readtable)
  (:export #:and-bind
           #:cond-bind
           #:dowhile-bind
           #:if-bind
           #:when-bind))

(defpackage #:reasonable-utilities.misc
  (:nicknames #:rutils.misc)
  (:documentation "Basic control structures and predicates.")
  (:use :common-lisp #:rutils.readtable #:rutils.symbol)
  (:export #:and2
           #:array-index
           #:array-length
           #:coercef
           #:less
           #:more
           #:named-lambda
           #:not-less
           #:not-more
           #:or2
           #:xor
           #:xor2))

(defpackage #:reasonable-utilities.list
  (:nicknames #:rutils.list)
  (:use :common-lisp #:rutils.readtable #:rutils.symbol)
  (:documentation "List utilities.")
  (:export #:alistp
           #:alist-to-plist
           #:appendf
           #:assoc1
           #:atomize
           #:butlast2
           #:delete-from-plist
           #:dyadic
           #:ensure-list
           #:flatten
           #:group
           #:interleave
           #:last1
           #:nconcf
           #:nreversef
           #:nunionf
           #:plistp
           #:plist-to-alist
           #:remove-from-plist
           #:reversef
           #:set-equal
           #:single
           #:take
           #:tryadic
           #:unionf
           #:with-output-to-list))

(defpackage #:reasonable-utilities.string
  (:nicknames #:rutils.string)
  (:use :common-lisp #:rutils.symbol #:rutils.readtable
        #:rutils.ana/it #:rutils.list)
  (:documentation "String utilities.")
  (:export #:blankp
           #:dolines
           #:ends-with
           #:read-file
           #:slurp
           #:split-string
           #:starts-with
           #:strcat
           #:strcat_
           #:strjoin
           #:string-designator
           #:substr
           #:white-char-p))

(defpackage #:reasonable-utilities.hash-table
  (:nicknames #:rutils.hash-table)
  (:use :common-lisp #:rutils.symbol #:rutils.readtable
        #:rutils.string #:rutils.list)
  (:documentation "Hash-table utilities.")
  (:export #:copy-hash-table
           #:hash-table-keys
           #:hash-table-vals
           #:merge-hash-tables
           #:hash-table-from-alist
           #:hash-table-from-plist
           #:hash-table-to-alist
           #:hash-table-to-plist
           #:print-hash-table
           #:sethash
           #:takehash))

(defpackage #:reasonable-utilities.tree
  (:nicknames #:rutils.tree)
  (:use :common-lisp #:rutils.readtable #:rutils.symbol)
  (:documentation "Tree utilities.")
  (:export #:dotree
           #:maptree
           #:tree-size))

(defpackage #:reasonable-utilities.short
  (:nicknames #:rutils.short)
  (:use :common-lisp #:rutils.readtable
        #:rutils.symbol #:rutils.list #:rutils.hash-table #:rutils.misc)
  (:documentation "Short variants of some common utilities with very long names.")
  (:export #:2nd
           #:defpar
           #:ds-bind
           #:filter
           #:fmt
           #:fn
           #:get#
           #:ht-keys
           #:ht-vals
           #:ht->plist
           #:ht->alist
           #:plist->ht
           #:alist->ht
           #:merge-hts
           #:print-ht
           #:mkeyw
           #:mklist
           #:mksym
           #:mv-bind
           #:rem#
           #:set#
           #:take#
           #:w/instr
           #:w/outstr
           #:w/uniqs))

(defpackage #:reasonable-utilities.iter
  (:nicknames #:rutils.iter)
  (:documentation "Iterate macro, using keywords for clauses.")
  (:use :common-lisp #:rutils.readtable #:rutils.symbol
        #:rutils.ana/it #:rutils.short #:rutils.string)
  (:export #:iter
           #:iter-version
           #:declare-variables
           #:defclause
           #:defclause-driver
           #:defclause-sequence
           #:defmacro-clause
           #:defmacro-driver
           #:display-iter-clauses))

(defpackage #:reasonable-utilities.sequence
  (:nicknames #:rutils.sequence)
  (:use :common-lisp #:rutils.symbol #:rutils.readtable
        #:rutils.misc #:rutils.iter)
  (:documentation "Sequence utilities, including SPLIT-SEQUENCE.")
  (:export #:deletef
           #:doindex
           #:emptyp
           #:equal-lengths
           #:length=
           #:partition-with
           #:removef
           #:rotate
           #:sequence-of-length
           #:shuffle
           #:split-sequence
           #:split-sequence-if
           #:split-sequence-if-not))


(defpackage #:reasonable-utilities
  (:nicknames #:rutils)
  (:documentation "The whole set of core reasonable utilities, excluding short.")
  (:use :common-lisp #:rutils.readtable))

(defpackage #:rutil
  (:documentation "rutils + rutils.short")
  (:use :common-lisp #:rutils.readtable))
