;;;;; RUTILS package definitions
;;;;; see LICENSE file for permissions


(in-package :cl-user)


;;; core

(defpackage #:rutils.readtable
  (:documentation "Literate syntax definitions.")
  (:use :common-lisp #:named-readtables)
  (:export #:rutils-readtable
           #:standard-readtable

           #:+default-opts+

           #:|@-reader|
           #:|#h-reader|
           #:|#v-reader|
           #:|#{-reader|
           #:|#`-reader|
           #:|#/-reader|
           #:%
           #:%%))

(defpackage #:rutils.symbol
  (:documentation "Core symbol-manipulation utilities.")
  (:use :common-lisp #:rutils.readtable)
  (:export #:abbr
           #:eval-always
           #:ensure-keyword
           #:ensure-symbol
           #:make-gensym-list
           #:mkeyw
           #:mksym
           #:package-symbols
           #:package-internal-symbols
           #:package-external-symbols
           #:re-export-symbols
           #:rutils-style-warning
           #:with-gensyms
           #:with-unique-names
           #:w/uniqs))

(defpackage #:rutils.misc
  (:documentation "Assorted small utilities.")
  (:use :common-lisp #:rutils.readtable #:rutils.symbol)
  (:export #:2nd
           #:and2
           #:case-failure
           #:coercef
           #:cswitch
           #:dcase
           #:dccase
           #:decase
           #:eswitch
           #:fn
           #:less
           #:more
           #:multiple-value-prog2
           #:named-lambda
           #:not-less
           #:not-more
           #:once-only
           #:or2
           #:pcase
           #:pccase
           #:pecase
           #:re-setf
           #:switch
           #:true
           #:void
           #:xor
           #:xor2
           #:->
           #:->>
           #:=>))

(defpackage #:rutils.anaphora
  (:documentation "Anaphoric control constructs.")
  (:use :common-lisp #:rutils.readtable #:rutils.symbol #:rutils.misc)
  (:export #:aand
           #:acond
           #:adowhile
           #:aif
           #:awhen
           #:and-bind
           #:and-it
           #:and-let
           #:cond-bind
           #:cond-it
           #:cond-let
           #:dowhile-bind
           #:dowhile-it
           #:dowhile-let
           #:if-bind
           #:if-it
           #:if-let
           #:it
           #:when-bind
           #:when-it
           #:when-let))


;;; data-structures

(defpackage #:rutils.list
  (:use :common-lisp #:rutils.readtable #:rutils.symbol #:rutils.misc)
  (:documentation "List utilities.")
  (:export #:alist
           #:alistp
           #:alist-to-plist
           #:appendf
           #:assoc1
           #:atomize
           #:butlast2
           #:concat
           #:dcons
           #:delete-from-plist
           #:dlist
           #:dlistp
           #:doplist
           #:dyadic
           #:ensure-list
           #:flatten
           #:flat-map
           #:interleave
           #:interpose
           #:last1
           #:listcase
           #:remove-idx
           #:mapindex
           #:mapcat
           #:mapcanindex
           #:maptimes
           #:mappend
           #:mklist
           #:nconcf
           #:nreversef
           #:nunionf
           #:permutations
           #:plistp
           #:plist-to-alist
           #:range
           #:remove-from-plist
           #:reversef
           #:set-equal
           #:single
           #:take
           #:tryadic
           #:unionf
           #:with-output-to-list
           #:zip
           #:zip-with
           #:zip*
           #:zip*-with))

(defpackage #:rutils.string
  (:use :common-lisp #:rutils.readtable #:rutils.symbol #:rutils.misc
        #:rutils.anaphora #:rutils.list)
  (:documentation "String utilities.")
  (:export #:blankp
           #:dolines
           #:ends-with
           #:fmt
           #:last-char
           #:read-file
           #:slurp
           #:split-string
           #:starts-with
           #:strcat
           #:strjoin
           #:string-designator
           #:substr
           #:with-out-file
           #:white-char-p))

(defpackage #:rutils.hash-table
  (:use :common-lisp #:rutils.readtable #:rutils.symbol #:rutils.misc
        #:rutils.string #:rutils.list)
  (:documentation "Hash-table utilities.")
  (:export #:copy-hash-table
           #:dotable
           #:get#
           #:getset#
           #:getsethash
           #:hash-table-keys
           #:hash-table-vals
           #:ht-keys
           #:ht-vals
           #:ht-count
           #:in#
           #:merge-hash-tables
           #:merge-hash-tables-with
           #:merge-hts
           #:merge-hts-with
           #:hash-table-from-alist
           #:hash-table-from-plist
           #:hash-table-to-alist
           #:hash-table-to-plist
           #:ht->plist
           #:ht->alist
           #:plist->ht
           #:alist->ht
           #:print-hash-table
           #:print-ht
           #:p#
           #:rem#
           #:sethash
           #:set#
           #:takehash
           #:take#
           #-(or abcl ecl) #:toggle-print-hash-table
           #:with-keys))

(defpackage #:rutils.hash-set
  (:use :common-lisp #:rutils.readtable #:rutils.symbol #:rutils.misc
        #:rutils.hash-table)
  (:documentation "Hash-set utilities.")
  (:export #:hash-set
           #:emptyp#
           #:add#
           #:inter#
           #:union#
           #:diff#
           #:xor#))

(defpackage #:rutils.array
  (:use :common-lisp #:rutils.readtable #:rutils.symbol #:rutils.misc
        #:rutils.anaphora)
  (:documentation "Array utilities.")
  (:export #:array-index
           #:array-length
           #:copy-array
           #:dovec
           #:slice
           #:vec))

(defpackage #:rutils.sequence
  (:use :common-lisp #:rutils.readtable #:rutils.symbol #:rutils.misc
        #:rutils.list #:rutils.hash-table #:rutils.array)
  (:documentation "Sequence utilities, including SPLIT-SEQUENCE.")
  (:export #:deletef
           #:doindex
           #:emptyp
           #:equal-lengths
           #:group
           #:keep
           #:keep-if
           #:keep-if-not
           #:last-elt
           #:length=
           #:map*
           #:nshuffle
           #:partition-with
           #:product
           #:removef
           #:rotate
           #:safe-sort
           #:sequence-of-length
           #:shuffle
           #:split
           #:split-if
           #:split-if-not
           #:split-sequence
           #:split-sequence-if
           #:split-sequence-if-not
           #:sum))

(defpackage #:rutils.pair
  (:use :common-lisp #:rutils.readtable #:rutils.symbol #:rutils.misc
        #:rutils.list)
  (:documentation "Generic pair data structure.")
  (:export #:ht->pairs
           #:lt
           #:make-pair
           #:pair
           #:pair-left
           #:air-right
           #:pairs
           #:pairs->ht
           #:rt
           #:with-pair))

(defpackage #:rutils.tree
  (:use :common-lisp #:rutils.readtable #:rutils.symbol #:rutils.misc
        #:rutils.anaphora #:rutils.list)
  (:documentation "Tree utilities.")
  (:export #:doleaves
           #:dotree
           #:mapleaves
           #:maptree
           #:tree-depth
           #:tree-size))

(defpackage #:rutils.kv
  (:use :common-lisp #:rutils.readtable #:rutils.symbol #:rutils.misc
        #:rutils.hash-table #:rutils.list)
  (:documentation "Key-value utilities.")
  (:export #:eq-test
           #:dokv
           #:mapkv
           #:keys
           #:kvs
           #:vals))


;;; generic stuff

(defpackage #:rutils.generic
  (:use :common-lisp #:rutils.readtable #:rutils.symbol #:rutils.misc
        #:rutils.pair #:rutils.list #:rutils.hash-table #:rutils.sequence)
  (:documentation "Generic access, copy, tally.")
  (:export #:copy
           #:tally
           #:generic-elt
           #:generic-elt-error
           #:generic-elt-error-obj
           #:generic-elt-error-key
           #:smart-slot-value
           #:?))

(defpackage #:rutils.bind
  (:documentation "Unified extensible bind operator.")
  (:use :common-lisp #:rutils.readtable #:rutils.symbol #:rutils.misc
        #:rutils.list #:rutils.generic)
  (:export #:bind
           #:bind-dispatch
           #:with
           #:@
           #:_))

(defpackage #:rutils.iter
  (:documentation "Iterate macro with keywords for clauses.")
  (:use :common-lisp #:rutils.readtable #:rutils.symbol #:rutils.misc
        #:rutils.list #:rutils.string)
  (:export #:iter
           #:iter-version
           #:declare-variables
           #:defclause
           #:defclause-driver
           #:defclause-sequence
           #:defmacro-clause
           #:defmacro-driver
           #:display-iter-clauses
           #:dsetq))


;;; special packages

(defpackage #:rutils.abbr
  (:use :common-lisp #:rutils.symbol #:rutils.misc #:rutils.list)
  (:documentation "Abbreviations of some common utilities with long names.")
  (:export #:call
           #:defpar
           #:ds-bind
           #:flet*
           #:just
           #:m1
           #:make
           #:mv-bind
           #:pushx
           #:sub
           #:w/instr
           #:w/outstr
           #:w/uniqs))

(defpackage #:rutils
  (:documentation "The whole set of utilities in one package.")
  (:use :common-lisp))

(defpackage #:rutils-user
  (:documentation "CL-USER + RUTILS.")
  (:use :cl-user #:rutils))

(defpackage #:rtl
  (:documentation "The whole set of utilities + abbreviations in one package.")
  (:use :common-lisp)
  (:nicknames #:rutil))

(defpackage #:rtl-user
  (:documentation "CL-USER + RTL.")
  (:use :common-lisp :cl-user #:rtl))


(in-package #:rutils.readtable)

(defvar +default-opts+
  #-rutils-dev '(optimize (speed 3) (space 1))
  #+rutils-dev '(optimize (debug 3))
  "Default optimization settings for RUTILS.")
