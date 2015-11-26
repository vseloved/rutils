;;;;; RUTILS package definitions
;;;;; see LICENSE file for permissions


(cl:in-package :cl-user)


(defpackage #:rutils.readtable
  (:documentation "Literate syntax definitions.")
  (:use :common-lisp #:named-readtables)
  (:export #:rutils-readtable

           #:+default-opts+

           #:|#h-reader|
           #:|#v-reader|
           #:|#{-reader|
           #:|#`-reader|
           #:|#/-reader|
           #:%
           #:%%))

(defpackage #:rutils.core
  (:documentation "Core utilities.")
  (:use :common-lisp #:rutils.readtable)
  (:export #:abbr
           #:eval-always
           #:ensure-keyword
           #:ensure-symbol
           #:make-gensym-list
           #:package-symbols
           #:package-internal-symbols
           #:package-external-symbols
           #:re-export-symbols
           #:rutils-style-warning
           #:with-gensyms
           #:with-unique-names))

(defpackage #:rutils.misc
  (:documentation "Assorted small utilities.")
  (:use :common-lisp #:rutils.readtable #:rutils.core)
  (:export #:2nd
           #:and2
           #:case-failure
           #:coercef
           #:cswitch
           #:dcase
           #:dccase
           #:decase
           #:eswitch
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
           #:xor2))

(defpackage #:rutils.anaphora
  (:documentation "Anaphoric control constructs.")
  (:use :common-lisp #:rutils.readtable #:rutils.core #:rutils.misc)
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


;;; basic data-structures

(defpackage #:rutils.list
  (:use :common-lisp #:rutils.readtable #:rutils.core #:rutils.misc)
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
           #:group
           #:interleave
           #:interpose
           #:last1
           #:listcase
           #:remove-idx
           #:mapindex
           #:mapcanindex
           #:maptimes
           #:mappend
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
  (:use :common-lisp #:rutils.readtable #:rutils.core #:rutils.misc
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
  (:use :common-lisp #:rutils.readtable #:rutils.core #:rutils.misc
        #:rutils.string #:rutils.list)
  (:documentation "Hash-table utilities.")
  (:export #:copy-hash-table
           #:dotable
           #:getsethash
           #:hash-table-keys
           #:hash-table-vals
           #:merge-hash-tables
           #:hash-table-from-alist
           #:hash-table-from-plist
           #:hash-table-to-alist
           #:hash-table-to-plist
           #:print-hash-table
           #:sethash
           #:takehash
           #:toggle-print-hash-table
           #:with-keys))

(defpackage #:rutils.array
  (:use :common-lisp #:rutils.readtable #:rutils.core #:rutils.misc
        #:rutils.anaphora)
  (:documentation "Array utilities.")
  (:export #:array-index
           #:array-length
           #:dovec
           #:slice))

(defpackage #:rutils.sequence
  (:use :common-lisp #:rutils.readtable #:rutils.core #:rutils.misc
        #:rutils.list #:rutils.hash-table #:rutils.array)
  (:documentation "Sequence utilities, including SPLIT-SEQUENCE.")
  (:export #:deletef
           #:doindex
           #:emptyp
           #:equal-lengths
           #:last-elt
           #:length=
           #:nshuffle
           #:partition-with
           #:removef
           #:rotate
           #:safe-sort
           #:sequence-of-length
           #:shuffle
           #:split-sequence
           #:split-sequence-if
           #:split-sequence-if-not))

(defpackage #:rutils.pair
  (:use :common-lisp #:rutils.readtable #:rutils.core #:rutils.misc
        #:rutils.list)
  (:documentation "Generic pair data structure.")
  (:export #:ht->pairs
           #:lt
           #:make-pair
           #:pair
           #:pairs
           #:pairs->ht
           #:rt
           #:with-pair))

(defpackage #:rutils.tree
  (:use :common-lisp #:rutils.readtable #:rutils.core #:rutils.misc
        #:rutils.anaphora #:rutils.list)
  (:documentation "Tree utilities.")
  (:export #:doleaves
           #:dotree
           #:mapleaves
           #:maptree
           #:tree-depth
           #:tree-size))


;;; special packages

(defpackage #:rutils.abbr
  (:use :common-lisp #:rutils.core #:rutils.misc
        #:rutils.sequence #:rutils.list #:rutils.hash-table)
  (:documentation "Abbreviations of some common utilities with long names.")
  (:export #:defpar
           #:ds-bind
           #:filter
           #:flat-map
           #:fn
           #:get#
           #:getset#
           #:ht-count
           #:ht-keys
           #:ht-vals
           #:ht->plist
           #:ht->alist
           #:in#
           #:plist->ht
           #:alist->ht
           #:merge-hts
           #:print-ht
           #:p#
           #:m1
           #:make
           #:mapcat
           #:mkeyw
           #:mklist
           #:mksym
           #:mv-bind
           #:rem#
           #:set#
           #:split
           #:split-if
           #:split-if-not
           #:sub
           #:take#
           #:w/instr
           #:w/outstr
           #:w/uniqs))

(defpackage #:rutils
  (:documentation "The whole set of utilities in one package.")
  (:use :common-lisp))

(defpackage #:rutil
  (:documentation "The whole set of utilities + abbreviations in one package.")
  (:use :common-lisp))


(in-package #:rutils.readtable)

(defvar +default-opts+
  #-rutils-dev '(optimize (speed 3) (space 1))
  #+rutils-dev '(optimize (debug 3))
  "Default optimization settings for RUTILS.")
