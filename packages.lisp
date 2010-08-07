;;; RUTILS packages' definition
;;; see LICENSE file for permissions

(in-package :cl-user)


;; general syntax

(defpackage #:reasonable-utilities.core
  (:nicknames #:rutils.core)
  (:documentation "The infrastructural utilities, mostly concerned with
manipulation of <_:class symbol />s and reader syntax")
  (:use :common-lisp)
  (:export #:abbrev
           #:defconst
           #:disable-literal-syntax
           #:enable-literal-syntax
           #:ensure-keyword
           #:ensure-symbol
           #:eval-always
           #:export-exported-symbols
           #:it
           #:locally-disable-literal-syntax
           #:locally-enable-literal-syntax
           #:make-gensym-list
           #:make-reader-error-fun
           #:once-only
           #:package-symbols
           #:package-external-symbols
           #:with-gensyms
           #:with-unique-names))

(defpackage #:reasonable-utilities.pkg
  (:nicknames #:rutils.pkg)
  (:documentation "A per-form control over the reader side-effects
\(like interning of symbols)")
  (:use :common-lisp
        #:rutils.core)
  (:export ;; nothing
   ))

(defpackage #:reasonable-utilities.short
  (:nicknames #:rutils.short)
  (:use :common-lisp #:rutils.core)
  (:documentation "<_:fun Abbrevs />es of some common utilities with very
long names")
  (:export #:2nd
           #:co
           #:defpar
           #:ds-bind
           #:fmt
           #:mk
           #:mkeyw
           #:mksym
           #:mv-bind
           #:w/instr
           #:w/outstr
           #:w/uniqs))

(defpackage #:reasonable-utilities.function
  (:nicknames #:rutils.function)
  (:documentation "Basic utilities to support functional programming.
Also defines the #` reader-macro for 1-argument <_:fun lambda />s")
  (:use :common-lisp #:rutils.core #:rutils.short)
  (:export #:compose
           #:conjoin
           #:curry
           #:disjoin
           #:ensure-function
           #:mkfunction
           #:multiple-value-compose
           #:rcurry
           #:_))

(defpackage #:reasonable-utilities.control
  (:nicknames #:rutils.control)
  (:documentation "Basic control structures and predicates")
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function)
  (:export #:and2
           #:dowhile
           #:dountil
           #:gcase
           #:less
           #:more
           #:not-less
           #:not-more
           #:or2
           #:true
           #:when/t
           #:xor
           #:xor2))


;; basic data-structures

(defpackage #:reasonable-utilities.list
  (:nicknames #:rutils.list)
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function)
  (:documentation "<_:class List /> utilities")
  (:export #:alistp
           #:alist-to-plist
           #:assoc1
           #:butlast2
           #:delete-from-plist
           #:docoll
           #:dyadic
           #:ensure-list
           #:first-n
           #:flatten
           #:group
           #:interlay
           #:last1
           #:mapfil
           #:mklist
           #:plistp
           #:plist-to-alist
           #:rearrange-pairs
           #:remove-from-plist
           #:single
           #:tryadic
           #:with-output-to-list))

(defpackage #:reasonable-utilities.string
  (:nicknames #:rutils.string)
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function
        #:rutils.list)
  (:documentation "@class{String} utilities")
  (:export #:blankp
           #:read-file
           #:split-string
           #:strcat
           #:strcat_
           #:s+
           #:to-string))


;; special syntax

(defpackage #:reasonable-utilities.anaphoric/a
  (:nicknames #:rutils.anaphoric/a #:rutils.ana/a)
  (:documentation "Anaphoric control constructs with a- prefix and
automatic binding of test to <_:code it />")
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function)
  (:export #:aand
           #:acond
           #:adowhile
           #:aif
           #:awhen))

(defpackage #:reasonable-utilities.anaphoric/it
  (:nicknames #:rutils.anaphoric/it #:rutils.ana/it)
  (:documentation "Anaphoric control constructs with -it suffix and
automatic binding of test to <_:code it />")
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function)
  (:export #:and-it
           #:cond-it
           #:dowhile-it
           #:if-it
           #:when-it))

(defpackage #:reasonable-utilities.anaphoric/let
  (:nicknames #:rutils.anaphoric/let #:rutils.ana/let)
  (:documentation "Anaphoric control constructs with -let suffix")
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function)
  (:export #:and-let
           #:cond-let
           #:dowhile-let
           #:if-let
           #:when-let))

(defpackage #:reasonable-utilities.anaphoric/bind
  (:nicknames #:rutils.anaphoric/bind #:rutils.ana/bind)
  (:documentation "Anaphoric control constructs with -bind suffix")
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function)
  (:export #:and-bind
           #:cond-bind
           #:dowhile-bind
           #:if-bind
           #:when-bind))

(defpackage #:reasonable-utilities.bind
  (:nicknames #:rutils.bind)
  (:documentation "A unified <_:fun bind /> macro and destructuring utilities")
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.list #:rutils.function
        #:rutils.string #:rutils.ana/it)
  (:export #:bind
           #:def-bind-rule
           #:tmpl-bind))

(defpackage #:reasonable-utilities.iter
  (:nicknames #:rutils.iter)
  (:documentation "An iteration macro with control keywords")
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function #:rutils.control
        #:rutils.list #:rutils.string #:rutils.ana/it #:rutils.bind)
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


;; collections and other data types

(defpackage #:reasonable-utilities.array
  (:nicknames #:rutils.array)
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function)
  (:documentation "<_:class Vector /> and array utilities")
  (:export #:copy-array))

(defpackage #:reasonable-utilities.hash-table
  (:nicknames #:rutils.hash-table)
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function
        #:rutils.string #:rutils.list)
  (:documentation "<_:class Hash-table /> utilities")
  (:export #:copy-hash-table
           #:hash-table-keys
           #:hash-table-vals
           #:hash-table-values
           #:maphash-keys
           #:maphash-vals
           #:maphash-values
           #:merge-hash-tables
           #:hash-table-from-alist
           #:hash-table-from-list
           #:hash-table-to-alist
           #:hash-table-to-list))

(defpackage #:reasonable-utilities.genhash
  (:nicknames #:rutils.genhash)
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function
        #:rutils.ana/it)
  (:documentation "<_:class Genhash /> implementation (CDR 2) and
utilities, similar to <_:pkg rutils.hash-table />")
  (:export #:all-hash-keys
           #:all-hash-vals
           #:all-hash-values
           #:copy-generic-hash-table
           #:expand-hash-table
           #:generic-hash-table-count
           #:generic-hash-table-p
           #:generic-hash-table-size
           #:genhash-from-alist
           #:genhash-from-list
           #:genhash-to-alist
           #:genhash-to-list
           #:hashclr
           #:hashmap
           #:hashmap-keys
           #:hashmap-vals
           #:hashmap-values
           #:hashref
           #:hashrem
           #:hash-container
           #:hash-exists
           #:hash-test-designator
           #:make-generic-hash-table
           #:register-builtin
           #:register-test-designator
           #:unknown-hash
           #:with-generic-hash-table-iterator))

(defpackage #:reasonable-utilities.seq
  (:nicknames #:rutils.seq)
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function #:rutils.list
        #:rutils.ana/it #:rutils.hash-table #:rutils.genhash)
  (:documentation "An implementation of the SEQ interface for generic
sequential iteration. And some utilities, based on it")
  (:export #:doseq
           #:elm
           #:head
           #:interleave
           #:into
           #:next
           #:seq
           #:tail
           #:take
           #:with-acc))

(defpackage #:reasonable-utilities.number
  (:nicknames #:rutils.number)
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function
        #:rutils.list #:rutils.string)
  (:documentation "Numeric utilities and the implementation of
CDR 5 (Sub-interval numeric types)")
  (:export #:array-index
           #:array-length

           #:do-range
           #:map-range
           #:maxf
           #:minf

           #:negative-integer
           #:negative-double-float
           #:negative-fixnum
           #:negative-float
           #:negative-long-float
           #:negative-ratio
           #:negative-rational
           #:negative-real
           #:negative-short-float

           #:non-negative-integer
           #:non-negative-double-float
           #:non-negative-fixnum
           #:non-negative-long-float
           #:non-negative-ratio
           #:non-negative-rational
           #:non-negative-ratioreal
           #:non-negative-short-float

           #:non-positive-integer
           #:non-positive-double-float
           #:non-positive-fixnum
           #:non-positive-long-float
           #:non-positive-ratio
           #:non-positive-rational
           #:non-positive-ratioreal
           #:non-positive-short-float

           #:parse-float

           #:positive-integer
           #:positive-double-float
           #:positive-fixnum
           #:positive-long-float
           #:positive-ratio
           #:positive-rational
           #:positive-ratioreal
           #:positive-short-float

           #:range

           #:ratiop
           #:ratio-minusp
           #:ratio-plusp))

(defpackage #:reasonable-utilities.sequence
  (:nicknames #:rutils.sequence)
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function #:rutils.control
        #:rutils.number #:rutils.seq #:rutils.iter)
  (:documentation "@class{Sequence} utilities, including @fun{split-sequence}")
  (:export #:deletef
           #:doindex
           #:find-all
           #:is-or-in
           #:split-sequence
           #:split-sequence-if
           #:split-sequence-if-not
           #:partition-with
           #:removef
           #:shuffle-sequence))

(defpackage #:reasonable-utilities.tree
  (:nicknames #:rutils.tree)
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function)
  (:documentation "Tree utilities")
  (:export #:dotree
           #:maptree))

#+:closer-mop
(defpackage #:reasonable-utilities.object
  (:nicknames #:rutils.obj)
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.hash-table #:rutils.genhash)
  (:documentation "OOP utilities")
  (:export #:obj-equal
           #:obj-equal-by-slots))

(defpackage #:reasonable-utilities.condition
  (:nicknames #:rutils.condition)
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function)
  (:documentation "Utilities, connected with conditions and error-handling")
  (:export #:maybe))


;; experimental           

(defpackage #:reasonable-utilities.experimental
  (:nicknames #:rutils.experimental)
  (:use :common-lisp
        #:rutils.core #:rutils.short #:rutils.function #:rutils.ana/it)
  (:documentation "Experimental utilities, whose presence in the library
is not absolutely justified")
  (:export #:defmulti))


;; aggregate

(defpackage #:reasonable-utilities.user
  (:nicknames #:rutils.user)
  (:documentation "The basic set of utilities (mostly everything,
except experimental (incl. SEQ) and different synonyms.
The symbols are rexeported from appropriate packages")
  (:use :common-lisp))

(defpackage #:reasonable-utilities.usr
  (:nicknames #:rutils.usr)
  (:documentation "rutils.user + rutils.short")
  (:use :common-lisp))

(defpackage #:reasonable-utilities
  (:nicknames #:rutils #:reasonable-utilities.* #:rutils.*)
  (:documentation "All of the utilities in one package.
The symbols are rexeported from appropriate packages")
  (:use :common-lisp))

;;; end