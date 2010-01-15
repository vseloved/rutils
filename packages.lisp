;;; RUTILS packages' definition
;;; see LICENSE file for permissions

(in-package :cl-user)


;; general syntax

(defpackage "REASONABLE-UTILITIES.CORE"
  (:nicknames "RUTILS.CORE")
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

(defpackage "REASONABLE-UTILITIES.SHORT"
  (:nicknames "RUTILS.SHORT")
  (:use :common-lisp "RUTILS.CORE")
  (:documentation "<_:fun Abbrevs />es of some common utilities with very
long names")
  (:export #:2nd
           #:co
           #:ds-bind
           #:mk
           #:mkeyw
           #:mksym
           #:mv-bind
           #:w/instr
           #:w/outstr
           #:w/uniqs))

(defpackage "REASONABLE-UTILITIES.PKG"
  (:nicknames "RUTILS.PKG")
  (:documentation "A per-form control over the reader side-effects
\(like interning of symbols)")
  (:use :common-lisp
        "RUTILS.CORE")
  (:export ;; nothing
   ))


(defpackage "REASONABLE-UTILITIES.FUNCTION"
  (:nicknames "RUTILS.FUNCTION")
  (:documentation "Basic utilities to support functional programming.
Also defines the #` reader-macro for 1-argument <_:fun lambda />s")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT")
  (:export #:compose
           #:conjoin
           #:curry
           #:disjoin
           #:ensure-function
           #:mkfunction
           #:multiple-value-compose
           #:rcurry
           #:_))

(defpackage "REASONABLE-UTILITIES.CONTROL"
  (:nicknames "RUTILS.CONTROL")
  (:documentation "Basic control structures and predicates")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION")
  (:export #:and2
           #:dowhile
           #:dountil
           #:gcase
           #:less
           #:more
           #:not-less
           #:not-more
           #:or2
           #:when/t
           #:xor
           #:xor2))


;; basic data-structures

(defpackage "REASONABLE-UTILITIES.LIST"
  (:nicknames "RUTILS.LIST")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION")
  (:documentation "<_:class List /> utilities")
  (:export #:alistp
           #:alist-to-plist
           #:assoc1
           #:butlast2
           #:delete-from-plist
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

(defpackage "REASONABLE-UTILITIES.STRING"
  (:nicknames "RUTILS.STRING")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION" "RUTILS.LIST")
  (:documentation "<_:class String /> utilities")
  (:export #:blankp
           #:read-file
           #:strcat
           #:strcat_
           #:to-string))


;; special syntax

(defpackage "REASONABLE-UTILITIES.ANAPHORIC/A"
  (:nicknames "RUTILS.ANAPHORIC/A" "RUTILS.ANA/A")
  (:documentation "Anaphoric control constructs with a- prefix and
automatic binding of test to <_:code it />")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION")
  (:export #:aand
           #:acond
           #:adowhile
           #:aif
           #:awhen))

(defpackage "REASONABLE-UTILITIES.ANAPHORIC/IT"
  (:nicknames "RUTILS.ANAPHORIC/IT" "RUTILS.ANA/IT")
  (:documentation "Anaphoric control constructs with -it suffix and
automatic binding of test to <_:code it />")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION")
  (:export #:and-it
           #:cond-it
           #:dowhile-it
           #:if-it
           #:when-it))

(defpackage "REASONABLE-UTILITIES.ANAPHORIC/LET"
  (:nicknames "RUTILS.ANAPHORIC/LET" "RUTILS.ANA/LET")
  (:documentation "Anaphoric control constructs with -let suffix")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION")
  (:export #:and-let
           #:cond-let
           #:dowhile-let
           #:if-let
           #:when-let))

(defpackage "REASONABLE-UTILITIES.ANAPHORIC/BIND"
  (:nicknames "RUTILS.ANAPHORIC/BIND" "RUTILS.ANA/BIND")
  (:documentation "Anaphoric control constructs with -bind suffix")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION")
  (:export #:and-bind
           #:cond-bind
           #:dowhile-bind
           #:if-bind
           #:when-bind))

(defpackage "REASONABLE-UTILITIES.BIND"
  (:nicknames "RUTILS.BIND")
  (:documentation "A unified <_:fun bind /> macro and destructuring utilities")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.LIST" "RUTILS.FUNCTION"
        "RUTILS.STRING" "RUTILS.ANA/IT")
  (:export #:bind
           #:def-bind-rule
           #:tmpl-bind))

(defpackage "REASONABLE-UTILITIES.ITER"
  (:nicknames "RUTILS.ITER")
  (:documentation "An iteration macro with control keywords")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION" "RUTILS.CONTROL"
        "RUTILS.LIST" "RUTILS.STRING" "RUTILS.ANA/IT" "RUTILS.BIND")
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

(defpackage "REASONABLE-UTILITIES.ARRAY"
  (:nicknames "RUTILS.ARRAY")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION")
  (:documentation "<_:class Vector /> and array utilities")
  (:export #:copy-array))

(defpackage "REASONABLE-UTILITIES.HASH-TABLE"
  (:nicknames "RUTILS.HASH-TABLE")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION" "RUTILS.STRING")
  (:documentation "<_:class Hash-table /> utilities")
  (:export #:copy-hash-table
           #:hash-table-keys
           #:hash-table-vals
           #:hash-table-values
           #:maphash-keys
           #:maphash-vals
           #:maphash-values
           #:hash-table-from-alist
           #:hash-table-from-list
           #:hash-table-to-alist
           #:hash-table-to-list))

(defpackage "REASONABLE-UTILITIES.GENHASH"
  (:nicknames "RUTILS.GENHASH")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION"
        "RUTILS.ANA/IT")
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

(defpackage "REASONABLE-UTILITIES.SEQ"
  (:nicknames "RUTILS.SEQ")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION" "RUTILS.LIST"
        "RUTILS.ANA/IT" "RUTILS.HASH-TABLE" "RUTILS.GENHASH")
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

(defpackage "REASONABLE-UTILITIES.NUMBER"
  (:nicknames "RUTILS.NUMBER")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION"
        "RUTILS.LIST" "RUTILS.STRING")
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

(defpackage "REASONABLE-UTILITIES.SEQUENCE"
  (:nicknames "RUTILS.SEQUENCE")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION" "RUTILS.CONTROL"
        "RUTILS.NUMBER" "RUTILS.SEQ" "RUTILS.ITER")
  (:documentation "<_:class Sequence /> utilities,
including <_:fun split-sequence />")
  (:export #:deletef
           #:doindex
           #:is-or-in
           #:split-sequence
           #:split-sequence-if
           #:split-sequence-if-not
           #:partition-with
           #:removef
           #:shuffle-sequence))

(defpackage "REASONABLE-UTILITIES.TREE"
  (:nicknames "RUTILS.TREE")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION")
  (:documentation "Tree utilities")
  (:export #:dotree
           #:maptree))

#+:closer-mop
(defpackage "REASONABLE-UTILITIES.OBJECT"
  (:nicknames "RUTILS.OBJ")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT"
        "RUTILS.HASH-TABLE" "RUTILS.GENHASH")
  (:documentation "OOP utilities")
  (:export #:obj-equal
           #:obj-equal-by-slots))

(defpackage "REASONABLE-UTILITIES.CONDITION"
  (:nicknames "RUTILS.CONDITION")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION")
  (:documentation "Utilities, connected with conditions and error-handling")
  (:export #:maybe))


;; experimental           

(defpackage "REASONABLE-UTILITIES.EXPERIMENTAL"
  (:nicknames "RUTILS.EXPERIMENTAL")
  (:use :common-lisp "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION")
  (:documentation "Experimental utilities, whose presence in the library
is not absolutely justified")
  (:export #:defmulti))


;; aggregate

(defpackage "REASONABLE-UTILITIES.USER"
  (:nicknames "RUTILS.USER")
  (:documentation "The basic set of utilities (mostly everything,
except experimental (incl. SEQ) and different synonyms.
The symbols are rexeported from appropriate packages")
  (:use :common-lisp
        "RUTILS.CORE" "RUTILS.FUNCTION" "RUTILS.CONTROL" "RUTILS.PKG"
        "RUTILS.LIST" "RUTILS.STRING" "RUTILS.SEQUENCE"
        "RUTILS.ANA/IT" "RUTILS.ANA/BIND" "RUTILS.ITER" "RUTILS.BIND"
        "RUTILS.ARRAY" "RUTILS.HASH-TABLE" "RUTILS.GENHASH" "RUTILS.TREE"
        "RUTILS.NUMBER" "RUTILS.CONDITION"
        #+:closer-mop "RUTILS.OBJ"))

(defpackage "REASONABLE-UTILITIES.USR"
  (:nicknames "RUTILS.USR")
  (:documentation "RUTILS.USER + RUTILS.SHORT")
  (:use :common-lisp
        "RUTILS.USER" "RUTILS.SHORT"))

(defpackage "REASONABLE-UTILITIES"
  (:nicknames "RUTILS" "REASONABLE-UTILITIES.*" "RUTILS.*")
  (:documentation "All of the utilities in one package.
The symbols are rexeported from appropriate packages")
  (:use :common-lisp
        "RUTILS.USER" "RUTILS.SHORT"
        "RUTILS.ANA/A" "RUTILS.ANA/LET"
        "RUTILS.EXPERIMENTAL" "RUTILS.SEQ"))

;;; end