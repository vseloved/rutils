;;; see LICENSE file for permissions

(in-package #:rutils.abbr)

(abbr call funcall)
(abbr make make-instance)
(abbr just identity)

(defmacro flet* (&rest body)
  "An abbreviation for LABELS."
  `(labels ,@body))

(abbr defpar defparameter)
(abbr m1 macroexpand-1)

(abbr ds-bind destructuring-bind)
(abbr mv-bind multiple-value-bind)

(abbr w/instr with-input-from-string)
(abbr w/outstr with-output-to-string)

(abbr sub subseq)

(abbr pushx vector-push-extend)

;;; emoji-setters

(handler-bind ((error (lambda (e)
                        (let ((r (find-restart 'continue e)))
                          (when r
                            (invoke-restart r))))))
  (defmacro := (&rest places-vals &environment env)
    "Like PSETF but returns the set value of the last expression."
    (declare (ignore env))
    (with-gensyms (rez)
      `(let (,rez)
         (psetf ,@(butlast places-vals 2)
                ,(first (last places-vals 2)) (setf ,rez ,(last1 places-vals)))
         ,rez)))

  (abbr :+ incf)
  (abbr :- decf)

  (defmacro :* (place n)
    "Multiply in-lace PLACE by N."
    `(setf ,place (* ,place ,n)))

  (defmacro :/ (place n)
    "Divide in-lace PLACE by N."
    `(setf ,place (/ ,place ,n))))


;;; package-specific aliases

(in-package #:rutils.symbol)

(abbr mkeyw  ensure-keyword)
(abbr mksym  ensure-symbol)

(in-package #:rutils.misc)

(abbr fn named-lambda)

(in-package #:rutils.pair)

(abbr lt pair-left)
(abbr rt pair-right)

(in-package #:rutils.list)

(abbr mklist ensure-list)
(abbr flat-map mappend)
(abbr mapcat mappend)

(in-package #:rutils.string)

(abbr slurp read-file)

(in-package #:rutils.sequence)

(abbr split split-sequence)
(abbr split-if split-sequence-if)
(abbr split-if-not split-sequence-if-not)

(abbr keep-if remove-if-not)
(abbr keep-if-not remove-if)


(in-package #:rutils.anaphora)

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

(in-package #:rutils.hash-table)

(abbr p# print-hash-table)
(abbr get# gethash (key hashtable &optional default))
(abbr set# sethash)
(abbr getset# getsethash)
(abbr rem# remhash)
(abbr take# takehash)

(abbr ht-count hash-table-count)
(abbr ht-keys hash-table-keys)
(abbr ht-vals hash-table-vals)
(abbr ht->alist hash-table-to-alist)
(abbr ht->plist hash-table-to-plist)
(abbr alist->ht hash-table-from-alist)
(abbr plist->ht hash-table-from-plist)
(abbr merge-hts merge-hash-tables)
(abbr merge-hts-with merge-hash-tables-with)
(abbr print-ht print-hash-table)

(in-package #:rutils.generic)

(abbr ? generic-elt)
(defsetf ? generic-setf)

(in-package #:rutils.bind)

(abbr with bind)
