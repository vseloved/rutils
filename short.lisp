;;; RUTILS short names definition
;;; see LICENSE file for permissions

(in-package #:reasonable-utilities.short)

(abbrev mksym ensure-symbol)
(abbrev mkeyw ensure-keyword)

;; generic MK ("making" something into something similar)

(defgeneric mk (to smth &key)
  (:documentation
   "Make <_:arg smth /> into something similar (specified by <_:arg to />)")
  (:method ((to (eql 'symbol)) smth &key format package &allow-other-keys)
    (mksym smth :format format :package package))
  (:method ((to (eql 'keyword)) smth &key format &allow-other-keys)
    (mkeyw smth :format format)))

#-clisp
(abbrev mv-bind multiple-value-bind)
(abbrev ds-bind destructuring-bind)

(abbrev w/uniqs with-gensyms)

(abbrev w/instr with-input-from-string)
(abbrev w/outstr with-output-to-string)

(defmacro 2nd (form)
  "(<_:fun Nth-value /> 1 <_:arg form />)"
  `(nth-value 1 ,form))

(abbrev defpar defparameter)

(abbrev @ slot-value)

(proclaim '(inline fmt))
(defun fmt (format-string &rest args)
  (apply #'format nil format-string args))

;;; end
