;;; RUTILS condition handling
;;; see LICENSE file for permissions

(in-package #:reasonable-utilities.condition)

(defmacro maybe (form)
  "Return a value, returned by a <_:arg form /> or nil,
if <_:class error /> is signalled"
  `(restart-case
    (handler-bind ((error #'(lambda (c)
                              (declare (ignore condition))
                              (invoke-restart 'skip))))
      ,form)
    (skip () nil)))

;;; end