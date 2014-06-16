;;; RUTILS THREADING macros

(cl:in-package #:rutilsx.threading)
(named-readtables:in-readtable rutils-readtable)
(declaim #.+default-opts+)

(declaim (declaration declare-variables))

(defun lambda-form-p (form)
  (member '% form))

(defmacro -> (x &rest forms)
  (if (first forms)
      (let* ((form (first forms))
             (threaded (if (listp form)
                           (if (lambda-form-p form)
                               `(funcall (macroexpand #`(,form)) ,x)
                               `(,(first form) ,x ,@(rest form)))
                           `(,form ,x))))
        `(-> ,threaded ,@(rest forms)))
      x))

(defmacro ->> (x &rest forms)
  (if (first forms)
      (let* ((form (first forms))
             (threaded (if (listp form)
                           (if (lambda-form-p form)
                               `(funcall (macroexpand #`(,form)) ,x)
                               `(,(first form) ,@(rest form) ,x))
                           `(,form ,x))))
        `(->> ,threaded ,@(rest forms)))
      x))
