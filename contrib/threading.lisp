;;; RUTILS THREADING macros

(cl:in-package #:rutilsx.threading)
(named-readtables:in-readtable rutils-readtable)
(declaim #.+default-opts+)


(defmacro -> (x &rest forms)
  "Threads the expr through FORMS. Inserts X as the
   second item in the first form, or in place of % if it is present in the form,
   making a list of it if it is not a list already.
   If there are more forms, inserts the first form
   as the second item in second form, etc."
  (if (first forms)
      (let* ((form (first forms))
             (threaded (if (listp form)
                           (if (member '% form)
                               `(funcall (macroexpand #`(,form)) ,x)
                               `(,(first form) ,x ,@(rest form)))
                           `(,form ,x))))
        `(-> ,threaded ,@(rest forms)))
      x))

(defmacro ->> (x &rest forms)
  "Threads the expr through FORMS. Inserts X as the
   last item in the first form, or in place of % if it is present in the form,
   making a list of it if it is not a list already.
   If there are more forms, inserts the first form
   as the second item in second form, etc."
  (if (first forms)
      (let* ((form (first forms))
             (threaded (if (member '% form)
                           (if (lambda-form-p form)
                               `(funcall (macroexpand #`(,form)) ,x)
                               `(,(first form) ,@(rest form) ,x))
                           `(,form ,x))))
        `(->> ,threaded ,@(rest forms)))
      x))
