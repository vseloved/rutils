;;; see LICENSE file for permissions

(cl:in-package #:rutilsx.bind)
(named-readtables:in-readtable rutils-readtable)
(declaim #.+default-opts+)


(defmacro bind ((&rest bindings) &body body)
  "Bind variables from BINDINGS to be active inside BODY, as if by LET*,
combined with MULTIPLE-VALUE-BIND, DESTRUCTURING-BIND and other -bind forms,
depending on the type of the first argument."
  (let ((rez body))
    (dolist (binding (reverse bindings))
      (setf rez `((,@(funcall #'expand-binding binding rez)))))
    (car rez)))

(defun expand-binding (binding form)
  (append (apply #'bind-dispatch binding)
          form))

(defgeneric bind-dispatch (arg1 arg2 &rest args)
  (:method ((arg1 symbol) arg2 &rest args)
    (if args
        `(multiple-value-bind (,arg1 ,arg2 ,@(butlast args)) ,(last1 args))
        `(let ((,arg1 ,arg2)))))
  (:method ((arg1 list) arg2 &rest args)
    (case arg2
      (? `(let (,@(mapcar (lambda (var-key)
                             `(,(first var-key) (? ,(first args) ,(second var-key))))
                          arg1))))
      (@ `(with-slots ,arg1 ,(first args)))
      (t `(destructuring-bind ,arg1 ,arg2)))))

(abbr with bind)
