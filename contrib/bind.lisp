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

(defgeneric bind-dispatch (arg &rest args)
  (:method ((arg symbol) &rest args)
    (if (cdr args)
        `(multiple-value-bind (,arg ,@(butlast args)) ,(car (last args)))
        `(let ((,arg ,(car args))))))
  (:method ((arg list) &rest args)
    `(destructuring-bind ,arg ,@args))
  (:method ((arg hash-table) &rest args)
    `(let (,@(let (bindings)
               (dotable (k v arg (reverse bindings))
                 (push (list v `(gethash ,k ,(car args)))
                       bindings)))))))
