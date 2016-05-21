;;; see LICENSE file for permissions

(cl:in-package #:rutilsx.bind)
(named-readtables:in-readtable rutils-readtable)
(declaim #.+default-opts+)


(defvar *bind-ignores* nil
  "List of gensymed symbols that should be declared ignored for bind.")

(eval-always
  (defmacro bind ((&rest bindings) &body body)
  "Bind variables from BINDINGS to be active inside BODY, as if by LET*,
combined with MULTIPLE-VALUE-BIND, DESTRUCTURING-BIND and other -bind forms,
depending on the type of the first argument."
    (let ((rez body))
      (dolist (binding (reverse bindings))
        (:= rez `((,@(call #'expand-binding binding rez)))))
      (first rez)))

  (abbr with bind)
)

(defun expand-binding (binding form)
  (append (apply #'bind-dispatch binding)
          form))

(defun subst-ignore (arg)
  (if (eql '_ arg)
      (let ((arg (gensym "IGNORED")))
        (push arg *bind-ignores*)
        arg)
      arg))

(defgeneric bind-dispatch (arg1 arg2 &rest args)
  (:method ((arg1 symbol) arg2 &rest args)
    (if args
        (let (*bind-ignores*)
          `(multiple-value-bind (,(subst-ignore arg1)
                                 ,(subst-ignore arg2)
                                 ,@(mapcar 'subst-ignore (butlast args)))
               ,(last1 args)
             ,@(when *bind-ignores*
                 `((declare (ignore ,@*bind-ignores*))))))
        `(let ((,arg1 ,arg2)))))
  (:method ((arg1 list) (arg2 (eql '?)) &rest args)
    `(let (,@(mapcar (lambda (var-key)
                       `(,(first (mklist var-key))
                         (? ,(first args) ,(last1 (mklist var-key)))))
                     arg1))))
  (:method ((arg1 list) (arg2 (eql '@)) &rest args)
    (with-gensyms (obj)
      `(let* ((,obj ,(first args))
              ,@(mapcar (lambda (var-slot)
                          `(,(first (mklist var-slot))
                            (smart-slot-value ,obj ',(last1 (mklist var-slot)))))
                        arg1)))))
  (:method ((arg1 list) arg2 &rest args)
    (declare (ignore args))
    (let (*bind-ignores*)
      `(destructuring-bind ,(mapcar 'subst-ignore arg1) ,arg2
         ,@(when *bind-ignores*
             `((declare (ignore ,@*bind-ignores*))))))))
