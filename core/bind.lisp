;;; see LICENSE file for permissions

(in-package #:rutils.bind)
(named-readtables:in-readtable rutils-readtable)
(eval-when (:compile-toplevel)
  (declaim #.+default-opts+))


(defvar *bind-ignores* nil
  "List of gensymed symbols that should be declared ignored for bind.")

(defmacro bind ((&rest bindings) &body body)
  "Bind variables from BINDINGS to be active inside BODY, as if by LET*,
combined with MULTIPLE-VALUE-BIND, DESTRUCTURING-BIND and other -bind forms,
depending on the type of the first argument."
  (let ((rez body))
    (dolist (binding (reverse bindings))
      (setf rez `((,@(funcall #'expand-binding binding rez)))))
    (first rez)))

(defun expand-binding (binding form)
  (append (apply #'bind-dispatch binding)
          form))

(defun subst-ignore (arg)
  (if (string= "_" (symbol-name arg))
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
                         (? ,(first args) ',(last1 (mklist var-key)))))
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
      `(destructuring-bind ,(rutils.tree:mapleaves 'subst-ignore arg1) ,arg2
         ,@(when *bind-ignores*
             `((declare (ignore ,@*bind-ignores*)))))))
  (:method ((arg1 vector) arg2 &rest args)
    (declare (ignore args))
    (let ((*bind-ignores*))
      (with-gensyms (value-vector)
        `(let* ((,value-vector ,arg2)
                ,@(loop
                    :for sym :across arg1
                    :for i :upfrom 0
                    :collect `(,(subst-ignore sym) (aref ,value-vector ,i))))
           ,@(when *bind-ignores*
               `((declare (ignore ,@*bind-ignores*)))))))))
