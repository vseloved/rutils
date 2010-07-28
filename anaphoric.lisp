;;; RUTILS anaphoric utils
;;; see LICENSE file for permissions


(in-package #:reasonable-utilities.anaphoric/it)

(locally-enable-literal-syntax :sharp-backq)


(defmacro if-it (test then &optional else)
  "Like <_:fun if />. IT is bound to <_:arg test />"
  `(let ((it ,test))
    (if it ,then ,else)))

(defmacro when-it (test &body body)
  "Like <_:fun when />. IT is bound to <_:arg test />"
  `(let ((it ,test))
    (when it
      ,@body)))

(defmacro and-it (&rest args)
  "Like <_:fun and />. IT is bound to the value of
the previous <_:fun and /> form"
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(when-it ,(car args) (and-it ,@(cdr args))))))

(defmacro dowhile-it (test &body body)
  "Like <_:fun dowhile />. IT is bound to <_:arg test />"
  `(do ((it ,test ,test))
       ((not it))
     ,@body))

(defmacro cond-it (&body body)
  "Like <_:fun cond />. IT is bound to the passed <_:fun cond /> test"
  `(let (it)
     (cond
       ,@(mapcar #``((setf it ,(car _)) ,(cadr _))
                 ;; uses the fact, that SETF returns the value set
                 body))))


(in-package #:reasonable-utilities.anaphoric/a)

(abbrev aand rutils.anaphoric/it:and-it)
(abbrev acond rutils.anaphoric/it:cond-it)
(abbrev adowhile rutils.anaphoric/it:dowhile-it)
(abbrev aif rutils.anaphoric/it:if-it)
(abbrev awhen rutils.anaphoric/it:when-it)


(in-package #:reasonable-utilities.anaphoric/let)

(defmacro if-let (var test then &optional else)
  "Like <_:fun if />. <_:arg Var /> will be bound to <_:arg test />"
  `(let ((,var ,test))
    (if ,var ,then ,else)))

(defmacro when-let (var test &body body)
  "Like <_:fun when />. <_:arg Var /> will be bound to <_:arg test />"
  `(let ((,var ,test))
    (when ,var
      ,@body)))

(defmacro and-let (var &rest args)
  "Like <_:fun and />. <_:arg Var /> will be bound to the value of
the previous <_:fun and /> form"
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(when-let ,var ,(car args) (and-let ,@(cdr args))))))

(defmacro dowhile-let (var test &body body)
  "Like <_:fun dowhile />. <_:arg Var /> will be bound to <_:arg test />"
  `(do ((,var ,test ,test))
       ((not ,var))
     ,@body))

(defmacro cond-let (var &body body)
  "Like <_:fun cond />. <_:arg Var /> will be bound to
the passed <_:fun cond /> test"
  `(let (,var)
     (cond
       ,@(mapcar #``((setf ,var ,(car _)) ,(cadr _))
                 ;; uses the fact, that SETF returns the value set
                 body))))


(in-package #:reasonable-utilities.anaphoric/bind)

(abbrev and-bind rutils.anaphoric/let:and-let)
(abbrev cond-bind rutils.anaphoric/let:cond-let)
(abbrev dowhile-bind rutils.anaphoric/let:dowhile-let)
(abbrev if-bind rutils.anaphoric/let:if-let)
(abbrev when-bind rutils.anaphoric/let:when-let)


;;; end