;;; see LICENSE file for permissions

(cl:in-package #:reasonable-utilities.anaphoric/it)
(named-readtables:in-readtable rutils-readtable)

(declaim (optimize (speed 3) (space 1) (debug 0)))


(eval-always

(defmacro! if-it (test then &optional else)
  "Like IF. IT is bound to TEST."
  `(let ((,e!-it ,test))
     (if ,e!-it ,then ,else)))

(defmacro! when-it (test &body body)
  "Like WHEN. IT is bound to TEST."
  `(let ((,e!-it ,test))
     (when ,e!-it
       ,@body)))

(defmacro! and-it (&rest args)
  "Like AND. IT is bound to the value of the previous AND form."
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(when-it ,(car args) (and-it ,@(cdr args))))))

(defmacro! dowhile-it (test &body body)
  "Like DOWHILE. IT is bound to TEST."
  `(do ((,e!-it ,test ,test))
       ((not ,e!-it))
     ,@body))

(defmacro cond-it (&body body)
  "Like COND. IT is bound to the passed COND test."
  `(let (,e!-it)
     (cond
       ,@(mapcar (lambda (clause)
                   `((setf ,e!-it ,(car clause)) ,@(cdr clause)))
                 ;; uses the fact, that SETF returns the value set
                 body))))

) ; end of eval-always

(cl:in-package #:reasonable-utilities.anaphoric/let)
(named-readtables:in-readtable rutils-readtable)

(eval-always

(defmacro if-let ((var test) then &optional else)
  "Like IF. VAR will be bound to TEST."
  `(let ((,var ,test))
     (if ,var ,then ,else)))

(defmacro when-let ((var test) &body body)
  "Like WHEN. VAR will be bound to TEST."
  `(let ((,var ,test))
     (when ,var
       ,@body)))

(defmacro and-let (var &rest args)
  "Like AND. VAR will be bound to the value of the previous AND form"
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(when-let ,var ,(car args) (and-let ,@(cdr args))))))

(defmacro dowhile-let ((var test) &body body)
  "Like DOWHILE. VAR will be bound to TEST."
  `(do ((,var ,test ,test))
       ((not ,var))
     ,@body))

(defmacro cond-let (var &body body)
  "Like COND. VAR will be bound to the passed COND test."
  `(let (,var)
     (cond
       ,@(mapcar #``((setf ,var ,(car %)) ,(cadr %))
                 ;; uses the fact, that SETF returns the value set
                 body))))

) ; end of eval-always


(cl:in-package #:reasonable-utilities.anaphoric/a)
(named-readtables:in-readtable rutils-readtable)

(abbr aand rutils.anaphoric/it:and-it)
(abbr acond rutils.anaphoric/it:cond-it)
(abbr adowhile rutils.anaphoric/it:dowhile-it)
(abbr aif rutils.anaphoric/it:if-it)
(abbr awhen rutils.anaphoric/it:when-it)


(cl:in-package #:reasonable-utilities.anaphoric/bind)
(named-readtables:in-readtable rutils-readtable)

(abbr and-bind rutils.anaphoric/let:and-let)
(abbr cond-bind rutils.anaphoric/let:cond-let)
(abbr dowhile-bind rutils.anaphoric/let:dowhile-let)
(abbr if-bind rutils.anaphoric/let:if-let)
(abbr when-bind rutils.anaphoric/let:when-let)
