;;; see LICENSE file for permissions

(cl:in-package #:rutils.anaphora)
(named-readtables:in-readtable rutils-readtable)
(eval-when (:compile-toplevel)
  (declaim #.+default-opts+))


(defmacro if-it (test then &optional else)
  "Like IF. IT is bound to TEST."
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro when-it (test &body body)
  "Like WHEN. IT is bound to TEST."
  `(let ((it ,test))
     (when it
       ,@body)))

(defmacro and-it (&rest args)
  "Like AND. IT is bound to the value of the previous AND form."
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(when-it ,(car args) (and-it ,@(cdr args))))))

(defmacro dowhile-it (test &body body)
  "Like DOWHILE. IT is bound to TEST."
  `(do ((it ,test ,test))
       ((not it))
     ,@body))

(defmacro cond-it (&body body)
  "Like COND. IT is bound to the passed COND test."
  `(let (it)
     (cond
       ,@(mapcar (lambda (clause)
                   `((setf it ,(car clause)) ,@(cdr clause)))
                 ;; uses the fact, that SETF returns the value set
                 body))))

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
        (t `(when-let (,var ,(car args))
              (and ,@(cdr args))))))

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
