;; For license see LICENSE

(in-package "REASONABLE-UTILITIES.CONTROL")

(proclaim '(optimize speed))


;; do

(defmacro dowhile (test &rest body)
  "Execute <_:arg body /> in a <_:fun do />-loop, while <_:arg test />
is satisfied"
  `(do ()
       ((not ,test))
     ,@body))    

(defmacro dountil (test &body body)
  "Execute <_:arg body /> in a <_:fun do />-loop, until <_:arg test />
is satisfied"
  `(do ()
       (,test)
     ,@body))


;; gcase

(defmacro gcase ((keyform &key (test #'eql) err) &body clauses)
  "Generalized case. Unlike <_:fun case /> it can use any <_:arg test />
function. <_:class Type-errors /> signaled by <_:arg test /> function's
applicattion are coverted to <_:class warnings />, unless <_:arg err />
is provided"
  (unless (listp clauses)
    (error "~a -- bad clause in CASE" clauses))
  (let ((it (gensym "IT")))
    `(let ((,it ,keyform))
       (cond
         ,@(loop :for clause-and-tail :on clauses
              :collect
              (if (and (find (caar clause-and-tail) '(t otherwise))
                       (not (cdr clause-and-tail)))
                  `(t ,@(cdar clause-and-tail))
                  (with-gensyms (e)
                    `((handler-case (funcall ,test ,it ,(caar clause-and-tail))
                        (type-error (,e)
                          (funcall (if ,err #'error #'warn)
                                   "The value ~a is not of type ~a"
                                   (type-error-datum ,e)
                                   (type-error-expected-type ,e))))
                      ,@(cdar clause-and-tail)))))))))

;; logic

(defmacro when/t (condition &body body)
  "Like <_:fun when />, but returns T instead of NIL,
if condition doesn't satisfy. Is logically equivalent to:
<_:code (or (not condition) (progn body)) />,
but cleraly expresses intent"
  `(if ,condition (progn ,@body)
       t))

(defun or2 (x y)
  "OR for 2 arguments as a function"
  (or x y))

(defun and2 (x y)
  "AND for 2 arguments as a function"
  (and x y))

(defun xor2 (x y)
  "XOR for 2 arguments as a function"
  (or (and x (not y))
      (and (not x) y)))

(defmacro xor (&rest args)
  "Evaluates the <_:arg args /> one at a time. If more than one returns T,
evaluation stops and NIL is returned. If exactly one arg returns T that,
value is returned"
  (let ((state (gensym "XOR-state-"))
        (block-name (gensym "XOR-block-"))
        (arg-temp (gensym "XOR-arg-temp-")))
    `(let (,state ,arg-temp)
       (block ,block-name
         ,@(loop
              :for arg :in args
              :collect `(setf ,arg-temp ,arg)
              :collect `(when ,arg-temp
                          ;; arg is T, this can change the state
                          (if ,state
                              ;; a second T value, return NIL
                              (return-from ,block-name nil)
                              ;; a first T, swap the state
                              (setf ,state ,arg-temp))))
         (return-from ,block-name ,state)))))        

(defun less (x y)
  "Like <, but works for NIL values of <_:arg x /> and <_:arg y />.
Obviously, NIL is <_:fun less />, than anything"
  (cond ((null x) y)
        ((null y) nil)
        (t (< x y))))

(defun not-more (x y)
  "Like <=, but works for NIL values of <_:arg x /> and <_:arg y />
Obviously, NIL is <_:fun less />, than anything"
  (cond ((null x) y)
        ((null y) nil)
        (t (<= x y))))

(defun more (x y)
  "Like >, but works for NIL values of <_:arg x /> and <_:arg y />
Obviously, NIL is <_:fun less />, than anything"
  (cond ((null x) nil)
        ((null y) x)
        (t (> x y))))

(defun not-less (x y)
  "Like >=, but works for NIL values of <_:arg x /> and <_:arg y />
Obviously, NIL is <_:fun less />, than anything"
  (cond ((null x) nil)
        ((null y) x)
        (t (>= x y))))

;;; end