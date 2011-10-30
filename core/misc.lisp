;; For license see LICENSE

(in-package #:reasonable-utilities.misc)
(named-readtables:in-readtable rutils-readtable)

(proclaim '(optimize speed))
(proclaim '(inline or2 and2 xor2))

;; gcase

(defmacro pcase (pred keyform &rest clauses)
  "Like CASE, but uses given PRED instead of EQL to select appropriate CLAUSE.
Example usage:
CL-USER> (pcase 'apply 1
           ('numberp  (print \"It's a number\"))
           ('strindp  (print \"It's a string\"))
           (otherwise (error \"Oops\")))
"
  (once-only (keyform)
    `(cond
       ,@(loop :for (key actions) :in clauses
            :collect (cons (if (member key '(t otherwise)) t
                               `(funcall ,pred ,keyform ,key))
                           actions)))))

(defun or2 (x y)
  "OR for 2 arguments as a function."
  (or x y))

(defun and2 (x y)
  "AND for 2 arguments as a function."
  (and x y))

(defun xor2 (x y)
  "XOR for 2 arguments as a function."
  (or (and x (not y))
      (and (not x) y)))

(defmacro xor (&rest args)
  "Evaluates the ARGS one at a time. If more than one is T,
evaluation stops and NIL is returned. If exactly one arg is T,
that value is returned."
  (let ((state (gensym "XOR-state-"))
        (block-name (gensym "XOR-block-"))
        (arg-temp (gensym "XOR-arg-temp-")))
    `(let (,state
           ,arg-temp)
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
  "Like <, but works for NIL values of X and Y.
Obviously, NIL is LESS, than anything, including itself."
  (cond
    ((null x) t)
    ((null y) nil)
    (t (< x y))))

(defun not-more (x y)
  "Like <=, but works for NIL values of X and Y.
Obviously, NIL is NOT-MORE, than anything, including itself."
  (cond ((null x) y)
        ((null y) nil)
        (t (<= x y))))

(defun more (x y)
  "Like >, but works for NIL values of X and Y.
Obviously, NIL is not MORE, than anything, including itself."
  (cond ((null x) nil)
        ((null y) x)
        (t (> x y))))

(defun not-less (x y)
  "Like >=, but works for NIL values of X and Y.
Obviously, NIL is not NOT-LESS, than anything, including itself."
  (cond ((null x) nil)
        ((null y) x)
        (t (>= x y))))