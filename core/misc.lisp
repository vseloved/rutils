;; For license see LICENSE

(in-package #:reasonable-utilities.misc)
(named-readtables:in-readtable rutils-readtable)

(declaim (optimize (speed 3) (space 1) (debug 0)))

(declaim (inline or2 and2 xor2))


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

(defmacro named-lambda (name lambda-list &body body)
  "Expands into a lambda-expression within whose BODY NAME denotes the
corresponding function."
  `(labels ((,name ,lambda-list ,@body))
     #',name))

(deftype array-index (&optional (length array-dimension-limit))
  "Type designator for an index into array of LENGTH: an integer between
0 (inclusive) and LENGTH (exclusive). LENGTH defaults to ARRAY-DIMENSION-LIMIT."
  `(integer 0 (,length)))

(deftype array-length (&optional (length array-dimension-limit))
  "Type designator for a dimension of an array of LENGTH: an integer between
0 (inclusive) and LENGTH (inclusive). LENGTH defaults to ARRAY-DIMENSION-LIMIT."
  `(integer 0 ,length))

(define-modify-macro coercef (type-spec) coerce "Modify-macro for COERCE.")
