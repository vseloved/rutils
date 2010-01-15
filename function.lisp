;; For license see MIT-LICENSE

(in-package "REASONABLE-UTILITIES.FUNCTION")


;; literal syntax

(eval-always 
  (defun |#`-reader| (stream char arg)
    "Reader syntax for one argument lambdas. Examples:
- #`(+ 2 _) => (lambda (x) (+ 2 x))
- #`((1+ _) (print _)) => (lambda (x) (1+ x) (print x))"
    (declare (ignore char arg))
    (let ((sexp (read stream t nil t))
          (x (gensym "X")))
      `(lambda (&optional ,x)
         ,@(subst x '_ (if (listp (car sexp)) sexp (list sexp))))))

  (defmethod enable-literal-syntax ((which (eql :sharp-backq)))
    (set-dispatch-macro-character #\# #\` #'|#`-reader|))

  (defmethod disable-literal-syntax ((which (eql :sharp-backq)))
    (set-dispatch-macro-character #\# #\` (make-reader-error-fun #\`))))


;; ensure function

(declaim (inline mkfun ensure-function))

(declaim (ftype (function (t) (values function &optional))
                ensure-function))
(eval-always
  (defun ensure-function (function-designator)
    "Return the function, designated by <_:arg function-designator />:
if <_:arg function-designator /> is a function, it is returned, otherwise
it must be a function name and its <_:fun fdefinition /> is returned."
    (if (functionp function-designator)
        function-designator
        (fdefinition function-designator))))

(abbrev mkfun ensure-function)

(defmethod mk ((to (eql 'function)) smth &key &allow-other-keys)
  (mkfun smth))


;; composition and partial application

(defun disjoin (predicate &rest more-predicates)
  "Return the function, that applies each of <_:arg predicate /> and
<_:arg more-predicates /> functions in turn to its arguments,
returning the primary value of the first predicate that returns true,
without calling the remaining predicates.
If none of the predicates returns true, NIL is returned."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((predicate (ensure-function predicate))
	(more-predicates (mapcar #'ensure-function more-predicates)))
    (lambda (&rest arguments)
      (or (apply predicate arguments)
	  (some (lambda (p)
		  (declare (type function p))
		  (apply p arguments))
		more-predicates)))))

(defun conjoin (predicate &rest more-predicates)
  "Return the function, that applies each of <_:arg predicate /> and
<_:arg more-predicates /> functions in turn to its arguments,
returning NIL if any of the predicates returns false,
without calling the remaining predicates. If none of the predicates
returns false, returns the primary value of the last predicate."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (lambda (&rest arguments)
    (and (apply predicate arguments)
	 ;; Cannot simply use CL:EVERY because we want to return the
	 ;; non-NIL value of the last predicate if all succeed.
         (do ((tail (cdr more-predicates) (cdr tail))
              (head (car more-predicates) (car tail)))
             ((not tail)
              (apply head arguments))
           (unless (apply head arguments)
             (return nil))))))


(defun compose (function &rest more-functions)
  "Return the function, composed of <_:arg function /> and
<_:arg more-functions />,  that applies its arguments to each in turn,
starting from the rightmost of <_:arg more-functions />, and then
calling the next one with the primary value of the last"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (reduce (lambda (f g)
	    (let ((f (ensure-function f))
		  (g (ensure-function g)))
	      (lambda (&rest arguments)
		(declare (dynamic-extent arguments))
		(funcall f (apply g arguments)))))
          more-functions
          :initial-value function))

(define-compiler-macro compose (function &rest more-functions)
  (labels ((compose-1 (funs)
             (if (cdr funs)
                 `(funcall ,(car funs) ,(compose-1 (cdr funs)))
                 `(apply ,(car funs) arguments))))
    (let* ((args (cons function more-functions))
           (funs (make-gensym-list (length args) "COMPOSE")))
      `(let ,(loop for f in funs for arg in args
		   collect `(,f (ensure-function ,arg)))
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest arguments)
           (declare (dynamic-extent arguments))
           ,(compose-1 funs))))))

(defun multiple-value-compose (function &rest more-functions)
    "Return the function, composed of <_:arg function /> and
<_:arg more-functions />,  that applies its arguments to each in turn,
starting from the rightmost of <_:arg more-functions />, and then
calling the next one with all the return values of the last"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (reduce (lambda (f g)
	    (let ((f (ensure-function f))
		  (g (ensure-function g)))
	      (lambda (&rest arguments)
		(declare (dynamic-extent arguments))
		(multiple-value-call f (apply g arguments)))))
          more-functions
          :initial-value function))

(define-compiler-macro multiple-value-compose (function &rest more-functions)
  (labels ((compose-1 (funs)
             (if (cdr funs)
                 `(multiple-value-call ,(car funs) ,(compose-1 (cdr funs)))
                 `(apply ,(car funs) arguments))))
    (let* ((args (cons function more-functions))
           (funs (make-gensym-list (length args) "MV-COMPOSE")))
      `(let ,(mapcar #'list funs args)
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest arguments)
           (declare (dynamic-extent arguments))
           ,(compose-1 funs))))))

(defun curry (function &rest arguments)
  "Return the function, that applies <_:arg arguments /> and the arguments
it is called with to <_:arg function />"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((fn (ensure-function function)))
    (lambda (&rest more)
      (declare (dynamic-extent more))
      ;; Using M-V-C we don't need to append the arguments.
      (multiple-value-call fn (values-list arguments) (values-list more)))))

(define-compiler-macro curry (function &rest arguments)
  (let ((curries (make-gensym-list (length arguments) "CURRY")))
    `(let ,(mapcar #'list curries arguments)
       (declare (optimize (speed 3) (safety 1) (debug 1)))
       (lambda (&rest more)
         (apply ,function ,@curries more)))))

(defun rcurry (function &rest arguments)
  "Return the function, that applies the arguments it is called with
and <_:arg arguments /> to <_:arg function />"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((fn (ensure-function function)))
    (lambda (&rest more)
      (declare (dynamic-extent more))
      (multiple-value-call fn (values-list more) (values-list arguments)))))

;;; end