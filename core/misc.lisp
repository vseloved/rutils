;; For license see LICENSE

(in-package #:rutils.misc)
(named-readtables:in-readtable rutils-readtable)
(declaim #.+default-opts+)


(declaim (inline or2 and2 xor2 void true))


(defmacro multiple-value-prog2 (first-form second-form &body forms)
  "Evaluates FIRST-FORM, then SECOND-FORM, and then FORMS. Yields as its value
   all the value returned by SECOND-FORM."
  `(progn ,first-form (multiple-value-prog1 ,second-form ,@forms)))

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

(define-modify-macro coercef (type-spec) coerce "Modify-macro for COERCE.")

(defmacro void (place)
  "Nullify PLACE."
  `(setf ,place nil))

(defmacro re-setf (var &rest clauses)
  "SETFs VAR to the value of each clause subsequently.
   Re-uses the new value of VAR in subsequent clauses.
   Expects VAR to be a valid place for SETF."
  `(progn ,@(mapcar (lambda (clause)
                      `(setf ,var ,clause))
                    clauses)))

(defun true (val)
  "The complement to NULL.
   Unlike IDENTITY will return T if VAL is not NIL."
  (and val t))

(defmacro 2nd (form)
  "(NTH-VALUE 1 FORM)"
  `(nth-value 1 ,form))

(defmacro once-only (specs &body forms)
  "Evaluate FORMS with names rebound to temporary variables, ensuring
   that each is evaluated only once.

   Each SPEC must be either a NAME, or a (NAME INITFORM), with plain
   NAME using the named variable as initform.

   Example:

       CL-USER> (defmacro cons1 (x)
                  (once-only (x)
                    `(cons ,x ,x)))
       CL-USER> (let ((y 0))
                  (cons1 (incf y)))
       (1 . 1)
"
  (let ((gensyms (make-gensym-list (length specs) "OO"))
        (names-and-forms (mapcar (lambda (spec)
                                   (etypecase spec
                                     (list
                                      (destructuring-bind (name form) spec
                                        (cons name form)))
                                     (symbol
                                      (cons spec spec))))
                                 (remove-if-not #`(or (symbolp %) (listp %))
                                                specs))))
    ;; bind in user-macro
    `(let ,(mapcar #`(list % `(gensym ,(string (car %%))))
                   gensyms names-and-forms)
       ;; bind in final expansion
       `(let (,,@(mapcar (lambda (g n) ``(,,g ,,(cdr n)))
                         gensyms names-and-forms))
          ;; bind in user-macro
          ,(let ,(mapcar #`(list (car %) %%)
                         names-and-forms gensyms)
             ,@forms)))))

(define-condition case-failure (#+sbcl sb-kernel:case-failure
                                type-error)
  ((name :reader case-failure-name :initarg :name)
   (possibilities :reader case-failure-possibilities :initarg :possibilities))
  (:report
    (lambda (condition stream)
      (format stream "~@<~S fell through ~S expression. ~
                      ~:_Wanted one of ~:S.~:>"
              (type-error-datum condition)
              (slot-value condition 'name)
              (slot-value condition 'possibilities)))))


;;; Predicate case

(defun expand-predicate-case (pred keyform clauses case)
  (once-only (keyform)
    (let ((cases (loop :for (key actions) :in clauses
                    :collect (list (or (and (eql case 'case)
                                            (eql key 'otherwise))
                                       `(funcall ,pred ,keyform ,key))
                                   actions))))
      `(cond
         ,@cases
         ,@(ecase case
             (case nil)
             (ecase '((t (error 'case-failure))))
             (ccase `((t (cerror "Return NIL from PCASE" 'case-failure
                                 :datum ,keyform
                                 :possibilities ',(mapcar #'first cases)
                                 :name 'pccase)))))))))

(defmacro pcase (pred keyform &rest clauses)
  "Like CASE, but uses given PRED instead of EQL to select appropriate CLAUSE.

   Example usage:

       CL-USER> (pcase '< 1
                  (0 (print \"Below zero\"))
                  (2 (print \"OK\"))
                  (otherwise (error \"Oops\")))
"
  (expand-predicate-case pred keyform clauses 'case))

(defmacro pccase (pred keyform &rest clauses)
  "Like CCASE, but uses given PRED instead of EQL to select appropriate CLAUSE.

   Example usage:

       CL-USER> (pccase '< 1
                  (0 (print \"Below zero\"))
                  (2 (print \"OK\")))
"
  (expand-predicate-case pred keyform clauses 'ccase))

(defmacro pecase (pred keyform &rest clauses)
  "Like ECASE, but uses given PRED instead of EQL to select appropriate CLAUSE.

   Example usage:

       CL-USER> (pecase '< 1
                  (0 (print \"Below zero\"))
                  (2 (print \"OK\")))
"
  (expand-predicate-case pred keyform clauses 'ecase))


;;; Desctructuring case

(defun expand-destructuring-case (key clauses case)
  (once-only (key)
    `(if (typep ,key 'cons)
         (,case (car ,key)
           ,@(mapcar (lambda (clause)
                       (destructuring-bind ((keys . lambda-list) &body body)
                           clause
                         `(,keys
                           (destructuring-bind ,lambda-list (cdr ,key)
                             ,@body))))
                     clauses))
         (error "Invalid key to D~S: ~S" ',case ,key))))

(defmacro dcase (keyform &body clauses)
  "DCASE is a combination of CASE and DESTRUCTURING-BIND.
   KEYFORM must evaluate to a CONS.

   Clauses are of the form:

       ((CASE-KEYS . DESTRUCTURING-LAMBDA-LIST) FORM*)

   The clause whose CASE-KEYS matches CAR of KEY, as if by CASE,
   is selected, and FORMs are then executed with CDR of KEY is destructured and
   bound by the DESTRUCTURING-LAMBDA-LIST."
  (expand-destructuring-case keyform clauses 'case))

(defmacro dccase (keyform &body clauses)
  "DCCASE is a combination of CCASE and DESTRUCTURING-BIND.
   KEYFORM must evaluate to a CONS.

   Clauses are of the form:

       ((CASE-KEYS . DESTRUCTURING-LAMBDA-LIST) FORM*)

   The clause whose CASE-KEYS matches CAR of KEY, as if by CCASE,
   is selected, and FORMs are then executed with CDR of KEY is destructured and
   bound by the DESTRUCTURING-LAMBDA-LIST."
  (expand-destructuring-case keyform clauses 'ccase))

(defmacro decase (keyform &body clauses)
  "DECASE is a combination of ECASE and DESTRUCTURING-BIND.
   KEYFORM must evaluate to a CONS.

   Clauses are of the form:

       ((CASE-KEYS . DESTRUCTURING-LAMBDA-LIST) FORM*)

   The clause whose CASE-KEYS matches CAR of KEY, as if by ECASE,
   is selected, and FORMs are then executed with CDR of KEY is destructured and
   bound by the DESTRUCTURING-LAMBDA-LIST."
  (expand-destructuring-case keyform clauses 'ecase))


;;; Switch

(defun extract-function-name (spec)
  "Useful for macros that want to mimic the functional interface for functions
   like #'eq and 'eq."
  (if (and (consp spec)
           (member (first spec) '(quote function)))
      (second spec)
      spec))

(defun generate-switch-body (whole object clauses test key &optional default)
  (with-gensyms (value)
    (setf test (extract-function-name test))
    (setf key (extract-function-name key))
    `(let ((,value (,key ,object)))
      (cond ,@(mapcar (lambda (clause)
                        (if (member (first clause) '(t otherwise))
                            (progn
                              (when default
                                (error "Multiple default clauses or illegal use ~
                                        of a default clause in ~S."
                                       whole))
                              (setf default `(progn ,@(rest clause)))
                              '(()))
                            (destructuring-bind (key-form &body forms) clause
                              `((,test ,value ,key-form)
                                ,@forms))))
                      clauses)
            (t ,default)))))

(defmacro switch (&whole whole (object &key (test 'eql) (key 'identity))
                         &body clauses)
  "Evaluate first matching clause, returning its values, or evaluates and
   returns the values of DEFAULT if no keys match."
  (generate-switch-body whole object clauses test key))

(defmacro cswitch (&whole whole (object &key (test 'eql) (key 'identity))
                          &body clauses)
  "Like SWITCH, but signals a continuable error if no key matches."
  (generate-switch-body whole object clauses test key
                        `(cerror "Return NIL from SWITCH" 'case-failure
                                 :datum ,object
                                 :possibilities ',(mapcar #'first clauses)
                                 :name 'cswitch)))

(defmacro eswitch (&whole whole (object &key (test 'eql) (key 'identity))
                          &body clauses)
  "Like SWITCH, but signals an error if no key matches."
  (generate-switch-body whole object clauses test key
                        `(error 'case-failure
                                :datum ,object
                                :possibilities ',(mapcar #'first clauses)
                                :name ,(format nil "ESWITCH with test ~A" test))))
