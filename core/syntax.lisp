;; For license see LICENSE

(in-package #:reasonable-utilities.syntax)

(declaim (optimize (speed 3) (space 1) (debug 0)))


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

;; predicate case

(defun expand-predicate-case (pred keyform clauses case)
  `(once-only (,keyform)
     `(cond
        ,@(loop :for (key actions) :in ,clauses
             :collect (cons (if (and (eq ,case 'case) (eq key 'otherwise))
                                t
                                `(funcall ,',pred ,keyform ,key))
                            actions))
        ,@(ecase case
            (case nil)
            (ccase '((t (cerror 'case-failure))))
            (ecase '((t error  'case-failure)))))))

(defmacro pcase (pred keyform &rest clauses)
  "Like CASE, but uses given PRED instead of EQL to select appropriate CLAUSE.
Example usage:
CL-USER> (pcase '< 1
           (0  (print \"Below zero\"))
           (2  (print \"OK\"))
           (otherwise (error \"Oops\")))
"
  (expand-predicate-case pred keyform clauses 'case))

(defmacro pcase (pred keyform &rest clauses)
  "Like CCASE, but uses given PRED instead of EQL to select appropriate CLAUSE.
Example usage:
CL-USER> (pccase '< 1
           (0  (print \"Below zero\"))
           (2  (print \"OK\")))
"
  (expand-predicate-case pred keyform clauses 'ccase))

(defmacro pecase (pred keyform &rest clauses)
  "Like ECASE, but uses given PRED instead of EQL to select appropriate CLAUSE.
Example usage:
CL-USER> (pecase '< 1
           (0  (print \"Below zero\"))
           (2  (print \"OK\")))
"
  (expand-predicate-case pred keyform clauses 'ecase))


;; desctructuring case

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

The clause whose CASE-KEYS matches CAR of KEY, as if by CASE, CCASE, or ECASE,
is selected, and FORMs are then executed with CDR of KEY is destructured and
bound by the DESTRUCTURING-LAMBDA-LIST.

Example:

 (defun dcase-test (x)
   (dcase x
     ((:foo a b)
      (format nil \"foo: ~S, ~S\" a b))
     ((:bar &key a b)
      (format nil \"bar, ~S, ~S\" a b))
     (((:alt1 :alt2) a)
      (format nil \"alt: ~S\" a))
     ((t &rest rest)
      (format nil \"unknown: ~S\" rest))))

  (dcase-test (list :foo 1 2))        ; => \"foo: 1, 2\"
  (dcase-test (list :bar :a 1 :b 2))  ; => \"bar: 1, 2\"
  (dcase-test (list :alt1 1))         ; => \"alt: 1\"
  (dcase-test (list :alt2 2))         ; => \"alt: 2\"
  (dcase-test (list :quux 1 2 3))     ; => \"unknown: 1, 2, 3\"

 (defun decase-test (x)
   (dcase x
     ((:foo a b)
      (format nil \"foo: ~S, ~S\" a b))
     ((:bar &key a b)
      (format nil \"bar, ~S, ~S\" a b))
     (((:alt1 :alt2) a)
      (format nil \"alt: ~S\" a))))

  (decase-test (list :foo 1 2))        ; => \"foo: 1, 2\"
  (decase-test (list :bar :a 1 :b 2))  ; => \"bar: 1, 2\"
  (decase-test (list :alt1 1))         ; => \"alt: 1\"
  (decase-test (list :alt2 2))         ; => \"alt: 2\"
  (decase-test (list :quux 1 2 3))     ; =| error
"
  (expand-destructuring-case keyform clauses 'case))

(defmacro dccase (keyform &body clauses)
  "DCCASE is a combination of CCASE and DESTRUCTURING-BIND.
KEYFORM must evaluate to a CONS.

Clauses are of the form:

  ((CASE-KEYS . DESTRUCTURING-LAMBDA-LIST) FORM*)

The clause whose CASE-KEYS matches CAR of KEY, as if by CASE, CCASE, or ECASE,
is selected, and FORMs are then executed with CDR of KEY is destructured and
bound by the DESTRUCTURING-LAMBDA-LIST.

Example:


 (defun dccase-test (x)
   (dcase x
     ((:foo a b)
      (format nil \"foo: ~S, ~S\" a b))
     ((:bar &key a b)
      (format nil \"bar, ~S, ~S\" a b))
     (((:alt1 :alt2) a)
      (format nil \"alt: ~S\" a))))

  (decase-test (list :foo 1 2))        ; => \"foo: 1, 2\"
  (decase-test (list :bar :a 1 :b 2))  ; => \"bar: 1, 2\"
  (decase-test (list :alt1 1))         ; => \"alt: 1\"
  (decase-test (list :alt2 2))         ; => \"alt: 2\"
  (decase-test (list :quux 1 2 3))     ; =| continueable error
"
  (expand-destructuring-case keyform clauses 'ccase))

(defmacro decase (keyform &body clauses)
  "DECASE is a combination of ECASE and DESTRUCTURING-BIND.
KEYFORM must evaluate to a CONS.

Clauses are of the form:

  ((CASE-KEYS . DESTRUCTURING-LAMBDA-LIST) FORM*)

The clause whose CASE-KEYS matches CAR of KEY, as if by CASE, CCASE, or ECASE,
is selected, and FORMs are then executed with CDR of KEY is destructured and
bound by the DESTRUCTURING-LAMBDA-LIST.

Example:

 (defun decase-test (x)
   (dcase x
     ((:foo a b)
      (format nil \"foo: ~S, ~S\" a b))
     ((:bar &key a b)
      (format nil \"bar, ~S, ~S\" a b))
     (((:alt1 :alt2) a)
      (format nil \"alt: ~S\" a))))

  (decase-test (list :foo 1 2))        ; => \"foo: 1, 2\"
  (decase-test (list :bar :a 1 :b 2))  ; => \"bar: 1, 2\"
  (decase-test (list :alt1 1))         ; => \"alt: 1\"
  (decase-test (list :alt2 2))         ; => \"alt: 2\"
  (decase-test (list :quux 1 2 3))     ; =| error
"
  (expand-destructuring-case keyform clauses 'ecase))


;; switch

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
    (when (and (consp default)
               (member (first default) '(error cerror)))
      (setf default `(,@default "No keys match in SWITCH. Testing against ~S with ~S."
                      ,value ',test)))
    `(let ((,value (,key ,object)))
      (cond ,@(mapcar (lambda (clause)
                        (if (member (first clause) '(t otherwise))
                            (progn
                              (when default
                                (error "Multiple default clauses or illegal use of a default clause in ~S."
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
  "Evaluates first matching clause, returning its values, or evaluates and
returns the values of DEFAULT if no keys match."
  (generate-switch-body whole object clauses test key))

(defmacro eswitch (&whole whole (object &key (test 'eql) (key 'identity))
                          &body clauses)
  "Like SWITCH, but signals an error if no key matches."
  (generate-switch-body whole object clauses test key '(error)))

(defmacro cswitch (&whole whole (object &key (test 'eql) (key 'identity))
                          &body clauses)
  "Like SWITCH, but signals a continuable error if no key matches."
  (generate-switch-body whole object clauses test key
                        '(cerror "Return NIL from CSWITCH.")))

(defmacro dotable ((k v table &optional rez) &body body)
  "Like DOLIST but iterates over key-value pairs (K V) in anything, that can be
   viewed as a table (hash-table, alist, plist, object).
   Autodeclares variables named _ as ignored."
  (with-gensyms (pair)
    (once-only (table)
      (let ((_ (find "_" (list k v)
                     :test 'string=
                     :key 'symbol-name)))
        `(block nil
           (etypecase ,table
             (hash-table (maphash (lambda (,k ,v)
                                    ,(when _ `(declare (ignore ,_)))
                                    ,@body)
                                  ,table))

             (list (if (rutils.list:alistp ,table)
                       (dolist (,pair ,table)
                         (destructuring-bind (,k . ,v) ,pair
                           ,(when _ `(declare (ignore ,_)))
                           ,@body))
                       (error 'simple-type-error
                              :format-control "Can't iterate over proper list ~
                                               in DOTABLE: need an alist"))))
           ,rez)))))


(defmacro multiple-value-prog2 (first-form second-form &body forms)
  "Evaluates FIRST-FORM, then SECOND-FORM, and then FORMS. Yields as its value
all the value returned by SECOND-FORM."
  `(progn ,first-form (multiple-value-prog1 ,second-form ,@forms)))


;; bind

(defmacro bind ((&rest bindings) &body body)
  "Bind variables from BINDINGS to be active inside BODY, as if by LET*,
combined with MULTIPLE-VALUE-BIND, DESTRUCTURING-BIND and other -bind forms,
depending on the type of the first argument."
  (let ((rez body))
    (dolist (binding (reverse bindings) (car rez))
      (setf rez `((,@(funcall #'expand-binding binding rez)))))))

(defun expand-binding (binding form)
  (append (apply #'bind-dispatch binding)
          form))

(defgeneric bind-dispatch (arg &rest args)
  (:method ((arg symbol) &rest args)
    (if (cdr args)
        `(multiple-value-bind (,arg ,@(butlast args)) ,(car (last args)))
        `(let ((,arg ,(car args))))))
  (:method ((arg list) &rest args)
    `(destructuring-bind ,arg ,args))
  (:method ((arg hash-table) &rest args)
    `(let (,@(let (bindings)
               (dotable (k v arg (reverse bindings))
                 (push (list v `(gethash ,k ,(car args)))
                       bindings)))))))


#+:cl-ppcre
(defmethod bind-dispatch ((arg string) &rest args)
  (assert (cdr args))
  `(ppcre:register-groups-bind ,(car args) (,arg ,(cadr args))))
