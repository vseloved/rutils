;;; RUTILS ITER macro with keywords
;;; see LICENSE file for permissions

;;;                 Copyright 1989 by Jonathan Amsterdam
;;;         Adapted to ANSI Common Lisp in 2003 by Andreas Fuchs

;;; To avoid confusiomn with original Iterate, only ITER name is exported
;;; (instead of ITERATE) and only keyword keywords are allowed inside ITER
;;; (instead of symbol keywords from ITERATE package).
;;; See iter.txt for details

(in-package #:reasonable-utilities.iter)

(declaim (declaration declare-variables))

(locally-enable-literal-syntax :sharp-backq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utils

(defvar *genvar-counter* 0
  "Counter of <_:fun genvar />'s")

(defun genvar (&optional (string "TEMP"))
  "A cross between <_:fun gensym /> and <_:fun gentemp />"
  (prog1 (make-symbol (format nil "~a~d" string *genvar-counter*))
    (incf *genvar-counter*)))

(defun synonym (symbol)
  "Get <_:arg synonym /> for a <_:arg symbol />. When there's no
<_:arg synonym />, return the <_:arg symbol /> itself"
  (or (get symbol 'synonym) symbol))

(defmacro defsynonym (symbol synonym)
  "Set <_:arg synonym /> for a <_:arg symbol /> "
  `(eval-always
     (setf (get ',symbol 'synonym) ',synonym)))

(defmacro augment (var stuff)
  "Add <_:arg stuff /> to the end of <_:arg var /> list"
  `(setf ,var (nconc ,var ,stuff)))

(defmacro prepend (stuff var)
  "Add <_:arg stuff /> to the beginning of <_:arg var /> list"
  `(setf ,var (nconc ,stuff ,var)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants

(defconst iterate-version "1.4.3"
  "Current version of Iterate")

(defconst +fill-col+ 77)

(defconst +standard-type-symbols+  ; of CLtL2
  '(array atom bignum bit bit-vector boolean character compiled-function
    complex cons double-float fixnum float function hash-table integer
    keyword list long-float nil null number package pathname random-state
    ratio rational readtable real sequence short-float signed-byte simple-array 
    simple-bit-vector simple-string simple-vector single-float standard-char
    stream string string-char symbol t unsigned-byte vector)
  "Table 4-1 of the Common Lisp Manual")


;;; These next two can be used for maximizing and minimizing.

#+nil  ; unused
(defconst smallest-number-alist
  `((fixnum . ,most-negative-fixnum)
    (float . ,most-negative-long-float)
    (long-float . ,most-negative-long-float)
    (short-float . ,most-negative-short-float)
    (double-float . ,most-negative-double-float)
    (single-float . ,most-negative-single-float))
  "Number types")

#+nil  ; unused
(defconst largest-number-alist
  `((fixnum . ,most-positive-fixnum)
    (float . ,most-positive-long-float)
    (long-float . ,most-positive-long-float)
    (short-float . ,most-positive-short-float)
    (double-float . ,most-positive-double-float)
    (single-float . ,most-positive-single-float))
  "Number types")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Controls (global variables)

(defvar *always-declare-variables* nil
  "Like <_:code (declare (declare-variables)) />")

(defparameter *list-end-test* 'atom
  "This is so the advanced user can choose how the end of a list
is checked for.
There are three choices for termination predicate in
<_:iter-clause for...on /> and <_:iter-clause for...in />,
differing in their behavior on lists with a non-nil cdr:
<_ :type list
* NULL: If lucky, will get an error when taking the cdr. Bad choice
* ATOM: Will terminate correctly with no error
* ENDP: Will give an appropriate error message />")

(defvar *result-var* nil
  "<_:var *result-var* /> is bound to a <_:fun gensym /> before the
clauses of an iterate form are processed. In the generated code,
the <_:fun gensym /> is bound to nil before any other bindings are
performed. Clauses are free to generate code that sets
the value of <_:var *result-var* />.")

(defvar *type-alist* nil
  "Iterate binds <_:var *type-alist* /> to an alist of variables and
their types before processing clauses. It does this by looking at
<_:code (declare (type ...)) /> forms in the clauses and recording
the information there. (Only variable type information, not function)")

(defvar *declare-variables* nil
  "<_:var *declare-variables* /> is bound to T if the 
<_:code (declare (iterate:declare-variables)) /> was seen at top-level,
or if <_:var *always-declare-variables* /> is non-nil. This indicates,
that variables, that haven't been declared by the user should be
declared to have the appropriate types.  What 'appropriate' means
depends on the context.")

(defvar *clause* nil
  "<_:var *clause* /> is bound to each entire iterate clause before the
clause is processed. Mostly for error output (see <_:class clause-error />)")

(defvar *top-level?* nil
  "<_:var *top-level?* /> is bound to T at top-level (i.e. before any
forms, that contain clauses inside them, like <_:fun if />, <_:fun let />,
etc.) and to NIL inside such forms. It is useful to ensure, that certain
forms (particularly iteration drivers) occur only at top-level")

(defvar *declaration-context?* nil
  "<_:var *declaration-context?* /> is bound to T inside a form,
that allows declarations (<_:fun flet />, <_:fun labels />)."
;; We used to just see if *internal-variables* was non-nil,
;; but that's wrong -- you can be inside a binding context,
;; that binds no variables
)

(defvar *bindings* nil
  "For the use of <_:fun make-binding-internal />, to pass back bindings.
<_:code (if first-time) /> also uses it to create first-time variables")

(defvar *internal-variables* nil
  "This is a list of lists, containing the variables, made by
internal <_:fun let />s or other binding forms. It is used to check
for the error of having iterate try to bind one of these variables at
top-level.  E.g.
<_:code (iter (:for i :from 1 :to 10)
              (let ((a nil))
                (:collect i :into a))) />
is an error.")

(defvar *declarations* nil
  "For functions (like <_:fun make-binding />), that don't want to
or can't pass declarations normally. These are really decl-specs,
not full declarations.")


(defvar *accum-var-alist* nil
  "This is how we get multiple accumulations into the same variable
to come out right.  See <_:fun make-accum-var-binding />.
It's an alist of <_:pseudo (accum-var kind <possibly other info>) />.
The currently used kinds are:
<_ :type list
* :collect   - for collect, nconc, append, etc.
* :increment - for count, sum and multiply
* :max       - for maximize
* :min       - for minimize
* :if-exists - for always/never/thereis and finding such-that />
Note that we do not check for type conflict in the re-use of these
variables.")

(defvar *shared-bindings-alist* nil
  "Shared variables created by <_:fun make-shared-binding />.
It's an alist of <_:pseudo (name gensym-var <possibly other info>) />.
Tipical use is <_:iter-clause first-iteration-p />")

(defvar *block-name* nil
  "Name of the block for this <_:fun iterate /> form. Used in generating
return statements")

(defvar *clause-info-index* (list :index)
  "The index of standard clauses (a discrimination tree). This is a
<_:fun defvar />, so that reloading doesn't clobber existing defs
\(though it will clobber those clauses, that are defined in this file)")

(eval-when (:compile-toplevel)
  (unless (boundp '*clause-info-index*)
    (setq *clause-info-index* (list :index))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special-forms

(defparameter *special-form-alist*
  '(;; First the special operators that every code walker must recognize
    (block .                walk-cddr) 
    (catch .                walk-cdr)
    (declare .              walk-declare)
    (eval-when .            walk-cddr)
    (flet .                 walk-flet)
    (function .             walk-function)
    (go .                   nil)
    (if .                   walk-cdr)  ; also walk test form
    (labels .               walk-flet)
    (let .                  walk-let)
    (let* .                 walk-let)
    (load-time-value .      nil)
    (locally .              walk-cdr-with-declarations)
    (macrolet .             walk-macrolet)
    (multiple-value-call .  walk-cdr)
    (multiple-value-prog1 . walk-cdr)
    (progn .                walk-progn)
    (progv .                walk-cdr)
    (quote .                nil)
    (return-from .          walk-cddr)
    (setq .                 walk-setq)
    (symbol-macrolet .      walk-cddr-with-declarations)
    (tagbody .              walk-cdr)
    (the .                  walk-cddr)
    (throw .                walk-cdr) 
    (unwind-protect .       walk-cdr)

    ;; Next some special cases:
    ;; m-v-b is a macro, not a special form, but we want to recognize bindings.
    ;; Furthermore, Lispworks macroexpands m-v-b into some unknown m-v-BIND-call
    ;; special form.
    (multiple-value-bind .     walk-multiple-value-bind)
    ;; Allegro treats cond as a special form, it does not macroexpand.
    #+allegro (cond .          walk-cond)
    ;; Prior to 2005, CLISP expanded handler-bind into some
    ;; sys::%handler-bind syntax not declared as a special operator.
    #+clisp (handler-bind .    walk-cddr)
    ;; does not recognize clauses in handlers

    ;; A suitable generalization would be a pattern language that describes
    ;; which car/cdr are forms to be walked, declarations or structure.
    ;; Walk with-*-iterator ourselves in order to avoid macrolet warnings.
    ;; Note that walk-cddr-with-declarations won't walk the
    ;; package/hash-table descriptor argument, but it's good enough for now.
    (with-package-iterator .    walk-cddr-with-declarations)
    (with-hash-table-iterator . walk-cddr-with-declarations)

    ;; Finally some cases where code, compiled from the macroexpansion,
    ;; may not be as good as code compiled from the original form and
    ;; iterate's own expansion becomes more readable
    (and .                      walk-cdr)
    (ignore-errors .            walk-cdr)  ; expands to handler-bind in CLISP
    (multiple-value-list .      walk-cdr)
    (multiple-value-setq .      walk-cddr)
    (nth-value .                walk-cdr)
    (or .                       walk-cdr)
    (prog1 .                    walk-cdr)
    (prog2 .                    walk-cdr)
    (psetq .                    walk-setq))
  "An alist of lisp special forms and the functions for handling them.
nil as function means leave form as-is")

(defvar *special-clause-alist* nil
  "Clauses, that are 'special' in the sense, that they don't conform to the
keyword-argument syntax of Iterate clauses")


;;; These two are for conserving temporaries. *temps* is a list
;;; of temporaries that have already been created and given bindings.
;;; *temps-in-use* is a list of temporaries that are currently being used.
;;; See with-temporary, with-temporaries.
;;; This seems to stem from a time where it was more efficient to use
;;; (prog (temp)
;;;    ... (setq temp #) ; somewhere deep inside the body
;;;        (foo temp)
;;;        (bar temp)
;;;    ...)
;;; than using a local let deep inside that body, as in
;;; (tagbody ... (let ((temp #)) (foo temp) (bar temp)) ...)
;;; which may be easier for compiler data flow and lifetime analysis.

(defvar *temps* nil)
(defvar *temps-in-use* nil)

(defvar *env* nil
  "Environment, for <_:fun macroexpand />")

(defvar *driver-info-alist* nil
  "List of info about drivers for use by the <_:iter-clause next /> mechanism")

(defvar *previous-vars-alist* nil
  "Used by <_:iter-clause previous />")

;; loop labels

(defvar *loop-top* nil
  "A <_:fun loop /> label")
(defvar *loop-step* nil
  "A <_:fun loop /> label")
(defvar *loop-end* nil
  "A <_:fun loop /> label")
(defvar *loop-step-used?* nil
  "Whether a label was used already (to avoid generating them)?
This is so we don't get a warning from compilers that check for unused tags.")
(defvar *loop-end-used?* nil
  "Whether a label was used already (to avoid generating them)?
This is so we don't get a warning from compilers that check for unused tags.")
(defvar *loop-body-wrappers* nil
  "Things that we should wrap the <_:fun loop />'s body in")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structures

(eval-always
  (defstruct clause-info
    "Clause-info structures, which are put in the clause index"
    function
    keywords
    req-keywords
    doc-string
    generator?)

  (defstruct driver-info
    "Driver-info structures, for information about driver variables --
used by <_:iter-clause next />"
    next-code
    generator?
    (used nil))


  (defstruct previous-info
    "Info structure, used by the <_:iter-clause previous /> mechanism"
    var
    save-info-list
    code
    (class :step))

  (defstruct save-info
    "Info structure, used by the <_:iter-clause previous /> mechanism"
    save-var
    save-vars
    iv-ref)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The ITER macro

(defmacro iter (&body body &environment env)
  "Jonathan Amsterdam's powerful and extensible iteration facility,
providing multiple accumulation, generators, memory of previous
iterations, over 50 clauses to start with and a Lisp-like syntax.
Evaluate (iter:display-iterate-clauses) for an overview of clauses"
  (let* ((*env* env)
         (*result-var* (genvar 'result))
         (*type-alist* nil)
         (*declare-variables* *always-declare-variables*)
         (*bindings* nil)
         (*internal-variables* nil)
         (*previous-vars-alist* nil)
         (*declarations* nil)
         (*loop-body-wrappers* nil)
         (*accum-var-alist* nil)
         (*shared-bindings-alist* nil)
         (*top-level?* t)
         (*declaration-context?* nil)
         (*temps* nil)
         (*temps-in-use* nil)
         (*driver-info-alist* nil)
         (*block-name* (when (symbolp (car body))
                         (pop body)))
         (*loop-top*  (mksym *block-name* :format "loop-top-~a"))
         (*loop-step* (mksym *block-name* :format "loop-step-~a"))
         (*loop-end*  (mksym *block-name* :format "loop-end-~a"))
         (*loop-step-used?* nil)
         (*loop-end-used?* nil))
    (process-top-level-decls body)
    (bind ((body decls init-code steppers final-code final-prot
                 (walk-list body))
           (init step (insert-previous-code)))
      (augment init-code init)
      (augment steppers step)
      (prepend (default-driver-code) body)
      (let ((it-bod `(block ,*block-name*
                       (tagbody
                          ,.init-code
                          ,*loop-top*
                          ,.body
                          ,.(when *loop-step-used?* (list *loop-step*))
                          ,.steppers
                          (go ,*loop-top*)
                          ,.(when *loop-end-used?* (list *loop-end*))
                          ,.final-code)
                       ,(if (member *result-var* *bindings* :key #'car)
                            *result-var*
                            nil))))
        (wrap-form *loop-body-wrappers*
                   `(let* ,(nreverse *bindings*)
                      ,.(if *declarations*
                            `((declare .,*declarations*)))
                      ,.decls
                      ,(if final-prot 
                           `(unwind-protect ,it-bod .,final-prot)
                           it-bod)))))))

(defun process-top-level-decls (clauses)
  "Process top-level declarations, present among <_:arg clauses />
Sets <_:var *type-alist* /> to an alist of (var . type), and
sets <_:var *declare-variables* /> to t, when such a declaration was seen"
  (dolist (clause clauses)
    (when (and (consp clause) (eq (car clause) 'declare))
      (dolist (spec (cdr clause))
        (let ((type (car spec)))
          (cond ((eq type 'declare-variables)
                 (setq *declare-variables* t))
                ((or (eq type 'type)  ; We don't do ftypes
                     ;; FIXME: recognize all shorthand type declarations
                     ;; e.g. (declare ((unsigned-byte 8) x) etc.
                     ;; -- but how to recognize type specifications?
                     (member type +standard-type-symbols+ :test #'eq))
                 (let ((vars (cdr spec)))
                   (when (eq type 'type)
                     (setq type (pop vars)))
                   (dolist (var vars)
                     (push (cons var type) *type-alist*))))))))))


(defun default-driver-code ()
  "Default code, that will be executed if no drivers are defined"
  nil)

(defun wrap-form (wrappers form)
  "Wrap <_:arg form /> with <_:arg wrappers />, that should be
given as a list"
  (if (consp wrappers)
      (wrap-form (cdr wrappers)
                 (nconc (copy-list (car wrappers))
                        (list form)))
      form))

(defun add-loop-body-wrapper (wrapper)
  "Ad a <_:arg wrapper /> to <_:var *loop-body-wrappers* />,
if it's not already there"
  (pushnew wrapper *loop-body-wrappers* :test #'equalp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The code walker

(defun walk (form)
  "Walk the <_:arg form /> and return the usual 6 things as multiple-values:
<_ :type list
* body
* type declarations
* initializations code
* stepping code
* final code
* final protected (by <_:fun unwind-protect />) code />"
  (cond
   ((atom form)
    ;; different processing for symbol-macros and regular symbols
    (multiple-value-bind (ex-form expanded?) (macroexpand-1 form *env*)
      (if expanded? (walk ex-form)
          (list form))))
   ((symbolp (car form))
    (cond
      ;; The ordering of these checks is such that:
      ;; 1. We handle special operators that any Common Lisp code walker
      ;;    must recognize and some special cases (like Allegro's cond).
      ;;    This is declared in *special-form-alist* 
      ;; 2. Then we expand macros.
      ;; 3. Then only do we recognize Iterate clauses
      ;;    -- which may thus be shadowed (e.g. by macros)
      ;;
      ;; Note that implementations are permitted to let SPECIAL-OPERATOR-P
      ;; return T for any macros (e.g. CLISP for WHEN). Yet they must provide
      ;; a macroexpansion for these.

     ((special-form? (car form)) (walk-special-form form))

     ((macro-function (car form) *env*)
      ;; Some compilers (e.g. Lucid on Sparcs) treat macros differently at
      ;; compile-time; macroexpand does not expand them.  We assume that if
      ;; this happens, macroexpand's second value is nil.  
      ;;   What do we do with the form in that case?  This is actually a
      ;; very serious problem: if we don't walk it, we miss things, but if we
      ;; do walk it, we don't know how to walk it.  Right now, we don't walk
      ;; it and print out a warning.
      ;;  --Jeff Siskind says try binding *macroexpand-hook* to #'funcall.
      (multiple-value-bind (ex-form expanded?) (macroexpand-1 form *env*)
        (if expanded? (walk ex-form)
            (progn (clause-warning "The form ~a is a macro that won't expand. ~
  It will not be walked, which means that Iterate clauses inside it will ~
  not be seen."
                                   form)
                   (list form)))))

      ((special-operator-p (car form))
       (clause-warning "Iterate does not know how to handle the special ~
form ~s~%. It will not be walked, which means that Iterate clauses inside ~
it will not be seen."
                       form)
       (list form))

      ((starts-clause? (synonym (car form))) (process-clause form))

      (t ;; Lisp function call
       (return-code-modifying-body #'walk-arglist (cdr form)
                                   #`(list (cons (car form) _))))))

   ((lambda-expression? (car form))
    ;; Function call with a lambda in the car
    (bind ((bod decs init step final final-prot
                (walk-fspec (car form)))
           (abod adecs ainit astep afinal afinal-prot
                 (walk-arglist (cdr form))))
      (values (list (cons bod abod))
              (nconc decs adecs)
              (nconc init ainit)
              (nconc step astep)
              (nconc final afinal) 
              (nconc final-prot afinal-prot))))

  #+clisp ; some macros expand into ((setf foo) value other-args...)
  ((typep form '(cons (cons (eql setf) *) *)) (apply #'walk-cdr form))

  (t (clause-error "The form ~a is not a valid Lisp expression" form))))

(defun walk-list (forms)
  "Walk <_:arg forms />, as a simple list"
  (walk-list-nconcing forms #'walk))

(defun walk-arglist (args)
  "Walk <_:arg args /> list, possibly containing <_:fun Iterate /> clauses"
  (let ((*top-level?* nil))
    (walk-list-nconcing args #'walk
                        (lambda (form body)
                          (if (is-iterate-clause? form)
                              (list (progn-wrap body))
                              body)))))

(defun walk-fspec (form)
  "Walk lambdas' and functions' specs in <_:fun flet /> and <_:fun labels />.
<_:arg Form /> is <_:pseudo (lambda-or-name args . body) />.
Only walk at the body. The args are set up as internal variables.
Declarations are kept internal to the body"
  (bind ((args (cadr form))
         (body (cddr form))
         (*top-level?* nil)
         (*declaration-context?* t)
         (*internal-variables* (add-internal-vars args))
         (bod decs init step final finalp (walk-list body)))
    (values `(,(first form) ,args ,.decs ,.bod)
            nil
            init
            step
            final 
            finalp)))

(defun walk-list-nconcing (lst walk-fn 
                           &optional (body-during (lambda (form body)
                                                    (declare (ignore form))
                                                    body)))
  "Lowest-level walking function for lists. Applies <_:arg walk-fn /> to
<_:arg lst />. Applies <_:arg body-during /> to the body part, returned by
<_:arg walk-fn />."
  (let (body-code decls init-code step-code final-code finalp-code)
    (dolist (form lst)
      (declare (optimize (speed 0)))
      (multiple-value-bind (body decs init step final finalp)
          (funcall walk-fn form)
        (augment decls decs)
        (augment init-code init)
        (augment body-code (funcall body-during form body))
        (augment step-code step)
        (augment final-code final)
        (augment finalp-code finalp)))
    (values body-code
            decls
            init-code
            step-code
            final-code
            finalp-code)))

(defun return-code-modifying-body (f stuff mod-f)
  "Call <_:arg f /> with <_:arg stuff /> and return the regular 6 values
\(see <_:fun walk />) with <_:code body /> (1st return value) being the result
of application of <_:fun mod-f /> to <_:code body />, returned by <_:arg f />"
  (declare (optimize (speed 0)))
  (multiple-value-bind (bod decs init step final finalp)
      (funcall f stuff)
    (values (funcall mod-f bod)
            decs
            init
            step
            final
            finalp)))


(defun add-internal-var (var)
  "Return a list of <_:arg var />, <_:fun cons />ed at the front to
<_:var *internal-variables* />"
  (cons (car (mklist var)) *internal-variables*))

(defun add-internal-vars (vars)
  "Return a list of <_:arg vars />, <_:fun nconc />ed at the front to
<_:var *internal-variables* />.

<_:arg Vars /> can be a lambda-list, a list of <_:fun let /> bindings,
or just a list of variables"
  (nconc (lambda-list-vars vars) *internal-variables*))

(defun lambda-list-vars (lambda-list)
  "Return the variables in the <_:arg lambda-list />, omitting keywords, default
and values"
  (mapcan #`(cond ((consp _)
                   (if (consp (car _)) ; this is a full keyword spec
                       (list (second (car _)))
                       (list (car _))))
                  ((not (find _
                              '(&aux &optional &key &allow-other-keys &whole)))
                   (list _)))
          lambda-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special forms

(defun special-form? (symbol)
  "Check, that <_:arg symbol /> is present in <_:var *special-form-alist* />.
Used instead of <_:fun special-operator-p />, which doesn't work properly
in some compilers (like Lucid). Besides, to track Iterate special clauses"
  (assoc symbol *special-form-alist*))

(defun walk-special-form (form)
  "Walk a special form, defined in <_:var *special-form-alist* />"
  (let ((*clause* form)
        (func (cdr (assoc (if (keywordp (car form))
                              (find-symbol (string (car form)) 'rutils.iter)
                              (car form))
                          *special-form-alist*))))
    (if func (apply func form)
        (list form))))  ; there's nothing to transform

(defun walk-cdr (first &rest stuff)
  "Don't walk the <_:fun car /> of a list.
<_:fun Cdr /> might not be walked as well, though"
  (return-code-modifying-body #'walk-arglist stuff
                              #`(list (cons first _))))

(defun walk-cddr (first second &rest stuff)
  "Don't walk the <_:fun car /> and <_:fun cadr /> of a list.
<_:fun Cddr /> might not be walked as well, though"
  (return-code-modifying-body #'walk-arglist stuff
                              #`(list (cons first (cons second _)))))

(defun walk-progn (progn &rest stuff)
  "Don't walk the <_:fun car /> of a list.
<_:fun Cdr /> might not be walked as well, though
The only difference between this and <_:fun walk-cdr /> is
that <_:var *top-level* /> is not bound. This is so macros can return
<_:fun progn />s of things. It's exactly like the definition of 'top-level'
in lisp. (Also, just for looks, this returns nil if the progn is empty.)"
  (return-code-modifying-body #'walk-list stuff 
                              #`(when _ (list (cons progn _)))))

(defun walk-setq (setq &rest things)
  "Walk every thing in <_:arg things />"
  (let ((*top-level?* nil)
        (i 1)
        body-code decls init-code step-code final-code finalp-code)
    (dolist (thing things)
      (if (oddp i) (push thing body-code)
          (multiple-value-bind (body decs init step final finalp)
              (walk thing)
            (augment decls decs)
            (augment init-code init)
            (push (progn-wrap body) body-code)
            (augment step-code step)
            (augment final-code final)
            (augment finalp-code finalp)))
      (incf i))
    (values (list (cons setq (nreverse body-code)))
            decls
            init-code
            step-code
            final-code
            finalp-code)))

(defun walk-function (function form)
  "Walk <_:class function /> specification"
  (if (lambda-expression? form)
      (return-code-modifying-body #'walk-fspec form
                                  #`(list (list function _)))
      (list (list function form))))

(defun walk-declare (&rest declaration)
  "Walk <_:arg declaration />. Declarations should be put in the declaration
section of the loop. They are only allowed at top-level, except that they
are allowed within binding environments, in which case they apply only to
that binding environment"
  #+ symbolics (setq declaration (copy-list declaration))
  (if (or *top-level?* *declaration-context?*)
      (return-code :declarations (list declaration)) 
      (clause-error "Declarations must occur at top-level, or inside a ~
  binding context like let or multiple-value-bind.")))

(defun walk-let (let-key bindings &rest body)
  "Walk <_:fun let />-form, with <_:arg bindings /> and <_:arg body />,
which may contain <_:fun iterate /> clauses.
The declarations go inside this let, not to the top-level. It is an error
to use a variable in the <_:fun let /> bindings as the target of an accumulation
\(i.e. <_:iter-clause into />), because <_:fun iterate /> will try to make
a top-level binding for that variable. The same goes for other variables,
that might be so bound."
  (bind ((*top-level?* nil)
         (binds b-decls b-init b-step b-final b-finalp
                (walk-let-bindings let-key bindings))
         (*declaration-context?* t)
         (*internal-variables* (add-internal-vars binds))
         (bod decls init step final finalp (walk-list body)))
    (return-code :declarations b-decls
                 :initial (nconc b-init init)
                 :body (list `(,let-key ,binds ,.decls ,.bod))
                 :step (nconc b-step step)
                 :final (nconc b-final final)
                 :final-protected (nconc b-finalp finalp))))

(defun walk-let-bindings (let-key bindings)
  "Walk <_:arg bindings />, established by <_:fun let /> or <_:fun let* />"
  (if (eq let-key 'let)
      (walk-list-nconcing bindings #'walk-let-binding
                          (lambda (form body)
                            (declare (ignore form))
                            (list body)))
      (walk-let*-bindings bindings)))


(defun walk-let*-bindings (bindings)
  "Walk <_:arg bindings /> for <_:fun let* /> one at a time,
to get the variable scoping right"
  (when bindings
    (bind ((bod decls init step final finalp
                (walk-let-binding (car bindings)))
           (*internal-variables* (add-internal-var (car bindings)))
           (bod1 decls1 init1 step1 final1 finalp1
                 (walk-let*-bindings (cdr bindings))))
      (values (cons bod bod1)
              (nconc decls decls1)
              (nconc init init1)
              (nconc step step1)
              (nconc final final1)
              (nconc finalp finalp1)))))

      
(defun walk-let-binding (binding)
  "Walk a simgle <_:var binding /> for <_:fun let /> or <_:fun let* />"
  (if (consp binding)
      (multiple-value-bind
            (bod decls init step final finalp) (walk (second binding))
        (values (list (first binding) (progn-wrap bod))
                decls
                init
                step
                final
                finalp))
      binding))
    
(defun walk-multiple-value-bind (mvb vars expr &rest body)
  "Walk <_:arg bindings /> for <_:fun multiple-value-bind />.
Decls go inside the <_:fun multiple-value-bind /> form, not to the top-level.
See <_:fun walk-let /> for binding subtleties"
  (declare (ignore mvb))
  (bind ((*top-level?* nil)
         (ebod edecls einit estep efinal efinalp
               (walk expr))
         (*declaration-context?* t)
         (*internal-variables* (add-internal-vars vars))
         (bod decls init step final finalp
              (walk-list body)))
    (return-code :declarations edecls
                 :initial (nconc einit init)
                 :body (list `(multiple-value-bind ,vars ,(progn-wrap ebod)
                                ,.decls ,.bod))
                 :step (nconc estep step)
                 :final (nconc efinal final)
                 :final-protected (nconc efinalp finalp))))

(defun walk-flet (flet bindings &rest body)
  "Walk <_:fun flet /> or <_:fun labels /> declarations.
We don't worry about the function bindings"
  (bind ((*top-level?* nil)
         (binds b-decls b-init b-step b-final b-finalp
                (walk-list-nconcing bindings #'walk-fspec
                                    (lambda (x y)
                                      (declare (ignore x))
                                      (list y))))
         (*declaration-context?* t)
         (bod decls init step final finalp (walk-list body)))
    (return-code :declarations b-decls
                 :initial (nconc b-init init)
                 :body (list `(,flet ,binds ,.decls ,.bod))
                 :step (nconc b-step step)
                 :final (nconc b-final final)
                 :final-protected (nconc b-finalp finalp))))

(defun walk-cdr-with-declarations (first &rest stuff) ; aka walk-locally
  "Walk <_:fun cdr />, ignoring declarations, that might be present in it.
Set <_:var *top-level?* /> to nil (via <_:fun walk-arglist />).
Note that when <_:var *top-level?* /> is nil, walk won't yield declarations, ~
 because <_:fun walk-declare /> errors out since all forms with ~
<_:var *declaration-context?* /> T keep them local (that is, in ~
<_:fun walk-let />, <_:fun walk-flet /> and <_:fun walk-multiple-value-bind /> ~
b-decls/edecls are always NIL)"
  ;; Ignoring code-movement issues, this approach should be fine
  (let* ((forms (member 'declare stuff
                        :key #`(and (consp _) (car _)) :test-not #'eq))
         (decls (ldiff stuff forms)))
    (return-code-modifying-body #'walk-arglist forms
                                #`(list (cons first (nconc decls _))))))

(defun walk-cddr-with-declarations (first second &rest stuff) ; aka walk-locally
  "Walk <_:fun cddr />, ignoring declarations, that might be present in it.
Set <_:var *top-level?* /> to nil (via <_:fun walk-arglist />).
Note that when <_:var *top-level?* /> is nil, walk won't yield declarations, ~
 because <_:fun walk-declare /> errors out since all forms with ~
<_:var *declaration-context?* /> T keep them local (that is, in ~
<_:fun walk-let />, <_:fun walk-flet /> and <_:fun walk-multiple-value-bind /> ~
b-decls/edecls are always NIL)"
  (let* ((forms (member 'declare stuff
                        :key #`(and (consp _) (car _)) :test-not #'eq))
         (decls (ldiff stuff forms)))
    (return-code-modifying-body #'walk-arglist forms
                                #`(list (cons first
                                              (cons second
                                                    (nconc decls _)))))))


(defun walk-macrolet (form-name &rest stuff)
  "<_:fun Macrolet /> is not supported inside <_:fun iter />.
Signal an error"
  (declare (ignore form-name stuff))
  (error "MACROLET is not permitted inside Iterate. Please ~
refactor the Iterate form (e.g. by using macrolets that wrap ~
the ITERATE form)."))

#+allegro
(defun walk-cond (cond &rest stuff)
  "Walk <_:fun cond />, because the allegro compiler insists on treating
it as a special form, and because some version <_:fun macroexpand />s
<_:code (cond #) /> into <_:code (cond #) />!"
  (declare (ignore cond))
  (unless stuff
    (let* ((first-clause (first stuff))
           (test (if (consp first-clause) (car first-clause)
                     (error "cond clause ~a is not a list" first-clause)))
           (thens (cdr first-clause))
           (if-form (if thens `(if ,test (progn ,@thens) (cond ,@(cdr stuff)))
                        (with-gensyms (var)
                          `(let ((,var ,test))
                             (if ,var ,var (cond ,@(cdr stuff))))))))
      (walk if-form))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Processing clauses

(defvar *initial* nil
  "Global collection of initial forms")
(defvar *decls* nil
  "Global collection of declaration forms")
(defvar *step* nil
  "Global collection of stepping forms")
(defvar *final* nil
  "Global collection of final forms")
(defvar *finalp* nil
  "Global collection of final forms, protected by <_:fun unwind-protect />")


(defun process-clause (clause)
  "Process <_:fun iterate /> <_:arg clause /> according to the rules,
defined for it.

This should observe the invariant, that the forms it returns are already
copied from the original code, hence <_:arg nconc />-able"
  (let ((*clause* clause)
        (special-func (assoc (car clause) *special-clause-alist*)))
    (if special-func (apply-clause-function (car clause) (cdr clause))
        (let* ((ppclause (preprocess-clause clause))
               (info (get-clause-info ppclause)))
          (if info
              (progn (arg-check ppclause info)
                     (let ((args (cons (mkeyw (first ppclause))
                                       (cdr ppclause)))
                           (func (clause-info-function info)))
                       (if (macro-function func *env*)
                           (walk (macroexpand-1 (cons func args) *env*))
                           (apply-clause-function func args))))
              (clause-error "No iterate function for this clause; do ~
\(~S) to see the existing clauses."
                            'display-iterate-clauses))))))

(defun apply-clause-function (func args)
  "Apply a function, defined for <_:fun iterate /> clause <_:arg func />
to <_:arg args />"
  (when (keywordp func)
    (setf func (find-symbol (string func) 'rutils.iter)))
  (let ((*initial* nil)
        (*decls* nil)
        (*step* nil)
        (*final* nil)
        (*finalp* nil))
    (declare (optimize (speed 0)))
    (multiple-value-bind (body decls init step final finalp) (apply func args)
      (values body
              (nconc *decls* decls)
              (nconc *initial* init)
              (nconc *step* step)
              (nconc *final* final)
              (nconc *finalp* finalp)))))
  
(defun preprocess-clause (clause)
  "Preprocess <_:fun iterate /> <_:arg clause />"
  ;; check for errors
  (do ((cl clause (cddr cl)))
      ((null cl))
    (cond
      ((not (symbolp (car cl)))
       (clause-error "~a should be a symbol" (car cl)))
      ((null (cdr cl))
       (clause-error "Missing value for ~a keyword" (car cl)))))
  ;; keywordize every iterate symbol and replace synonyms
  (let (new-clause
        (syn (synonym (first clause))))
    (do ((cl (cddr clause) (cddr cl)))
        ((null cl))
      (push (mkeyw (first cl)) new-clause)
      (push (second cl) new-clause))
    ;; turn (generate ...) into (for ... :generate t)
    (if (eq (mkeyw syn) :generate)
        `(for  ,(second clause) ,.(nreverse new-clause) :generate t)
        `(,syn ,(second clause) ,.(nreverse new-clause)))))

(defun arg-check (clause info)
  "Make sure that each keyword in <_:fun iterate /> <_:arg clause />
is in <_:arg info />"
  (let ((keywords (clause-info-keywords info)))
    (do ((cl clause (cddr cl)))
        ((null cl))
      (unless (cdr cl)
        (clause-error "Missing a value for ~a" (car cl)))
      (unless (member (car cl) keywords :test #'eq)
        (if (eq (car cl) :generate)
            (if (not (clause-info-generator? info))
                (clause-error "Clause cannot be used as a generator"))
            (clause-error "Unknown keyword ~a" (car cl)))))))

(defun walk-expr (expr)
  "Walk <_:arg expr /> and return just the <_:arg progn-wrap />ped bode.
Other returned by walking values are <_:fun augment />ed to globals and
returned by <_:fun process-clause /> in the end of processing it.

This isn't used by the code walker itself, but is useful for clauses, that
need to walk parts of themselves"
  (multiple-value-bind (body decls init step final finalp) (walk expr)
    (augment *decls* decls)
    (augment *initial* init)
    (augment *step* step)
    (augment *final* final)
    (augment *finalp* finalp)
    (progn-wrap body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Displaying clauses

(defun display-iterate-clauses (&optional clause-spec)
  "Display <_:fun iterate /> clause"
  (fresh-line)
  (cond
    ((and clause-spec (symbolp clause-spec)) (setq clause-spec
                                                   (list clause-spec)))
    ((member '&optional clause-spec) (error "Iterate: clause-spec cannot ~
mention optional keywords"))
    (clause-spec (setq clause-spec (cons (car clause-spec)
                                         (mapcar #'mkeyw (cdr clause-spec))))))
  (dolist (spec-entry *special-clause-alist*)
    (let ((spec-clause-kws (list (car spec-entry))))
      (when (clause-matches? clause-spec spec-clause-kws)
        (display-clause spec-clause-kws (cdr spec-entry)))))
  (disp-std-clauses clause-spec *clause-info-index*)
  t)

(defun disp-std-clauses (clause-spec index)
  "Display standard (i.e. defined in <_:package iter />) clauses"
  (cond
    ((index? index) (dolist (entry (cdr index))
                      (disp-std-clauses clause-spec (cdr entry))))
    ((clause-matches? clause-spec (clause-info-keywords index))
     (display-clause (clause-info-keywords index)
                     (clause-info-doc-string index)))))
  
(defun display-clause (kws doc-string)
  "Display a single <_:fun iterate /> clause with a <_:arg doc-string />,
which accepts keywords <_:arg kws />"
  (display-kws kws)
  (if doc-string (format t "~25,4t ~a~%" doc-string)
      (terpri)))

(defun display-kws (kws)
  "Display <_:fun iterate /> clause keywords <_:arg kws />"
  (do* ((col 1)
        (kw-list kws (cdr kw-list))
        (kw (car kw-list) (car kw-list)))
      ((null kw-list))
    (let ((len (length (symbol-name kw))))
      (when (>= (+ col len) +fill-col+)
        (format t "~%~4t")
        (setq col 4))
      (if (= col 1)  ; the first one -- print package name
          (format t "~s" kw)
          (format t "~a" kw))
      (incf col len)
      (when (cdr kw-list) 
        (if (>= (+ col 1) +fill-col+)
            (progn (format t "~%~4t")
                   (setq col 4))
            (progn (format t " ")
                   (incf col)))))))
      

(defun clause-matches? (clause-spec kws)
  "Test wheather <_:arg clause-spec /> matches <_:arg kws />.
If no <_:arg clause-spec /> is given, assume, that it matches"
  (or (null clause-spec)
      (every #'eq clause-spec kws)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indexing of clause functions

;;; Each clause has one or more required keywords, which must
;;; appear in order, and zero or more optional keywords, which may be
;;; omitted and may appear in any order.
;;;
;;; The first word of a clause, though used as a keyword when the
;;; clause function is called, is kept in its original package for
;;; indexing purposes. This provides iterate's interface with the
;;; package system.
;;;
;;; Two clauses can be ambiguous when
;;; 1) they have the same list of required keywords
;;; 2) #1's required-list is a prefix of #2's and #1 has optional keywords,
;;;    which match the remaining keywords of #2's required-list
;;; We check for these situations and signal an error.
;;;
;;; Indexing scheme: basically a discrimination tree. There is a tree
;;; of alists with root *clause-info-index*.

(defun get-clause-info (clause &optional (index *clause-info-index*))
  "Get <_:fun iterate /> <_:arg clause /> info from the <_:arg index />"
  (let ((entry (cdr (index-lookup (car clause) index))))
    (if (index? entry)
        (let ((result (get-clause-info (cddr clause) entry)))
          ;; It could be that the required part of the clause ends here
          (or result (get-clause-info nil entry)))
        entry)))
      

(defun is-iterate-clause? (form)
  "Test wheather a <_:fun car /> of <_:arg form /> is defined
as <_:fun iterate /> <_:arg clause />"
  (and (consp form)
       (symbolp (car form))
       (starts-clause? (car form))))


(defun starts-clause? (symbol)
  "Tests wheather <_:arg symbol /> is defined as an <_:fun iterate /> ~
<_:arg clause /> starting symbol.

A symbol starts a clause, when it appears in the top-level index ~
\(<_:var *clause-info-index* />), it is in the ~
<_:var *special-clause-alist* />, or it is GENERATE. This is used to ~
distinguish the case when there's a lisp form (in which case the symbol ~
doesn't start a clause), versus the situation, when an erroneous clause ~
is provided"
  (or (assoc symbol *special-clause-alist*)
      (index-lookup symbol *clause-info-index*)
      (find symbol '(:generate generate))))


;;; The code generated by DEFINE-CLAUSE (below) is the only code that
;;; invokes this:

(eval-always
  (defun install-clause-info (req-keywords keywords function doc-string 
                              generator?)
    ""
    (install-clause-info-1 req-keywords *clause-info-index*
                           (make-clause-info :function function
                                             :keywords keywords
                                             :req-keywords req-keywords
                                             :doc-string doc-string
                                             :generator? generator?)))

  (defun install-clause-info-1 (keywords index info)
    ""
    ;; Here, KEYWORDS is a list of the required keywords.
    ;; The basic rule here is to build indices all the way out to the
    ;; end of the list of keywords.  That way it will be necessary for
    ;; the user's clause to contain all of the required keywords.
    ;;   If index contains no entry for the first keyword, build a full
    ;; set of indices and put it in index.  
    ;;   If there is an entry and it's an index, call recursively.
    ;;   If there's an entry and it's not an index, then we have a case of
    ;; duplication or prefix. If duplication, we replace and warn; if
    ;; prefix, we check for ambiguity, and if so, error.
    (unless keywords
      (ambiguity-check-index info index))
    (let ((entry (index-lookup (car keywords) index)))
      (cond
        ((null entry)
         (index-add (car keywords) (build-index (cdr keywords) info) index))
        ((index? (cdr entry))
         (install-clause-info-1 (cdr keywords) (cdr entry) info))
        ((clause-info-p (cdr entry))
         (if (cdr keywords)
             (progn (ambiguity-check-clause (cdr entry) info 2)
                    ;; replace this entry with an index
                    (let ((index2 (build-index (cdr keywords) info)))
                      (index-add nil (cdr entry) index2)
                      (setf (cdr entry) index2)))
             (progn (unless (equal (clause-info-keywords (cdr entry))
                                   (clause-info-keywords info))
                      ;; Duplication; warn if they are not completely identical.
                      (warn "replacing clause ~a~%with ~a"
                            (clause-info-keywords (cdr entry))
                            (clause-info-keywords info)))
                    (setf (cdr entry) info))))
        (t (bug "INSTALL-CLAUSE-INFO-1: index is broken")))))

  (defun build-index (keywords info)
    ""
    (if keywords
        `(:index (,(car keywords) . ,(build-index (cdr keywords) info)))
        info))
  
  (defun index? (x)
    "Simple test wheather <_:arg x /> is an index list (i.e. starts with ~
:index)"
    (and (consp x) (eq (car x) :index)))

  (defun index-add (key thing index)
    "Add <_:arg thing />, qualified by <_:arg key /> to the alits inside ~
<_:var index />"
    (push (cons key thing) (cdr index)))

  (defun index-lookup (item index)
    "Search for <_:var item /> in the alist inside <_:var index />"
    (assoc item (cdr index) :test #'eq))

  (defun ambiguity-check-index (ci1 index)
    "Check <_:arg ci1 /> pair for being already present in the alist inside ~
<_:var index /> with the subset of required keywors"
    (dolist (entry (cdr index))
      (if (clause-info-p (cdr entry))
          (ambiguity-check-clause ci1 (cdr entry) 1)
          (ambiguity-check-index ci1 (cdr entry)))))

  (defun ambiguity-check-clause (ci1 ci2 insert-n)
    "???"
    ;; It is known that the required keywords of CI1 are a prefix of those
    ;; of CI2, and that we are trying to add INSERT-N (1 or 2).
    (when (ambiguous-clauses? ci1 ci2)
      (let ((kw1 (clause-info-keywords ci1))
            (kw2 (clause-info-keywords ci2)))
        (when (= insert-n 2)
          (rotatef kw1 kw2))
        (error "Iterate: Inserting clause ~a would create ~
an ambiguity with clause ~a"
               kw1 kw2))))

  (defun ambiguous-clauses? (ci1 ci2)
    "Test wheather required-keywords of <_:arg ci1 /> are a prefix of
required-keywords of <_:arg ci2 /> (i.e. introduce ambiguity)"
    (let* ((rk1 (clause-info-req-keywords ci1))
           (rk2 (clause-info-req-keywords ci2))
           (rest-rk2 (nthcdr (length rk1) rk2))
           (ok1 (cdr (member '&optional (clause-info-keywords ci1)))))
      ;; Don't consider identical clauses ambiguous -- that will be
      ;; handled elsewhere
      (when rest-rk2
        (dolist (k2 rest-rk2 t)
          (unless (member k2 ok1)
            (return nil))))))

  (defun display-index (&optional (index *clause-info-index*) (indent 0))
    "???"
    ;; for debugging
    (if (index? index)
        (dolist (entry (cdr index))
          (format t "~vt~a:~%" indent (car entry))
          (display-index (cdr entry) (+ indent 2)))
        (format t "~vt~a~%" indent (clause-info-keywords index))))

  (defun remove-clause (clause-keywords)
    "Remove clause from the index. It is identified by ~
<_:arg clause-keywords />, which is a list with its symbols keywordized"
    (let* ((all-keywords
            (cons (first clause-keywords)
                  (mapcar #`(if (eq _ '&optional) _ (mkeyw _))
                          (rest clause-keywords))))
           (req-keywords
            (ldiff all-keywords (member '&optional all-keywords :test #'eq))))
      (labels ((remove-clause-internal (keywords index)
                 (let ((entry (and keywords
                                   (index-lookup (car keywords) index))))
                   (cond ((null entry)
                          (error "Clause ~a not found" clause-keywords))
                         ((clause-info-p (cdr entry))
                          (when (equal all-keywords 
                                       (clause-info-keywords (cdr entry)))
                            ;; else warn that an &optional part is missing??
                            (rplacd index (delete entry (cdr index)))
                            t))
                         (t ;; an index
                          (prog1 (remove-clause-internal (cdr keywords)
                                                         (cdr entry))
                            ;; if the index is empty, delete it too
                            (unless (cddr entry)
                              (rplacd index (delete entry (cdr index))))))))))
        (remove-clause-internal req-keywords *clause-info-index*))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defining iterate clauses

(defmacro defmacro-clause (clause-template &body body)
  "Macro for defining iterate clauses (with <_:fun defmacro />)"
  (define-clause 'defmacro clause-template body nil))

(defmacro defclause (clause-template &body body)
  "Macro for defining iterate clauses (with <_:fun defun />)"
  (define-clause 'defun clause-template body nil))

(defmacro defmacro-driver (clause-template &body body)
  "Macro for for defining iterators (with <_:fun defmacro />),
which may also be used as generators"
  (define-clause 'defmacro clause-template body t))

(defmacro defclause-driver (clause-template &body body)
  "Macro for for defining (with <_:fun defun />) iterators,
which may also be used as generators"
  (define-clause 'defun clause-template body t))

(eval-always
  (defconst +sequence-keyword-list+
    '(:from from :upfrom upfrom :downfrom downfrom :to to :downto downto
      :above above :below below (:by 1) (by 1) :with-index with-index)
    "Keywords, used in sequence <_:fun iter />ation clauses")

  (defun define-clause (define-form clause-template body generator?)
    ""
    ;; CLAUSE-TEMPLATE is of the form 
    ;;  (<sym1> <spec1> ... [&optional <symk> <speck> ...] [&sequence])
    ;; The <sym> forms must be symbols (in any package); they are the
    ;; keywords for the clause. The <spec> forms are to be bound to the
    ;; values of those keywords when the clause is processed; such a
    ;; form can be either a symbol, a list (symbol initform), or a list
    ;; (symbol initform svar).  These are processed exactly as if they
    ;; were keyword specifiers.  To be precise, a pair of keyword and
    ;; value-form behaves exactly like the keyword specification
    ;; ((:keyword var) initform svar). 
    ;;   If the special symbol &sequence occurs, it must be the last
    ;; form.  It is equivalent to specifying all the sequence optional
    ;; symbols (FROM, TO, etc.), with specs of the same name (i.e. the
    ;; variable bound to the FROM keyword is "from", etc.).  There are
    ;; no defaults except that BY defaults to 1.
    ;;   The BODY is just an ordinary lisp body; it will refer to the 
    ;; value-forms in the clause template. It should use return-code to
    ;; return the appropriate arguments.
    (unless clause-template
      (error "Iterate: empty clause template with body ~a" body))
    (flet ((make-keyword-spec (kw val)
             (if (symbolp val)
                 `((,kw ,val))
                 `((,kw ,(car val)) ,@(cdr val)))))
      (let ((last (car (last clause-template))))
        (when (and (symbolp last) (string= last '&sequence))
          (setq clause-template 
                (nconc (butlast clause-template)
                       (if (member '&optional clause-template)
                           +sequence-keyword-list+
                           (cons '&optional +sequence-keyword-list+))))))
      (bind ((rkws rvals okws ovals (split-clause-template clause-template))
             (req-keywords (mapcar #'mkeyw rkws))
             (req-kws-but-first (cons (car clause-template)
                                      (cdr req-keywords)))
             (opt-keywords (mapcar #'mkeyw okws))
             (keywords&opt (if opt-keywords
                               (append req-kws-but-first
                                       '(&optional) opt-keywords)
                               req-kws-but-first))
             (rkw-specs (mapcar #'make-keyword-spec req-keywords rvals))
             (okw-specs (mapcar #'make-keyword-spec opt-keywords ovals))
             (func-name (make-function-name rkws))
             (doc-string (when (stringp (car body))
                           (car body)))
             (all-keywords (append req-keywords opt-keywords))
             (arglist `(&key ,@rkw-specs ,@okw-specs)))
        (when (contains-duplicates? all-keywords)
          (error "While defining ~a: keyword list contains duplicates"
                 clause-template))
        (when generator?
          (augment arglist (list 'generate)))
        ;; Actually define a named function, instead of using an
        ;; anonymous lambda, to ensure that it gets compiled.  A
        ;; compiler should compile a sharp-quoted lambda, but the
        ;; Symbolics one doesn't. Also, use the original first symbol
        ;; of the clause for indexing. This provides the following behavior
        ;; for the package system: the first symbol of the user's clause
        ;; must be eq to (hence in the same package as) the first symbol of
        ;; the defined clause; but the packages of the other symbols don't
        ;; matter
        `(eval-always
           (,define-form ,func-name  ,arglist .,body)
           (install-clause-info ',req-kws-but-first
                                ',keywords&opt
                                ',func-name
                                ,doc-string
                                ,generator?)
           ',clause-template))))

  (defun make-function-name (req-syms)
    "From a list of clause symbols (<_:arg req-syms />)
make a unique symbol"
    (let ((req-string "CLAUSE-"))
      (dolist (sym req-syms)
        (setq req-string (strcat req-string (symbol-name sym) "-")))
      (gentemp req-string)))

  (defun split-clause-template (ct)
    "Split <_:arg ct /> into required keywords, optional keywords and values"
    (let* ((opt&-list (member '&optional ct))
           (req-list (ldiff ct opt&-list))
           (opt-list (cdr opt&-list)))
      (cond
        ((zerop (length req-list))
         (error "DEFCLAUSE: template ~a has no required part" ct))
        ((oddp (length req-list))
         (error "DEFCLAUSE: required part of template ~a is of odd length" ct))
        ((oddp (length opt-list))
         (error "DEFCLAUSE: optional part of template ~a is of odd length" ct)))
      (bind ((rkws rvals (split-list-odd-even req-list))
             (okws ovals (split-list-odd-even opt-list)))
        (values rkws
                rvals
                okws
                ovals))))

  (defun split-list-odd-even (lst)
    "Split <_:arg lst /> into odd- and even-numbered elements,
returned with <_:fun values />"
    (do ((head lst (cddr head))
         (odds nil)
         (evens nil))
        ((null head) (values (nreverse odds) (nreverse evens)))
      (push (car  head) odds)
      (push (cadr head) evens)))

  (defun contains-duplicates? (lst)
    "Check, wheather <_:arg lst /> contains duplicate symbols"
    (not (equal lst (remove-duplicates lst :test #'eq))))
)

(defmacro defclause-sequence (element-name index-name
                              &key access-fn size-fn
                              element-type sequence-type
                              element-doc-string index-doc-string)
  "A simple way to define a <_:iter-clause for />-like &sequence clause"

;Package subtlety: <_:iter-clause for /> should be in the same package as the
;element-name or index-name"
  (let* ((seq-for (when element-name 
                    (intern (symbol-name 'for) (symbol-package element-name))))
         (seq-def (when element-name
                    `(defclause-driver
                         (,seq-for var ,element-name seq &sequence)
                       ,element-doc-string
                       (return-sequence-code :element-var var
                                             :sequence seq
                                             :access-fn ,access-fn
                                             :size-fn ,size-fn
                                             :element-type  ,element-type
                                             :sequence-type ,sequence-type))))
         #+nil
         (inx-for (when index-name
                    (intern (symbol-name 'for) (symbol-package index-name))))
         (inx-def (when index-name
                    `(defclause-driver (:for var ,index-name seq &sequence)
                       ,index-doc-string
                       (if with-index
                           (clause-error "WITH-INDEX keyword should not ~
be specified for this clause")
                           (progn
                             (setq with-index var)
                             (return-sequence-code
                              :sequence seq
                              :size-fn ,size-fn
                              :sequence-type ,sequence-type)))))))
    `(progn ,seq-def ,inx-def)))

(defun if-1st-time (then &optional else first-time-var)
  "Return:
<_ :type list
# a form, which evaluates <_:arg then /> the first time through the loop,
  <_:arg else /> -- the subsequent times
# the variable, which may be passed with <_:arg first-time-var />,
  that keeps track of first time />"
  (let* ((var (or first-time-var 
                  (make-var-and-binding 'first-time t :type 'boolean)))
         (code (if else `(if ,var
                             (progn (setq ,var nil)
                                    ,@then)
                             (progn ,@else))
                   `(when ,var 
                      (setq ,var nil)
                      ,@then))))
    (values code
            var)))

(defmacro with-temporary (var &body body)
  ;; Deprecated. Dangerous when incorrectly nested
  "Execute <_:arg body /> iside the temporary binding of <_:arg var /> to"
  (let ((old-var (gensym "OLD"))
        (vars (mklist var)))
    `(flet ((get-free-temp ()
              (let ((temp (some #`(unless (member _ *temps-in-use*) _)
                                *temps*)))
                (unless temp
                  (setq temp (make-var-and-default-binding 'temp))
                  (push temp *temps*))
                (push temp *temps-in-use*)
                temp)))
       (let ((,old-var *temps-in-use*))
         (unwind-protect
              (let ,(mapcar #``(,_ (get-free-temp))
                            vars)
                .,body)
           (setq *temps-in-use* ,old-var))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Typing

(defun var-type (var)
  "Return type of <_:arg var />, which can be given as
<_:fun the />-declaration or as symbol"
  (if (the-expression? var) (second var)
      (var-declaration var)))

(defun var-declaration (var)
  "Return the declaration, associated with <_:arg var />"
  (cdr (assoc var *type-alist* :test #'eq)))

(defun expr-type-only (expr)
  ""
  ;; If expr is self-evaluating, return its type (using type-of);
  ;; if expr is of the form (the <type> <form>), return <type>; 
  ;; else, return nil.
  (cond 
   ((self-evaluating? expr)
    ;; Attempt to work-around (type-of 0) -> useless types like
    ;; (integer 0 0) [cmucl/sbcl], (integer 0 16777215) or BIT [clisp]
    ;; -- possibly conterproductive for (array type dim1 .. dimn) types
    (let ((type (type-of expr)))
      (if (consp type) (first type) type)))
   ((the-expression? expr)
    (second expr))))

(defun expression-type (form)
  ""
  (if (symbolp form) (var-type form)
      (expr-type-only form)))

(defun quoted? (x)
  "Test, wheather <_:var x /> is of the form <_:code (quote ...) />"
  (and (consp x) (eq (car x) 'quote)))

(defun function-quoted? (x)
  "Test, wheather <_:var x /> is of the form <_:code (function ...) /> 
\(same as <_:code #'(...) />"
  (and (consp x) (eq (car x) 'function)))

(defun lambda-expression? (x)
  "Test, wheather <_:var x /> is a lambda-expression"
  (and (consp x) (eq (car x) 'lambda)))

(defun the-expression? (x)
  "Test, wheather <_:var x /> is a <_:fun the />-expression"
  (and (consp x) (eq (first x) 'the)))

(defun self-evaluating? (x)
  "Test, wheather <_:var x /> is a self-evaluating entity.

Everything but symbols and lists are self-evaluating since CLtL2.
This differs from <_:fun constantp /> in that it returns nil for quoted
things and <_:fun defconstant />s"
  (typep x '(and atom (or (not symbol) keyword (member t nil))))
  (and (atom x)
       (or (null x)
           (not (symbolp x))
           (eq x t)
           (keywordp x))))

(defun constant? (x)
  "Test, wheather <_:var x /> is a constant.

This differs from <_:fun constantp /> in that it doesn't acknowledge
<_:fun defconstant />s to be constants; the problem with so acknowledging
them is that the run-time and compile-time environments may differ.
The things <_:fun constant? /> returns T for are really and truly constant
everywhere"
  (or (self-evaluating? x) (quoted? x) (function-quoted? x)))

(defun duplicable? (x)
  "Return T if <_:var x /> can be copied in code. This is true for symbols,
on the assumption that the copies are close enough to each other so that
updating the variable cannot occur"
  (or (numberp x) (symbolp x) (characterp x)))

(defun var-spec? (x)
  "Test, if <_:arg x /> is a proper specifivation of a variable
\(a symbol or <_:fun the /> declaration)"
  (or (the-expression? x) (symbolp x)))

(defun extract-var (var-spec)
  "Extract variable from <_:fun the /> declaration or return as is"
  (if (the-expression? var-spec) (third var-spec)
      var-spec))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Binding

(defun make-binding (var value &key type using-type-of)
  "???"
  ;; This creates a binding of VAR to VALUE. TYPE means declare VAR
  ;; to be of that type if it isn't declared to be a type already.
  ;; (But do so only when declare-variables has been declared.)
  ;; USING-TYPE-OF means to use the type of that form, if any.  
  ;; Specifying both keyword args is an error.
  ;;   It is okay to pass nil for VAR; in this case, nothing will
  ;; happen and nil will be returned.  This is done just to simplify
  ;; coding of clauses.
  (make-binding-internal var value t type using-type-of))

(defun make-default-binding (var &key type using-type-of)
  "???"
  ;; This makes a random binding of VAR (i.e. you should not depend on
  ;; the binding's value).  It will observe TYPE and USING-TYPE-OF in
  ;; choosing a value to bind to (see the comment for make-binding).
  ;;   It is okay to pass nil for VAR; in this case, nothing will
  ;; happen and nil will be returned.  This is done just to simplify
  ;; coding of clauses.
  (make-binding-internal var nil nil type using-type-of))

(defun make-var-and-binding (string value &key type using-type-of)
  "Make a binding for a newly <_:fun genvar />ed variable, denoted by
<_:arg string />, to the given <_:arg value />. Possibly account for
it's <_arg type />"
  (let ((var (genvar string)))
    (make-binding-internal var value t type using-type-of)
    var))

(defun make-var-and-default-binding (string &key type using-type-of)
  "???"
  (let ((var (genvar string)))
    (make-binding-internal var nil nil type using-type-of)
    var))

(defun make-accum-var-binding (var value kind &key type using-type-of)
  "???"
  (make-accum-var-binding-internal var value t kind type using-type-of))

(defun make-accum-var-default-binding (var kind &key type using-type-of)
  "???"
  (make-accum-var-binding-internal var nil nil kind type using-type-of))

(defun make-accum-var-binding-internal (var value value-supplied?
                                        kind type using-type-of)
  "???"
  ;; Possibly creates a binding for an accumulation variable, like
  ;; those generated by COLLECT, MAXIMIZE, COUNT, etc.  
  ;; It checks *accum-var-alist* to see if the variable already exists.
  ;; If so, and it is of the right kind, it does not create a new
  ;; binding.  If it is of the wrong kind, an error is signalled.  If kind is
  ;; NIL, then we don't do this error check.  However, we aways check to make
  ;; sure the initial value, if supplied, is correct.
  ;;    In all cases, *internal-variables* is checked to make sure the
  ;; variable does not occur there.
  ;;    The alist entry is returned.  It can be used to store
  ;; additional info, like the end-pointer for collections.
  (let ((entry (assoc var *accum-var-alist* :test #'eq)))
    (cond
     ((null entry)
      (if value-supplied?
          (make-binding var value :type type :using-type-of using-type-of)
          (make-default-binding var :type type :using-type-of using-type-of))
      (setq entry (list var kind))
      (push entry *accum-var-alist*)
      entry)
     ((and kind (second entry) (not (eq (second entry) kind)))
      (clause-error "Attempt to do ~a accumulation into a variable already ~
being used for ~a accumulation."
                    kind (second entry)))
     (t (when value-supplied?
          (let ((orig-value (second (assoc var *bindings*))))
            (unless (equal value orig-value)
              (clause-error "Initial values ~a and ~a are not equal for ~
variable ~a"
                            orig-value value var))))
        (check-internal-variables var)
        entry))))

(defun make-shared-binding (var value &key type using-type-of)
  "Look up in <_:var *shared-bindings-alist* /> or create an entry,
keyed by <_:arg var />, store <_:fun gensym /> in the <_:arg value /> and also
add it as a binding. Return the entry"
  (let ((entry (assoc var *shared-bindings-alist* :test #'eq)))
    (unless entry
      (setq entry (list var (gensym (string var))))
      (push entry *shared-bindings-alist*)
      (make-binding (second entry) value
                    :type type :using-type-of using-type-of))
    entry))

(defun make-binding-internal
    (var-spec value value-supplied? use-type using-type-of)
  "???"
  ;; This returns T if it actually created a binding, else NIL.
  ;; Declaration and typing rules: first of all, no declaration is
  ;; generated unless *declare-variables* is T and var doesn't already
  ;; have a type declaration.  If there is no type for var, we infer
  ;; it as best we can as follows: if use-type is supplied, we use
  ;; that type.  If using-type-of is supplied, we try to determine a
  ;; type for that variable or expression (see expression-type) and
  ;; use that if we find it.  (It is erroneous to supply both use-type
  ;; and using-type-of.)  If neither is supplied, we DO NOT try to
  ;; infer the type of value--we just give up.  Otherwise, someone who
  ;; innocently did (make-binding 'foo nil) would discover that the
  ;; resulting code, if declare-variables was used, would 
  ;; have foo declared to be of type symbol (since, in Lucid at least,
  ;; (type-of nil) == symbol).  Note that we do not check for a type
  ;; conflict between a supplied type and the existing type; the
  ;; existing type just wins.
  ;;
  ;;  The var can actually be of the form (the <type> var).
  (let ((var (extract-var var-spec)))
    (cond
     ((null var-spec) nil)
     ((not (symbolp var)) (clause-error "The variable ~a is not a symbol" var))
     (t (let* ((existing-type (var-type var-spec))
               (declared? (var-declaration var))
               (type (or existing-type
                         use-type
                         (and using-type-of (expression-type using-type-of)))))
          (when (or declared? (and *declare-variables* type))
            ;; We only have to be concerned about getting value to be
            ;; the right type if there will actually be a declaration
            ;; for var.  This will be either when there is an existing
            ;; declaration, or when *declare-variables* is true and
            ;; there is some type.
            (setq value (make-initial-value value value-supplied? type)))
          (when (and (not declared?) *declare-variables* type)
            (push `(type ,type ,var) *declarations*))
          (add-binding var value)
          t)))))

(defun make-initial-value (value value-supplied? type)
  "???"
  ;; This should really be done by trying to coerce, then trapping the error,
  ;; because the subtype checks aren't really right--nil, for instance, is a
  ;; subtype of anything, but you can't coerce anything to it.  (Sure, we
  ;; check for nil explicitly, but there are other things like it.)  Yet if we
  ;; omit the subtype tests currently, how will we know that we can convert
  ;; nil to a vector?
  (cond
   ((null type) value)
   (value-supplied? (if (self-evaluating? value)
                        (coerce value type)
                        `(the ,type ,value)))
   ((or (subtypep 'number type) (subtypep type 'number))
    (coerce 0 type))
   ((or (subtypep 'sequence type) (subtypep 'symbol type)
        (subtypep type 'sequence) (subtypep type 'symbol))
    (coerce nil type))
   ((subtypep type 'character)
    ;; Neither #\Null nor #\Nul are valid characters
    (coerce (code-char 0) type))
   (t (clause-warning "Cannot supply an initial value for type ~s; using NIL."
                      type)
      nil)))

(defun add-binding (var value)
  "???"
  (if (var-binding? var) (clause-error "Duplicate variable: ~a" var)
      (progn (check-internal-variables var)
             (push (list var value) *bindings*))))

(defun check-internal-variables (var)
  "???"
  (when (internal-variable? var)
    (clause-error "The variable ~a, which Iterate would like to bind, ~
already has a binding in a context internal to the iterate form. Give ~
the variable another name."
                  var)))

(defun internal-variable? (var)
  "Test, if <_:arg var /> is a <_:fun member /> of ~
<_:var *internal-variables* />"
  (member var *internal-variables* :test #'eq))

(defun var-binding? (var)
  "Test, if <_:arg var /> is present in <_:var *bindings* />"
  (car (member var *bindings* :test #'eq :key #'car)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Destructuring

;;; Where destructuring happens:
;;;  WITH (bind)
;;;  FOR...INIT...THEN (setq)
;;;  FOR...FIRST...THEN (setq)
;;;  FOR...= (setq)
;;;  FOR...IN-FILE (setq)
;;;  FOR...IN-STREAM (setq)
;;;  FOR...IN-HASHTABLE (setq)
;;;  FOR...IN-PACKAGE (setq)
;;;  element-var of sequence & list drivers (setq)

(defmacro dsetq (template value)
  "Destructuring assignment; supports both <_:code (values ...) />
for destructuring a multiple-value form and NIL as a variable name,
meaning to ignore that position,
e.g. <_:code (dsetq (values (a . b) nil c) form) />"
  (do-dsetq template value nil))
  
(defun do-dsetq (template value &optional (bindings? t) type)
  "???"
  (cond
   ((null template) (dsetq-error "Can't bind to nil"))
   ((var-spec? template) ; not only (symbolp template)
    (when bindings?
      (make-default-binding template :type type))
    `(setq ,(extract-var template) ,value))
   ((and (consp template) (eq (car template) 'values))
    ;; Just do a simple check for the most common errors.  There's no way we
    ;; can catch all problems.
    (if (or (atom value) (member (car value) '(car cdr cdar caar aref get)))
        (dsetq-error "Multiple values make no sense for this expression" )
        (make-mv-dsetqs (cdr template) value bindings?)))
   (t (let ((temp (gensym "DSETQ")))
        `(let ((,temp ,value))
           ,.(when (and type *declare-variables*)
               `((declare (type ,type ,temp))))
           ,.(make-dsetqs template temp bindings?)
           ,temp)))))

(defun make-dsetqs (template value bindings?)
  "???"
  (cond
   ((null template) nil)
   ((var-spec? template)
    (when bindings?
      (make-default-binding template))
    `((setq ,(extract-var template) ,value)))
   ((atom template) (dsetq-error "Invalid binding form: ~a" template))
   ((eq (car template) 'values)
    (dsetq-error "Multiple-value destructuring cannot be nested"))
   (t (nconc (make-dsetqs (car template) `(car ,value) bindings?)
             (make-dsetqs (cdr template) `(cdr ,value) bindings?)))))

(defun make-mv-dsetqs (templates value bindings?)
  "???"
  (let (temps vars tplates)
    (declare (type list temps vars tplates))
    (dolist (tp templates)
      (cond
       ((and tp (var-spec? tp))  ; either var or (the type var)
        (push nil tplates)
        (push nil temps)
        (push (extract-var tp) vars)
        (when bindings?
          (make-default-binding tp)))
       (t  ; either NIL or destructuring template
        (let ((temp (gensym "VALUE")))
          (push tp tplates)
          (push temp temps)
          (push temp vars)))))
    (setq temps (nreverse temps))
    (setq vars (nreverse vars))
    (setq tplates (nreverse tplates))
    (let ((mv-setq `(multiple-value-setq ,vars ,value))
          ;; Remove, don't delete. Bug
          ;; reported by Francois Ren'e Rideau on 2005-11-01
          (temp-vars (remove nil temps)))
      (if temp-vars
          `(let ,temp-vars
             (declare (ignorable .,temp-vars))  ; in case of NIL template
             ,mv-setq
             ,.(mapcan (lambda (tmpl temp)
                         (make-dsetqs tmpl temp bindings?))
                       tplates temps)
             ,(car vars))
          mv-setq))))

(defun dsetq-error (format-string &rest args)
  "Signal an <_:fun error /> in <_:fun dsetq />"
  (if (boundp '*result-var*)  ; inside ITER
      (apply #'clause-error format-string args)
      (apply #'error (strcat_ "DSETQ:" format-string) args)))


(defun make-destructuring-bindings (template value 
                                    &key type using-type-of)
  "???"
  (cond
   ((null template) (clause-error "Can't bind to NIL: ~a" value))
   ((var-spec? template)
    (make-binding template value :type type :using-type-of using-type-of))
   ((atom template) (clause-error "Invalid binding form: ~a" template))
   ((eq (car template) 'values)
    (clause-error "Cannot perform multiple-value destructuring in ~
this context"))
   (t (let ((var (make-var-and-binding 'temp value)))
        (push var *temps*)  ; so that others can benefit
        (do-destructuring-bindings template var)))))

(defun do-destructuring-bindings (template value)
  "Examine <_:arg template /> and if it's a proper <_fun var-spec? />,
make a binding of a var to <_:arg value />"
  (cond
   ((null template) nil)
   ((var-spec? template) (make-binding template value)
                         nil)
   ((atom template) (clause-error "Invalid binding form: ~a" template))
   ((eq (car template) 'values)
    (clause-error "Multiple-value destructuring cannot be nested"))
   (t (nconc (do-destructuring-bindings (car template) `(car ,value))
             (do-destructuring-bindings (cdr template) `(cdr ,value))))))

(defun extract-vars (template)
  "Like <_:fun extract-var />, but will work with a destructuring
template as well. Returns a list of variables"
  (cond
   ((null template) nil)
   ((var-spec? template) (list (extract-var template)))
   ((not (consp template)) (clause-error "Invalid binding form: ~a" template))
   ((eq (car template) 'values) (mapcan #'extract-vars (cdr template)))
   (t (nconc (extract-vars (car template))
             (extract-vars (cdr template))))))

(defun local-binding-check (form)
  "???"
  (when *internal-variables* ; else no need to extract free variables
    (when-it (remove-if-not #'internal-variable? (free-variables form))
      (clause-error "The variable~p ~{~a~^, ~} ~:[is~;are~] bound in a context ~
internal to the Iterate form.~% This part of the clause will be moved outside ~
the body of the loop, so it must not contain anything that depends on the body."
                    (length it) it (rest it)))))


(defun free-variables (form)
  "???"
  ;; This will return a list of the (lexically) free variables in FORM.  It
  ;; will never return anything that is not a free variable (except for not
  ;; processing MACROLET), but it may not get all of them. 
  (delete-duplicates (free-vars form nil) :test #'eq))

(defun free-vars (form bound-vars)
  "???"
  ;; To compute the variables that are free in a form, we have to walk it,
  ;; keeping track of what variables are bound.
  (cond
   ((constantp form) nil)
   ((symbolp form) (unless (member form bound-vars :test #'eq)
                     (list form)))
   ((atom form) nil)
   ((symbolp (car form))
    (cond
     ((or (special-operator-p (car form)) 
          ;; Lucid doesn't think that these are special forms
          ;; and we need to handle declarations:
          (member (car form) '(declare multiple-value-bind flet labels let let*)
                  :test #'eq))
      (case (car form)
        ((catch if locally multiple-value-call multiple-value-prog1
                progn progv setq tagbody throw unwind-protect)
         (free-vars-list (cdr form) bound-vars))
        ((block eval-when return-from the)
         (free-vars-list (cddr form) bound-vars))
        (multiple-value-bind
              (free-vars-list (cddr form) (append (cadr form) bound-vars)))
        (function
         (free-vars-fspec (second form) bound-vars))
        ((flet labels macrolet)
         (nconc (mapcan #`(free-vars-fspec _ bound-vars)
                        (second form))
                (free-vars-list (cddr form) bound-vars)))
        ((let symbol-macrolet)
         (let* ((bindings (second form))
                (body (cddr form))
                (vars (mapcar #`(if (consp _) (car _) _)
                              bindings)))
           (nconc (mapcan #`(when (consp _)
                              (free-vars (second _) bound-vars))
                          bindings)
                  (free-vars-list body (append vars bound-vars)))))
        (let*
            (let* ((bindings (second form))
                   (body (cddr form))
                   (free-vars nil))
              (dolist (binding bindings)
                (when (consp binding)
                  (augment free-vars (free-vars (second binding) 
                                                bound-vars)))
                (push (car (mklist binding)) bound-vars))
              (nconc free-vars (free-vars-list body bound-vars))))
        (otherwise nil)))
     ((macro-function (car form) *env*)
      (free-vars (macroexpand-1 form *env*) bound-vars))
     (t  ; function call
      (free-vars-list (cdr form) bound-vars))))
   ((and (consp (car form)) (eq (caar form) 'lambda))
    (nconc (free-vars-fspec (car form) bound-vars)
           (free-vars-list (cdr form) bound-vars)))
   (t (error "The form ~a is not a valid Lisp expression" form))))

(defun free-vars-list (list bound-vars)
  "???"
  (mapcan #`(free-vars _ bound-vars)
          list))

(defun free-vars-fspec (fspec bound-vars)
  "<_:arg Fspec /> is either: a symbol, or (<_:fun setf /> <symbol>),
or <_:pseudo (<name-or-lambda> (<vars>) . body) />."
  (unless (or (symbolp fspec) (eq (car fspec) 'setf))
    (free-vars-list (cddr fspec) (append (second fspec) bound-vars))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions, that return code

(defun return-code (&key declarations initial body step final final-protected)
  "???"
  (values body declarations initial step final final-protected))

(defmacro return-driver-code (&key variable initial declarations body step
                              final final-protected next)
  "???"
  ;; This assumes there is a local var called 'generate'
  (with-gensyms (btemp ntemp)
    `(let ((,btemp ,body)
           (,ntemp ,next))
       (add-driver-info ,variable ,ntemp generate)
       (unless generate
         (augment ,btemp ,ntemp))
       (values ,btemp ,declarations ,initial ,step ,final ,final-protected))))

(defun add-driver-info (var-template next-code generator?)
  "???"
  ;; VAR-TEMPLATE could be a single var-spec or a destructuring template.
  ;; Copy the code--the original could be nconc'ed.
  (let ((vars (extract-vars var-template))
        (di (make-driver-info :next-code (copy-list next-code)
                              :generator? generator?)))
    (register-previous-code vars next-code :next)
    (push (cons vars di) *driver-info-alist*)))

(defmacro return-sequence-code (&key element-var sequence access-fn
                                size-fn element-type sequence-type)
  "???"
  ;; This assumes all the sequence keywords will be in the lexical
  ;; environment. 
  `(return-seq-code
    :element-var ,element-var
    :sequence ,sequence
    :access-fn ,access-fn
    :size-fn ,size-fn
    :element-type ,element-type
    :sequence-type ,sequence-type
    :from from :upfrom upfrom :to to :downto downto :above above :below below
    :downfrom downfrom :by by
    :with-index with-index
    :generate generate))

(defun return-seq-code (&key element-var sequence access-fn size-fn
                        element-type sequence-type
                        from upfrom to downto above below downfrom 
                        with-index (by 1) generate)
  "???"
  ;; element-var might involve destructuring; the others won't.  If
  ;; access-fn is NIL, don't generate element-accessing code at all.
  (top-level-check)
  (check-sequence-keywords from upfrom downfrom to downto above below t)
  (let* ((index-var-spec (or with-index (genvar 'index))) 
         (index-var (extract-var index-var-spec))
         (seq-var (if (or access-fn (not (symbolp sequence)))
                      (make-var-and-default-binding 'sequence
                                                    :type sequence-type)))
         (seq-code (or seq-var sequence))
         (step-var (unless (constant? by)
                     (make-var-and-default-binding 'step :type 'fixnum)))
         (step (or step-var by))
         (step-func (if (or downto downfrom above) '- '+))
         (test-func (cond (to '>)
                          ((or downto downfrom) '<)
                          (below '>=)
                          (above '<=)
                          (t '>=)))
         (size-code (make-application size-fn seq-code))
         (limit-value (cond ((or to below))
                            ((or downto above))
                            (downfrom 0)
                            (t size-code)))
         (limit-var (unless (numberp limit-value)
                      (make-var-and-default-binding 'limit :type 'fixnum)))
         (limit-code (or limit-var limit-value))
         (other-func (if (eq step-func '-) '+ '-))
         (initial-value (eval-const-expr
                         (cond ((or from upfrom downfrom)
                                `(,other-func ,(or from upfrom downfrom) ,step))
                               ((or downto above)
                                (if (eql step 1) size-code
                                    `(+ ,size-code (1- ,step))))
                               (t `(- ,step)))))
         (access-code (when access-fn
                        (make-application access-fn seq-code index-var)))
         (step-code `(setq ,index-var (,step-func ,index-var ,step)))
         (setqs (when access-fn
                  (do-dsetq element-var access-code t element-type)))
         (test `(when (,test-func ,index-var ,limit-code)
                  (go ,*loop-end*))))
    (make-default-binding index-var-spec :type 'fixnum)
    (setq *loop-end-used?* t)
    (return-driver-code
     :initial (nconc (if seq-var `((setq ,seq-var ,sequence)))
                     (if step-var `((setq ,step-var ,by)))
                     (if limit-var `((setq ,limit-var ,limit-value)))
                     (if index-var `((setq ,index-var ,initial-value))))
     :next (list step-code test setqs)
     ;; say (list nil ...) in case element-var = VALUES
     :variable (list nil element-var index-var))))

(defun check-sequence-keywords (from upfrom downfrom to downto above below
                                known-limits? &aux count)
  "???"
  ;; If the limits aren't known, the possibilities are: FROM; UPFROM;
  ;; DOWNFROM; TO; BELOW; or FROM and exactly one of TO, DOWNTO, ABOVE and
  ;; BELOW.
  ;; If the limits are known: you also have DOWNTO; ABOVE; and nothing.
  (cond
    ((or (and upfrom downfrom)
         (and (or upfrom downfrom) (or from to downto above below)))
     (clause-error "UPFROM or DOWNFROM must occur alone"))
    ((> (setq count (count-if #'identity (list to downto above below))) 1)
     (clause-error "Use at most one of TO, DOWNTO, ABOVE and BELOW"))
    ((and (not known-limits?)
          ;; eliminate the cases DOWNTO, ABOVE, and nothing.
          (not (or from upfrom downfrom))
          (or downto above (zerop count)))
     (clause-error "Illegal set of sequence keywords"))))

(defun eval-const-expr (expr)
  "???"
  ;; This is very simple: if expr is a list, and all the args are constants,
  ;; it will evaluate it; else it will just return it.
  (if (and (consp expr) (every #'constantp (cdr expr)))
      (eval expr)
      expr))

(defun make-funcall (fn &rest args)
  "???"
  ;; This should be used when FN is something the user has written in a
  ;; clause. 
  #+symbolics (setq args (copy-list args))
  (cond
   ((or (quoted? fn) (function-quoted? fn)) `(,(second fn) ,@args))
   ((lambda-expression? fn) `(,fn ,@args))
   ;;((functionp fn) `(funcall ,fn ,@args)) ; same treatment as default case
   (t `(funcall ,fn ,@args))))

(defun make-application (fn &rest args)
  "???"
  ;; Use this when FN is given in the implementation code.
  #+ symbolics (setq args (copy-list args))
  (cond
   ((or (symbolp fn) (lambda-expression? fn))
    `(,fn ,@args))
   ((function-quoted? fn)
    `(,(second fn) ,@args))
   ((and (consp fn) (eq (car fn) 'subst))
    (apply-subst-expr fn args))
   ((functionp fn) `(funcall ,fn ,@args)) ; for compiled fns
   (t (clause-error "~a should denote a function, but it doesn't" fn))))
      
(defun apply-subst-expr (subst-expr args)
  "???"
  (progn-wrap (sublis (pairlis (second subst-expr) args)
                      (cddr subst-expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special clauses. These must return freshly consed lists that are nconcable

(defmacro def-special-clause (name arglist &body body)
  "Define clause by <_:fun defun />. Special-clause names should be exported"
  `(progn
     (defun ,(mksym name) ,arglist .,body)
     (install-special-clause-function ',name
                                      ,(when (stringp (car body))
                                         (car body)))))

(defun install-special-clause-function (symbol &optional doc-string)
  "Put the <_:arg symbol /> with <_:arg doc-string /> at the end
of <_:var *special-clause-alist* />, if not already present"
  (let* ((key-symbol (mkeyw symbol))
         (entry (assoc key-symbol *special-clause-alist*)))
    (if entry (setf (cdr entry) doc-string)
        (augment *special-clause-alist* (list (cons key-symbol doc-string))))
    symbol))

;;; (INITIALLY &rest)
  (def-special-clause :initially (&rest forms)
  "<_:arg Forms /> to execute before loop starts"
  (mapc #'local-binding-check forms)
  (return-code :initial (copy-list forms)))

(defsynonym initially :initially)

;;; (AFTER-EACH &rest)
(def-special-clause :after-each (&rest forms)
  "<_:arg Forms /> to execute after each iteration"
  (mapc #'local-binding-check forms)
  (return-code :step (walk-list forms)))

(defsynonym step :step)

;;; (ELSE &rest)
(def-special-clause :else (&rest forms)
  "<_:arg Forms /> to execute if the loop is never entered"
  (mapc #'local-binding-check forms)
  (let ((flag (make-var-and-binding 'else t :type 'boolean)))
    (return-code :final `((when ,flag
                            .,(walk-list forms)))
                 :body (list `(setq ,flag nil)))))

(defsynonym else :else)

;;; (FINALLY &rest)
(def-special-clause :finally (&rest forms)
  "<_:arg Forms /> to execute after loop ends"
  (mapc #'local-binding-check forms)
  (return-code :final (copy-list forms)))

(defsynonym finally :finally)

;;; (FINALLY-PROTECTED &rest)
(def-special-clause :finally-protected (&rest forms)
  "<_:arg Forms /> in an <_:fun unwind-protect /> after loop ends"
  (mapc #'local-binding-check forms)
  (return-code :final-protected (copy-list forms)))

(defsynonym finally-protected :finally-protected)

;;; (if (FIRST-TIME-P) ...)
(def-special-clause :first-time-p ()
  "Return T for the first time it is evaluated"
  (let ((first-time-var (make-var-and-binding 'first-time t :type 'boolean)))
    (return-code :body `(if ,first-time-var
                         (progn
                           (setf ,first-time-var nil)
                           t)))))

(defsynonym first-time-p :first-time-p)

;;; (if (FIRST-ITERATION-P) ...)
(def-special-clause :first-iteration-p ()
  "Return T in the first iteration of the loop"
  (let* ((entry (make-shared-binding 'first-iteration t :type 'boolean))
         step-body
         (first-usage (not (cddr entry)))
         (var (second entry)))
    (when first-usage
      (setf step-body (list `(setf ,var nil)))
      (setf (cddr entry) (list t)))
    (return-code :body `(,var)
                 :step step-body)))

(defsynonym first-iteration-p :first-iteration-p)

;;; (IN &body)
(def-special-clause :in (block-name &rest forms)
  "Process <_:arg forms /> in an <_:fun iter /> block with <_:arg block-name />"
  (if (eq block-name *block-name*)
      (walk-list forms)
      `((in ,block-name ,.(copy-list forms)))))

(defsynonym in :in)

;;; (NEXT var)
(def-special-clause :next (var &optional (n 1))
  "Explicitly step a driver <_:arg var />iable <_:arg n /> times.
Return <_:arg var />, after stepping.

Enclose the returned code in a <_:fun progn />, so that the variable reference ~
isn't confusable with a tag (since the code might appear within a ~
<_:fun tagbody />). The <_:fun progn /> is also necessary, so that spliced-in ~
save code will not result in extra forms, for cases when the NEXT appears as ~
an argument"
  (let ((entry (assoc var *driver-info-alist* :test #'member)))
    (if (and entry (driver-info-generator? (cdr entry)))
        (let* ((vars (car entry))
               (di (cdr entry))
               (code (copy-list (driver-info-next-code di))))
          (when (internal-variable? var)
            (clause-error "The variable ~a is bound in a context internal ~
to the Iterate form. It cannot be stepped at this point in the code."
                          var))
          (when (some #'internal-variable? vars)
            (clause-error "Some of the variables ~a, which will be stepped ~
when this clause is executed, are bound in a context internal to the Iterate ~
form, so ~a cannot be stepped at this point in the code."
                          vars var))
          (setf (driver-info-used di) t)
          (register-previous-code vars code :next)
          (return-code :body (make-next-code var code n)))
        (clause-error "Variable is not associated with a generator"))))

(defsynonym next :next)

(defun make-next-code (var code n)
  "Construct the body for <_:iter-clause next /> carefully (avoid backquote), ~
ensuring that <_:arg code />, and not a copy, appears in it"
  (if (eql n 1)
      (let ((var-code (unless (eq var (var-value-returned code))
                        (list var))))
        ;; This var-value-returned optimization benefits FOR IN-VECTOR/SEQ....
        ;; Too small a benefit in light of current compilers?
        (list (cons 'progn (nconc code var-code))))
      (let ((i (genvar 'next)))
        (list (list* 'dotimes (list i n var) `(declare (ignorable ,i)) code)))))

(defun var-value-returned (forms)
  "???"
  ;; If the result of evaluating FORMS would be the value of some variable,
  ;; then that variable is returned; else NIL.
  ;;  We only check for progns, setqs and raw variables.
  (let ((form (car (last forms))))
    (cond
     ((symbolp form) form)
     ((atom form) nil)
     ((eq (car form) 'setq) (second (last form 3))) ; support degenerated (setq)
     ((eq (car form) 'progn) (var-value-returned (cdr form)))
     (t nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Iteration-driving clauses

;;; (REPEAT n)
(defclause (:repeat n)
  "Repeat the loop <_:arg n /> times"
  (top-level-check)
  (let* ((c-type (or (expression-type n) 'fixnum))
         (count-var (make-var-and-default-binding 'count :type c-type)))
    (setq *loop-end-used?* t)
    (return-code :initial `((setq ,count-var ,n))
                 :body `((when (<= ,count-var 0)
                           (go ,*loop-end*)))
                 :step `((setq ,count-var (1- ,count-var))))))

;(defsynonym repeat :repeat)
(defsynonym :repeating :repeat)
;(defsynonym repeating :repeat)

;;; (FOR &sequence)
(defclause-driver (:for var-spec &sequence)
  "General <_:iter-clause for /> clause"
  (top-level-check)
  (when with-index
    (clause-error "WITH-INDEX should not be specified for this clause"))
  (check-sequence-keywords from upfrom downfrom to downto above below nil)
  (make-default-binding var-spec :type 'number)
  (let* ((var (extract-var var-spec))
         (initial (or from upfrom downfrom 0))
         (limit (or to downto above below))
         (step-func (if (or downfrom downto above) '- '+))
         (test-func (cond (to '>)
                          (downto '<)
                          (below '>=)
                          (above '<=)))
         (limit-var (when (and limit (not (constant? limit)))
                      (make-var-and-default-binding
                       'limit
                       :using-type-of (if (expression-type limit) limit
                                          var))))
         (step-var (unless (constantp by)
                     (make-var-and-default-binding 'step
                                                   :using-type-of by)))
         (step-thing (or step-var by))
         (limit-code (or limit-var limit))
         (init-val (eval-const-expr
                    (list (if (eq step-func '+) '- '+) initial step-thing)))
         (test (when limit
                 (setq *loop-end-used?* t)
                 `((when (,test-func ,var ,limit-code) 
                     (go ,*loop-end*)))))
         (next `((setq ,var (,step-func ,var ,step-thing)) .,test)))
    (return-driver-code :initial `(,.(when limit-var
                                       `((setq ,limit-var ,limit)))
                                     ,.(when step-var
                                         `((setq ,step-var ,by)))
                                     (setq ,var ,init-val))
                        :next next
                        :variable var)))

;(defsynonym for :for)
(defsynonym :as :for)
;(defsynonym as :for)

;(defsynonym generate :generate)
(defsynonym :generating :generate)
;(defsynonym generating :generate)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sequence iteration

;;; (FOR ON &optional BY)
(defclause-driver (:for var :on list &optional :by (step ''cdr))
  "<_:fun Iter /> over sublists of a <_:class list />"
  (top-level-check)
  (let* ((list-var (make-var-and-default-binding
                    'list
                    ;; Handle dotted lists unless *list-end-test* is NULLP
                    :type (if (eq 'null *list-end-test*) 'list)))
         (setqs (do-dsetq var list-var t 'list))
         ;; declaring type cons would be incompatible with initial value nil
         (test `(if (,*list-end-test* ,list-var) (go ,*loop-end*))))
    (setq *loop-end-used?* t)
    (return-driver-code :initial `((setq ,list-var ,list))
                        :next (list test
                                    setqs
                                    (generate-function-step-code 
                                     list-var step))
                        :variable var)))

;;; (FOR IN &optional BY)
(defclause-driver (:for var :in list &optional :by (step ''cdr))
  "<_:fun Iter /> over elements of a <_:class list />"
  (top-level-check)
  (let* ((on-var (make-var-and-default-binding
                  'list :type (if (eq 'null *list-end-test*) 'list 't)))
         (setqs (do-dsetq var `(car ,on-var)))
         (test `(when (,*list-end-test* ,on-var)
                  (go ,*loop-end*))))
    (setq *loop-end-used?* t)
    (return-driver-code :initial `((setq ,on-var ,list))
                        :next (list test
                                    setqs
                                    (generate-function-step-code on-var step))
                        :variable var)))


(defun generate-function-step-code (var step)
  "???"
  ;; If the stepping function is quoted or sharp-quoted, we don't need to make
  ;; a variable for it.  The two constant cases are distinguished solely for
  ;; compilers too stupid to compile (funcall 'cdr foo) the same as (cdr foo).
  ;; (Really, for cosmetics--there probably are no such stupid compilers.)
  (cond ((quoted? step)
         `(setq ,var (,(second step) ,var)))
        ((function-quoted? step)
         `(setq ,var (funcall ,step ,var)))
        (t (let ((step-var (make-var-and-binding 'step step :type 'function)))
             `(setq ,var (funcall ,step-var ,var))))))


;;; (FOR IN-VECTOR &sequence)
(defclause-sequence :in-vector index-of-vector
  ;; This observes fill-pointers.
  :access-fn 'aref
  :size-fn 'length
  :sequence-type 'vector
  :element-doc-string "<_:fun Iter /> over elements of a <_:class vector />"
  :index-doc-string "Indices of a vector")

;;; (FOR IN-SEQUENCE)
(defclause-sequence :in-sequence index-of-sequence
  ;; This observes fill pointers, and works for any sequence.
  :access-fn 'elt
  :size-fn 'length
  :sequence-type 'sequence
  :element-doc-string "<_:fun Iter /> over elements of a <_:class sequence />"
  :index-doc-string "Indices of a sequence (vector or list)")

;;; (FOR IN-STRING)
(defclause-sequence :in-string index-of-string
  :access-fn 'char
  :size-fn 'length
  :sequence-type 'string
  :element-type 'character
  :element-doc-string "<_:fun Iter /> over characters of a <_:class string />"
  :index-doc-string "Indices of a string")

;;; (FOR IN-HASHTABLE)
(defclause-driver (:for key-val-vars :in-hashtable table)
  "<_:fun Iter /> over elements and keys of a <_:class hash-table />"
  (top-level-check)
  (unless (consp key-val-vars)
    (clause-error "~a should be a list of up to two variables: the first ~
for the keys, the second for the values."
                  key-val-vars))
  (let* ((iterator (gensym "HASH-TABLE-ITERATOR-"))
         (more?    (gensym "HASH-MORE"))
         (var-spec `(values ,more? .,key-val-vars))
         (setqs    (do-dsetq var-spec `(,iterator)))
         (test     `(if (not ,more?) (go ,*loop-end*))))
    ;; FIXME: destructure only after termination test
    (setq *loop-end-used?* t)
    (add-loop-body-wrapper `(with-hash-table-iterator (,iterator ,table)))
    (return-driver-code :next (list setqs test)
                        :variable var-spec)))

;;; (FOR IN-PACKAGES &optional HAVING-ACCESS)
(defclause-driver (:for sym-access-pkg-vars :in-packages pkgs
                       &optional :having-access
                                 (sym-types '(:external :internal :inherited)))
  "<_:fun Iter /> over <_:class symbols /> and their access-types
in <_:clas packages />"
  ;;defclause-driver has the benefit over defmacro-driver of less code walking
  (top-level-check)
  (unless (and (listp sym-access-pkg-vars) ; empty list is allowed (count)
               (every #'symbolp sym-access-pkg-vars))
    (clause-error "~a should be a list of up to three variables: the symbol, ~
  the access type and the home package." sym-access-pkg-vars))
  (unless (consp sym-types)
    (clause-error "~s should be a list of symbols indicating the symbols' ~
  access types." sym-types))
  (let* ((iterator (gensym "PACKAGE-ITERATOR-"))
         (more?    (gensym "PACKAGE-MORE"))
         (var-spec `(values ,more? .,sym-access-pkg-vars))
         (setqs    (do-dsetq var-spec `(,iterator)))
         (test     `(if (not ,more?) (go ,*loop-end*))))
    (setq *loop-end-used?* t)
    (add-loop-body-wrapper `(with-package-iterator
                                (,iterator ,pkgs .,sym-types)))
    (return-driver-code :next (list setqs test)
                        :variable var-spec)))

;;; (FOR IN-PACKAGE &optional EXTERNAL-ONLY)
(defmacro-driver (:for var :in-package pkg &optional :external-only (ext nil))
  "<_:fun iter /> over symbols accessible in a package"
  `(,(if generate 'generate 'for) (,var) in-packages ,pkg having-access
     ,(if ext '(:external) '(:external :internal :inherited))))

;;; (FOR IN-FILE &optional USING)
(defclause-driver (:for var :in-file filename &optional :using (reader '#'read))
  "<_:fun iter /> over forms from a file"
  (top-level-check)
  (return-stream-driver-code var filename reader :file generate))

;;; (FOR IN-STREAM &optional USING)
(defclause-driver (:for var :in-stream stream &optional :using (reader '#'read))
  "<_:fun iter /> over forms from a stream (which will be closed at the end)"
  (top-level-check)
  (return-stream-driver-code var stream reader :stream generate))

(defun return-stream-driver-code (var thing reader stream-or-file generate)
  "???"
  (let* ((evar (extract-var var))
         (type (or (var-type evar) t))
         (stream-var (make-var-and-binding 'stream nil))
         (set-var (if (and (var-spec? var)
                           (subtypep 'symbol type))
                      ;; We can use the given variable directly if no
                      ;; destructuring is required, and if the type of the
                      ;; variable can hold a symbol (since we use a gensym for
                      ;; the eof-marker).
                      evar
                      (genvar 'element)))
         (setq (cond ((eq set-var evar)
                      (make-default-binding var) ())
                     (t (make-default-binding set-var)
                        (list (do-dsetq var set-var)))))
         (eof (gensym "EOF")))
    (setq *loop-end-used?* t)
    (return-driver-code 
     :initial (if (eq stream-or-file :file)
                  `((setq ,stream-var (open ,thing :direction :input)))
                  `((setq ,stream-var ,thing)))
     :next `((if (eq (setq ,set-var ,(make-funcall
                                      reader stream-var nil `',eof))
                     ',eof) (go ,*loop-end*))
             .,setq)
     :final-protected `((if (streamp ,stream-var)
                            (close ,stream-var)))
     :variable var)))
  
  
;;; (FOR NEXT)
(defclause-driver (:for var :next next)
  "General driver; VAR is set to value of NEXT"
  (return-driver-code :variable var
                      :next (list (do-dsetq var (walk-expr next)))))

;;; (FOR DO-NEXT)
(defclause-driver (:for var :do-next next)
  "General driver; VAR must be set in DO-NEXT"
  (do-dsetq var '(list))
  ;; for effect only, to make var known
  ;; We can't use (make-destructuring-bindings var) here because
  ;; we support the (values ...) template,
  ;; to maintain the documented equivalence with FOR ... NEXT.
  (return-driver-code
   :variable var
   :next (mapcar #'walk-expr
                 (if (and (consp next)
                          (consp (car next))
                          (not (eq (caar next) 'lambda)))
                     (copy-list next)
                     (list next)))))

  (defun list-of-forms? (x)
    (and (consp x) (consp (car x))
         (not (eq (caar x) 'lambda))))


;; No NEXT:
;; LOOP-TOP: SET
;;           (if test (go LOOP-END))
;;           STEP
;;
;; NEXT:
;; ...
;; LOOP-TOP ...
;;          [next] SET; (if test (go LOOP-END)); STEP
;;
;; (FOR var FROM n) => (initially (setq var (- n 1)))
;;                     (FOR var NEXT (1+ var))
;;
;; (FOR var FROM n TO m) => (initially (setq var (- n 1)) (setq limit (- m 1)))
;;                          (FOR var NEXT (if (> var limit) (finish) (1+ var))
;;
;; (FOR var ON list)  =>    (initially (setq temp list))
;;                          (FOR var NEXT (if (null temp) 
;;                                            (finish)
;;                                            (progn (setq temp (cdr temp))
;;                                                   temp)))
;;
;; (FOR var IN list) =>    (initially (setq temp list))
;;                         (FOR var NEXT (if (null temp)
;;                                           (finish)
;;                                           (pop temp)))
;;
;; (FOR var IN-VECT v) =>  (initially (setq index -1
;;                                          len (1- (length v))))
;;                         (FOR var NEXT (if (>= index len) (finish))
;;                                           (setq index (1+ index))
;;                                           (aref v index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variable binding and setting; pseudo-drivers

;;; (WITH &optional =)
(defclause (:with var &optional := (value nil supplied?))
  "Bind a variable"
  ;; Special case: if value is not supplied, var can be a list of
  ;; variables, all bound defaultly.
  (if supplied?
      (make-destructuring-bindings var value)
      (mapc #'make-default-binding (if (var-spec? var) (list var) var)))
  (return-code))  ; nothing to return

;(defsynonym :with with)

;;; (FOR INITIALLY THEN)
(defclause (:for var :initially initial :then then)
  "Set var initially, then on subsequent iterations"
  ;; This is a pseudo-driver: it doesn't work with NEXT.
  ;; Set var in initialization code, then set it in the step section on
  ;; subsequent iterations.  
  (top-level-check)
  (let* ((initial-setq (list (do-dsetq var initial)))
         (then-setq (list (do-dsetq var (walk-expr then) nil))))
    (register-previous-code (extract-vars var) then-setq :initial)
    (return-code :initial initial-setq
                 :step then-setq)))

;;; (FOR =)
(defclause (:for var := expr)
  "Set a variable on each iteration"
  ;; Set var each time through the loop.
  ;; VALUE: primary value of expr.
  (let ((vars (extract-vars var))
        (code (list (do-dsetq var (walk-expr expr)))))
    (register-previous-code vars code :next)
    (return-code :body code)))


;;; (FOR FIRST THEN)
(defclause (:for var :first first-expr :then then-expr)
  "Set var on first, and then on subsequent iterations"
  ;; Set var in the loop, but differently the first time.  Most
  ;; inefficient of the three.
  ;; VALUE: primary value of first- or then-expr.
  (let* ((first-setq (list (do-dsetq var (walk-expr first-expr))))
         (then-setq  (list (do-dsetq var (walk-expr then-expr) nil))))
    (register-previous-code (extract-vars var) then-setq :initial)
    (return-code :body (list (if-1st-time first-setq then-setq)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reducers

(defun return-reduction-code (&key identity operation external-op? variable
                              expression test type using-type-of
                              accum-kind)
  "???"
  ;;  A reduction is an iteration pattern in which a value is accumulated
  ;;(into VARIABLE) by repeatedly applying a binary OPERATION to the 
  ;;variable and an EXPRESSION.  The first time, the operation is applied
  ;;to the IDENTITY and the expression.
  ;;  Some other options allow for a wider range of patterns.  If TEST
  ;;is present, the result will only be accumulated on each iteration if
  ;;it succeeds. 
  ;;  TYPE is the type of the accumulation variable.
  ;;  ACCUM-KIND is the kind of accumulation this is--:increment, :max,
  ;;etc.  If NIL, then it matches any kind.
  ;; VALUE: the value accumulated so far.
  (setq variable (or variable *result-var*))
  (let* ((var (extract-var variable))
         (expr (walk-expr expression))
         (test-expr (when test (walk-expr test)))
         (op-expr (if external-op?
                      (make-funcall operation var expr)
                      (make-application operation var expr)))
         (update-code `(setq ,var ,op-expr)))
    (make-accum-var-binding variable identity accum-kind 
                            :type type :using-type-of using-type-of)
    (return-code :body (if test
                           `((if ,test-expr ,update-code ,var))
                           (list update-code)))))

;;; (COUNT &optional INTO)
(defclause (:count expr &optional :into var)
  "Increment a variable if expression is non-nil"
  (return-reduction-code :identity 0
                         :operation '(subst (var expr) (1+ var))
                         :expression nil
                         :test expr
                         :variable var
                         :type 'fixnum
                         :accum-kind :increment))

;(defsynonym count :count)
(defsynonym :counting :count)
;(defsynonym counting :count)

;;; (SUM &optional INTO)
(defclause (:sum expr &optional :into var)
  "Sum into a variable"
  (return-reduction-code :identity 0
                         :operation '+
                         :expression expr
                         :test nil
                         :variable var
                         :type 'number
                         :accum-kind :increment))

;(defsynonym sum :sum)
(defsynonym :summing :sum)
;(defsynonym summing :sum)

;;; (MULT &optional INTO)
(defclause (:mult expr &optional :into var)
  "Multiply into a variable"
  (return-reduction-code :identity 1
                         :operation '*
                         :expression expr
                         :test nil
                         :variable var
                         :type 'number
                         :accum-kind :increment))

;(defsynonym mult :mult)
(defsynonym :multiply :mult)
;(defsynonym multiply :mult)
(defsynonym :multiplying :mult)
;(defsynonym multiplying :mult)


;;; (REDUCE BY &optional INITIAL-VALUE INTO)
(defclause (:reduce expr :by op &optional :initial-value (init-val nil iv?)
                                          :into var-spec)
  "Generalized reduction"
  ;; VALUE: the value accumulated so far.
  ;; If we don't know the initial value, we can't use RETURN-REDUCTION-CODE.
  ;; We have to be inefficient and do something different the first time.
  ;; Also, we have to share the first-time-var in case of multiple reductions
  ;; into the same variable.
  (if iv?
      (progn (local-binding-check init-val)
             (return-reduction-code :identity init-val
                                    :operation op
                                    :external-op? t
                                    :expression expr
                                    :test nil
                                    :variable var-spec
                                    :type (expr-type-only op)
                                    :accum-kind nil))  ; matches anything
      (progn
        (setq expr (walk-expr expr))
        (setq var-spec (or var-spec *result-var*))
        (bind ((var (extract-var var-spec))
               (entry (make-accum-var-default-binding var-spec nil
                                                      :using-type-of expr))
               (prev-first-time-var (third entry))
               (update-code first-time-var
                           (if-1st-time
                            `((setq ,var ,expr))
                            `((setq ,var ,(make-funcall op var expr)))
                            prev-first-time-var)))
          (unless prev-first-time-var
            (setf (cddr entry) (list first-time-var)))
          (return-code :body (list update-code))))))

;(defsynonym reducing :reduce)
(defsynonym :reducing :reduce)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extrema

;;; (MAX &optional INTO)
(defclause (:max expr &optional :into var)
  "Maximize value of an expression"
  (return-extremum-code expr var 'max))

;(defsynonym max :max)
(defsynonym :maximize :max)
;(defsynonym maximize :max)
(defsynonym :maximizing :max)
;(defsynonym maximizing :max)

;;; (MIN &optional INTO)
(defclause (:min expr &optional into var)
  "Minimize value of an expression"
  (return-extremum-code expr var 'min))

;(defsynonym min :min)
(defsynonym :minimize :min)
;(defsynonym minimize :min)
(defsynonym :minimizing :min)
;(defsynonym minimizing :min)


(defun return-extremum-code (expr var-spec operation)
  ;; If we know the extremal value for the type of var, we COULD generate
  ;; a reduction...but don't right now, because it complicates
  ;; multiple accumulation.
  ;;  In order to accomodate multiple maxmins into the same variable, 
  ;; we store the first-time-variable in the accum-var-alist entry and
  ;; reuse it.  We have to do it this way, testing the var each time
  ;; through the loop, because due to conditionalization we don't know
  ;; if any of the first-time code will be executed.
  ;; VALUE: extremum so far.
  (setq expr (walk-expr expr))
  (bind ((m-var-spec (or var-spec *result-var*))
         (m-var (extract-var m-var-spec))
         (entry (make-accum-var-default-binding m-var-spec 
                                                (if (eq operation 'min)
                                                    :min :max)
                                                :using-type-of expr))
         (prev-first-time-var (third entry))
         (update-code first-time-var (if-1st-time 
                                      `((setq ,m-var ,expr))
                                      `((setq ,m-var (,operation ,m-var ,expr)))
                                      prev-first-time-var)))
      (unless prev-first-time-var
        (setf (cddr entry) (list first-time-var)))
      (return-code :body (list update-code))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Control flow

;;; (LEAVE &optional)
(def-special-clause :leave (&optional expr)
  "Exit the loop without running the epilogue code"
  `((return-from ,*block-name* ,expr)))

;(defsynonym leave :leave)

;;; (FINISH)
(def-special-clause :finish ()
  "Leave the loop gracefully, executing the epilogue"
  (setq *loop-end-used?* t)
  `((go ,*loop-end*)))

;(defsynonym finish :finish)

;;; (TERMINATE)
(def-special-clause :terminate () ; recommended for use with FOR ... NEXT
  "Use within <_:iter-clause for ... do-/next /> to end the iteration"
  (setq *loop-end-used?* t)
  `((go ,*loop-end*)))

;(defsynonym terminate :terminate)

;;; (NEXT-ITERATION)
(def-special-clause :next-iteration ()
  "Begin the next iteration"
  (setq *loop-step-used?* t)
  `((go ,*loop-step*)))

;(defsynonym next-iteration :next-iteration)

;;; (WHILE)
(defclause (:while expr)
  "Exit loop, when test is nil"
  (setq *loop-end-used?* t)
  (return-code :body `((unless ,(walk-expr expr) (go ,*loop-end*)))))

;(defsynonym while :while)

;;; (UNTIL)
(defclause (:until expr)
  "Exit loop, when test is non-nil"
  (setq *loop-end-used?* t)
  (return-code :body `((when ,(walk-expr expr) (go ,*loop-end*)))))

;(defsynonym until :until)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Aggregated Boolean tests

;; Use same :if-exists kind of accumulation as finding ... such-that
;; so the clauses can be used together.

;;; (ALWAYS)
(defclause (:always expr)
  "Return last value, when expression is always non-nil"
  ;; VALUE: primary value of expr
  (setq expr (walk-expr expr))
  (let ((var *result-var*))
    (make-accum-var-binding var t :if-exists)
    (return-code :body `((or (setq ,var ,expr) 
                             (return-from ,*block-name* nil))))))

;(defsynonym always :always)

;;; (NEVER)
(defclause (:never expr)
  "Return T, when expression is never non-nil"
  ;; VALUE: always nil
  (setq expr (walk-expr expr))
  (let ((var *result-var*))
    ;; Do not use :type 'symbol so as be compatible with ALWAYS
    (make-accum-var-binding var t :if-exists)
    (return-code :body `((when ,expr
                           (return-from ,*block-name* nil))))))

;(defsynonym never :never)

;;; (THERE-IS)
(defclause (:there-is expr)
  "Return value of expression, as soon as it is non-nil"
  ;; VALUE: always nil
  (setq expr (walk-expr expr))
  (let ((var *result-var*))
    (make-accum-var-default-binding var :if-exists)
    (return-code :body `((when (setq ,var ,expr) 
                           (return-from ,*block-name* ,var))))))

;(defsynonym there-is :there-is)
(defsynonym :thereis :there-is)
;(defsynonym thereis :there-is)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finders

;;; (FIND SUCH-THAT &optional INTO ON-FAILURE)
(defclause
    (:find expr :such-that test &optional :into var-spec :on-failure fval)
  "Return expression, when test is non-nil"
  ;; VALUE: undefined.
  (setq expr (walk-expr expr))
  (setq test (walk-expr test))
  (local-binding-check fval)
  (setq var-spec (or var-spec *result-var*))
  (setq *loop-end-used?* t)
  (let ((var (extract-var var-spec)))
    (make-accum-var-binding var-spec fval :if-exists :using-type-of fval) 
    (if (function-quoted? test)
        (if (duplicable? expr)
            (return-code :body `((when ,(make-funcall test expr)
                                   (setq ,var ,expr)
                                   (go ,*loop-end*))))
            (let ((temp-var (gensym "FINDING")))
              (return-code :body `((let ((,temp-var ,expr))
                                     (when ,(make-funcall test temp-var)
                                       (setq ,var ,temp-var)
                                       (go ,*loop-end*)))))))
        (return-code :body `((when ,test
                               (setq ,var ,expr)
                               (go ,*loop-end*)))))))

(defsynonym :finding :find)
;(defsynonym finding :find)
(defsynonym :search :find)
;(defsynonym search :find)

;;; (FIND MAX &optional INTO)
(defclause (:find expr :max max-expr &optional :into variable)
  "Return value, which maximizes expression"
  (return-find-extremum-code expr max-expr variable :max))

;;; (FIND MIN &optional INTO)
(defclause (:find expr :min min-expr &optional :into variable)
  "Return value, which minimizes expression"
  (return-find-extremum-code expr min-expr variable :min))

(defun return-find-extremum-code (expr m-expr var kind)
  "???"
  ;; VALUE: expr corresponding to max/min-expr so far.
  ;; Variable can be a list of two variables, in which case the first
  ;; is used for the expr and the second for the extremum.
  ;; The update code looks something like this:
  ;; When m-expr is not a function:
  ;;     (setq temp m-expr)
  ;;     (cond
  ;;      ((> temp m-var)
  ;;       (setq m-var temp)
  ;;       (setq expr-var expr))
  ;;      (t expr-var))
  ;;
  ;; When m-expr is a function:
  ;;     (setq temp2 expr)
  ;;     (setq temp (funcall m-expr temp2)) ;; or (m-expr temp2)
  ;;     (cond 
  ;;      ((> temp m-var)
  ;;       (setq m-var temp)
  ;;       (setq expr-var temp2))
  ;;      (t expr-var))
  ;;
  (setq expr (walk-expr expr))
  (setq m-expr (walk-expr m-expr))
  (let* ((function? (function-quoted? m-expr))
         (temp-var (make-var-and-default-binding 'temp :using-type-of 
                                                 (if (not function?) m-expr)))
         (temp-var-2 (when (and function? (not (duplicable? expr)))
                       (make-var-and-default-binding 'temp
                                                     :using-type-of expr)))
         (test (if (eq kind :max) '> '<))
         expr-var m-var)
    (cond
     ((null var)
      ;; no var means return expr as a result
      (setq expr-var *result-var*)
      (setq m-var (genvar kind)))
     ((var-spec? var)
      ;; a single var-spec means set expr to that var
      (setq expr-var var)
      (setq m-var (genvar kind)))
     ((and (consp var) (= (length var) 2) (every #'var-spec? var))
      ;; a two-element list means set expr to 1st, m to 2nd
      (setq expr-var (first var))
      (setq m-var (second var)))
     (t (clause-error "The value for INTO, ~a, should be a variable specifier ~
or a list of two variable specifiers." var)))
    (make-default-binding expr-var :using-type-of expr)
    (make-accum-var-default-binding m-var kind :using-type-of m-expr)
    (setq expr-var (extract-var expr-var))
    (setq m-var (extract-var m-var))
    (let* ((expr-code (or temp-var-2 expr))
           (esetq-code (if temp-var-2 `((setq ,temp-var-2 ,expr))))
           (m-code (if function?
                       (make-funcall m-expr expr-code)
                       m-expr)))
      (return-code :body `(,.esetq-code
                           (setq ,temp-var ,m-code)
                           ,(if-1st-time 
                             `((setq ,m-var ,temp-var)
                               (setq ,expr-var ,expr-code))
                             `((cond
                                 ((,test ,temp-var ,m-var)
                                  (setq ,m-var ,temp-var)
                                  (setq ,expr-var ,expr-code))
                                 (t ,expr-var)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Collectors
    
(defun return-collection-code (&key variable expression 
                               start-operation end-operation
                               one-element
                               test
                               (place 'end) (result-type 'list))
  "???"
  ;; VALUE: the list so far.
  ;; TODO: remove the "maybe quoted" idiom from documentation & code
  (when (quoted? result-type) (setq result-type (second result-type)))
  (when (quoted? place) (setq place (second place)))
  (let ((place-string (locally (declare (optimize safety))
                        (symbol-name place))))
    (cond
     ((string= place-string '#:end)
      (setq place 'end))
     ((or (string= place-string '#:start)
          (string= place-string '#:beginning))
      (setq place 'start))
     (t (clause-error "~a is neither 'start', 'beginning' nor 'end'" place))))
  (let* ((collect-var-spec (or variable *result-var*))
         (collect-var (extract-var collect-var-spec))
         (entry (make-accum-var-binding collect-var-spec nil :collect
                                        :type (if (eq result-type 'list) 'list
                                                  `(or list ,result-type))))
         (end-pointer (third entry))
         (prev-result-type (fourth entry)))
    (if end-pointer
        (unless (equal result-type prev-result-type)
          (clause-error "Result type ~a doesn't match ~a" 
                        result-type prev-result-type))
        (progn (when (eq place 'end)
                 (setq end-pointer (make-var-and-binding 'end-pointer nil 
                                                         :type 'list)))
               (setf (cddr entry) (list end-pointer result-type))))
    (let* ((expr (walk-expr expression))
           (op-expr
            (if (eq place 'start)
                (if start-operation
                    (make-application start-operation expr collect-var)
                    expr)
                (if end-operation
                    (make-application end-operation collect-var expr)
                    expr))))
      (if (eq place 'start)
          (return-code :body `((setq ,collect-var ,op-expr)))
          (with-temporary temp-var
            ;; In the update code, must test if collect-var is null to allow
            ;; for other clauses to collect into same var.  This code
            ;; is a tad bummed, but probably more for looks than real
            ;; efficiency.
            (let* ((update-code `(if ,collect-var
                                     (setf (cdr ,end-pointer) ,temp-var)
                                     (setq ,collect-var ,temp-var)))
                   (main-code (cond
                                ((not one-element)
                                 `((when (setq ,temp-var ,op-expr)
                                     (setq ,end-pointer 
                                           (last ,update-code)))))
                                (test `((when ,(make-application
                                                test
                                                collect-var expr)
                                         (setq ,temp-var ,op-expr)
                                         (setq ,end-pointer ,update-code))))
                                (t `((setq ,temp-var ,op-expr)
                                     (setq ,end-pointer ,update-code))))))
              
              (return-code 
               ;; Use a progn so collect-var isn't mistaken for a tag.
               :body `((progn ,.main-code ,collect-var))
               :final (if (eq result-type 'list)
                          nil
                          `((setq ,collect-var 
                                  ;; BUG FIX: removed ',result-type, so that
                                  ;; it could be non-literal
                                  (coerce ,collect-var ,result-type)))))))))))


;;; (COLLECT &optional INTO AT RESULT-TYPE)
(defclause (:collect expr &optional :into var
                                    :at (place 'end) 
                                    :result-type (type 'list))
  "Collect into a <_:class list /> (with <_:fun cons />)"
  (return-collection-code
   :variable var
   :expression expr
   :one-element t
   :start-operation 'cons 
   :end-operation '(subst (var expr) (list expr))
   :place place
   :result-type type))

;(defsynonym collect :collect)
(defsynonym :collecting :collect)
;(defsynonym collecting :collect)

;;; (ADJOIN &optional INTO AT TEST RESULT-TYPE)
(defclause (:adjoin expr &optional :into var
                                   :at (place 'end)
                                   :test (test '#'eql)
                                   :result-type (type 'list))
  "Adjoin into a <_:class list /> (tests for membership first)"
  (if (duplicable? expr)
      (return-collection-code
       :variable var
       :expression expr
       :start-operation `(subst (expr var) (adjoin expr var :test ,test))
       :test `(subst (var expr) (not (member expr var :test ,test)))
       :end-operation '(subst (var expr) (list expr))
       :one-element t
       :result-type type
       :place place)
      (with-temporary temp
        (return-collection-code
         :variable var
         :expression expr
         :start-operation `(subst (expr var)
                                  (progn ,temp ; silence unused variable warning
                                         (adjoin expr var :test ,test)))
         :test `(subst (var expr)
                       (progn
                         (setq ,temp expr)
                         (not (member ,temp var :test ,test))))
         :end-operation `(subst (var expr) (list ,temp))
         :one-element t
         :result-type type
         :place place))))

(defsynonym :adjoining :adjoin)
;(defsynonym adjoining :adjoin)

;;; (NCONC &optional INTO AT)
(defclause (:nconc expr &optional :into var :at (place 'end))
  "<_:fun Nconc /> into a <_:class list />"
  (return-collection-code
   :variable var
   :expression expr
   :start-operation 'nconc
   :place place
   :one-element nil))

(defsynonym :nconcing :nconc)
;(defsynonym nconcing :nconc)
   
;;; (APPEND &optional INTO AT)
(defclause (:append expr &optional :into var :at (place 'end))
  "<_:fun Append /> into a <_:class list />"
  (return-collection-code
   :variable var
   :expression expr
   :start-operation 'append
   :end-operation '(subst (var expr) (copy-list expr))
   :place place
   :one-element nil))

(defsynonym :appending :append)
;(defsynonym appending :append)

;;; (UNION &optional INTO AT TEST)
(defclause (:union expr &optional :into var
                                  :at (place 'end) 
                                  :test (test '#'eql))
  "Union into a <_:class list />"
  ;; Can't use UNION because it says nothing about the order.
  (return-collection-code
    :variable var
    :expression expr
    :start-operation `(subst (expr var)
                             (nconc (delete-if #`(member _ var :test ,test)
                                               (copy-list expr))
                                    var))
    :end-operation `(subst (var expr) 
                           (delete-if #`(member _ var :test ,test)
                                      (copy-list expr)))
    :place place
    :one-element nil))

(defsynonym :unioning :union)
;(defsynonym unioning :union)

;;; (NUNION &optional INTO AT TEST)
(defclause (:nunion expr &optional :into var
                                   :at (place 'end) 
                                   :test (test '#'eql))
  "Union into a <_:class list />, destructively"
  ;; Can't use NUNION because it says nothing about the order.
  (return-collection-code
    :variable var
    :expression expr
    :start-operation `(subst (expr var)
                             (nconc (delete-if #`(member _ var :test ,test)
                                               expr)
                                    var))
    :end-operation `(subst (var expr) 
                           (delete-if #`(member _ var :test ,test)
                                      expr))
    :place place
    :one-element nil))

(defsynonym :nunioning :union)
;(defsynonym nunioning :union)

;;; (ACCUMULATE BY &optional INITIAL-VALUE INTO)
(defclause
    (:accumulate expr :by op &optional :initial-value init-val :into var-spec)
  "Generalized accumulation"
  ;; VALUE: the value accumulated so far.
  ;; This is just like REDUCING except, 1. the args to OP are in the other
  ;; order, and 2. if no initial value is supplied, NIL is used. 
  (local-binding-check init-val)
  (setq var-spec (or var-spec *result-var*))
  ;; ignore the THE expression--it was a bad idea
  (when (the-expression? op) 
    (setq op (third op)))
  (let* ((var (extract-var var-spec))
         (op-expr (make-funcall op (walk-expr expr) var)))
    (make-accum-var-binding var-spec init-val nil :type nil)
    (return-code :body `((setq ,var ,op-expr)))))

;(defsynonym accumulate :accumulate)
(defsynonym :accumulating :accumulate)
;(defsynonym accumulating :accumulate)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The PREVIOUS mechanism

;;; It makes no sense to save local vars, so this is not as complex as I had
;;; thought.  There is one list: an alist of top-level vars and their info
;;; (*previous-vars-alist*).  Also, I now insist that the default value be
;;; fixed at the initialization section of the loop, so the old *unset*
;;; implementation is unnecessary.
;;;   However, generators complicate things.  In the absence of a generator,
;;; the save code can go in the step portion of the loop; but if there is a
;;; generator, the best we can do is use a flag for the first time.

;;; (FOR PREV &optional INITIALLY BACK)
(defclause (:for pvar :prev var &optional :initially (default nil default?)
            :back (n-expr 1))
  "Previous value of a variable"
  ;; Set each save variable to the default in the initialization.
  (top-level-check)
  (unless (constantp n-expr)
    (clause-error "~a should be a compile-time constant"
                  n-expr))
  (let ((n (eval n-expr)))
    (if (and (integerp n) (> n 0))
        (let* ((p-i (intern-previous-info var))
               (init-val (make-initial-value default default? (var-type var)))
               (temp (unless (duplicable? init-val)
                       (make-var-and-default-binding 'temp
                                                     :using-type-of init-val)))
               (iv-ref (or temp init-val))
               (save-vars (cons pvar (make-save-vars var (1- n))))
               (inits (mapcar #``(setq ,_ ,iv-ref) save-vars)))
          (when temp
            (push `(setq ,temp ,init-val) inits))
          (make-default-binding pvar)
          (push (make-save-info :save-var pvar
                                :iv-ref iv-ref
                                :save-vars save-vars)
                (previous-info-save-info-list p-i))
          (return-code :initial inits))
        (clause-error "~a should be a positive integer" n-expr))))

;(defsynonym prev :prev)
(defsynonym :previous :prev)
;(defsynonym previous :prev)

(defun register-previous-code (vars code class)
  "???"
  ;; It's important for this that code is never copied;
  ;; we keep a pointer to it
  (dolist (var (mklist vars))
    (let ((p-i (intern-previous-info var)))
      (setf (previous-info-class p-i) class)
      (push (cons code (last code)) (previous-info-code p-i)))))

(defun intern-previous-info (var)
  "If <_:arg var /> already has a previous-info structure, return it;
else create a new one, put it where it belongs, and return it.
Make sure that if <_:arg var /> is itself a save-var, the new record
goes after the one for <_:arg var />'s var, so that the previous code
is generated before it is itself considered update code for another
previous splicing"
  (or (cdr (assoc var *previous-vars-alist*))
      (let ((p-i (make-previous-info :var var))
            (place (member var *previous-vars-alist* 
                           :test #'is-save-var)))
        (if place
            (push (cons var p-i) (cdr place))
            (push (cons var p-i) *previous-vars-alist*))
        p-i)))

(defun is-save-var (var entry)
  "???"
  (member var (previous-info-save-info-list (cdr entry))
          :key #'save-info-save-var))

(defun make-save-vars (var n)
  "???"
  (let ((str (format nil "SAVE-~a-" var))
        rez)
    (dotimes (i n)
      (let ((svar (make-var-and-default-binding str :using-type-of var)))
        (push svar rez)))
    rez))

(defun insert-previous-code ()
  "For each variable that requires a previous value, get all the update code
for that variable and splice in code that will remember the previous
values for the desired number of iterations. Return code to put in the
init and step sections of the loop.

There are three situations here: 
1. Variable has its initial value at the beginning of the loop, or gets
   its initial value in a different place than where it is updated. In
   this case, we can put the save code just before each update of the
   variable. Applicable clauses are: :FOR-PREVIOUS, :FOR-INITIALLY-THEN,
   and :FOR-FIRST-THEN. (class :INITIAL)
2. The variable is updated somewhere inside the loop, and the update also
   gives it its first value. We use another, internal save variable,
   which is set to the variable after each update. This is for :FOR-:= and
   driver clauses when :NEXT is used.(class :NEXT)
3. Variable is a driver with no :NEXT. We can put the update in the step
   portion of the loop, since we know the update code occurs at the
   beginning. (class :STEP)

Note that (3) is really an optimization of (2), and we could perform such
an optimization more generally if we could show that a variable in class
\(2) was always updated before being used.  Right now, we don't bother.
*** (3) is no longer done because driver code stays where the driver is.
We could try to detect that the driver is at the beginning, but don't
for now."
  (let ((pv-list *previous-vars-alist*)
        init-code
        step-code)
    ;; Step through this manually, because it may be that we add to it in
    ;; the process, and we must make sure that we don't cdr till we have to. 
    (loop
     (unless pv-list (return))
     (let* ((entry (car pv-list))
            (var (car entry))
            (p-i (cdr entry))
            (save-info-list (previous-info-save-info-list p-i))
            (code-list (previous-info-code p-i))
            (class (previous-info-class p-i)))
       (when save-info-list
         (if (or code-list (eq class :step))
             (let ((prev-code (unless (eq class :next)
                                (mapcan #`(make-prev-code var _)
                                        save-info-list))))
               (case class
                 (:initial (splice-in-code prev-code nil code-list))
                 ((:next :step) (augment init-code
                                         (do-extra-save-var-hack var
                                                                 save-info-list
                                                                 code-list)))
                 (otherwise (bug "unknown class ~a" class))))
             (clause-error "Cannot obtain previous values of ~a" var))))
       (setq pv-list (cdr pv-list)))
    (values init-code
            step-code)))

(defun do-extra-save-var-hack (var save-info-list code-list)
  "The name speaks for itself"
  (let (init-code prev-code post-code)
    (dolist (s-i save-info-list)
      (let* ((extra-save-var (make-post-save-var var))
             (prev (make-prev-code extra-save-var s-i :next)))
        (augment init-code `((setq ,extra-save-var ,(save-info-iv-ref s-i))))
        (augment post-code `((setq ,extra-save-var ,var)))
        (augment prev-code prev)))
    (splice-in-code prev-code post-code code-list)
    init-code))

(defun make-post-save-var (var)
  "??"
  (make-var-and-default-binding (format nil "POST-SAVE-~a-" var) 
                                :using-type-of var))


(defun make-prev-code (set-var s-i &optional (class :initial))
  "Generate code fro the PREVIOUS mechanism"
  (let ((prev (make-save-previous-code set-var (save-info-save-vars s-i))))
    (register-previous-code (save-info-save-var s-i) prev class)
    prev))

(defun make-save-previous-code (var save-vars)
  "???"
  ;; The first save-var is the furthest back.
  (if (cdr save-vars)
      (cons `(setq ,(first save-vars) ,(second save-vars))
            (make-save-previous-code var (cdr save-vars)))
      `((setq ,(car save-vars) ,var))))

(defun splice-in-code (prev-code post-code code-list)
  "Put <_:arg prev-code /> in at the first cons cell of <_:arg code />,
and <_:arg post-code /> at the last cons cell. Both <_:arg prev-code />
and <_:arg post-code /> are single forms."
  ;; Some list splicing here--danger.  It's crucial that
  ;; CODE actually appears in the code to be generated.
  ;; Can't use PROGN-WRAP here, because other people might have pointers to
  ;; this code, and when PROGN-WRAP takes the car it ruins that.
  (setq prev-code (add-progn prev-code))
  (setq post-code (add-progn post-code))
  (dolist (code code-list)
    (let* ((first-cons-cell (car code))
           (last-cons-cell (cdr code)))
      (when post-code
        (setf (cdr last-cons-cell) (cons post-code (cdr last-cons-cell))))
      (when prev-code
        (let ((new-start (cons (car first-cons-cell) (cdr first-cons-cell))))
          (setf (car first-cons-cell) prev-code)
          (setf (cdr first-cons-cell) new-start))))))

(defun add-progn (forms)
  "When <_:arg forms /> is more than one form, cons the <_:fun progn /> inIf
more "
  (cond ((null forms) nil)
        ((and (listp (car forms)) (not (lambda-expression? (car forms))))
         (cons 'progn forms))
        (t forms)))

(defun progn-wrap (forms)
  "Trabsform forms, if more than one is present, and the first is a list:
wrap <_:fun progn /> around. Not copying forms"
  (if (cdr forms)
      (if (and (listp (car forms)) (not (eq (caar forms) 'lambda)))
          (cons 'progn forms)
          forms)
      (car forms)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error reporting

(defun top-level-check ()
  "Check for being at <_:var *top-level?* /> and signal <_:fun clause-error />
otherwise"
  (unless *top-level?*
    (clause-error "Clause can occur only at top-level")))

(defun clause-error (format-string &rest args)
  "Signal an <_:fun error /> of improper use of <_:fun iterate />"
  (apply #'error
         (strcat "Iterate~@[, in ~a~]:~%" format-string)
         (and (boundp '*clause*) *clause*)
         args))

(defun clause-warning (format-string &rest args)
  "Signal an <_:fun warn />ing of some improper use of <_:fun iterate />,
which was omitted"
  (let ((*print-pretty* t))
    (apply #'warn
           (strcat "Iterate~@[, in clause ~a~]:~%" format-string)
           (and (boundp '*clause*) *clause*)
           args)))

(defun bug (format-string &rest args)
  "Signal an <_:fun Iterate /> bug to <_:var *error-output* />"
  (apply #'format *error-output*
         (strcat "Bug in Iterate: " format-string) args))

(eval-always
  (pushnew :iter *features*))


;;; end