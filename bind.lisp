;;; RUTILS BIND macro
;;; see LICENSE file for permissions

(in-package "REASONABLE-UTILITIES.BIND")

(locally-enable-literal-syntax :sharp-backq)


(defmacro bind ((&rest clauses) &body body)
  "A general binding construct, which aims to incorporate all ~
frequently used binding forms, and provide an extension interface.

Dispatches to the primitive CL binding forms.  Dispatch is performed ~
at comile-time by structural and identity comparisons (although any other ~
comparison approach can be utilized) according to the dispatch rules, ~
that are stored in <_:var *bind-dispatch-table* />.  Rules are evaluated ~
in the order of their addition, so the most specific ones should be ~
added at the top.  Each binding is performed sequentially, so the effect ~
is similar to <_:fun let* />, rather than <_:fun let />"
  (let ((rez body))
    (mapc #`(setf rez `((,@(apply #'expand-bind-clause _ rez))))
          (reverse clauses))
    (car rez)))

(defparameter *bind-dispatch-table* (make-array 0 :fill-pointer t)
  "A vector, storing BIND-RULES")

(defmacro def-bind-rule (rule expansion)
  "Add a new BIND-RULE, which is specified as a pair of anonymous functions:
 - a predicate, by which the rule is triggered
 - an expansion
to <_:var *bind-dispatch-table* />.
A free variable <_:arg clause /> is captures inside boh functions, ~
while the second one as well captures <_:arg body />"
  `(vector-push-extend (cons (lambda (clause)
                               (and (listp clause) ,@rule))
                             (lambda (clause &rest body)
                               ,expansion))
                       *bind-dispatch-table*))

(defun expand-bind-clause (clause &rest body)
  "Try to find a rule for the <_:arg clause /> in ~
<_:var *bind-dispatch-table* /> and expand it into the code, ~
otherwise signal an error"
  (apply (or (cdr (find t *bind-dispatch-table*
                        :key #`(funcall (car _) clause)))
             (error "No bind rule for clause: ~a" clause))
         clause body))

;; rules

(def-bind-rule ((dyadic clause)
                (stringp (car clause))
                (symbolp (cadr clause)))
    `(with-input-from-string (,(cadr clause) ,(car clause))
       ,@body))

(def-bind-rule ((single clause)
                (symbolp (car clause)))
    `(with-output-to-string (,(car clause))
       ,@body))

(def-bind-rule ((if (dyadic clause)
                    (pathnamep (cadr clause))
                    (keywordp (caddr clause)))
                (symbolp (car clause)))
    `(with-open-file (,(car clause) ,(cadr clause) ,@(cddr clause))
       ,@body))

(def-bind-rule ((dyadic clause)
                (symbolp (car clause))
                (not (keywordp (car clause))))
    `(let (,clause)
       ,@body))

(def-bind-rule ((dyadic clause)
                (listp (car clause)))
    `(tmpl-bind ,(car clause) ,(cadr clause)
       ,@body))

(def-bind-rule ((cddr clause)
                (every #`(and (symbolp _) (not (keywordp _))) (butlast clause))
                (listp (last1 clause)))
    `(multiple-value-bind ,(butlast clause) ,@(last clause)
       ,@body))

#+nil
(def-bind-rule ((intersection clause '(:slots :accessors))
                (not (keywordp (last1 clause))))
    (ds-bind (slots accessors)
        (loop :for decl :in (last1 (butlast clause))
           :if (symbolp decl) :collect decl :into slot-decls
           :else :if (listp decl) :collect decl :into accessor-decls
           :else :do (warn "Improper slot/accessor declaration: ~
~a -- in BIND-clause: ~a"
                           decl clause)
           :finally (return (list slot-decls accessor-decls)))
      (let ((instance (gensym))
            (rez body))
        (when slots
          (setf rez `((with-slots ,slots ,instance ,@rez))))
        (when accessors
          (setf rez `((with-accessors ,accessors ,instance ,@rez))))
        `(let ((,instance ,(last1 clause))) ,@rez))))

#+:cl-ppcre
(def-bind-rule ((tryadic clause)
                (listp (car clause)))
               `(cl-ppcre:register-groups-bind
                    ,(car clause) (,(cadr clause) ,(caddr clause))
                  ,@body))


;; destructuring

(defmacro tmpl-bind (tmpl seq &body body)
  "Perform destructuring on collection <_:arg coll /> ~
to the template <_:arg tmpl />"
  (let ((gseq  (gensym "SEQ"))
        (gkeys (gensym "KEYS")))
    (wrap-bindings (make-bindings nil tmpl seq gseq gkeys) body)))

(defun make-bindings (state tmpl seq gseq gkeys &optional (n 0))
  "Prepare binding forms for <_:fun ds-bind-seq />"
  (when tmpl
    (case (car tmpl)
      (&whole    (if (null state)
                     (if-it (cadr tmpl)
                            (add-binding
                             `(let* ((,gseq ,seq)
                                     (,it ,gseq)))
                             :regular (cddr tmpl) seq gseq gkeys (1+ n))
                            (error "No more bindings after &whole"))
                     (error "&whole not at the start")))
      (&optional (if (member state '(nil :regular))
                     (if-it (cadr tmpl)
                            (add-binding
                             `(let ((,it (ignore-errors (elt ,gseq ,n)))))
                             :optional (cddr tmpl) seq gseq gkeys (1+ n))
                            (error "No more bindings after &optional"))
                     (error "&optional after &key/&rest")))
      (&key      (if (member state '(nil :regular :rest))
                     (if-it (cadr tmpl)
                            (add-binding
                             `(let* ((,gkeys (subseq ,gseq ,n))
                                     (,it (getf ,gkeys ,(mkeyw it)))))
                             :key (cddr tmpl) seq gseq gkeys (1+ n))
                            (error "No more bindings after &key"))
                     (error "&key after &optional")))
      (&rest     (if (member state '(nil :regular :optional))
                     (if-it (cadr tmpl)
                            (add-binding `(let ((,it (subseq ,gseq ,n))))
                                         :rest (cddr tmpl) seq gseq gkeys n)
                            (error "No more bindings after &rest"))
                     (error "&rest after &key")))
      (otherwise  (let ((var (car tmpl)))
                    (case state
                      (:regular  (add-binding
                                  `(let ((,var (elt ,gseq ,n))))
                                  :regular (cdr tmpl) seq gseq gkeys (1+ n)))
                      (:optional (add-binding
                                  `(let ((,var (ignore-errors (elt ,gseq ,n)))))
                                  :optional (cdr tmpl) seq gseq gkeys (1+ n)))
                      (:key      (add-binding
                                  `(let ((,var (getf ,gkeys ,(mkeyw var)))))
                                  :key (cdr tmpl) seq gseq gkeys (1+ n)))
                      (:rest     nil)
                      (otherwise (add-binding
                                  `(let* ((,gseq ,seq)
                                          (,var (elt ,gseq ,n))))
                                  :regular (cdr tmpl) seq gseq gkeys
                                  (1+ n)))))))))

(defun add-binding (binding &rest args)
  ""
  (cons binding (apply #'make-bindings args)))

(defun wrap-bindings (bindings body)
  "Wrap <_:arg bindings /> around <_:arg body />"
  (if bindings (append (car bindings)
                       (list (wrap-bindings (cdr bindings) body)))
      `(progn ,@body)))


;; dsetq

(defmacro dsetq (template value)
  "Destructuring assignment; supports both <_:code (values ...) />
for destructuring a multiple-value form and NIL as a variable name,
meaning to ignore that position,
e.g. <_:code (dsetq (values (a . b) nil c) form) />"
  (do-dsetq template value nil))

(defun do-dsetq (template value &optional (bindings? t) type)
  ""
  (cond
   ((null template) (dsetq-error "Can't bind to nil"))
   ((var-spec-p template) ; not only (symbolp template)
    (when bindings?
      (make-default-binding template :type type))
    `(setq ,(extract-var template) ,value))
   ;; m-v-setq
   ((and (consp template) (eq (car template) 'values))
    ;; Just do a simple check for the most common errors. There's no way we
    ;; can catch all problems.
    (if (or (atom value) (member (car value) '(car cdr cdar caar aref get)))
        (dsetq-error "Multiple values make no sense for this expression" )
        (make-mv-dsetqs (cdr template) value bindings?)))
   (t (with-gensyms (temp)
        `(let ((,temp ,value))
           ,.(when type
               `((declare (type ,type ,temp))))
           ,.(make-dsetqs template temp bindings?)
           ,temp)))))

(defun make-dsetqs (template value bindings?)
  ""
  (cond
   ((null template) nil)
   ((var-spec-p template)
    (when bindings?
      (make-default-binding template))
    `((setq ,(extract-var template) ,value)))
   ((atom template) (dsetq-error "Invalid binding form: ~a" template))
   ((eq (car template) 'values)
    (dsetq-error "Multiple-value destructuring cannot be nested"))
   (t (nconc (make-dsetqs (car template) `(car ,value) bindings?)
             (make-dsetqs (cdr template) `(cdr ,value) bindings?)))))

(defun make-mv-dsetqs (templates value bindings?)
  ""
  (let (temps vars tplates)
    (declare (type list temps vars tplates))
    (dolist (tp templates)
      (if (and tp (var-spec-p tp))  ; either var or (the type var)
          (progn (push nil tplates)
                 (push nil temps)
                 (push (extract-var tp) vars)
                 (when bindings?
                   (make-default-binding tp)))
          ;; either NIL or destructuring template
          (let ((temp (gensym "VALUE")))
            (push tp tplates)
            (push temp temps)
            (push temp vars))))
    (setq temps (nreverse temps))
    (setq vars (nreverse vars))
    (setq tplates (nreverse tplates))
    (let ((mv-setq `(multiple-value-setq ,vars ,value))
          (temp-vars (remove nil temps)))  ; remove, don't delete
      (if temp-vars
          `(let ,temp-vars
             (declare (ignorable .,temp-vars))  ; in case of NIL template
             ,mv-setq
             ,.(mapcan (lambda (tplate temp)
                         (make-dsetqs tplate temp bindings?))
                       tplates temps)
             (car vars))
          mv-setq))))

;; utils

(defun dsetq-error (format-string &rest args)
  "Signal an <_:fun error /> in <_:fun dsetq />"
  (apply #'error (strcat "DSETQ: " format-string) args))

(defun the-expression-p (x)
  "Test wheather <_:arg x /> is a <_:fun the />-declaration"
  (and (consp x) (eq (first x) 'the)))

(defun var-spec-p (x)
  "Test wheather <_:arg x /> is a <_:fun the />-declaration or a symbol"
  (or (the-expression-p x) (symbolp x)))

(defun extract-var (var-spec)
  "Either extract variable name from <_:fun the />-expression <_:arg var-spec />
or return <_:arg var-spec /> as-is"
  (if (the-expression-p var-spec) (third var-spec)
      var-spec))

(defun make-default-binding (var-spec &optional type)
  ""
  (let ((var (extract-var var-spec)))
    (cond
     ((null var-spec) nil)
     ((not (symbolp var)) (dsetq-error "The variable ~a is not a symbol" var))
     (t (add-binding var :regular nil)))))


;;; end