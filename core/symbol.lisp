;; For license see LICENSE

(cl:in-package #:reasonable-utilities.symbol)

(declaim (optimize (speed 3) (space 1) (debug 0)))


(defmacro eval-always (&body body)
  "Wrap BODY in eval-when with all keys (compile, load and execute) mentioned."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro abbr (short long &optional lambda-list)
  "Abbreviate LONG macro or function name as SHORT. If LAMBDA-LIST is present,
also copy appropriate SETF-expander."
  `(eval-always
    (cond
      ((macro-function ',long)
       (setf (macro-function ',short) (macro-function ',long)))
      ((special-operator-p ',long)
       (error "Can't abbreviate a special-operator ~a" ',long))
      ((fboundp ',long)
       (setf (fdefinition ',short) (fdefinition ',long))
       ,(when lambda-list
          `(define-setf-expander ,short ,(append lambda-list)
             (values ,@(multiple-value-bind
                        (dummies vals store store-form access-form)
                        (get-setf-expansion
                         (cons long (remove-if (lambda (sym)
                                                 (member sym '(&optional &key)))
                                               lambda-list)))
                        (let ((expansion-vals (mapcar (lambda (x) `(quote ,x))
                                                      (list dummies
                                                            vals
                                                            store
                                                            store-form
                                                            access-form))))
                          (setf (second expansion-vals)
                                (cons 'list vals))
                          expansion-vals))))))
      (t
       (error "Can't abbreviate ~a" ',long)))
    (setf (documentation ',short 'function) (documentation ',long 'function))
    ',short))


;; symbols

(defun make-gensym-list (length &optional (x "G"))
  "Return a list of LENGTH gensyms, using the second (optional,
defaulting to 'G') argument."
  (let ((g (if (typep x '(integer 0)) x (string x))))
    (loop :repeat length :collect (gensym g))))

(defmacro with-gensyms ((&rest names) &body body)
  "Provide gensyms for given NAMES."
  `(let ,(loop :for n :in names :collect `(,n (gensym)))
     ,@body))

) ; end of eval-when

(abbr with-unique-names with-gensyms)

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
\(1 . 1)"
  (let ((gensyms (make-gensym-list (length specs) "OO"))
        (names-and-forms (mapcar (lambda (spec)
                                   (etypecase spec
                                     (list
                                      (destructuring-bind (name form) spec
                                        (cons name form)))
                                     (symbol
                                      (cons spec spec))))
                                 specs)))
    ;; bind in user-macro
    `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string (car n)))))
                   gensyms names-and-forms)
       ;; bind in final expansion
       `(let (,,@(mapcar (lambda (g n) ``(,,g ,,(cdr n)))
                         gensyms names-and-forms))
          ;; bind in user-macro
          ,(let ,(mapcar (lambda (n g) (list (car n) g))
                         names-and-forms gensyms)
             ,@forms)))))

(defun ensure-symbol (obj &key (format "~a") package)
  "Make a symbol in either PACKAGE or *PACKAGE* from OBJ according to FORMAT."
  (intern (string-upcase (format nil format obj))
          (or package *package*)))

(defun ensure-keyword (obj &key (format "~a"))
  "Make a keyword from OBJ according to FORMAT."
  (ensure-symbol obj :format format :package :keyword))

(defun package-symbols (package)
  "List all symbols in a PACKAGE."
  (loop :for sym :being :the :present-symbol :of package
     :collect sym))

(defun package-external-symbols (package)
  "List all symbols in a PACKAGE."
  (loop :for sym :being :the :external-symbol :of package
     :collect sym))

(defun re-export-symbols (from-package to-package)
  "Make the exported symbols in FROM-PACKAGE be also exported from TO-PACKAGE."
  (use-package from-package to-package)
  (do-external-symbols (sym (find-package from-package))
    (export sym (find-package to-package))))
