;; For license see LICENSE

(in-package #:reasonable-utilities.core)

(proclaim '(optimize speed))


(defmacro eval-always (&body body)
  "Wrap <_:arg body /> in <_:fun eval-when /> with all keys
\(compile, load and execute) mentioned"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro abbrev (short long)
  "Abbreviate a <_:arg long /> MACRO or FUNCTION name as <_:arg short />"
  `(eval-always
    (cond
      ((macro-function ',long)
       (setf (macro-function ',short) (macro-function ',long)))
      ((special-operator-p ',long)
       (error "Can't ABBREViate a special-operator ~a" ',long))
      ((fboundp ',long)
       (setf (fdefinition ',short) (fdefinition ',long)))
      (t
       (error "Can't ABBREViate ~a" ',long)))
    (setf (documentation ',short 'function) (documentation ',long 'function))
    ',short))


;; literal syntax

(defgeneric enable-literal-syntax (which)
  (:documentation "Dynamically modify read-table
to enable some reader-macros"))

(defgeneric disable-literal-syntax (which)
  (:documentation "Dynamically modify read-table
to disable some reader-macros"))

(defmacro locally-enable-literal-syntax (which)
  "Modify read-table to enable some reader-macros
at compile/load time"
  `(eval-always
     (enable-literal-syntax ,which)))

(defmacro locally-disable-literal-syntax (which)
  "Modify read-table to disable some reader-macros
at compile/load time"
  `(eval-always
     (disable-literal-syntax ,which)))

;; symbols

(defun make-gensym-list (length &optional (x "G"))
  "Return a list of <_:arg length /> gensyms,
using the second (optional, defaulting to \"G\") argument"
  (let ((g (if (typep x '(integer 0)) x (string x))))
    (loop :repeat length :collect (gensym g))))

(defmacro with-gensyms ((&rest names) &body body)
  "Provide gensyms for given <_:arg names />"
  `(let ,(loop :for n :in names :collect `(,n (gensym)))
     ,@body))

(abbrev with-unique-names with-gensyms)

(defmacro once-only (specs &body forms)
  "Evaluate <_:arg forms /> with names rebound to temporary variables, ensuring
that each is evaluated only once.

Each <_:arg spec /> must be either a NAME, or a (NAME INITFORM), with plain
NAME using the named variable as initform.

Example:
  (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
  (let ((y 0)) (cons1 (incf y))) => (1 . 1)"
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

(proclaim '(inline ensure-symbol ensure-keyword))

(eval-always
  (defun ensure-symbol (obj &key (format "~a") package)
    "Make a symbol in either <_:arg package /> or <_:var *package* />
from <_:arg obj /> according to <_:arg format />"
    (intern (string-upcase (format nil format obj))
            (or package *package*))))

(eval-always
  (defun ensure-keyword (obj &key (format "~a"))
    "Make a keyword from <_:arg obj /> according to <_:arg format />"
    (ensure-symbol obj :format format :package :keyword)))

(defun package-symbols (package)
  "List all symbols in a <_:arg package />"
  (loop :for sym :being :the :present-symbol :of package
     :collect sym))

(defun package-external-symbols (package)
  "List all symbols in a <_:arg package />"
  (loop :for sym :being :the :external-symbol :of package
     :collect sym))

(defun export-exported-symbols (from-package to-package)
  "Make the exported symbols in <_:arg from-package />
be also exported from <_:arg to-package />."
  (use-package from-package to-package)
  (do-external-symbols (sym (find-package from-package))
    (export sym (find-package to-package))))

(defmacro defpackage+ (name (&rest parent-packages) &rest defpackage-options)
  "Define package <_:arg name />, exporting all external symbols
from <_:arg parent-packages /> and adding them to :use'd packages.
<_:arg Defpackage-options /> are regular options for <_:fun defpackage /> form."
  (let ((parent-symbols (mapcan #'package-external-symbols parent-packages)))
    (mapcar (lambda (option data)
              (if (assoc option defpackage-options)
                  (rplacd (assoc option defpackage-options)
                          (remove-duplicates
                           (append (cdr (assoc option defpackage-options))
                                   data)))
                  (push (cons option data) defpackage-options)))
            (list :export :use)
            (list parent-symbols parent-packages))
    `(defpackage ,name ,@defpackage-options)))

(defmacro defconst (name value &optional doc)
  "<_:fun Defconstant /> only, whent it's not already defined"
  `(eval-always
     (unless (boundp ',name)
       (defconstant ,name ,value ,doc))))


;;; end