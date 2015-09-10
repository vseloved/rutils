;; For license see LICENSE

(cl:in-package #:rutils.core)
(named-readtables:in-readtable rutils-readtable)
(declaim #.+default-opts+)


(define-condition rutils-style-warning (simple-condition style-warning) ())

(defmacro eval-always (&body body)
  "Wrap BODY in eval-when with all keys (compile, load and execute) mentioned."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro abbr (short long &optional lambda-list)
  "Abbreviate LONG macro or function name as SHORT.
   If LAMBDA-LIST is present, also copy appropriate SETF-expander."
  `(eval-always
     ;; Lispworks signals error while abbreviating to keywords
     ;; SBCL has package locks when accessing built-in functionality
     ;; other similar things are probably possible in other implementations
     (handler-bind ((error (lambda (e)
                             (let ((r (find-restart 'continue e)))
                               (when r
                                 (warn 'rutils-style-warning
                                       :format-control
                                       "Skipped error during abbreviation: ~A"
                                       :format-arguments (list e))
                                 (invoke-restart r))))))
       (cond
         ((macro-function ',long)
          (setf (macro-function ',short) (macro-function ',long))
          #+ccl (setf (ccl:arglist ',short) (ccl:arglist ',long)))
         ((special-operator-p ',long)
          (error "Can't abbreviate a special-operator ~a" ',long))
         ((fboundp ',long)
          (setf (fdefinition ',short) (fdefinition ',long))
          #+ccl (setf (ccl:arglist ',short) (ccl:arglist ',long))
          ,(when lambda-list
            `(define-setf-expander ,short ,lambda-list
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
       ',short)))


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

(defun package-internal-symbols (package)
  "List all symbols in a PACKAGE that are not imported."
  (let ((imported-syms (make-hash-table)))
    (dolist (pkg (package-use-list package))
      (do-external-symbols (sym pkg)
        (setf (gethash sym imported-syms) t)))
    (loop :for sym :being :the :present-symbol :of package
       :unless (gethash sym imported-syms)
       :collect sym)))

(defun package-external-symbols (package)
  "List all symbols in a PACKAGE."
  (loop :for sym :being :the :external-symbol :of package
     :collect sym))

(defun re-export-symbols (from-package to-package)
  "Make the exported symbols in FROM-PACKAGE be also exported from TO-PACKAGE."
  (use-package from-package to-package)
  (do-external-symbols (sym (find-package from-package))
    (export sym (find-package to-package))))
