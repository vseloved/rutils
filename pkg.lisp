;;; RUTILS read time manipulation of package visibility
;;; see LICENSE file for permissions

;;; see pkg.txt

(in-package "REASONABLE-UTILITIES.PKG")

(eval-always
  (defconstant %directive-package% (find-package "RUTILS.PKG"))
  (defconstant %dispatch-char% #\@)

  (defvar *env* nil)

  (defstruct env
    (parent)
    (this-package)
    (previous-package)
    (retain-syms)
    (suppress-syms)
    (stash))

  (defun toplevel-package (env)
    (if (env-parent env)
        (toplevel-package (env-parent env))
        (env-previous-package env)))

  (defun reconcile-package (here-package above-package 
                            retain-syms suppress-syms)
    (loop :for sym :being :each present-symbol :of here-package
       :when (and (eq (symbol-package sym) here-package)
                 (or (eq retain-syms t)
                     (member sym retain-syms :test #'string=)))
       :collect sym :into syms
       :finally (import (set-difference syms suppress-syms)
                        above-package)))

  (defun specifier-to-sym (specifier)
    (typecase specifier
      (string (intern specifier %directive-package%))
      (symbol specifier)
      (otherwise (error "#@: ~A does not name a symbol." specifier))))

  (defun specifiers-to-syms (specifiers)
    (mapcar #'specifier-to-sym specifiers))

  (defun specifier-to-package (specifier)
    (or (and (packagep specifier) specifier) 
        (find-package (specifier-to-sym specifier))
        (error "#@: package ~A does not exist." specifier)))

  (defun copy-visibility (from-package &optional (to-package *package*))
    (loop for sym being each present-symbol of from-package
          do (import (or sym (list sym)) to-package))
    (loop for sym in (package-shadowing-symbols from-package)
          do (shadow (symbol-name sym) to-package))
    (loop for pkg in (package-use-list from-package)
          do (use-package pkg to-package)))

  (defun copy-package (package)
    (let ((new-package (make-package (gensym) :use ())))
      (copy-visibility package new-package)
      new-package))

  (defun use-packages (specifiers)
    (loop for package-name in specifiers
          do (let ((package (specifier-to-package package-name)))
               (loop for sym being each external-symbol of package
                     do (if (find-symbol (symbol-name sym))
                          (shadowing-import sym))
                     finally (use-package package)))))

  (defun import-specified-syms (from-package specifiers &key no-error)
    (let ((package (specifier-to-package from-package)))
      (loop for specifier in (specifiers-to-syms specifiers)
            do (let ((symbol 
                       (or (find-symbol (symbol-name specifier) package)
                           (unless no-error
                             (error "#@: no symbol ~A in package ~A."
                                    specifier (package-name package))))))
                 (if symbol
                   (shadowing-import symbol))))))

  (defun intern-specified-syms (specifiers)
    (loop for specifier in (specifiers-to-syms specifiers)
          collect (let ((sym (find-symbol (symbol-name specifier))))
                    (when sym
                      (unintern sym))
                    (when (find-symbol (symbol-name specifier))
                      (shadow specifier))
                    (intern (symbol-name specifier)))))

  (defun evaluate (form)
    (cond
      ((null form))
      ((consp form)
       (if (consp (first form))
           (mapc #'evaluate form)
           (let ((sym (first form)))
             (case sym
               (top (setf (env-previous-package *env*) (toplevel-package *env*)
                          (env-this-package *env*) (copy-package 
                                                    (env-previous-package
                                                     *env*))
                          (env-parent *env*) nil
                          *package* (progn (delete-package *package*)
                                           (setf (env-suppress-syms *env*) nil)
                                           (env-this-package *env*))))
               (in (let ((package (specifier-to-package (second form))))
                     (setf (env-previous-package *env*) package
                           (env-this-package *env*) (copy-package package)
                           (env-parent *env*) nil
                           *package* (progn (delete-package *package*)
                                            (setf (env-suppress-syms *env*) nil)
                                            package))))
               (inherit (setf (env-this-package *env*) (make-package (gensym) 
                                                                     :use nil)
                              *package* (progn (delete-package *package*)
                                               (setf (env-suppress-syms *env*)
                                                     nil)
                                               (env-this-package *env*)))
                        (import-specified-syms (env-previous-package *env*)
                                               (rest form)))
               (keep (setf (env-retain-syms *env*) (rest form))
                     (import-specified-syms (env-previous-package *env*)
                                            (rest form)
                                            :no-error t))
               (keep-all (setf (env-retain-syms *env*) t)
                         (copy-visibility (env-previous-package *env*)))
               (unique (let ((newsyms (intern-specified-syms (rest form))))
                         (setf (env-suppress-syms *env*)
                               (union (env-suppress-syms *env*) newsyms))))
               (use (use-packages (rest form)))
               (from (destructuring-bind (from from-package import 
                                               &rest specifiers) form
                       (unless (and (eq import 'import))
                         (error "#@: bad FROM package IMPORT syms syntax."))
                       (import-specified-syms from-package specifiers)))
               (otherwise (error "#@: ~A is an unknown directive." sym))))))
      (t (error "#@: bad syntax: ~A" form))))

  (defun dispatch-macro (stream sub-character integer-param)
    ""
    (declare (ignore integer-param))
    (let* ((temp-package (copy-package *package*))
           (*env* (make-env :parent *env* 
                            :this-package temp-package
                            :previous-package *package*))
           (*package* temp-package))
      (evaluate (let ((*package* %directive-package%))
                  (read stream t nil t)))
      (prog1 (read stream t nil t)
        (reconcile-package (env-this-package *env*) 
                           (env-previous-package *env*)
                           (env-retain-syms *env*)
                           (env-suppress-syms *env*)))))

  (defmethod enable-literal-syntax ((which (eql :reader-control)))
    (set-dispatch-macro-character #\# #\@ #'dispatch-macro))

  (defmethod disable-literal-syntax ((which (eql :reader-control)))
    (set-dispatch-macro-character #\# #\@ (make-reader-error-fun #\@)))
)

;;; end