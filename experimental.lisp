;;; RUTILS experimental stuff
;;; see LICENSE file for permissions

(in-package "REASONABLE-UTILITIES.EXPERIMENTAL")


;; defmulti

(define-method-combination multi (dispatch-fn)
  ((all *))
  (:arguments &whole args)
  `(or ,@(mapcar (lambda (method)
                   `(when (find (apply ,dispatch-fn ,args)
                                (method-qualifiers ,method))
                      (call-method ,method)))
                 all)))

(defmacro defmulti (fun-name dispatch-fn lambda-list)
  "Clojure style multimethods:

CL-USER> (defmulti foo #'length (collection))
#<STANDARD-GENERIC-FUNCTION FOO (0)>
CL-USER> (defmethod foo 3 (x) (format t \"~a contains 3 things\" x))
#<STANDARD-METHOD FOO 3 (T) {B6238A9}>
CL-USER> (defmethod foo 10 (_) (format t \"A collection of 10 elements.~%\"))
#<STANDARD-METHOD FOO 10 (T) {B03CBC9}>
CL-USER> (foo \"yes\")
yes contains 3 things
NIL
CL-USER> (foo (range 10))
A collection of 10 elements.
NIL
"
  `(defgeneric ,fun-name ,lambda-list
     (:method-combination multi ,dispatch-fn)))


;;; end