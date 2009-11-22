;;; RUTILS tree handling
;;; see LICENSE file for permissions

(in-package "REASONABLE-UTILITIES.TREE")

(defmacro dotree ((var tree-form &optional result-form) &body body)
  "The analog of <_:fun dolist />, operating on trees"
  (with-unique-names (traverser list list-element)
    `(progn
       (labels ((,traverser (,list)
                  (dolist (,list-element ,list)
                    (if (consp ,list-element)
                        (,traverser ,list-element)
                        (let ((,var ,list-element))
                          ,@body)))))
         (,traverser ,tree-form)
         ,result-form))))

;;; end