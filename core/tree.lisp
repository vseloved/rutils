;;; see LICENSE file for permissions

(cl:in-package #:reasonable-utilities.tree)
(named-readtables:in-readtable rutils-readtable)

(declaim (optimize (speed 3) (space 1) (debug 0)))


(defmacro dotree ((var tree-form &optional result-form) &body body)
  "The analog of DOLIST, operating on trees."
  (with-gensyms (traverser list list-element)
    `(labels ((,traverser (,list)
                (dolist (,list-element ,list)
                  (if (consp ,list-element)
                      (,traverser ,list-element)
                      (let ((,var ,list-element))
                        ,@body)))))
       (,traverser ,tree-form)
       ,result-form)))

(defun maptree (fn tree)
  (labels ((rec (node)
             (if (consp node)
                 (mapcar #'rec node)
                 (funcall fn node))))
    (mapcar #'rec tree)))

(defun tree-size (tree)
  "Returns the number of nodes (internal & external) in the indicated tree."
  (when tree
    (1+ (apply #'+ (mapcar #'tree-size tree)))))
