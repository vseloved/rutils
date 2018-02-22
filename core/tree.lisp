;;; see LICENSE file for permissions

(cl:in-package #:rutils.tree)
(named-readtables:in-readtable rutils-readtable)
(eval-when (:compile-toplevel)
  (declaim #.+default-opts+))


(defmacro dotree ((subtree tree &optional result) &body body)
  "Iterate over each SUBTREE of the TREE in depth-first order.
   Optionally return RESULT."
  (with-gensyms (rec child)
    `(labels ((,rec (,subtree)
                ,@body
                (unless (atom ,subtree)
                  (dolist (,child (cdr ,subtree))
                    (,rec ,child)))))
       (when-it ,tree
         (,rec it))
       ,result)))

(defmacro doleaves ((node tree &optional result) &body body)
  "Iterate over each leaf NODE of the TREE in depth-first order.
   Optionally return RESULT."
  (with-gensyms (rec child)
    `(labels ((,rec (,node)
                (unless (atom ,node)
                  (dolist (,child (cdr ,node))
                    (if (atom ,child)
                        (let ((,node ,child))
                          ,@body)
                        (,rec ,child))))))
       (when-it ,tree
         (,rec it))
       ,result)))

(defun maptree (fn tree)
  "Map a one-argument function FN over subtree of the TREE
   in depth-first order, returning a new tree with the same structure."
  (labels ((rec (node)
             (if (atom node)
                 (funcall fn node)
                 (cons (rec (car node))
                       (mapcar #'rec (cdr node))))))
    (when-it tree
      (rec it))))

(defun mapleaves (fn tree)
  "Map a one-argument function FN over each leaf node of the TREE
   in depth-first order, returning a new tree with the same structure."
  (labels ((rec (node)
             (if (atom node)
                 (funcall fn node)
                 (cons (car node)
                       (mapcar #'rec (cdr node))))))
    (when-it tree
      (rec it))))

(defun tree-size (tree)
  "Returns the number of nodes (internal & external) in the indicated TREE."
  (let ((acc 0))
    (dotree (_ tree)
      (incf acc))
    acc))

(defun tree-depth (tree)
  "Returns the length of the largest of nodes from the root tree."
  (cond ((atom tree) 0)
        ((rest tree)
         (1+ (reduce #'max (cons 1 (mapcar #'tree-depth (rest tree))))))
        (t 1)))
