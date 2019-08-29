;;; see LICENSE file for permissions

(in-package #:rutils.hash-set)
(named-readtables:in-readtable rutils-readtable)
(eval-when (:compile-toplevel)
  (declaim #.+default-opts+))

(declaim (inline add# xor#))

(defun hash-set (&optional (test 'eql) &rest items)
  "Create a hash-set with a given TEST and initial contents ITEMS."
  (let ((set (make-hash-table :test test)))
    (dolist (item items)
      (sethash item set t))
    set))

(defun emptyp# (set)
  "Test wether a hash-set is empty"
  (zerop (hash-table-count set)))

(defun add# (item set)
  "Add ITEM to hash-set SET."
  (sethash item set t))

(defun inter# (set1 set2)
  "Set intersection between hash-sets SET1 & SET2."
  (let ((set (hash-set (hash-table-test set1))))
    (dotable (item _ set1)
      (when (gethash item set2)
        (add# item set)))
    set))

(defun union# (set1 set2)
  "Set union between hash-sets SET1 & SET2."
  (let ((set (hash-set (hash-table-test set1))))
    (dotable (item _ set1)
      (add# item set))
    (dotable (item _ set2)
      (add# item set))
    set))

(defun diff# (set1 set2)
  "Set difference between hash-sets SET1 & SET2."
  (let ((set (hash-set (hash-table-test set1))))
    (dotable (item _ set1)
      (unless (gethash item set2)
        (add# item set)))
    set))

(defun xor# (set1 set2)
  "Set xor between hash-sets SET1 & SET2."
  (union# (diff# set1 set2)
          (diff# set2 set1)))
