;;; see LICENSE file for permissions

(cl:in-package #:rutils.pair)
(named-readtables:in-readtable rutils-readtable)
(eval-when (:compile-toplevel)
  (declaim #.+default-opts+))


(declaim (inline pair))


;;; Pair

(defstruct (pair (:type list) (:conc-name nil))
  "A generic pair with left (LT) and right (RT) elements."
  lt rt)

(defun pair (x y)
  "A shortcut to make a pair of X and Y."
  (make-pair :lt x :rt y))

(defun ht->pairs (ht)
  "Dump hash-table HT to list of pairs."
  (with-hash-table-iterator (gen-fn ht)
    (loop
       :for (valid key val) := (multiple-value-list (gen-fn))
       :unless valid :do (return rez)
       :collect (pair key val) :into rez)))

(defun pairs->ht (pairs &rest hash-table-initargs)
  "Create hash-table from the list of PAIRS.
   Hash table is initialized using the HASH-TABLE-INITARGS."
  (loop
     :with ht := (apply #'make-hash-table hash-table-initargs)
     :for pair :in pairs
     :do (setf (gethash (lt pair) ht) (rt pair))
     :finally (return ht)))

(defgeneric pairs (table)
  (:documentation
   "Return a list of all key-value PAIRS in a TABLE.
    Order is unspecified.")
  (:method ((table hash-table))
    (ht->pairs table))
  (:method ((list list))
    (listcase list
      (alist (mapcar ^(pair (car %) (cdr %))
                     list))
      (dlist (mapcar 'pair
                     (car list) (cdr list)))
      (t (mapindex 'pair list)))))

(defmacro with-pair ((lt rt) pair &body body)
  "Bind LT and RT to PAIR's slots and execute BODY inside."
  `(let ((,lt (lt ,pair))
         (,rt (rt ,pair)))
     ,@body))
