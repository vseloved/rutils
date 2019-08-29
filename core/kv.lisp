;;; see LICENSE file for permissions

(in-package #:rutils.kv)
(named-readtables:in-readtable rutils-readtable)
(eval-when (:compile-toplevel)
  (declaim #.+default-opts+))


(defgeneric keys (kv)
  (:documentation
   "Return a list of all keys in a KV.
    Order is unspecified.")
  (:method ((kv hash-table))
    (ht-keys kv))
  (:method ((list list))
    (listcase list
      (alist (mapcar #'car list))
      (dlist (car list))
      (t (range 0 (length list))))))

(defgeneric vals (kv)
  (:documentation
   "Return a list of all values in a KV.
    Order is unspecified.")
  (:method ((kv hash-table))
    (ht-vals kv))
  (:method ((list list))
    (listcase list
      (alist (mapcar #'cdr list))
      (dlist (cdr list))
      (t list))))

(defgeneric kvs (kv &optional result-kind)
  (:documentation
   "Return a list of all key-value pairs in a KV in one the 3 kinds:

    - list of pairs (default)
    - alist
    - dlist

    Order is unspecified.")
  (:method ((kv hash-table) &optional (result-kind 'pairs))
    (ecase result-kind
      (alist (ht->alist kv))
      (dlist (cons (keys kv) (vals kv)))
      (pairs (ht->pairs kv)))))

(defgeneric eq-test (kv)
  (:documentation
   "Return an equality test predicate of the KV.")
  (:method ((kv hash-table))
    (hash-table-test kv))
  (:method ((list list))
    'equal))

(defgeneric mapkv (fn kv)
  (:documentation
   "Like MAPCAR but for a data structure that can be viewed as a KV.")
  (:method (fn (kv hash-table))
    (with-hash-table-iterator (gen-fn kv)
      (let ((rez (make-hash-table :test (hash-table-test kv))))
        (loop
          (multiple-value-bind (valid key val) (gen-fn)
            (unless valid (return))
            (set# key rez (funcall fn key val))))
        rez)))
  (:method (fn (list list))
    (listcase list
      (alist (mapcar #`(cons (car %)
                             (funcall fn (car %) (cdr %)))
                     list))
      (dlist (list (car list)
                   (mapcar #`(funcall fn % %%)
                           (car list) (cdr list))))
      (t (mapindex fn list)))))

(defmacro dokv ((k v kv &optional rez) &body body)
  "Like DOLIST but iterates over key-value pairs (K V) in anything, that can be
   viewed as a KV (hash-table, alist, plist, object).
   Autodeclares variables named _ as ignored."
  `(progn
     (mapkv (lambda (,k ,v)
              ,(when (find "_" (list k v)
                           :test 'string=
                           :key 'symbol-name)
                 `(declare (ignore _)))
              ,@body)
            ,kv)
     ,rez))
