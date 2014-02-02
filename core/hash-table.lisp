;;; see LICENSE file for permissions

(in-package #:rutils.hash-table)
(named-readtables:in-readtable rutils-readtable)
(declaim #.+default-opts+)


(declaim (inline sethash))
(defun sethash (key ht val)
  "Set VAL at KEY in hash-table HT."
  (setf (gethash key ht) val))

(declaim (inline takehash))
(defun takehash (key ht)
  "Get and remove VAL at KEY in hash-table HT."
  (prog1 (gethash key ht)
    (remhash key ht)))

(defun copy-hash-table (ht &key key test size
                                rehash-size rehash-threshold)
  "Returns a copy of hash table HT, with the same keys and values.
The copy has the same properties as the original, unless overridden
by the keyword arguments.

Before each of the original values is set into the new hash-table,
KEY is invoked on the value. As KEY defaults to IDENTITY
a shallow copy is returned by default."
  (setf key (or key 'identity))
  (setf test (or test (hash-table-test ht)))
  (setf size (or size (hash-table-size ht)))
  (setf rehash-size (or rehash-size (hash-table-rehash-size ht)))
  (setf rehash-threshold (or rehash-threshold (hash-table-rehash-threshold ht)))
  (let ((copy (make-hash-table :test test :size size
                               :rehash-size rehash-size
                               :rehash-threshold rehash-threshold)))
    (maphash (lambda (k v)
               (setf (gethash k copy) (funcall key v)))
             ht)
    copy))

(defun merge-hash-tables (ht &rest hts)
  "From 1 or more HTS create a single one with TEST of HT."
  (if hts
      (let ((rez (make-hash-table :test (hash-table-test ht))))
        (mapc (lambda (next)
                (maphash #`(sethash % rez %%)
                         next))
              (cons ht hts))
        rez)
      ht))

(defun hash-table-keys (ht)
  "Return a list of keys of hash-table HT."
  (loop :for k :being :the :hash-keys :of ht
     :collect k))

(defun hash-table-vals (ht)
  "Return a list of values of hash-table HT."
  (loop :for v :being :the :hash-values :of ht
     :collect v))

(defun hash-table-from-plist (plist &rest hash-table-initargs)
  "Returns a hash-table containing the keys and values, alternating in PLIST.
Hash table is initialized using the HASH-TABLE-INITARGS."
  (loop
     :with ht := (apply #'make-hash-table hash-table-initargs)
     :for (k v) :on plist :by #'cddr
     :do (setf (gethash k ht) v)
     :finally (return ht)))

(defun hash-table-to-plist (ht)
  "Returns a list containing the keys and values of hash-table HT."
  (with-hash-table-iterator (gen-fn ht)
    (loop
       :for (valid key val) := (multiple-value-list (gen-fn))
       :unless valid :do (return rez)
       :nconc (list key val) :into rez)))

(defun hash-table-from-alist (alist &rest hash-table-initargs)
  "Returns a hash-table containing the keys and values, alternating in ALIST.
Hash table is initialized using the HASH-TABLE-INITARGS."
  (loop
     :with ht := (apply #'make-hash-table hash-table-initargs)
     :for (k . v) :in alist
     :do (setf (gethash k ht) v)
     :finally (return ht)))

(defun hash-table-to-alist (ht)
  "Returns an alist containing the keys and values of hash-table HT."
  (with-hash-table-iterator (gen-fn ht)
    (loop
       :for (valid key val) := (multiple-value-list (gen-fn))
       :unless valid :do (return rez)
       :collect (cons key val) :into rez)))

(defun print-hash-table (ht &optional (stream *standard-output*))
  "Pretty print hash-table HT to STREAM."
  (let ((*print-pretty* t))
    (pprint-logical-block (stream nil)
      (princ "#{" stream)
      (unless (eq (hash-table-test ht) 'eql)
        (princ (hash-table-test ht) stream))
      (pprint-indent :block 2 stream)
      (maphash (lambda (k v)
                 (pprint-newline :mandatory stream)
                 (when (and k (listp k)) (princ #\' stream))
                 (if (typep k 'hash-table)
                     (print-hash-table k stream)
                     (prin1 k stream))
                 (princ " " stream)
                 (when (and v (listp v)) (princ #\' stream))
                 (if (typep v 'hash-table)
                     (print-hash-table v stream)
                     (prin1 v stream)))
               ht)
      (pprint-indent :block 1 stream)
      (pprint-newline :mandatory stream)
      (princ "} " stream)))
  ht)

(defmacro with-keys ((&rest kv-pairs) ht &body body)
  "Like WITH-ACCESSORS but for pairs in hash-table HT."
  (once-only (ht)
    `(let (,@(mapcar #`(list (car %) `(gethash ,(second %) ,ht))
                     kv-pairs))
       ,@body)))

(defmacro dotable ((k v table &optional rez) &body body)
  "Like DOLIST but iterates over key-value pairs (K V) in anything, that can be
   viewed as a table (hash-table, alist, plist, object).
   Autodeclares variables named _ as ignored."
  (with-gensyms (pair)
    (once-only (table)
      (let ((_ (find "_" (list k v)
                     :test 'string=
                     :key 'symbol-name)))
        `(block nil
           (etypecase ,table
             (hash-table (maphash (lambda (,k ,v)
                                    ,(when _ `(declare (ignore ,_)))
                                    ,@body)
                                  ,table))

             (list (if (rutils.list:alistp ,table)
                       (dolist (,pair ,table)
                         (destructuring-bind (,k . ,v) ,pair
                           ,(when _ `(declare (ignore ,_)))
                           ,@body))
                       (error 'simple-type-error
                              :format-control "Can't iterate over proper list ~
                                               in DOTABLE: need an alist"))))
           ,rez)))))
