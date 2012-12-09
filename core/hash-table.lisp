;;; see LICENSE file for permissions

(cl:in-package #:reasonable-utilities.hash-table)
(named-readtables:in-readtable rutils-readtable)

(declaim (optimize (speed 3) (space 1) (debug 0)))


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
              hts)
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
  (format stream "#{~@[~a ~]~a~%}~%"
          (unless (eq (hash-table-test ht) 'eql)
            (hash-table-test ht))
          (with-output-to-string (out)
            (maphash (lambda (k v)
                       (terpri out)
                       (when (listp k) (princ #\' out))
                       (prin1 k out)
                       (princ " " out)
                       (when (listp v) (princ #\' out))
                       (prin1 v out))
                     ht))))