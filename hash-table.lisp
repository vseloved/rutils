;;; RUTILS hash-table handling
;;; see LICENSE file for permissions

(in-package "REASONABLE-UTILITIES.HASH-TABLE")


;; literal syntax

(defun |#{-reader| (stream char arg)
  "Reader syntax for hash-tables. Examples:
- #{:a 1 :b 2} => #<HASH-TABLE :TEST EQL :COUNT 2>
                  It holds 2 key/value pairs: (:a 1) (:b 2)
- #{equalp \"a\" 1 \"b\" 2} => #<HASH-TABLE :TEST EQUALP :COUNT 2>
                               It holds 2 key/value pairs: (\"a\" 1) (\"b\" 2)"
  (declare (ignore char arg))
  (let* ((sexp (read-delimited-list #\} stream t))
         (test (when (oddp (length sexp))
                 (car sexp)))
         (kv-pairs (if test (cdr sexp) sexp)))
    `(hash-table-from-list (list ,@kv-pairs) ,test)))

(defmethod enable-literal-syntax ((which (eql :hash-table)))
  (set-dispatch-macro-character #\# #\{ #'|#{-reader|)
  (set-macro-character #\} (get-macro-character #\) nil)))

(defmethod disable-literal-syntax ((which (eql :hash-table)))
  (set-dispatch-macro-character #\# #\{ (make-reader-error-fun #\{))
  (set-macro-character #\} nil))

(defmethod to-string ((obj hash-table) &optional stream)
  (format stream "#{~@[~a ;~]~a}~%"
          (unless (eq (hash-table-test obj) 'eql)
            (hash-table-test obj))
          (with-output-to-string (out)
            (maphash (lambda (k v)
                       (format out "~%~a ~a" k v))
                     obj))))

;; copy

(defun copy-hash-table (ht &key key test size
                                rehash-size rehash-threshold)
  "Returns a copy of hash table <_:arg ht />, with the same keys and values.
The copy has the same properties as the original, unless overridden
by the keyword arguments.

Before each of the original values is set into the new hash-table,
<_:arg key /> is invoked on the value. As <_:arg key /> defaults to
<_:fun identity />, a shallow copy is returned by default"
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


;; sequencing

(proclaim '(inline
            hash-table-keys hash-table-values hash-table-vals 
            maphash-keys maphash-values maphash-vals))

(defun hash-table-keys (ht)
  "Return a list of keys of hash-table <_:arg ht />"
  (loop for k being the hash-keys of ht
     collect k))

(defun maphash-keys (function ht)
  "Like <_:fun maphash />, but calls <_:arg function />
with each key in the hash table <_:arg ht />"
  (maphash (lambda (k v)
             (declare (ignore v))
             (funcall function k))
           ht))

(eval-always
  (defun hash-table-values (ht)
    "Return a list of values of hash-table <_:arg ht />"
    (loop for v being the hash-values of ht
       collect v))

  (defun maphash-values (function ht)
    "Like <_:fun maphash />, but calls <_:arg function />
with each value in the hash table <_:arg ht />"
    (maphash (lambda (k v)
               (declare (ignore k))
               (funcall function v))
             ht)))

(abbrev hash-table-vals hash-table-values)
(abbrev maphash-vals maphash-values)

(defun hash-table-from-list (lst &optional test)
  "Returns a hash-table containing the keys and values,
alternating in <_:arg lst />. Unless hash-table <_:arg test />
is provided, it will be '<_:fun eql />"
  (loop
     :with ht = (make-hash-table :test (or test 'eql))
     :for (k v) :on lst :by #'cddr
     :do (setf (gethash k ht) v)
     :finally (return ht)))

(defun hash-table-to-list (ht)
  "Returns a list containing the keys and values of hash-table <_:arg ht />"
  (with-hash-table-iterator (gen-fn ht)
    (loop
       :for (valid key val) = (multiple-value-list (gen-fn))
       :unless valid :do (return rez)
       :nconc (list key val) :into rez)))

(defun hash-table-from-alist (alst &optional test)
  "Returns a hash-table containing the keys and values,
alternating in <_:arg alst />. Unless hash-table <_:arg test />
is provided, it will be '<_:fun eql />"
  (loop
     :with ht = (make-hash-table :test (or test 'eql))
     :for (k . v) :in alst
     :do (setf (gethash k ht) v)
     :finally (return ht)))

(defun hash-table-to-alist (ht)
  "Returns an alist containing the keys and values of hash-table <_:arg ht />"
  (with-hash-table-iterator (gen-fn ht)
    (loop
       :for (valid key val) = (multiple-value-list (gen-fn))
       :unless valid :do (return rez)
       :collect (cons key val) :into rez)))

;;; end