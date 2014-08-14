;;; see LICENSE file for permissions

(cl:in-package #:rutilsx.generic)
(named-readtables:in-readtable rutils-readtable)
(declaim #.+default-opts+)


;;; Generic element access protocol

(eval-always

(defgeneric generic-elt (obj key &rest keys)
  (:documentation
   "Generic element access in OBJ by KEY.
    Supports chaining with KEYS.")
  (:method :around (obj key &rest keys)
    (reduce #'generic-elt keys :initial-value (call-next-method obj key))))

(defmethod generic-elt ((obj list) key &rest keys)
  (declare (ignore keys))
  (listcase obj
    (alist (assoc1 key obj :test 'equal))
    (dlist (nth (position key (car obj) :test 'equal) (cdr obj)))
    (t (nth key obj))))

(defmethod generic-elt ((obj vector) key &rest keys)
  (declare (ignore keys))
  (svref obj key))

(defmethod generic-elt ((obj sequence) key &rest keys)
  (declare (ignore keys))
  (elt obj key))

(defmethod generic-elt ((obj hash-table) key &rest keys)
  (declare (ignore keys))
  (get# key obj))

(abbr ~ generic-elt)

) ; end of eval-always


;;; Generic sequence iteration protocol

(defgeneric seq (seq &optional key)
  (:documentation
   "Return as multiple values current element in SEQ, it's key,
    and a function to access the next element."))

(defmethod seq ((seq list) &optional (key 0))
  (when seq
    (values (first seq)
            key
            (lambda () (seq (rest seq) (1+ key))))))

(defmethod seq ((seq vector) &optional (key 0))
  (when (< key (length seq))
    (values (elt seq key)
            key
            (lambda () (seq seq (1+ key))))))

(defmethod seq ((seq hash-table)
                &optional (gen-fn (with-hash-table-iterator (gen-fn seq)
                                    (lambda () (gen-fn)))))
  (mv-bind (valid key val) (funcall gen-fn)
    (when valid
      (values val
              key
              (lambda () (seq seq gen-fn))))))

(defmacro donext ((elt seq &optional result) &body body)
  (with-gensyms (val key next new-next)
    `(let ((,next (lambda () (seq ,seq))))
       (loop (mv-bind (,val ,key ,new-next) (funcall ,next)
               (declare (ignore ,key))
               (unless ,new-next (return ,result))
               (setf ,next ,new-next)
               (let ((,elt ,val))
                 ,@body))))))


;;; Generic table access and iteration protocol

(defgeneric keys (table)
  (:documentation
   "Return a list of all keys in a TABLE.
    Order is unspecified.")
  (:method ((table hash-table))
    (ht-keys table))
  (:method ((list list))
    (listcase list
      (alist (mapcar #'car list))
      (dlist (car list))
      (t (range 0 (length list))))))

(defgeneric vals (table)
  (:documentation
   "Return a list of all values in a TABLE.
    Order is unspecified.")
  (:method ((table hash-table))
    (ht-vals table))
  (:method ((list list))
    (listcase list
      (alist (mapcar #'cdr list))
      (dlist (cdr list))
      (t list))))

(defgeneric kvs (table &optional result-kind)
  (:documentation
   "Return a list of all key-value pairs in a TABLE in one the 3 kinds:

    - list of pairs (default)
    - alist
    - dlist

    Order is unspecified.")
  (:method ((table hash-table) &optional (result-kind 'pairs))
    (ecase result-kind
      (alist (ht->alist table))
      (dlist (cons (keys table) (vals table)))
      (pairs (ht->pairs table)))))

(defgeneric eq-test (table)
  (:documentation
   "Return an equality test predicate of the TABLE.")
  (:method ((table hash-table))
    (hash-table-test table))
  (:method ((list list))
    'equal))

(defgeneric maptab (fn table)
  (:documentation
   "Like MAPCAR but for a data structure that can be viewed as a table.")
  (:method (fn (table hash-table))
    (with-hash-table-iterator (gen-fn table)
      (let ((rez (make-hash-table :test (hash-table-test table))))
        (loop
           (mv-bind (valid key val) (gen-fn)
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
