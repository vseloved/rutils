;;; see LICENSE file for permissions

(in-package #:rutils.generic)
(named-readtables:in-readtable rutils-readtable)
(eval-when (:compile-toplevel)
  (declaim #.+default-opts+))

(declaim (inline copy smart-slot-value smart-set-slot-value))

(defmacro adding-smart-slot-methods (obj slot expr)
  (with-gensyms (err class name alt args val)
    `(handler-case ,expr
       (simple-error (,err)
         (let ((,class (class-of ,obj))
               (,name (symbol-name ,slot)))
           (dolist (,alt (c2mop:class-slots ,class)
                         (error ,err))
             (let ((,alt (c2mop:slot-definition-name ,alt)))
               (when (string= ,name (symbol-name ,alt))
                 (add-method (ensure-generic-function 'smart-slot-value)
                             (make-instance
                              'standard-method
                              :specializers
                              (list ,class
                                    (c2mop:intern-eql-specializer ,slot))
                              :lambda-list '(,obj ,slot)
                              :function
                              (lambda (,args _)
                                (declare (ignorable _))
                                (slot-value (first ,args) ,alt))))
                 (add-method (ensure-generic-function 'smart-set-slot-value)
                             (make-instance
                              'standard-method
                              :specializers
                              (list ,class
                                    (c2mop:intern-eql-specializer ,slot)
                                    (find-class 't))
                              :lambda-list '(,obj ,slot ,val)
                              :function
                              (lambda (,args _)
                                (declare (ignorable _))
                                (:= (slot-value (first ,args) ,alt)
                                    (third ,args)))))
                 (let ((,slot ,alt))
                   (return ,expr))))))))))

(defgeneric smart-slot-value (obj slot)
  (:documentation
   "Similar to SLOT-VALUE but tries to find slot definitions regardless
    of the package.")
  (:method (obj slot)
    (adding-smart-slot-methods obj slot (slot-value obj slot))))

(defgeneric smart-set-slot-value (obj slot val)
  (:documentation
   "Similar to (SETF SLOT-VALUE) but tries to find slot definitions regardless
    of the package.")
  (:method (obj slot val)
    (adding-smart-slot-methods obj slot (:= (slot-value obj slot) val))))

(defsetf smart-slot-value smart-set-slot-value)


;;; generic element access protocol

(define-condition generic-elt-error ()
  ((obj :accessor generic-elt-error-obj :initarg :obj)
   (key :accessor generic-elt-error-key :initarg :key)))

(defmethod print-object ((err generic-elt-error) stream)
  (format stream
          "Generic element access error: object ~A can't be accessed by key: ~A"
          (slot-value err 'obj) (slot-value err 'key)))

(defgeneric generic-elt (obj key &rest keys)
  (:documentation
   "Generic element access in OBJ by KEY.
    Supports chaining with KEYS.")
  (:method :around (obj key &rest keys)
    (reduce #'generic-elt keys :initial-value (call-next-method obj key)))
  (:method (obj key &rest keys)
    (declare (ignore keys))
    (error 'generic-elt-error :obj obj :key key)))

(defmethod generic-elt ((obj list) key &rest keys)
  (declare (ignore keys))
  (when (minusp key) (setf key (- (length obj) key)))
  (nth key obj))

(defmethod generic-elt ((obj vector) key &rest keys)
  (declare (ignore keys))
  (when (minusp key) (setf key (- (length obj) key)))
  (aref obj key))

(defmethod generic-elt ((obj array) (key list) &rest keys)
  (declare (ignore keys))
  (apply 'aref obj key))

(defmethod generic-elt ((obj sequence) key &rest keys)
  (declare (ignore keys))
  (when (minusp key) (setf key (- (length obj) key)))
  (elt obj key))

(defmethod generic-elt ((obj hash-table) key &rest keys)
  (declare (ignore keys))
  (get# key obj))

(defmethod generic-elt ((obj structure-object) key &rest keys)
  (declare (ignore keys))
  (smart-slot-value obj key))

(defmethod generic-elt ((obj standard-object) key &rest keys)
  (declare (ignore keys))
  (smart-slot-value obj key))

(defmethod generic-elt ((obj (eql nil)) key &rest keys)
  (declare (ignore key keys))
  (error "Can't access NIL with generic-elt!"))

(defgeneric generic-setf (obj key &rest keys-and-val)
  (:documentation
   "Generic element access in OBJ by KEY.
    Supports chaining with KEYS.")
  (:method :around (obj key &rest keys-and-val)
    (if (single keys-and-val)
        (call-next-method)
        (multiple-value-bind (prev-keys kv) (butlast2 keys-and-val 2)
          (apply #'generic-setf
                 (apply #'generic-elt obj key prev-keys)
                 kv)))))

(defmethod generic-setf ((obj (eql nil)) key &rest keys)
  (declare (ignore key keys))
  (error "Can't access NIL with generic-setf!"))

(defmethod generic-setf ((obj list) key &rest keys-and-val)
  (setf (nth key obj) (atomize keys-and-val)))

(defmethod generic-setf ((obj vector) key &rest keys-and-val)
  (setf (aref obj key) (atomize keys-and-val)))

(defmethod generic-setf ((obj sequence) key &rest keys-and-val)
  (setf (elt obj key) (atomize keys-and-val)))

(defmethod generic-setf ((obj hash-table) key &rest keys-and-val)
  (set# key obj (atomize keys-and-val)))

(defmethod generic-setf ((obj structure-object) key &rest keys-and-val)
  (setf (smart-slot-value obj key) (atomize keys-and-val)))

(defmethod generic-setf ((obj standard-object) key &rest keys-and-val)
  (setf (smart-slot-value obj key) (atomize keys-and-val)))

(defsetf generic-elt generic-setf)


;;; generic copy

(defgeneric copy (obj &rest kvs)
  (:documentation
   "Create a shallow copy of an object.
    If KVS are specified, they may be used to update the relevant parts
    (like slots of an object, keys in a hash-table
     or indexed elements of a sequence).")
  (:method ((obj list) &rest kvs)
    (let ((rez (copy-list obj)))
      (loop :for (k v) :on kvs :by #'cddr :do
        (:= (? rez k) v))
      rez))
  (:method ((obj sequence) &rest kvs)
    (let ((rez (copy-seq obj)))
      (loop :for (k v) :on kvs :by #'cddr :do
        (:= (? rez k) v))
      rez))
  (:method ((obj hash-table) &rest kvs)
    (let ((rez (copy-hash-table obj)))
      (loop :for (k v) :on kvs :by #'cddr :do
        (:= (? rez k) v))
      rez))
  (:method ((obj structure-object) &rest kvs)
    (let ((rez (copy-structure obj)))
      (loop :for (k v) :on kvs :by #'cddr :do
        (:= (? rez (mksym k)) v))
      rez)))


;;; generic count

(defgeneric tally (obj)
  (:documentation
   "Return the count of items in a collection or other compound object.")
  (:method ((obj sequence))
    (length obj))
  (:method ((obj hash-table))
    (hash-table-count obj)))

