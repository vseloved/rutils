;;; RUTILS Clojure's SEQ protocol unstrict implementation
;;; see LICENSE file for permissions

;;; see seq.txt for rationale and examples

(in-package #:reasonable-utilities.seq)

(locally-enable-literal-syntax :sharp-backq)


(defclass seq ()
  ((ref :initarg :ref :reader seq-ref
        :documentation "reference to the underlying concrete data-structure")
   (pos :initarg :pos :initform 0
        :documentation
        "current postion in <_:slot ref />. used for copying by reference")
   (itr :initarg :itr :initform (error "ITR should be provided")
        :documentation
        "iterator closure, which returns 2 values: value and key (for simple ~
sequences key is number); when end is reached just should return nil ~
\(no errors)")
   (kv? :initarg :kv? :initform nil
        :documentation
        "wheather keys are meaningful for the collection (for hash-tables they ~
are)"))
  (:documentation
   "A wrapper around a concrete data-structure (sequence, hash-table, ~
genhash,...)"))

(declaim (inline seqp))
(defun seqp (x)
  "Test whether <_:arg x /> is a <_:class seq />"
  (typep x 'seq))

(defmethod mk ((to (eql 'seq)) smth &key)
  (if (seqp smth) smth (seq smth)))

(defgeneric seq (coll &optional start-pos)
  (:documentation
   "Construct an instance of <_:class seq /> from a concrete or abstract (seq) ~
data-structure <_:arg coll />, optionally starting from <_:arg start-pos />, ~
or from a starting position in <_:arg coll />, which for an instance of ~
<_:class seq /> can be arbitrary")
  (:method ((coll seq) &optional start-pos)
    (with-slots (ref pos itr kv?) coll
      (make-instance 'seq :ref ref :pos (or start-pos pos) :kv? kv? :itr itr)))
  (:method ((coll sequence) &optional start-pos)
    (make-instance 'seq :ref coll :pos (or start-pos 0)
                   :itr #`(handler-case
                              (values (elt coll _) _)
                            (error () nil))))
  (:method ((coll hash-table) &optional start-pos)
    (make-instance 'seq :ref coll :pos (or start-pos 0) :kv? t
                   :itr #`(with-hash-table-iterator (gen-fn coll)
                            (loop :repeat _
                               :do (multiple-value-bind (valid key val) (gen-fn)
                                     (unless valid (return))))
                            (multiple-value-bind (valid key val) (gen-fn)
                               (values val key)))))
  (:method ((coll hash-container) &optional start-pos)
    (make-instance 'seq :ref coll :pos (or start-pos 0)
                   :itr #`(with-generic-hash-table-iterator (gen-fn coll)
                            (loop :repeat _
                               :do (multiple-value-bind (valid key val) (gen-fn)
                                     (unless valid (return))))
                            (multiple-value-bind (valid key val) (gen-fn)
                               (values val key))))))

;; TODO: add genhash

(defgeneric into (type coll)
  (:documentation "Convert a <_:arg coll /> to <_:arg type />")
  (:method (type (coll seq))
    (into type (seq-ref coll)))
  (:method ((type seq) (coll seq))
    (into (seq-ref type) (seq-ref coll)))
  (:method (type (coll sequence))
    (coerce coll type))
  (:method ((type seq) (coll sequence))
    (into (seq-ref type) coll))
  (:method ((type (eql 'hash-table)) (coll sequence))
    (hash-table-from-list (coerce coll 'list)))
  (:method ((type (eql 'list)) (coll hash-table))
    (hash-table-to-list coll))
  (:method ((type (eql 'vector)) (coll hash-table))
    (with-hash-table-iterator (gen-fn coll)
      (loop :with rez := (make-array (* (hash-table-count coll) 2))
         :for i :from 0
         :for (valid key val) = (multiple-value-list (gen-fn))
         :unless valid :do (return rez)
         :do (setf (aref rez i) key
                   (aref rez (incf i)) val))))
  (:method ((type list) (coll sequence))
    (coerce coll 'list))
  (:method ((type vector) (coll sequence))
    (coerce coll 'vector))
  (:method ((type hash-table) (coll sequence))
    (hash-table-from-list (coerce coll 'list)))
  (:method ((type list) (coll hash-table))
    (hash-table-to-list coll))
  (:method ((type vector) (coll hash-table))
    (with-hash-table-iterator (gen-fn coll)
      (loop :with rez := (make-array (* (hash-table-count coll) 2))
         :for i :from 0
         :for (valid key val) = (multiple-value-list (gen-fn))
         :unless valid :do (return rez)
         :do (setf (aref rez i) key
                   (aref rez (incf i)) val)))))

(defgeneric head (coll)
  (:documentation "Return 1st element of a collection <_:arg coll />, ~
2nd value: it's key (for sequences key is the number)")
  (:method ((coll sequence))
    (values (first coll)
            0))
  (:method ((coll hash-table))
    (with-hash-table-iterator (gen-fn coll)
      (multiple-value-bind (valid key val) (gen-fn)
        (when valid (values val
                            key)))))
  (:method ((coll seq))
    (with-slots (pos itr) coll
      (funcall itr pos))))

(defgeneric tail (coll &optional n)
  (:documentation "Return (possibly, lazily) rest of the collection ~
<_:arg coll />, starting from <_: n />-th pos (default: 1)")
  (:method ((coll list) &optional (n 1))
    (nthcdr n coll))
  (:method ((coll sequence) &optional (n 1))
    (subseq coll n))
  (:method ((coll hash-table) &optional (n 1))
    (hash-table-from-list (nthcdr (* n 2) (hash-table-to-list coll))))
  (:method ((coll seq) &optional (n 1))
    (with-slots (pos itr) coll
      (let ((next-pos (+ pos n)))
        (seq coll next-pos)))))

(defgeneric next (coll)
  (:documentation "The analog of <_:fun pop />. Return the first ~
element of <_:arg coll /> and remove it from the top of <_:arg coll />. ~
For <_:class seq /> it's done in functional manner, while for concrete ~
types the removal will be destructive")
  (:method ((coll list))
    (values (pop coll)
            0))
  (:method ((coll vector))
    ;; terribly inefficient
    (multiple-value-prog1
        (values (elt coll 0)
                0)
      (loop :for i :from 0 :to (1- (length coll)) :do
         (setf (svref coll i) (svref coll (1+ i))))
      (ignore-errors (vector-pop coll))))
  (:method ((coll hash-table))
    (with-hash-table-iterator (gen-fn coll)
      (multiple-value-bind (valid key val) (gen-fn)
        (when valid
          (remhash key coll)
          (values val key)))))
  (:method ((coll seq))
    (multiple-value-prog1 (head coll)
      (incf (slot-value coll 'pos)))))

(declaim (inline elm))
(defun elm (coll n)
  "<_:arg N />-th element of collection <_:arg coll />"
  (head (tail coll n)))


(defmacro doseq ((var-form coll &optional result) &body body)
  "The analog of <_:fun dolist /> for any collection <_:arg coll />, ~
which can be put in a <_:class seq />.

<_:arg Var-form /> can be either <_:fun atom />, in which case this name ~
is bound to the value (1st), returned by <_:fun next />, or <_:fun list />: ~
the 1st symbol will be bound to the 1st value (value), returned by ~
<_:fun next />, and the 2nd -- to the 2nd (key).
If <_:arg result /> form is provided, it's value will be returned"
  (with-gensyms (gseq key val)
    `(let ((,gseq (seq ,coll)))
       (loop
          (multiple-value-bind (,val ,key) (next ,gseq)
            (multiple-value-bind ,(mklist var-form)
                (values ,val ,key)
              (if ,key
                  (progn ,@body)
                  (return ,result))))))))

(defmacro with-acc ((var-form acc-name coll &optional result-type) &body body)
  "Iterate over <_:arg coll /> with accumulation to list and conversion ~
into either <_:arg result-type /> or <_:fun type-of /> <_:arg coll />.

<_:arg Var-form /> can be either <_:fun atom />, in which case this name ~
is bound to the value (1st), returned by <_:fun next />, or <_:fun list />: ~
the 1st symbol will be bound to the 1st value (value), returned by ~
<_:fun next />, and the 2nd -- to the 2nd (key)."
  (with-gensyms (gseq)
    `(let ((,gseq (seq ,coll)))
       (into (or ,result-type (seq-ref ,gseq))
             (with-output-to-list (,acc-name)
               (doseq (,var-form ,gseq)
                 ,@body))))))


#+nil
#+iter
(defdriver-clause (:FOR var :SEQ coll)
  "Iterate over <_:arg coll /> as a <_:class seq />"
  (with-gensyms (gseq k v)
    `(progn (:with ,gseq := (seq ,coll))
            (:for ,var :next (multiple-value-bind (,v ,k) (next ,gseq)
                               (if ,k ,v (:terminate)))))))


;; some utilities, based on SEQ

(defun filter (fn coll)
  "Sequentially apply <_:arg fn /> to all elements of <_:arg coll /> ~
and return the collection of the same type, holding only non-null results"
  (let ((seq (mk 'seq coll)))
    (with-slots (kv?) seq
      (seq-ref (with-acc ((v k) acc seq)
                 (when-it (funcall fn v)
                   (when kv? (push k acc))
                   (push it acc)))))))

(defun take (coll n &optional (step 1))
  "From the <_:arg coll /> take upto first <_:arg n /> items as a collection ~
of the same type. If <_:arg step /> is provided, after each taken item skip ~
<_:arg step />-1 items"
  (declare (type (integer 1) step n))
  (loop :with acc
     :with seq := (mk 'seq coll) :with kv? := (slot-value seq 'kv?)
     :for i := 0 :then (incf i) :repeat n
     :do (multiple-value-bind (v k) (head seq)
           (if k (progn (when kv? (push k acc))
                        (push v acc)
                        (setf seq (tail seq step)))
               (loop-finish)))
     :finally (return (into coll (nreverse acc)))))

(defun interleave (seq &rest seqs)
  ""
  (apply #'mapcan (lambda (&rest args) args) seq seqs))

(eval-always
  (pushnew :seq *features*))


;;; end