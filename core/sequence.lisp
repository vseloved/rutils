;;; see LICENSE file for permissions

(cl:in-package #:rutils.sequence)
(named-readtables:in-readtable rutils-readtable)
(declaim #.+default-opts+)
(declaim (inline safe-sort map*))


(defun split-sequence (delimiter seq
                       &key (count nil) (remove-empty-subseqs nil)
                            (from-end nil) (start 0) (end nil)
                            (key nil key-supplied)
                            (test nil test-supplied)
                            (test-not nil test-not-supplied))
  "Return a list of subsequences in SEQ delimited by DELIMITER.

   If REMOVE-EMPTY-SUBSEQS is NIL, empty subsequences will
   be included in the result; otherwise they will be discarded.
   All other keywords work analogously to those for SUBSTITUTE.
   In particular, the behavior of FROM-END is possibly different
   from other versions of this function; FROM-END values of NIL
   and T are equivalent unless COUNT is supplied. The second return
   value is an index suitable as an argument to SUBSEQ into the
   sequence indicating where processing stopped."
  (when seq
    (let ((len (length seq))
          (other-keys (nconc (when test-supplied
                               (list :test test))
                             (when test-not-supplied
                               (list :test-not test-not))
                             (when key-supplied
                               (list :key key)))))
      (unless end (setq end len))
      (if from-end
          (loop :for right := end :then left
             :for left  := (max (or (apply #'position delimiter seq
                                           :end right
                                           :from-end t
                                           other-keys)
                                    -1)
                                (1- start))
             :unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
             :if (and count (>= nr-elts count))
             ;; we can't take any more. Return now.
             :return (values (nreverse subseqs) right)
             :else
             :collect (subseq seq (1+ left) right)
             :into subseqs
             :and sum 1 :into nr-elts
             :until (< left start)
             :finally (return (values (nreverse subseqs) (1+ left))))
          (loop :for left := start :then (+ right 1)
             :for right := (min (or (apply #'position delimiter seq
                                           :start left
                                           other-keys)
                                    len)
                                end)
             :unless (and (= right left)
                          remove-empty-subseqs) ; empty subseq we don't want
             :if (and count (>= nr-elts count))
             ;; we can't take any more. Return now.
             :return (values subseqs left)
             :else
             :collect (subseq seq left right) :into subseqs
             :and sum 1 :into nr-elts
             :until (>= right end)
             :finally (return (values subseqs right)))))))

(defun split-sequence-if (predicate seq
                          &key (count nil) (remove-empty-subseqs nil)
                               (from-end nil) (start 0) (end nil)
                               (key nil key-supplied))
  "Return a list of subsequences in SEQ delimited by items, satisfying PREDICATE.

   If REMOVE-EMPTY-SUBSEQS is NIL, empty subsequences will be included in the
   result; otherwise they will be discarded.  All other keywords work analogously
   to those for SUBSTITUTE.  In particular, the behavior of FROM-END is possibly
   different from other versions of this function; FROM-END values of NIL
   and T are equivalent unless COUNT is supplied.  The second return value is
   an index suitable as an argument to SUBSEQ into the sequence indicating where
   processing stopped."
  (when seq
    (let ((len (length seq))
          (other-keys (when key-supplied
                        (list :key key))))
      (unless end (setq end len))
      (if from-end
          (loop :for right := end :then left
             :for left := (max (or (apply #'position-if predicate seq
                                          :end right
                                          :from-end t
                                          other-keys)
                                   -1)
                               (1- start))
             :unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
             :if (and count (>= nr-elts count))
             ;; we can't take any more. Return now.
             :return (values (nreverse subseqs) right)
             :else
             :collect (subseq seq (1+ left) right) :into subseqs
             :and sum 1 :into nr-elts
             :until (< left start)
             :finally (return (values (nreverse subseqs) (1+ left))))
          (loop :for left := start :then (+ right 1)
             :for right := (min (or (apply #'position-if predicate seq
                                           :start left
                                           other-keys)
                                    len)
                                end)
             :unless (and (= right left)
                          remove-empty-subseqs) ; empty subseq we don't want
             :if (and count (>= nr-elts count))
             ;; we can't take any more. Return now.
             :return (values subseqs left)
             :else
             :collect (subseq seq left right) :into subseqs
             :and sum 1 :into nr-elts
             :until (>= right end)
             :finally (return (values subseqs right)))))))

(defun split-sequence-if-not (predicate seq
                              &key (count nil) (remove-empty-subseqs nil)
                                   (from-end nil) (start 0) (end nil)
                                   (key nil key-supplied))
  "Return a list of subsequences in SEQ delimited by items, satisfying
   (complement PREDICATE).

   If REMOVE-EMPTY-SUBSEQS is NIL, empty subsequences will be
   included in the result; otherwise they will be discarded. All other
   keywords work analogously to those for SUBSTITUTE.
   In particular, the behavior of FROM-END is possibly different
   from other versions of this function; FROM-END values of NIL
   and T are equivalent unless COUNT is supplied.  The second return
   value is an index suitable as an argument to SUBSEQ into the
   sequence indicating where processing stopped."
  (when seq
    (let ((len (length seq))
          (other-keys (when key-supplied
                        (list :key key))))
      (unless end (setq end len))
      (if from-end
          (loop :for right := end :then left
             :for left := (max (or (apply #'position-if-not predicate seq
                                          :end right
                                          :from-end t
                                          other-keys)
                                   -1)
                               (1- start))
             :unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
             :if (and count (>= nr-elts count))
             ;; we can't take any more. Return now.
             :return (values (nreverse subseqs) right)
             :else
             :collect (subseq seq (1+ left) right) :into subseqs
             :and sum 1 :into nr-elts
             :until (< left start)
             :finally (return (values (nreverse subseqs) (1+ left))))
          (loop :for left := start :then (+ right 1)
             :for right := (min (or (apply #'position-if-not predicate seq
                                           :start left
                                           other-keys)
                                    len)
                                end)
             :unless (and (= right left)
                          remove-empty-subseqs) ; empty subseq we don't want
             :if (and count (>= nr-elts count))
             ;; we can't take any more. Return now.
             :return (values subseqs left)
             :else
             :collect (subseq seq left right) :into subseqs
             :and sum 1 :into nr-elts
             :until (>= right end)
             :finally (return (values subseqs right)))))))

(defun partition-with (key-sequence sequence
                       &key (ordering #'less) (test #'eql) (key nil key-p)
                            (result-type 'list) keys-sorted)
  "Partition a SEQUENCE into a sequence of sequences, each one related
   by TEST to one key in KEY-SEQUENCE (which may be already sorted: KEYS-SORTED).

   Returns a sorted KEY-SEQUENCE as a 2nd value.

   Return values are coerced to RESULT-TYPE, that should be a sequence subtype
   (default is LIST).

   ORDERING is used for sorting both SEQUENCE and KEY-SEQUENCE.
   Accepts KEY."
  (let* ((seq-s (apply #'sort (copy-seq sequence) ordering
                       (when key-p (list :key key))))
         (key-s (if keys-sorted key-sequence
                    (sort (copy-seq key-sequence) ordering)))
         (key-rez (copy-seq key-s))
         (n (length key-s))
         (rez (make-list n))
         (i 0))
    (loop :while (and seq-s key-s)
       :for elt-k := (funcall (if key-p key #'identity) (first seq-s))
       :for k := (car key-s) :do
       (cond ((funcall test elt-k k)
              (push (pop seq-s) (elt rez i)))
             ((funcall ordering k elt-k)
              (pop key-s)
              (incf i))
             (t (pop seq-s))))
    (values (map result-type
                 #`(coerce % result-type)
                 rez)
            key-rez)))


(declaim (inline remove/swapped-arguments))
(defun remove/swapped-arguments (sequence item &rest keyword-arguments)
  (apply #'remove item sequence keyword-arguments))

(define-modify-macro removef (item &rest remove-keywords)
  remove/swapped-arguments
  "Modify-macro for REMOVE. Sets place designated by the first
   argument to the result of calling REMOVE with ITEM, place,
   and the REMOVE-KEYWORDS.")

(declaim (inline delete/swapped-arguments))
(defun delete/swapped-arguments (sequence item &rest keyword-arguments)
  (apply #'delete item sequence keyword-arguments))

(define-modify-macro deletef (item &rest remove-keywords)
  delete/swapped-arguments
  "Modify-macro for DELETE. Sets place designated by the first
   argument to the result of calling DELETE with ITEM, place,
   and the REMOVE-KEYWORDS.")


(defmacro doindex ((index-var elt-var sequence &optional result-form)
                   &body body)
  "Iterates over a sequence while keeping track of an index. A DO-style macro.

   Example usage:

       CL-USER> (doindex (i e '(a b c))
                  (format T \"~&~S ~S\" i e))
       1 a
       2 b
       3 c"
  (let ((sequence-var (gensym "SEQUENCE")))
    `(let ((,sequence-var (coerce ,sequence 'vector)))
       (dotimes (,index-var (length ,sequence-var) ,result-form)
         (let ((,elt-var (elt ,sequence-var ,index-var)))
           ,@body)))))

(defun shuffle (sequence &key (start 0) end)
  "Return a shuffled copy of SEQUENCE (in bounds of START and END)."
  (let* ((len (length sequence))
         (end (or end len))
         (indices (range 0 len))
         rez)
    (coerce (do ((i len (1- i)))
                ((zerop i) rez)
              (let ((idx (if (or (> i end) (<= i start))
                             (1- i)
                             (+ start (random (- i start))))))
                (push (elt sequence (elt indices idx)) rez)
                (if (zerop idx)
                    (setf indices (rest indices))
                    (rplacd (nthcdr (1- idx) indices)
                            (nthcdr (1+ idx) indices)))))
            (type-of sequence))))

(defun nshuffle (sequence &key (start 0) (end (length sequence)))
  "Shuffle SEQUENCE (in bounds of START and END) in-place."
  (loop :for i :from start :below end :do
     (rotatef (elt sequence i)
              (elt sequence (+ i (random (- end i))))))
  sequence)

(defun rotate (sequence &optional (n 1))
  "Returns a sequence of the same type as SEQUENCE, with the elements of
   SEQUENCE rotated by N: N elements are moved from the end of the sequence to
   the front if N is positive, and -N elements moved from the front to the end
   if N is negative. SEQUENCE must be a proper sequence. N must be an integer,
   defaulting to 1.

   If absolute value of N is greater then the length of the sequence, the results
   are identical to calling ROTATE with

   (* (signum n) (mod n (length sequence))).

   Note: the original sequence may be destructively altered,
   and result sequence may share structure with it."
  (if (plusp n)
      (rotate-tail-to-head sequence n)
      (if (minusp n)
          (rotate-head-to-tail sequence (- n))
          sequence)))

(defun rotate-tail-to-head (sequence n)
  (declare (type (integer 1) n))
  (if (listp sequence)
      (let ((m (mod n (length sequence))))
        (if (rest sequence)
            (let* ((tail (last sequence (+ m 1)))
                   (last (rest tail)))
              (void (rest tail))
              (nconc last sequence))
            sequence))
      (let* ((len (length sequence))
             (m (mod n len))
             (tail (subseq sequence (- len m))))
        (replace sequence sequence :start1 m :start2 0)
        (replace sequence tail)
        sequence)))

(defun rotate-head-to-tail (sequence n)
  (declare (type (integer 1) n))
  (if (listp sequence)
      (let ((m (mod (1- n) (length sequence))))
        (if (rest sequence)
            (let* ((headtail (nthcdr m sequence))
                   (tail (rest headtail)))
              (void (rest headtail))
              (nconc tail sequence))
            sequence))
      (let* ((len (length sequence))
             (m (mod n len))
             (head (subseq sequence 0 m)))
        (replace sequence sequence :start1 0 :start2 m)
        (replace sequence head :start1 (- len m))
        sequence)))

(defun emptyp (sequence)
  "Returns true if SEQUENCE is an empty sequence. Signals an error if SEQUENCE
   is not a sequence."
  (etypecase sequence
    (list (null sequence))
    (string (rutils.string:blankp sequence))
    (vector (and (vectorp sequence)
                 (equalp sequence #())))
    (sequence (zerop (length sequence)))))

(defun equal-lengths (&rest sequences)
  "Takes any number of sequences or integers in any order. Returns true iff
   the length of all the sequences and the integers are equal.
   Hint: there's a compiler macro that expands into more efficient code
   if the first argument is a literal integer."
  (declare (dynamic-extent sequences)
           (optimize speed))
  (unless (rest sequences)
    (error "You must call EQUAL-LENGTHS with at least two arguments"))
  (let* ((first (pop sequences))
         (current (if (integerp first)
                      first
                      (length first))))
    (declare (type array-index current))
    (dolist (el sequences)
      (if (integerp el)
          (unless (= el current)
            (return-from equal-lengths nil))
          (unless (equal-lengths el current)
            (return-from equal-lengths nil)))))
  t)

(define-compiler-macro equal-lengths (&whole form length &rest sequences)
  (if (zerop (length sequences))
      form
      (let ((optimizedp (integerp length)))
        (with-unique-names (tmp current)
          (declare (ignorable current))
          `(let (,tmp
                 ,@(unless optimizedp `((,current ,length))))
             ,@(unless optimizedp
                 `((unless (integerp ,current)
                     (setf ,current (length ,current)))))
             (and ,@(loop :for sequence :in sequences :collect
                       (let ((len (if optimizedp length current)))
                         `(if (integerp (setf ,tmp ,sequence))
                              (= ,tmp ,len)
                              (length= ,tmp ,len))))))))))

(defun length= (sequence length)
  "Return true if SEQUENCE's length equals LENGTH.
   Returns FALSE for circular lists.
   Signals an error if SEQUENCE is not a sequence."
  (declare (type array-index length)
           (inline length)
           (optimize speed))
  (etypecase sequence
    (null
     (zerop length))
    (cons
     (let ((n (1- length)))
       (unless (minusp n)
         (let ((tail (nthcdr n sequence)))
           (and tail
                (null (cdr tail)))))))
    (sequence
     (= length (length sequence)))))

(defun last-elt (sequence)
  "Returns the last element of SEQUENCE.
   Signals a type-error if SEQUENCE is not a proper sequence,
   or is an empty sequence."
  (block nil
    (typecase sequence
      (list (cond
              ((cdr sequence) (return (last1 sequence)))
              ((plusp (length sequence)) (return (car sequence)))))
      (sequence (let ((len (length sequence)))
                  (unless (zerop len)
                    (return (elt sequence (1- len)))))))
    (error 'type-error
           :datum sequence
           :expected-type '(and proper-sequence (not (satisfies emptyp))))))

(defun (setf last-elt) (object sequence)
  "Sets the last element of SEQUENCE.
   Signals a type-error if SEQUENCE is not a proper sequence,
   is an empty sequence."
  (block nil
    (typecase sequence
      (list (cond
              ((cdr sequence) (return (setf (last1 sequence) object)))
              ((plusp (length sequence)) (return (setf (car sequence) object)))))
      (sequence (let ((len (length sequence)))
                  (unless (zerop len)
                    (return (setf (elt sequence (1- len)) object))))))
    (error 'type-error
           :datum sequence
           :expected-type '(and proper-sequence (not (satisfies emptyp))))))

(defun safe-sort (sequence predicate &rest args &key key)
  "The destructuve nature of SORT triggers many obscure bugs.
   This function is a thin wrapper over SORT that enqures
   that an input SEQUENCE is copied."
  (declare (ignorable key))
  (apply 'sort (copy-seq sequence) predicate args))

(defun group (n seq)
  "Split SEQ into a list of sequences of the same type of length N
   (the last sequence may be shorter)."
  (declare (integer n))
  (when (zerop n)
    (error "Group length N shouldn't be zero."))
  (etypecase seq
    (list (labels ((rec (src &optional acc)
                     (let ((rest (nthcdr n src)))
                       (if (consp rest)
                           (rec rest (cons (subseq src 0 n) acc))
                           (nreverse (cons src acc))))))
            (when seq
              (rec seq))))
    (sequence
     (when (plusp (length seq))
       (do ((i 0 (+ i n))
            (len (length seq))
            (acc nil))
           ((>= (+ i n) len)
            (nreverse (push (subseq seq i) acc)))
         (push (subseq seq i (+ i n)) acc))))))

(defun keep (item sequence &rest args &key from-end test test-not start
                                        end count key)
  "The opposite of REMOVE."
  (declare (ignorable from-end start end count key))
  (let ((test (or test
                  (complement (or test-not #'eql)))))
    (apply 'remove item sequence :test test
           (loop :for (k v) :on args :by #'cddr
                 :unless (member k '(:test :test-not)) :nconc (list k v)))))

(defun sum (fn seq &rest seqs)
  "Sum the results of mapping FN to SEQ and other SEQS."
  (reduce '+ (apply 'map 'list fn seq seqs)))

(defun product (fn seq &rest seqs)
  "Product of the results of mapping FN to SEQ and other SEQS."
  (reduce '* (apply 'map 'list fn seq seqs)))

(defun map* (fn seq &rest seqs)
  "DWIM version of map that uses the type of first sequence (SEQ) for result."
  (apply 'map (type-of seq) fn seq seqs))


(eval-always (pushnew :split-sequence *features*))
