;;; see LICENSE file for permissions

(cl:in-package #:reasonable-utilities.sequence)
(named-readtables:in-readtable rutils-readtable)

(proclaim '(optimize speed))


(defun split-sequence (delimiter seq
                       &key (count nil) (remove-empty-subseqs nil)
                            (from-end nil) (include-delimiter nil)
                            (start 0) (end nil) (key nil key-supplied)
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
                           remove-empty-subseqs)  ; empty subseq we don't want
              :if (and count (>= nr-elts count))
              ;; we can't take any more. Return now.
              :return (values (nreverse subseqs) right)
              :else
              :collect (subseq seq (if include-delimiter left (1+ left)) right)
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
                         remove-empty-subseqs)  ; empty subseq we don't want
            :if (and count (>= nr-elts count))
            ;; we can't take any more. Return now.
            :return (values subseqs left)
            :else
            :collect (subseq seq left right) :into subseqs
            :and sum 1 :into nr-elts
            :until (>= right end)
            :finally (return (values subseqs right))))))

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
                           remove-empty-subseqs)  ; empty subseq we don't want
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
                           remove-empty-subseqs)  ; empty subseq we don't want
              :if (and count (>= nr-elts count))
              ;; we can't take any more. Return now.
              :return (values subseqs left)
              :else
              :collect (subseq seq left right) :into subseqs
              :and sum 1 :into nr-elts
              :until (>= right end)
              :finally (return (values subseqs right))))))

(defun split-sequence-if-not (predicate seq
                              &key (count nil) (remove-empty-subseqs nil)
                                   (from-end nil) (start 0) (end nil)
                                   (key nil key-supplied))
  "Return a list of subsequences in SEQ delimited by items, satisfying
\(complement PREDICATE).

If REMOVE-EMPTY-SUBSEQS is NIL, empty subsequences will be
included in the result; otherwise they will be discarded. All other
keywords work analogously to those for SUBSTITUTE.
In particular, the behavior of FROM-END is possibly different
from other versions of this function; FROM-END values of NIL
and T are equivalent unless COUNT is supplied.  The second return
value is an index suitable as an argument to SUBSEQ into the
sequence indicating where processing stopped."
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
                           remove-empty-subseqs)  ; empty subseq we don't want
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
                         remove-empty-subseqs)  ; empty subseq we don't want
            :if (and count (>= nr-elts count))
            ;; we can't take any more. Return now.
            :return (values subseqs left)
            :else
            :collect (subseq seq left right) :into subseqs
            :and sum 1 :into nr-elts
            :until (>= right end)
            :finally (return (values subseqs right))))))

(defun partition-with (key-sequence sequence
                       &key (ordering #'less) (test #'eql) (key nil key-p)
                            (result-type 'list) keys-sorted)
  "Partition a SEQUENCE into a sequence of sequences, each one related
by TEST to one key in KEY-SEQUENCE (which may be already sorted: KEYS-SORTED).

Returns a sorted KEY-SEQUENCE as a 2nd value.

Return values are coerced to RESULT-TYPE, that should be a sequence subtype
\(default is LIST).

ORDERING is used for sorting both SEQUENCE and KEY-SEQUENCE.
Accepts KEY."
  (let* ((seq-s (apply #'sort (copy-seq sequence) ordering
                       (when key-p (list :key key))))
         (key-s (if keys-sorted key-sequence
                    (sort (copy-seq key-sequence) ordering)))
         (key-rez (copy-seq key-s))
         (n (length key-s))
         (rez (make-list n)))
    (iter (:with i := 0)
          (:while (and seq-s key-s))
          (:for elt-k := (funcall (if key-p key #'identity) (first seq-s)))
          (:for k := (car key-s))
          (cond ((funcall test elt-k k) (push (pop seq-s) (elt rez i)))
                ((funcall ordering k elt-k) (pop key-s)
                                            (incf i))
                (t (pop seq-s))))
    (values (map result-type
                 #`(coerce @ result-type)
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

CL-USER> (doindex (i e '(a b c))
           (format T \"~&~S ~S\" i e))
1 a
2 b
3 c
"
  (let ((sequence-var (gensym "SEQUENCE")))
    `(let ((,sequence-var ,sequence))
       (dotimes (,index-var (length ,sequence-var) ,result-form)
         (let ((,elt-var (elt ,sequence-var ,index-var)))
           ,@body)))))


(eval-always
  (pushnew :split-sequence *features*))
