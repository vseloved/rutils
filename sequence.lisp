;;; RUTILS sequence handling
;;; see LICENSE file for permissions

;;; see split-sequence.txt for examples of it's usage

(in-package "REASONABLE-UTILITIES.SEQUENCE")

(locally-enable-literal-syntax :sharp-backq)


(defun split-sequence (delimiter seq
                       &key (count nil) (remove-empty-subseqs nil)
                            (from-end nil) (include-delimiter nil)
                            (start 0) (end nil) (key nil key-supplied)
                            (test nil test-supplied)
                            (test-not nil test-not-supplied))
  "Return a list of subsequences in <_:arg seq /> delimited by
<_:arg delimiter />.

If <_:arg remove-empty-subseqs /> is NIL, empty subsequences will
be included in the result; otherwise they will be discarded.
All other keywords work analogously to those for <_:fun substitute />.
In particular, the behaviour of <_:arg from-end /> is possibly different
from other versions of this function; <_:arg from-end /> values of NIL
and T are equivalent unless <_:arg count /> is supplied. The second return
value is an index suitable as an argument to <_:fun subseq /> into the
sequence indicating where processing stopped"
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
              :for :left := (max (or (apply #'position delimiter seq 
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
  "Return a list of subsequences in <_:arg seq /> delimited by items, ~
satisfying <_:arg predicate />.

If <_:arg remove-empty-subseqs /> is NIL, empty subsequences will be ~
included in the result; otherwise they will be discarded.  All other ~
keywords work analogously to those for <_:fun substitute />.  In ~
particular, the behaviour of <_:arg from-end /> is possibly different ~
from other versions of this function; <_:arg from-end /> values of NIL ~
and T are equivalent unless <_:arg count /> is supplied.  The second return ~
value is an index suitable as an argument to <_:fun subseq /> into the ~
sequence indicating where processing stopped"
  (let ((len (length seq))
        (other-keys (when key-supplied 
                      (list :key key))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position-if predicate seq 
                                         :end right
                                         :from-end t
                                         other-keys)
                                  -1)
                              (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs)  ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; we can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position-if predicate seq 
                                        :start left
                                        other-keys)
                                 len)
                             end)
            unless (and (= right left) 
                        remove-empty-subseqs)  ; empty subseq we don't want
            if (and count (>= nr-elts count))
            ;; we can't take any more. Return now.
            return (values subseqs left)
            else
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))

(defun split-sequence-if-not (predicate seq
                              &key (count nil) (remove-empty-subseqs nil)
                                   (from-end nil) (start 0) (end nil)
                                   (key nil key-supplied))
  "Return a list of subsequences in <_:arg seq /> delimited by items, ~
satisfying (<_:fun complement /> predicate).

If <_:arg remove-empty-subseqs /> is NIL, empty subsequences will be ~
included in the result; otherwise they will be discarded. All other ~
keywords work analogously to those for <_:fun substitute />.
In particular, the behaviour of <_:arg from-end /> is possibly different ~
from other versions of this function; <_:arg from-end /> values of NIL ~
and T are equivalent unless <_:arg count /> is supplied.  The second return ~
value is an index suitable as an argument to <_:fun subseq /> into the ~
sequence indicating where processing stopped"
  (let ((len (length seq))
        (other-keys (when key-supplied 
                      (list :key key))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position-if-not predicate seq 
                                         :end right
                                         :from-end t
                                         other-keys)
                                  -1)
                              (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs)  ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; we can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position-if-not predicate seq 
                                        :start left
                                        other-keys)
                                 len)
                             end)
            unless (and (= right left) 
                        remove-empty-subseqs)  ; empty subseq we don't want
            if (and count (>= nr-elts count))
            ;; we can't take any more. Return now.
            return (values subseqs left)
            else
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))


(defun partition-with (key-sequence sequence
                       &key (ordering #'less) (test #'eql) (key nil key-p)
                            (result-type 'list) keys-sorted)
  "Partition a <_:arg sequence /> into a sequence of sequences, ~
each one related by <_:arg test /> to one key in <_:arg key-sequence /> ~
\(which may be already sorted: <_:arg keys-sorted />).

Returns a sorted <_:arg key-sequence /> as a 2nd value.

Return values are coerced to <_:arg result-type />, that should be ~
a sequence type (default is <_:class list />). ~

<_:arg Ordering /> is used for sorting both <_:arg sequence /> and
<_:arg key-sequence />. Accepts <_:arg key />"
  (let* ((seq-s (apply #'sort (copy-seq sequence) ordering
                       (when key-p (list :key key))))
         (key-s (if keys-sorted key-sequence
                    (sort (copy-seq key-sequence) ordering)))
         (key-rez (copy-seq key-s))
         (n (length key-s)))
    (iter (:with rez := (make-list n))
          (:with i := 0)
          (:while (and seq-s key-s))
          (:for elt-k := (funcall (if key-p key #'identity) (first seq-s)))
          (:for k := (car key-s))
          (cond ((funcall test elt-k k) (push (pop seq-s) (elt rez i)))
                ((funcall ordering k elt-k) (pop key-s)
                                            (incf i))
                (t (pop seq-s)))
          (:finally (return
                      (values (map result-type
                                   #`(if (eq result-type 'list) _
                                         (coerce _ result-type))
                                   rez)
                              key-rez))))))


(declaim (inline remove/swapped-arguments))
(defun remove/swapped-arguments (sequence item &rest keyword-arguments)
  (apply #'remove item sequence keyword-arguments))

(define-modify-macro removef (item &rest remove-keywords)
  remove/swapped-arguments
  "Modify-macro for <_:fun remove />. Sets place designated by the first ~
argument to the result of calling <_:fun remove /> with <_:arg item />, place, ~
and the <_:arg remove-keywords />")

(declaim (inline delete/swapped-arguments))
(defun delete/swapped-arguments (sequence item &rest keyword-arguments)
  (apply #'delete item sequence keyword-arguments))

(define-modify-macro deletef (item &rest remove-keywords)
  delete/swapped-arguments
  "Modify-macro for <_:fun delete />. Sets place designated by the first ~
argument to the result of calling <_:fun delete /> with <_:arg item />, place, ~
and the <_:arg remove-keywords />")


(defun shuffle-sequence (seq)
  "Return a sequence of the same type as <_:arg seq /> with elements in ~
random order"
  (let ((len (length seq)))
    (iter (:for size :downfrom len)
          (:with pos-lst := (map-range #'identity 0 (1- len)))
          (:for item :in-sequence seq)
          (:for pos := (nth (random size) pos-lst))
          (:collect item :result-type (type-of seq))
          (removef pos pos-lst))))

(defun is-or-in (item seq &key (test 'eql))
  "Test whether <_:arg item /> is (in a sense of <_:arg test />) ~
a <_:arg seq />uence or belongs to it (tested as well by <_:arg test />. ~
Nil <_:fun is-or-in /> any <_:arg seq />"
  (or (and (null item) (null seq))
      (funcall test item seq)
      (handler-case (find item seq :test test)
        (error () nil))))

(defmacro doindex
    ((index-var elt-var sequence &optional result-form) &body body)
  "Iterates over a sequence while keeping track of an index.

\(doindex (i e '(a b c))
  (format T \"~&~S ~S\" i e))
=>
1 a
2 b
3 c"
  (let ((sequence-var (gensym "SEQUENCE")))
    `(let ((,sequence-var ,sequence))
       (dotimes (,index-var (length ,sequence-var) ,result-form)
         (let ((,elt-var (elt ,sequence-var ,index-var)))
           ,@body)))))

#|
;; TODO: needs more work
(defun partition-by (fn sequence
                     &key (test #'eql) (key nil key-p) (result-type 'list))
  "Partition a <_:arg sequence /> into a sequence of sequences,
representing continuos blocks of constant value of <_:arg fn />.
Return values are coerced to <_:arg result-type />, that should be
a sequence type (default is 'list).
Accepts <_:arg test /> and <_:arg key />"
    (iter (iter:with rez := (list))
          (iter:with seq := (copy-seq sequence))
          (iter:while seq)
          (iter:with prev := nil)
          (iter:with i := -1)
          (iter:for cur := (pop seq))
          (iter:for key := (funcall (compose fn (if key-p key #'identity))
                                    cur))
          (when (or (= i -1)
                    (not (print (funcall test key prev))))
            (incf i))
          (if (nth i rez)
              (setf (nth i rez) (nconc (nth i rez) (list cur)))
              (setf rez (nconc rez (list (list cur)))))
          (iter:finally (return (if (eql result-type 'list) rez
                                    (coerce rez result-type))))))
|#

(eval-always
  (pushnew :split-sequence *features*))


;;; end