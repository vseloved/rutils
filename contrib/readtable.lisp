;; For license see LICENSE

(in-package #:reasonable-utilities.readtable)

(declaim (optimize (speed 3) (space 1) (debug 0)))


(eval-when (:compile-toplevel :load-toplevel :execute)

(defun |@-reader| (stream char)
  "Short slot and sequence elements access syntax.

   Examples:

       CL-USER> (defclass foo () ((bar :initform 42)))
       CL-USER> (defvar *foo* (make 'foo))
       CL-USER> @*foo*.bar
       42
       CL-USER> (defvar *baz* #(1 *foo*))
       CL-USER> @*baz*#1.bar
       42
  "
  (declare (ignore char))
  (let ((whole (symbol-name (read stream)))
        sep
        acc)
    (flet ((next-sep (str start)
             (position-if (lambda (x) (member x '(#\. #\#)))
                          str :start (1+ start))))
      (do* ((prev 0 (1+ next))
            (next (next-sep whole prev) (next-sep whole prev)))
          ((null next) (push (cons sep (subseq whole prev)) acc))
        (push (cons sep (subseq whole prev next))
              acc)
        (setf sep (char whole next))))
    (reversef acc)
    (let ((rez (intern (cdar acc))))
      (dolist (pair (rest acc))
        (ecase (car pair)
          (#\. (setf rez `(slot-value ,rez ',(intern (cdr pair)))))
          (#\# (setf rez `(elt ,rez ,(parse-integer (cdr pair)))))))
      rez)))

(defvar *funcall-memory* (make-hash-table :test 'equal))

(defun |#^-reader| (stream char arg)
  "Special syntax for auto-memoization of function calls.

   #^(do-stuff args) =>
   (or (get# '(do-stuff args) *funcall-memory*)
       (set# '(do-stuff args) *funcall-memory* (do-stuff args)))

   Is aware of FUNCALL and APPLY.

   Caution: *funcall-memory* will leak memory, so be carefull with it.
  "
  (declare (ignore char arg))
  (let ((form (read stream))
        (key (gensym)))
    `(let ((,key ,(case (car form)
                    (funcall `(list ,@(cdr form)))
                    (apply `(append (list ,@(butlast (cdr form)))
                                    ,(last1 form)))
                    (otherwise `(cons ',(car form)
                                      (list ,@(cdr form)))))))
       (or (get# ,key *funcall-memory*)
           (set# ,key *funcall-memory* ,form)))))


(defreadtable rutils-readtable
    (:merge :standard)
  (:macro-char #\@ #'|@-reader| t)
  (:macro-char #\} (get-macro-character #\)))
  (:dispatch-macro-char #\# #\{ #'|#{-reader|)
  (:dispatch-macro-char #\# #\` #'|#`-reader|)
  (:dispatch-macro-char #\# #\/ #'|#/-reader|)
  (:dispatch-macro-char #\# #\^ #'|#^-reader|)
)

(defreadtable rutils-rt
    (:merge rutils-readtable))

) ; end of eval-when