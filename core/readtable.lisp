;; For license see LICENSE

(in-package #:reasonable-utilities.readtable)

(declaim (optimize (speed 3) (space 1) (debug 0)))


(eval-when (:compile-toplevel :load-toplevel :execute)

(defun |#{-reader| (stream char)
  "Literal syntax for hash-tables.
Examples:
CL-USER> #{:a 1 :b 2}
#<HASH-TABLE :TEST EQL :COUNT 2> holding 2 key/value pairs: ((:a . 1) (:b . 2))
CL-USER> #{equalp \"a\" 1 \"b\" 2}
#<HASH-TABLE :TEST EQUALP :COUNT 2> holding 2 key/value pairs: ((\"a\" . 1) ...)
"
  (declare (ignore char))
  (let* ((sexp (read-delimited-list #\} stream t))
         (test (when (oddp (length sexp))
                 (car sexp)))
         (kv-pairs (if test (cdr sexp) sexp)))
    `(rutils.hash-table:hash-table-from-plist '(,@kv-pairs) ',test)))

(defun |#`-reader| (stream char arg)
  "Literal syntax for zero/one/two argument lambdas.
Use % as the function's argument, %% as the second.
Examples:
- #`(+ 2 %) => (lambda (&optional x y) (+ 2 x))
- #`((1+ %) (print %)) => (lambda (&optional x) (1+ x) (print x))
- #`(+ 1 2) => (lambda (&optional x y) (+ 1 2))
- #`(+ % %%) => (lambda (&optional x y) (+ x y))
"
  (declare (ignore char arg))
  (let ((sexp (read stream t nil t))
        (x (gensym "X"))
        (y (gensym "Y")))
    `(lambda (&optional ,x ,y)
       (declare (ignorable ,x)
                (ignorable ,y))
       ,@(subst y '%%
                (subst x '%
                       (if (listp (car sexp))
                           sexp
                           (list sexp)))))))

(defun |#/-reader| (stream char arg)
  "Literal syntax for raw strings (which don't need escapin of control chars).
Example:
CL-USER> #/This is a \"test\" string/#
\"This is a \"test\" string\"
; here \" are actually unescaped, but you can't write it in docstring
"
  (declare (ignore char arg))
  (with-output-to-string (str)
    (loop :for char := (read-char stream) :do
         (if (and (char= #\/ char)
                  (char= #\# (peek-char nil stream)))
             (progn (read-char stream)
                    (loop-finish))
             (write-char char str)))))


(defreadtable rutils-readtable
    (:merge :standard)
  (:macro-char #\} (get-macro-character #\) nil))
  (:dispatch-macro-char #\# #\{ #'|#{-reader|)
  (:dispatch-macro-char #\# #\` #'|#`-reader|)
  (:dispatch-macro-char #\# #\/ #'|#/-reader|))
)

(in-readtable rutils-readtable)

(defparameter *print-literally* nil
  "Turn on overriding methods for literal printing of those types of objects,
that have special literal syntax.
Similar to *PRINT-READABLY*.")

(defmethod print-object :around ((obj hash-table) stream)
  (if *print-literally*
      (format stream "{~@[~a ~]~a}~%"
              (unless (eq (hash-table-test obj) 'eql)
                (hash-table-test obj))
              (with-output-to-string (out)
                (maphash (lambda (k v)
                           (terpri out)
                           (prin1 k out)
                           (princ " " out)
                           (prin1 v out))
                         obj)))
      (call-next-method)))

(defun println (obj &optional (stream *standard-output*))
  "Print OBJ readably to STREAM (default: *standard-output*) followed by Newline."
  (let ((*print-literally* t))
    (print obj stream)
    (terpri stream)))
