;; For license see LICENSE

(in-package #:rutils.readtable)
(declaim #.+default-opts+)


(eval-when (:compile-toplevel :load-toplevel :execute)

(defun |#v-reader| (stream char arg)
  "Literal syntax for vectors.
   Unlike #() evaluates its contents before vector creation

   Examples:

      CL-USER> #v(1 2 3)
      #(1 2 3)

      CL-USER> #v((+ 1 2))
      #(3)
  "
  (declare (ignore char arg))
  (read-char stream)
  (let* ((vals (read-delimited-list #\) stream t)))
    `(make-array ,(length vals) :initial-contents (list ,@vals))))

(defun |#h-reader| (stream char arg)
  "Literal syntax for hash-tables.

   Examples:

      CL-USER> #h(:a 1 :b 2)
      #<HASH-TABLE :TEST EQL :COUNT 2>
      ;; holding 2 key/value pairs: ((:a . 1) (:b . 2))

      CL-USER> #h(equalp \"a\" 1 \"b\" 2)
      #<HASH-TABLE :TEST EQUALP :COUNT 2>
      ;; holding 2 key/value pairs: ((\"a\" . 1) ...)
  "
  (declare (ignore char arg))
  (read-char stream)
  (let* ((sexp (read-delimited-list #\) stream t))
         (test (when (oddp (length sexp))
                 (car sexp)))
         (kvs (if test (cdr sexp) sexp))
         (ht (gensym)))
    `(let ((,ht (make-hash-table :test ',(or test 'eql))))
       ,@(loop :for tail :on kvs :by #'cddr :while kvs
            :collect `(setf (gethash ,(car tail) ,ht) ,(cadr tail)))
       ,ht)))

(defun |#{-reader| (stream char arg)
  "Literal syntax for fixed-size hash-tables.

   Examples:

      CL-USER> #{:a 1 :b 2}
      #<HASH-TABLE :TEST EQL :COUNT 2>
      ;; holding 2 key/value pairs: ((:a . 1) (:b . 2))

      CL-USER> #{equalp \"a\" 1 \"b\" 2}
      #<HASH-TABLE :TEST EQUALP :COUNT 2>
      ;; holding 2 key/value pairs: ((\"a\" . 1) ...)
  "
  (declare (ignore char arg))
  (let* ((sexp (read-delimited-list #\} stream t))
         (test (when (oddp (length sexp))
                 (car sexp)))
         (kvs (if test (cdr sexp) sexp))
         (ht (gensym)))
    `(let ((,ht (make-hash-table :test ',(or test 'eql)
                                 :size ,(/ (length kvs) 2))))
       ,@(loop :for tail :on kvs :by #'cddr :while kvs
            :collect `(setf (gethash ,(car tail) ,ht) ,(cadr tail)))
       ,ht)))

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
  `(trivial-positional-lambda ,@(read stream t nil)))

(defmacro trivial-positional-lambda (&environment env &body body)
  (let ((x (gensym "X"))
        (y (gensym "Y")))
    `(lambda (&optional ,x ,y)
       (declare (ignorable ,x)
                (ignorable ,y))
       #-sbcl
       ,@(subst y '%%
                (subst x '%
                       (if (listp (car body)) body (list sexp))))
       #+sbcl
       ,(sb-walker:walk-form
         body env
         (lambda (subform context env)
           (declare (ignore context env))
           (case subform
             (%  x)
             (%% y)
             (t subform)))))))

(defun |#/-reader| (stream char arg)
  "Literal syntax for raw strings (which don't need escapin of control chars).

   Example:

       CL-USER> #/This is a \"test\" string/#
       \"This is a \\\"test\\\" string\"
       ;; here \" are actually unescaped, but you can't write it in docstring :)
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
  (:macro-char #\} (get-macro-character #\)))
  (:dispatch-macro-char #\# #\v #'|#v-reader|)
  (:dispatch-macro-char #\# #\h #'|#h-reader|)
  (:dispatch-macro-char #\# #\{ #'|#{-reader|)
  (:dispatch-macro-char #\# #\` #'|#`-reader|)
  (:dispatch-macro-char #\# #\/ #'|#/-reader|))

)
