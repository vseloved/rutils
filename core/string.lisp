;; For license see LICENSE

(in-package #:rutils.string)
(named-readtables:in-readtable rutils-readtable)
(eval-when (:compile-toplevel)
  (declaim #.+default-opts+))


(declaim (inline white-char-p fmt strjoin blankp))


(defun strcat (&rest string-designators)
  "CONCATENATE all the strings in STRING-DESIGNATORS."
  (let ((*print-pretty* nil)
        (*print-circle* nil))
    (with-output-to-string (stream)
      (dolist (str string-designators)
        (when str
          (princ str stream))))))

(defun strjoin (delim strings)
  "Join STRINGS with DELIM."
  (format nil (format nil "~~{~~A~~^~A~~}" delim)
          strings))

(defun blankp (string)
  "Test whether a STRING is blank (empty)."
  (and (stringp string)
       (string= string "")))

(defun read-file (filename)
  "Read a whole file by FILENAME into a string."
  (with-open-file (in filename)
    (let* ((buf (make-array (file-length in) :element-type 'character
                            :adjustable t :fill-pointer t))
           (chars-read (read-sequence buf in)))
      (setf (fill-pointer buf) chars-read)
      buf)))

(defun white-char-p (char)
  "Is CHAR a whitespace character (newline chars are also considered white)."
  (member char '(#\Space #\Tab #\Return #\Linefeed #\Newline #\Page)))

(defun split-string (string)
  "Split STRING by WHITE-CHAR-P."
  (rutils.sequence:split-sequence-if #'white-char-p string))

(defun substr (string start &optional end)
  "Efficient substring of STRING from START to END (optional),
   where both can be negative, which means counting from the end."
  (let ((len (length string)))
    (subseq string
            (if (minusp start) (+ len start) start)
            (if (and end (minusp end)) (+ len end) end))))

;; (define-setf-expander substr (string start &optional end &environment env)
;;   "Like (SETF SUBSEQ), but for SUBSTR."
;;   (multiple-value-bind (dummies vals newval setter getter)
;;       (get-setf-expansion 'string env)
;;     (let (len
;;           (store (gensym)))
;;       (when (minusp start)
;;         (setf len (length string)
;;               start (+ len start)))
;;       (when (and end (minusp end))
;;         (setf end (+ (or len (length string)) end)))
;;       (values dummies
;;               vals
;;               `(,store)
;;               `(setf (subseq ,getter ,start ,end) ,store)
;;               `(substr ,getter ,start ,end)))))


(defun starts-with (prefix string &key (test 'string=) (start 0))
  "Test, whether STRING starts with PREFIX.
   If START is provided matches from this offset from the start."
  (if-it (mismatch prefix string :test test :start2 start)
         (= it (length prefix))
         t))

(defun ends-with (suffix string &key (test 'string=) -end)
  "Test, whether STRING ends with SUFFIX. Accepts TEST.
   If -END is provided matches from this offset from end."
  (if-it (mismatch suffix string :from-end t :test test
                                 :end2 (when -end (- (length string) -end)))
         (zerop it)
         t))

(deftype string-designator ()
  "A string designator type. It is either a string, a symbol, or a character."
  `(or symbol string character))

(defmacro dolines ((line src &optional result) &body body)
  "Iterate over each LINE in SRC (a stream or path to a file) as in DOLIST."
  (let ((in (gensym)))
    (once-only (src)
      `(if (streamp ,src)
           (loop :for ,line := (read-line ,src nil nil) :while ,line :do
             (progn ,@body)
                 :finally (return ,result))
           (with-open-file (,in ,src)
             (loop :for ,line := (read-line ,in nil nil) :while ,line :do
               (progn ,@body)
                   :finally (return ,result)))))))

(defmacro with-out-file ((var path) &body body)
  `(with-open-file (,var ,path :direction :output
                         :if-exists :supersede :if-does-not-exist :create)
     ,@body))

(defun last-char (string)
  "Return the last character of STRING if it's not empty, otherwise - nil."
  (unless (blankp string)
    (char string (1- (length string)))))

(defun fmt (format-string &rest args)
  "(FORMAT NIL FORMAT-STRING ARGS)"
  (apply #'format nil format-string args))
