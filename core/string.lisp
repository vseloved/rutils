;; For license see LICENSE

(cl:in-package #:reasonable-utilities.string)
(named-readtables:in-readtable rutils-readtable)

(declaim (optimize (speed 3) (space 1) (debug 0)))

(declaim (inline white-char-p))


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

(eval-always
(defun read-file (filename)
  "Read a whole file by FILENAME into a string."
  (with-open-file (in filename)
    (let* ((buf (make-array (file-length in) :element-type 'character
                            :adjustable t :fill-pointer t))
           (chars-read (read-sequence buf in)))
      (setf (fill-pointer buf) chars-read)
      buf)))
(abbr slurp read-file))

(defun white-char-p (char)
  "Is CHAR a whitespace character (newline chars are also considered white)."
  (member char '(#\Space #\Tab #\Return #\Linefeed #\Newline #\Page)))

(defun split-string (string)
  "Split STRING by WHITE-CHAR-P."
  (rutils.sequence:split-sequence-if #'white-char-p string
                                     :remove-empty-subseqs t))

(defun substr (string start &optional end)
  "Efficient substring of STRING from START to END (optional), where both can be
negative, which means counting from the end."
  (let (len)
    (when (minusp start)
      (setf len (length string)
            start (+ len start)))
    (when (and end (minusp end))
      (setf end (+ (or len (length string)) end)))
    (subseq string start end)))

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


(defun starts-with (prefix string)
  "Test, whether STRING starts with PREFIX."
  (if-it (mismatch prefix string)
         (= it (length prefix))
         t))

(defun ends-with (suffix string)
  "Test, whether STRING ends with SUFFIX."
  (if-it (mismatch suffix string :from-end t)
         (zerop it)
         t))

(deftype string-designator ()
  "A string designator type. It is either a string, a symbol, or a character."
  `(or symbol string character))

(defmacro dolines ((line src &optional result) &body body)
  "Iterate over each LINE in SRC (a stream or path to a file) as in DOLIST."
  (let ((in (gensym)))
    `(if (streamp ,src)
         (loop :for ,line := (read-line ,src nil nil) :while ,line :do
            (progn ,@body)
            :finally (return ,result))
         (with-open-file (,in ,src)
           (loop :for ,line := (read-line ,in nil nil) :while ,line :do
              (progn ,@body)
              :finally (return ,result))))))

(defmacro with-out-file ((var path) &body body)
  `(with-open-file (,var ,path :direction :output
                         :if-exists :supersede :if-does-not-exist :create)
     ,@body))

(defun last-char (string)
  "Return the last character of STRING if it's not empty, otherwise - nil."
  (unless (blankp string)
    (char string (1- (length string)))))