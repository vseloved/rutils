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

(defun strcat_ (&rest string-designators)
  "CONCATENATE all the strings in STRING-DESIGNATORS inserting whitespace
between them."
  (let ((*print-pretty* nil)
        (*print-circle* nil))
    (with-output-to-string (stream)
      (dolist (str (butlast string-designators))
        (princ str stream)
        (princ " " stream))
      (princ (last1 string-designators) stream))))

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


(defun starts-with (string prefix)
  "Test, whether STRING starts with PREFIX."
  (if-it (mismatch string prefix)
         (= it (length prefix))
         t))

(defun ends-with (string suffix)
  "Test, whether STRING ends with SUFFIX."
  (if-it (mismatch suffix string :from-end t)
         (= it 0)
         t))
