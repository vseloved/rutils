;; For license see LICENSE

(in-package #:reasonable-utilities.string)


(defgeneric to-string (obj &optional stream)
  (:documentation "Print string representation of an <_:arg obj/>ect
to <_:arg stream />. Alternative to <_:fun print-readably />"))

(proclaim
 '(inline strcat strcat_ blankp))

(eval-always
  (defun strcat (&rest string-designators)
    "<_:arg Concatenate /> all the strings in <_:arg string-designators />"
    (let ((*print-pretty* nil)
          (*print-circle* nil))
      (with-output-to-string (stream)
        (dolist (str string-designators)
          (when str
            (princ str stream)))))))

(defun strcat_ (&rest string-designators)
  "<_:arg Concatenate /> all the strings in <_:arg string-designators />
with whitespace between them"
  (let ((*print-pretty* nil)
        (*print-circle* nil))
    (with-output-to-string (stream)
      (dolist (str (butlast string-designators))
        (when str
          (princ str stream)
          (princ " " stream)))
      (princ (last1 string-designators) stream))))

(defmacro s+ (&rest strings)
  "A macro for concatenating STINGS at compile time.  Could be used for
pretty strings formatiing, like in Python's built-in string concatenating
behavior.
!!! Caution: variables won't be evaluated !!!"
  (apply #'strcat strings))

(defun blankp (string)
  "Test, whether a <_:arg string /> is blank (empty)"
  (and (stringp string)
       (string= string "")))

(defun read-file (filename &key (newline #\Newline))
  "Read a whole file by <_:arg filename /> into a string.
<_:arg Newline />, if provided, allows to control the style of newline markers,
e.g. #\Newline, #\Linefeed, #\Return,
<_:code (format nil \"~c~c\" #\Return #\Linefeed) />;
if NIL no newlines will be printed at line breaks"
  (with-open-file (in filename)
    (with-output-to-string (out)
      (loop :for line := (read-line in nil) :while line :do
         (princ line out)
         (when newline (terpri out))))))

(defun split-string (string)
  (rutils.sequence:split-sequence-if #`(member _ '(#\Space #\Tab)) string
                                     :remove-empty-subseqs t))

;; (defun strsubst (new old string         
;;                  &rest args &key from-end (test #'char=) (test-not nil)
;;                  (start 0) (count nil) (end nil) (key nil))
;;   "Returns a new string in which all the occurences of <_:arg old /> 
;; are replaced with <_:arg new />."
;;     (with-output-to-string (out)
;;       (loop with part-length = (length part)
;;             for old-pos = 0 then (+ pos part-length)
;;             for pos = (search part string
;;                               :start2 old-pos
;;                               :test test)
;;             do (write-string string out
;;                              :start old-pos
;;                              :end (or pos (length string)))
;;             when pos do (write-string replacement out)
;;             while pos))) 

;;; end