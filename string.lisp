;; For license see LICENSE

(in-package "REASONABLE-UTILITIES.STRING")


(defgeneric to-string (obj &optional stream)
  (:documentation "Print string representation of an <_:arg obj/>ect
to <_:arg stream />. Alternative to <_:fun print-readably />"))

(proclaim
 '(inline strcat strcat_ blankp))

(defun strcat (&rest string-designators)
  "<_:arg Concatenate /> all the strings in <_:arg string-designators />"
  (let ((*print-pretty* nil)
        (*print-circle* nil))
    (with-output-to-string (stream)
      (dolist (str string-designators)
        (when str
          (princ str stream))))))

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
      (princ (last1 string-designators)))))

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
         (when newline 
           (format out "~a" newline))))))

;;; end