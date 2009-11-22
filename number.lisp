;;; RUTILS number handling (incl. CDR 5)
;;; see LICENSE file for permissions

(in-package "REASONABLE-UTILITIES.NUMBER")

(locally-enable-literal-syntax :sharp-backq)


;; range

(defun map-range (fun min max &optional (step 1))
  "Map <_:fun fun /> across the integer range
from <_:arg min /> to <_:arg max /> by <_:arg step />"
  (loop :for i :from min :upto max :by step
     :collect (funcall fun i)))

(defmacro do-range ((index &optional min max step return-value)
                    &body body)
  ""
  (assert (or min max)
          (min max)
          "Must specify at least MIN or MAX")
  `(loop
      :for ,index ,@(when min  `(:from ,min))
                  ,@(when max  `(:upto ,max))
                  ,@(when step `(:by   ,step))
      :do (progn ,@body)
      :finally (return ,return-value)))

(defclass range ()
  ((start :initarg :start :reader range-start :initform 0)
   (end   :initarg :end   :reader range-end)
   (step  :initarg :step  :reader range-step  :initform 1))
  (:documentation ""))

(defun range (start &key end step)
  ""
  (make-instance 'range :start start :end end :step step))

#+:seq
(defmethod rutils.seq:seq ((coll range) &optional (start-pos 0))
  (make-instance 'seq :ref coll :pos start-pos
                 :itr #`(with-slots (step end) coll
                          (let ((rez (+ start-pos (* _ step))))
                            (and (or (not end) (<= rez end))
                                 (values rez
                                         _))))))

;; float

(defun parse-float (float-string
                    &key (start 0) (end nil) (radix 10)
                    (junk-allowed t)
                    (type 'single-float)
                    (decimal-character #\.))
  "A simple way to parse a float is <_:code
\(read-from-string (prin1-to-string value)) />
This is a more versatile function, which allows to specialize
different expected float representations"
  (flet ((radix-values (radix)
           (assert (<= 2 radix 35)
                   (radix)
                   "RADIX must be between 2 and 35 (inclusive), not ~D." radix)
           (make-array radix
                       :displaced-to "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                       :displaced-index-offset 0
                       :element-type 
                       #+lispworks 'base-char
                       #-lispworks 'character)))
    (let ((radix-array (radix-values radix))
          (integer-part 0)
          (mantissa 0)
          (mantissa-size 1)
          (sign 1))
      (with-input-from-string (float-stream (string-upcase
                                             (string-trim '(#\Space #\Tab)
                                                          float-string))
                                            :start start :end end)
        (labels ((peek () (peek-char nil float-stream nil nil nil))
                 (next () (read-char float-stream nil nil nil))
                 (sign () (cond  ; reads the (optional) sign of the number
                            ((char= (peek) #\+) (next) (setf sign 1))
                            ((char= (peek) #\-) (next) (setf sign -1)))
                       (integer-part))
                 (integer-part ()
                   (cond
                     ((position (peek) radix-array)
                      ;; the next char is a valid char
                      (setf integer-part (+ (* integer-part radix)
                                            (position (next) radix-array)))
                      ;; again
                      (return-from integer-part (integer-part)))
                     ((null (peek))
                      ;; end of string
                      (done))
                     ((char= decimal-character (peek))
                      ;; the decimal separator
                      (next)
                      (return-from integer-part (mantissa)))                   
                     ;; junk
                     (junk-allowed (done))
                     (t (bad-string))))
                 (mantissa ()     
                   (cond
                     ((position (peek) radix-array)
                      (setf mantissa (+ (* mantissa radix)
                                        (position (next) radix-array))
                            mantissa-size (* mantissa-size radix))
                      (return-from mantissa
                        (mantissa)))
                     ((or (null (peek)) junk-allowed)
                      ;; end of string
                      (done))
                     (t (bad-string))))
                 (bad-string ()
                   (error "Unable to parse ~S." float-string))
                 (done ()
                   (return-from parse-float
                     (coerce (* sign (+ integer-part
                                        (/ mantissa mantissa-size)))
                             type))))
          (sign))))))


;; max/min

(define-modify-macro maxf (&rest numbers) max
  "Modify-macro for <_:fun max />. Sets place, designated by the first ~
argument to the maximum of its original value and <_:arg numbers />")

(define-modify-macro minf (&rest numbers) min
  "Modify-macro for <_:fun min />. Sets place designated by the first ~
argument to the minimum of its original value and <_:arg numbers />")


;; CDR5: Sub-interval Numerical Types

(deftype array-index (&optional (length array-dimension-limit))
  "Type designator for an index into array of <_:arg length />:
an integer between 0 (inclusive) and <_:arg length /> (exclusive).
<_:arg Length /> defaults to <_:arg array-dimension-limit />"
  `(integer 0 (,length)))

(deftype array-length (&optional (length array-dimension-limit))
  "Type designator for a dimension of an array of <_:arg length />:
an integer between 0 (inclusive) and <_:arg length /> (exclusive).
<_:arg Length /> defaults to <_:arg array-dimension-limit />"
  `(integer 0 ,length))

;; The subtypes of type ratio may be defined as follows.
;; Note that ratio does not allow for compound type specifiers.  Also, there are
;; other technical difficulties in this case if we wanted to be very coherent
;; with the background of the ANSI specification. Ratios are defined exactly as
;; the ratio of two non-zero integers, whose greatest common divisor is one and
;; of which the denominator is greater than one. A consequence of the definition
;; is that (typep 42 'ratio) => NIL, and, in particular, (typep 0 'ratio) => NIL
;; This makes it very difficult to use the type specifier machinery effectively,
;; and we must resort to the satisfies type specifier. A possible definition of
;; the ratio sub-interval based on satisfies needs therefore the definition of
;; the ratiop predicate (which is absent from the ANSI specification) alongside
;; the ratio-plusp and ratio-minusp predicates

(defun ratiop (x)
  ""
  (and (rationalp x)
       (> (denominator x) 1)))

(defun ratio-plusp (x)
  ""
  (and (ratiop x) (plusp x)))

(defun ratio-minusp (x)
  ""
  (and (ratiop x) (minusp x)))

(deftype negative-ratio ()
  ""
  '(satisfies ratio-minusp))

(deftype non-positive-ratio ()
  ""
  'negative-ratio)

(deftype non-negative-ratio ()
  ""
  'positive-ratio)

(deftype positive-ratio ()
  ""
  '(satisfies ratio-plusp))

;; This MACROLET will generate all other CDR5 types
(macrolet
    ((frob (type &optional (base-type type))
       (let ((subtype-names (list))
             (predicate-names (list)))
         (flet ((make-subtype-name (format)
                  (let ((result (mksym (symbol-name type) :format format)))
                    (push result subtype-names)
                    result))
                (make-predicate-name (sybtype-name)
                  (let ((result (mksym (symbol-name type)
                                       :format (strcat sybtype-name "-P"))))
                    (push result predicate-names)
                    result))
                (make-docstring (range-beg range-end range-type)
                  (let ((inf (ecase range-type
                               (:negative "-inf")
                               (:positive "+inf"))))
                    (format nil "Type specifier denoting the ~(~A~) range from ~
~A to ~A."
                            type
                            (if (equal range-beg ''*)
                                inf
                                (car (mklist range-beg)))
                            (if (equal range-end ''*)
                                inf
                                (car (mklist range-end)))))))
           (let* ((negative-name       (make-subtype-name "NEGATIVE-~A"))
                  (non-positive-name   (make-subtype-name "NON-POSITIVE-~A"))
                  (non-negative-name   (make-subtype-name "NON-NEGATIVE-~A"))
                  (positive-name       (make-subtype-name "POSITIVE-~A"))
                  (negative-p-name     (make-predicate-name negative-name))
                  (non-positive-p-name (make-predicate-name non-positive-name))
                  (non-negative-p-name (make-predicate-name non-negative-name))
                  (positive-p-name     (make-predicate-name positive-name))
                  negative-extremum
                  positive-extremum
                  below-zero
                  above-zero
                  zero)
             (setf (values negative-extremum below-zero
                           above-zero positive-extremum zero)
                   (ecase type
                     (fixnum       (values 'most-negative-fixnum -1 1
                                           'most-positive-fixnum 0))
                     (integer      (values ''* -1       1        ''* 0))
                     (rational     (values ''* '(0)     '(0)     ''* 0))
                     (real         (values ''* '(0)     '(0)     ''* 0))
                     (float        (values ''* '(0.0E0) '(0.0E0) ''* 0.0E0))
                     (short-float  (values ''* '(0.0S0) '(0.0S0) ''* 0.0S0))
                     (single-float (values ''* '(0.0F0) '(0.0F0) ''* 0.0F0))
                     (double-float (values ''* '(0.0D0) '(0.0D0) ''* 0.0D0))
                     (long-float   (values ''* '(0.0L0) '(0.0L0) ''* 0.0L0))))
             `(progn
                (deftype ,negative-name ()
                  ,(make-docstring negative-extremum below-zero :negative)
                  `(,',base-type ,,negative-extremum ,',below-zero))

                (deftype ,non-positive-name ()
                  ,(make-docstring negative-extremum zero :negative)
                  `(,',base-type ,,negative-extremum ,',zero))

                (deftype ,non-negative-name ()
                  ,(make-docstring zero positive-extremum :positive)
                  `(,',base-type ,',zero ,,positive-extremum))

                (deftype ,positive-name ()
                  ,(make-docstring above-zero positive-extremum :positive)
                  `(,',base-type ,',above-zero ,,positive-extremum))

                (declaim (inline ,@predicate-names))

                (defun ,negative-p-name (n)
                  (and (typep n ',type)
                       (< n ,zero)))

                (defun ,non-positive-p-name (n)
                  (and (typep n ',type)
                       (<= n ,zero)))

                (defun ,non-negative-p-name (n)
                  (and (typep n ',type)
                       (<= ,zero n)))

                (defun ,positive-p-name (n)
                  (and (typep n ',type)
                       (< ,zero n)))))))))
  (frob fixnum integer)
  (frob integer)
  (frob rational)
  (frob real)
  (frob float)
  (frob short-float)
  (frob single-float)
  (frob double-float)
  (frob long-float))


;;; end