;;; see LICENSE file for permissions

(in-package #:rutils.array)
(named-readtables:in-readtable rutils-readtable)
(eval-when (:compile-toplevel)
  (declaim #.+default-opts+))


(deftype array-index (&optional (length array-dimension-limit))
  "Type designator for an index into array of LENGTH: an integer between
   0 (inclusive) and LENGTH (exclusive).
   LENGTH defaults to ARRAY-DIMENSION-LIMIT."
  `(integer 0 (,length)))

(deftype array-length (&optional (length array-dimension-limit))
  "Type designator for a dimension of an array of LENGTH: an integer between
   0 (inclusive) and LENGTH (inclusive).
   LENGTH defaults to ARRAY-DIMENSION-LIMIT."
  `(integer 0 ,length))


(defun slice (vec beg &optional end)
  "Return an array-slice into VEC from BEG to END."
  (let ((size (max 0 (- (or end (length vec)) beg))))
    (apply #'make-array size :element-type (array-element-type vec)
           (unless (zerop size)
             (list :displaced-to vec :displaced-index-offset beg)))))

(defmacro dovec ((var vec &optional result-form) &body body)
  "Iterates over a vector (like in DOLIST)."
  (let ((index-var (gensym "INDEX")))
    (once-only (vec)
      `(dotimes (,index-var (length ,vec) ,result-form)
         (let ((,var (aref ,vec ,index-var)))
           ,@body)))))

(defun vec (&rest args)
  "Make a new adjustable vector with ARGS as contents."
  (make-array (length args) :initial-contents args
                            :adjustable t :fill-pointer t))

(defun copy-array (arr)
  "Create a fresh copy of the array ARR."
  (let* ((dims (array-dimensions arr))
         (dim-count (length dims))
         (rez (make-array dims :element-type (array-element-type arr))))
    (labels ((drill-down (is)
               (if (= (length is) dim-count)
                   (:= (apply #'aref rez is)
                       (apply #'aref arr is))
                   (dotimes (i (array-dimension arr (- dim-count (length is))))
                     (drill-down (cons i is))))))
      (dotimes (i (array-dimension arr (1- dim-count)))
        (drill-down (list i))))
    rez))
