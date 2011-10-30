;;; see LICENSE file for permissions

(cl:in-package #:reasonable-utilities.list)
(named-readtables:in-readtable rutils-readtable)

(proclaim '(optimize speed))
(proclaim '(inline last1 single dyadic tryadic append1 conc1 ensure-list))


(defun last1 (list &optional (n 1))
  "Get the N-th element of LIST from end, starting from the last one (which
is number 1)."
  (car (last list n)))

(defun butlast2 (list &optional (n 1))
  "Split LIST in 2 parts and return them as multiple values:
head and tail.  If (= N 1), which is the most common case,
the tail will be a single element, otherwise -- a list as well."
  (assert (< n (length list)))
  (values (butlast list n)
          (if (eql n 1) (last1 list)
              (last list n))))

(defun single (list)
  "Test wheather LIST contains exactly 1 element."
  (and (consp list) (not (cdr list))))

(defun dyadic (list)
  "Test wheather LIST contains exactly 2 elements."
  (and (consp list) (cdr list) (not (cddr list))))

(defun tryadic (list)
  "Test wheather LIST contains exactly 3 elements."
  (and (consp list) (cddr list) (not (cdddr list))))


(defun ensure-list (obj)
  "Wrap OBJ in a list, if it's not a list."
  (if (listp obj) obj (list obj)))


(defmacro with-output-to-list ((out) &body body)
  "A simple list analog of WITH-OUTPUT-TO-STRING, which supports the general
pattern of using list as an accumulator. OUT is bound to a fresh list, that
will be returned NREVERSE-D.  BODY is wraped in implicit block NIL."
  `(let ((,out (list)))
     (block nil
       (unwind-protect
            (progn ,@body)))
     (nreverse ,out)))

(defun group (n list)
  "Split LIST into a list of lists of length N."
  (declare (integer n))
  (when (zerop n)
    (error "Group length N shouldn't be zero."))
  (labels ((rec (src acc)
             (let ((rest (nthcdr n src)))
               (if (consp rest)
                   (rec rest (cons (subseq src 0 n) acc))
                   (nreverse (cons src acc))))))
    (when list
      (rec list nil))))

(defun flatten (list)
  "Flatten possibly nested LIST."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec list nil)))

(defun interleave (list &rest lists)
  "Return a list whose elements are taken from LIST and each of LISTS like this:
    1st of list, 1st of 1st of lists,..., 1st of last of lists, 2nd of list,..."
  (apply #'mapcan (lambda (&rest els)
                    els)
         list lists))

(defun take (n list &optional (step 1))
  "Return a list with N elements, which are taken from LIST by this formula:
    INDEX of ELEMENT = I * STEP for I from 0"
  (declare (type (integer 1) step))
  (loop :for rest :on list :by #`(nthcdr step @) :repeat n
     :collect (car rest)))

(defun plistp (list)
  "Test wheather LIST is a properly formed plist."
  (when (listp list)
    (loop :for rest :on list :by #'cddr
       :unless (and (keywordp (car rest))
                    (cdr rest))
       :do (return nil)
       :finally (return list))))

(defun alistp (list)
  "Test wheather LIST is a properly formed alist."
  (and (listp list) (every #'consp list)))

(defun alist-to-plist (alist)
  "Make a plist from an alist ALIST."
  (mapcan #`(list (car @) (cdr @))
          alist))

(defun plist-to-alist (plist)
  "Make an alist from a plist PLIST."
  (loop :for pair :on plist :by #'cddr
     :collect (cons (car pair) (cadr pair))))

(defun remove-from-plist (plist &rest keys)
  "Returns a propery-list with same keys and values as PLIST,
except that keys in the list designated by KEYS and values,
corresponding to them are removed. The returned property-list may share
structure with the PLIST, but PLIST is not destructively
modified. Keys are compared using EQ."
  (declare (optimize (speed 3)))
  (loop :for (key . rest) :on plist :by #'cddr
;     :do (assert rest () "Expected a proper plist, got ~S" plist)
     :unless (member key keys :test #'eq)
     :collect key :and :collect (first rest)))

(defun delete-from-plist (plist &rest keys)
  "Just like REMOVE-FROM-PLIST, but this version may destructively
modify the provided PLIST."
  (declare (optimize (speed 3)))
  (loop :for pos := 0 :then (incf pos 2)
     :for (key . rest) := (nthcdr pos plist)
     :while rest
;     :do (assert rest () "Expected a proper plist, got ~S" plist)
     :do (when (member key keys :test #'eq)
           (rplacd (nthcdr (1- pos) plist) (nthcdr (+ pos 2) plist))
           (decf pos 2))
     :finally (return plist)))

(defun assoc1 (item alist &key default key (test nil testp) (test-not nil notp))
  "Return a value in ALIST, whose key is eql to ITEM. Also as 2nd value return,
whether ITEM was found. If there is no such entry, returns DEFAULT.
The usual KEY, TEST and TEST-NOT arguments apply."
  (let ((pair (apply #'assoc item alist :key key
                     (append (when testp (list :test     test))
                             (when notp  (list :test-not test-not))))))
    (if pair
        (values (cdr pair)
                t)
        (values default
                nil))))