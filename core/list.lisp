;;; see LICENSE file for permissions

(cl:in-package #:reasonable-utilities.list)
(named-readtables:in-readtable rutils-readtable)

(declaim (optimize (speed 3) (space 1) (debug 0)))

(declaim (inline last1 single dyadic tryadic append1 conc1 ensure-list))


(defun last1 (list &optional (n 1))
  "Get the N-th element of LIST from end, starting from the last one (which
is number 1)."
  (car (last list n)))

(defun (setf last1) (object list &optional (n 1))
  (setf (car (last list n)) object))

(defun butlast2 (list &optional (n 1))
  "Split LIST in 2 parts and return them as multiple values:
head and tail.  If (= N 1), which is the most common case,
the tail will be a single element, otherwise -- a list as well."
  (assert (<= n (length list)))
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
  (loop :for rest :on list :by #`(nthcdr step %) :repeat n
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
  (mapcan #`(list (car %) (cdr %))
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

(defun atomize (list-or-val)
  (if (listp list-or-val)
      (car list-or-val)
      list-or-val))


(define-modify-macro appendf (&rest lists) append
  "Modify-macro for APPEND. Appends LISTS to the place designated by the first
argument.")

(define-modify-macro nconcf (&rest lists) nconc
  "Modify-macro for NCONC. Concatenates LISTS to place designated by the first
argument.")

(define-modify-macro unionf (list &rest args) union
  "Modify-macro for UNION. Saves the union of LIST and the contents of the
place designated by the first argument to the designated place.")

(define-modify-macro nunionf (list &rest args) nunion
  "Modify-macro for NUNION. Saves the union of LIST and the contents of the
place designated by the first argument to the designated place. May modify
either argument.")

(define-modify-macro reversef () reverse
  "Modify-macro for REVERSE. Copies and reverses the list stored in the given
place and saves back the result into the place.")

(define-modify-macro nreversef () nreverse
  "Modify-macro for NREVERSE. Reverses the list stored in the given place by
destructively modifying it and saves back the result into the place.")

(defmacro doplist ((key val plist &optional values) &body body)
  (with-gensyms (tail)
    `(do ((,tail ,plist (cddr ,tail)))
         ((null ,tail) ,values)
       (ds-bind (,key ,val &rest) ,tail
         ,@body))))

(defun set-equal (list1 list2 &key (test #'eql) (key nil keyp))
  "Returns true if every element of LIST1 matches some element of LIST2 and
every element of LIST2 matches some element of LIST1. Otherwise returns false."
  (let ((keylist1 (if keyp (mapcar key list1) list1))
        (keylist2 (if keyp (mapcar key list2) list2)))
    (and (dolist (elt keylist1 t)
           (or (member elt keylist2 :test test)
               (return nil)))
         (dolist (elt keylist2 t)
           (or (member elt keylist1 :test test)
               (return nil))))))

(defun zip (&rest lists)
  "Return a single list whose elements are lists
   of the consecutive elements of LISTS,
   until one of the LISTS ends."
  (apply #'zip-with #'list lists))

(defun zip-with (fn &rest lists)
  "Return a single list whose elements are the results
   of applying FN to groups of the consecutive elements of LISTS,
   until one of the LISTS ends."
  (let (rez)
    (do ((tails lists (mapcar #'cdr tails)))
        ((some #'null tails))
      (push (apply fn (mapcar #'car tails)) rez))
    (reverse rez)))

(defun zip* (&rest lists)
  "Return a single list whose elements are lists
   of the consecutive elements of LISTS,
   until one of the LISTS ends."
  (apply #'zip*-with #'list lists))

(defun zip*-with (fn &rest lists)
  "Return a single list whose elements are the results
   of applying FN to groups of the consecutive elements of LISTS,
   until one of the LISTS ends."
  (let (rez)
    (do ((tails lists (mapcar #'cdr tails)))
        ((every #'null tails))
      (push (apply fn (mapcar #'car tails)) rez))
    (reverse rez)))
