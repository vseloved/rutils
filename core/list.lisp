;;; see LICENSE file for permissions

(cl:in-package #:rutils.list)
(named-readtables:in-readtable rutils-readtable)
(declaim #.+default-opts+)


(declaim (inline last1 single dyadic tryadic append1 conc1 ensure-list
                 dcons dlistp range concat))


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
   will be returned nreversed.  BODY is wraped in implicit block NIL."
  `(let ((,out (list)))
     (block nil
       (progn ,@body))
     (nreverse ,out)))

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

(defun interpose (separator list)
  "Returns a sequence of the elements of SEQUENCE separated by SEPARATOR."
  (labels ((rec (s acc)
             (if s
                 (rec (cdr s) (nconc acc
                                     (list separator (car s))))
                 acc)))
    (cdr (rec list nil))))

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
   structure with the PLIST, but PLIST is not destructively modified.
   Keys are compared using EQ."
  (declare (optimize (speed 3)))
  (loop :for (key . rest) :on plist :by #'cddr
     :unless (member key keys :test #'eql)
     :collect key :and :collect (first rest)))

(defun delete-from-plist (plist &rest keys)
  "Just like REMOVE-FROM-PLIST, but this version may destructively
   modify the provided PLIST."
  (declare (optimize (speed 3)))
  (loop :for pos := 0 :then (incf pos 2)
     :for (key . rest) :on plist :by #'cddr
     :while rest :do
     (when (member key keys :test #'eql)
       (if (zerop pos)
           (setf plist (nthcdr 2 plist))
           (progn
             (rplacd (nthcdr (1- pos) plist) (nthcdr (+ pos 2) plist))
             (decf pos 2))))
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

(defmacro doplist ((key val plist &optional rez) &body body)
  "Like DOLIST but iterate over 2 items of the PLIST at once,
   onsidered KEY and VAL. Asserts proper PLIST."
  (once-only (plist)
    (with-gensyms (tail)
      `(progn
;         (assert (plistp ,plist) ,plist "~A is not a proper plist" ,plist)
         (do ((,tail ,plist (cddr ,tail)))
             ((null ,tail) ,rez)
           (destructuring-bind (,key ,val &rest ,(gensym)) ,tail
             ,@body))))))

(defun set-equal (list1 list2 &key (test 'eql) (key nil keyp))
  "Return true if every element of LIST1 matches some element of LIST2 and
   every element of LIST2 matches some element of LIST1. Otherwise returns false."
  (let ((keylist1 (if keyp (mapcar key list1) list1))
        (keylist2 (if keyp (mapcar key list2) list2)))
    (and (dolist (elt keylist1 t)
           (unless (member elt keylist2 :test test)
             (return nil)))
         (dolist (elt keylist2 t)
           (unless (member elt keylist1 :test test)
             (return nil))))))

(defun zip (&rest lists)
  "Return a single list whose elements are lists
   of the consecutive elements of LISTS,
   until one of the LISTS ends."
  (when lists
    (apply #'zip-with #'list lists)))

(defun zip-with (fn &rest lists)
  "Return a single list whose elements are the results
   of applying FN to groups of the consecutive elements of LISTS,
   until one of the LISTS ends."
  (when lists
    (let (rez)
      (do ((tails lists (mapcar #'cdr tails)))
          ((some #'null tails))
        (push (apply fn (mapcar #'car tails)) rez))
      (reverse rez))))

(defun zip* (&rest lists)
  "Return a single list whose elements are lists
   of the consecutive elements of LISTS,
   until all of the LISTS end."
  (when lists
    (apply #'zip*-with #'list lists)))

(defun zip*-with (fn &rest lists)
  "Return a single list whose elements are the results
   of applying FN to groups of the consecutive elements of LISTS,
   until all of the LISTS end."
  (when lists
    (let (rez)
      (do ((tails lists (mapcar #'cdr tails)))
          ((every #'null tails))
        (push (apply fn (mapcar #'car tails)) rez))
      (reverse rez))))

(defun maptimes (times fn)
  "Map FN with number range from 0 to TIMES (exclusive)."
  (mapcar fn (range 0 times)))

(defun mapindex (fn list)
  "Mapcar FN with 2 lists:
   - numeric inidices for LIST, starting from 0
   - LIST itself"
  (let ((i -1))
    (mapcar #`(funcall fn (incf i) %)
            list)))

(defun mapcanindex (fn list)
  "Mapcan FN with 2 lists:
   - numeric inidices for LIST, starting from 0
   - LIST itself"
  (let ((i -1))
    (mapcan #`(funcall fn (incf i) %)
            list)))

(defun mappend (function &rest lists)
  "Apply FUNCTION to respective elements of each LIST, appending all the
result lists to a single list. FUNCTION must return a list."
  (loop :for results :in (apply #'mapcar function lists)
     :append results))

(defun remove-idx (idx list)
  "Return a copy of the list without IDX-th element."
  (loop :for i :from 0
        :for elt :in list
        :unless (= i idx) :collect elt))

(defun permutations (list)
  "Generate all permutations of LIST.
   Complexity: O(n!)"
  (labels ((perms (list acc)
             (if list
                 (mapcanindex (lambda (i el)
                                (perms (remove-idx i list)
                                       (mapcar #`(cons el %) acc)))
                              list)
                 acc)))
    (when list
      (perms list (list nil)))))

(defmacro listcase (list &body cases)
  "A typecase-like macro to destinguish between 3 possible kinds of LIST:
   simple lists, alists, and dlists.
   Evaluates the appropriate key-form depending on the kind:
   ALIST, DLIST or simple list (T)."
  (once-only (list)
    `(if (consp (first ,list))
         (if (consp (second ,list))
             (progn ,@(assoc1 'alist cases))
             (progn ,@(assoc1 'dlist cases)))
         (progn ,@(assoc1 't cases)))))

(defun range (start limit &key (step 1))
  "Return a list of numbers, starting from START up to LIMIT
incremented by STEP (default 1)."
  (loop :for i :from start :to (1- limit) :by step
     :collect i))

(defun concat (&rest lists)
  "CONCATENATE all the LISTS into a single list."
  (apply #'concatenate 'list lists))

;; D-Lists (see https://groups.google.com/forum/#!msg/comp.lang.lisp/pE-4JL9lnAA/7hiQSBexGLgJ)

(defun dcons (key val dlist)
  "Add KEY-VAL pair to the front of DLIST."
  (cons (cons key (car dlist))
        (cons val (cdr dlist))))

(defun dlistp (list)
  "Test wheather LIST is a properly formed dlist,
   i.e. a list-structure of the form: ((keys) vals)."
  (and (listp list)
       (consp (first list))
       (second list)
       (not (consp (second list)))))
