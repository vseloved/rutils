;;; RUTILS list handling
;;; see LICENSE file for permissions

(in-package #:reasonable-utilities.list)

(locally-enable-literal-syntax :sharp-backq)


(proclaim
 '(inline last1 single dyadic tryadic append1 conc1 ensure-list mklist))


(defun last1 (lst &optional (n 1))
  "Get the <_:arg N />th element of <_:arg lst /> from end,
starting from the last one (which is number 1)"
  (car (last lst n)))

;; (defun first2 (lst &optional (n 1))
;;   "Split <_:arg lst /> in 2 parts and return them as multiple values:
;; head and tail.  If (= <_:arg n /> 1), which is the most common case,
;; the head will be a single element, otherwise -- a list as well"
;;   (let ((length (length lst)))
;;     (assert (< n length))
;;     (values (if (eql n 1)
;;                 (first lst)
;;                 (butlast lst (- length n 1)))
;;             (last lst (- length n)))))

(defun butlast2 (lst &optional (n 1))
  "Split <_:arg lst /> in 2 parts and return them as multiple values:
head and tail.  If (= <_:arg n /> 1), which is the most common case,
the tail will be a single element, otherwise -- a list as well"
  (assert (< n (length lst)))
  (values (butlast lst n)
          (if (eql n 1) (last1 lst)
              (last lst n))))

(defun single (lst)
  "Test wheather <_:arg lst /> contains exactly 1 element"
  (and (consp lst) (not (cdr lst))))

(defun dyadic (lst)
  "Test wheather <_:arg lst /> contains exactly 2 elements"
  (and (consp lst) (cdr lst) (not (cddr lst))))

(defun tryadic (lst)
  "Test wheather <_:arg lst /> contains exactly 3 elements"
  (and (consp lst) (cddr lst) (not (cdddr lst))))


(eval-always
  (defun ensure-list (obj)
    "Wrap <_:arg obj /> in a list, if it's not a list"
    (if (listp obj) obj (list obj))))

(abbrev mklist ensure-list)

(defmethod mk ((to (eql 'list)) smth &key &allow-other-keys)
  (mklist smth))


(defmacro with-output-to-list ((out) &body body)
  "A simple list analogue of <_:fun with-output-to-string />, which ~
supports the general pattern of using list as an accumulator.
<_:arg Out /> is bound to a fresh list, that will be returned
<_:fun nrevers />ed.  <_:arg Body /> is wraped in implicit <_:fun block /> NIL"
  `(let ((,out (list)))
     (block nil
       (unwind-protect
            (progn ,@body)))
     (nreverse ,out)))


(defun group (lst n)
  "Split <_:arg lst /> into a list of lists of length <_:arg n />"
  (declare (integer n))
  (when (zerop n) (error "zero length"))
  (labels ((rec (src acc)
             (let ((rest (nthcdr n src)))
               (if (consp rest)
                   (rec rest (cons (subseq src 0 n) acc))
                   (nreverse (cons src acc))))))
    (when lst
      (rec lst nil))))

(defun flatten (lst)
  "Flatten <_:arg lst />"
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec lst nil))) 

(defun rearrange-pairs (pairs)
  "From a list of <_:fun cons />-<_:arg pairs /> make 2 separate lists"
  (loop :for pair :in pairs
     :collect (car pair) :into fsts
     :collect (cdr pair) :into secs
     :finally (return (values fsts secs))))

(defun mapfil (fun lst)
  "Accumulate in the resulting list only non-NIL results of application
of <_:arg fun /> to subsequent elements of <_:arg lst />.
Often called FILTER.  This name is selected for consistency with other ~
map- function as well as to allow the name FILTER to be used for ~
general sequences."
  (let (accum)
    (dolist (x lst)
      (let ((val (funcall fun x)))
        (when val (push val accum))))
    (nreverse accum)))

(defun interlay (lst &rest lsts)
  "Return a list, whose elements are subsequently taken from
<_:arg lst /> and each of <_:arg lsts /> as:
1st of lst, 1st of 1st of lsts, ..., 1st of last of lsts, 2nd of lst, ..."
  (apply #'mapcan (lambda (&rest els)
                    els)
         lst lsts))

(defun first-n (lst num &optional (step 1))
  "Return a list with <_:arg NUM /> elements, which are taken from <_:arg LST />
by this formula: INDEX of ELEMENT = I * STEP for I from 0"
  (declare (type (integer 1) step))
  (loop :for rst :on lst :by #`(nthcdr step _)
     :repeat num
     :collect (car rst)))

(defmacro docoll ((item sequence) &body body)
  "If <_:fun dolist /> is the approximate equivalent of <_:fun mapc />, then ~
<_:fun docall /> is the equivalent of <_:fun mapcar />"
  (with-gensyms (acc)
    `(let (,acc)
       (dolist (,item ,sequence)
         (push (progn ,@body) ,acc))
       (nreverse ,acc))))


;; plist

(defun plistp (lst)
  "Test wheather <_:arg lst /> is a properly formed plist"
  (when (listp lst)
    (loop :for rest :on lst :by #'cddr
       :unless (and (keywordp (car rest))
                   (cdr rest))
       :do (return nil)
       :finally (return lst))))

(defun alist-to-plist (alst)
  "Make a plist from an alist <_:arg alst />"
  (mapcan #`(list (car _) (cdr _)) alst))

(defun plist-to-alist (plst)
  "Make an alist from a plist <_:arg plst />"
  (loop :for pair :on plst :by #'cddr
     :collect (cons (car pair) (cadr pair))))

(defun remove-from-plist (plist &rest keys)
  "Returns a propery-list with same keys and values as <_:arg plist />,
except that keys in the list designated by <_:arg keys /> and values,
corresponding to them are removed. The returned property-list may share
structure with the <_:arg plist />, but <_:arg plist /> is not destructively
modified. Keys are compared using <_:fun eq />"
  (declare (optimize (speed 3)))
  (loop :for (key . rest) :on plist :by #'cddr
;     :do (assert rest () "Expected a proper plist, got ~S" plist)
     :unless (member key keys :test #'eq)
     :collect key :and :collect (first rest)))

(defun delete-from-plist (plist &rest keys)
  "Just like <_:fun remove-from-plist />, but this version may destructively
modify the provided <_:arg plist />"
  (declare (optimize (speed 3)))
  (loop :for pos := 0 :then (incf pos 2)
     :for (key . rest) := (nthcdr pos plist)
     :while rest
;     :do (assert rest () "Expected a proper plist, got ~S" plist)
     :do (when (member key keys :test #'eq)
           (rplacd (nthcdr (1- pos) plist) (nthcdr (+ pos 2) plist))
           (decf pos 2))
     :finally (return plist)))


;; alist

(defun alistp (lst)
  "Test wheather <_:arg lst /> is a properly formed alist"
  (and (listp lst) (every #'consp lst)))

(defun assoc1 (item alist &key default key (test nil testp) (test-not nil notp))
  "Return a value in <_:arg alist />, whose key is eql to <_:arg item />.
Also as 2nd value return, whether <_:arg item /> was found.
If there is no such entry, returns <_:arg default />.
The usual <_:arg key />, <_:arg test /> and <_:arg test-not /> arguments apply"
  (let ((pair (apply #'assoc item alist :key key
                     (append (when testp (list :test     test))
                             (when notp  (list :test-not test-not))))))
    (if pair
        (values (cdr pair)
                t)
        (values default
                nil))))


;;; end