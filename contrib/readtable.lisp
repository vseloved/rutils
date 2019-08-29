;; For license see LICENSE

(in-package #:rutilsx.readtable)
(declaim #.+default-opts+)

(eval-always

(defvar *funcall-memory* (make-hash-table :test 'equal))

(defun |#^-reader| (stream char arg)
  "Special syntax for auto-memoization of function calls.

   #^(do-stuff args) =>
   (or (get# '(do-stuff args) *funcall-memory*)
       (set# '(do-stuff args) *funcall-memory* (do-stuff args)))

   Is aware of FUNCALL and APPLY.

   Caution: *funcall-memory* will leak memory, so be carefull with it.
  "
  (declare (ignore char arg))
  (let ((form (read stream))
        (key (gensym)))
    `(let ((,key ,(case (car form)
                    (funcall `(list ,@(cdr form)))
                    (apply `(append (list ,@(butlast (cdr form)))
                                    ,(last1 form)))
                    (otherwise `(cons ',(car form)
                                      (list ,@(cdr form)))))))
       (or (get# ,key *funcall-memory*)
           (set# ,key *funcall-memory* ,form)))))

;; (defun |#_-reader| (stream char arg)
;;   "Special syntax for dimensional numbers.

;;    #_1d => 86400 (1 day in seconds)
;;    #_
;;    (or (get# '(do-stuff args) *funcall-memory*)
;;        (set# '(do-stuff args) *funcall-memory* (do-stuff args)))
;;   "
;;   (declare (ignore char arg))
;;   (let ((form (read stream))
;;         (key (gensym)))
;;     `(let ((,key ,(case (car form)
;;                     (funcall `(list ,@(cdr form)))
;;                     (apply `(append (list ,@(butlast (cdr form)))
;;                                     ,(last1 form)))
;;                     (otherwise `(cons ',(car form)
;;                                       (list ,@(cdr form)))))))
;;        (or (get# ,key *funcall-memory*)
;;            (set# ,key *funcall-memory* ,form)))))


(named-readtables:defreadtable rutilsx-readtable
    (:merge rutils-readtable)
  (:dispatch-macro-char #\# #\^ #'|#^-reader|)
  ;; (:dispatch-macro-char #\# #\_ #'|#_-reader|)
)

) ; end of eval-always
