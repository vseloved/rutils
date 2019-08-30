;;; see LICENSE file for permissions

(cl:in-package #:rutilsx.generators)
(named-readtables:in-readtable rutils-readtable)
(declaim #.+default-opts+)


(defvar *generator-sym* "gen")

(define-condition generated ()
  ((item :initarg :item :reader generated-item)))

(defun yield (&optional item)
  "Signal an ITEM from the generator with a possibility to resume computation."
  (restart-case (signal 'generated :item item)
    (resume (&optional item) item)))

(defun yield-to (generator item)
  "Pass contol to other GENERATOR and submit ITEM that will be returned
   from the call to yield in it."
  (let (already-triggered)
    (handler-bind ((generated (lambda (e)
                                (declare (ignore e))
                                (when already-triggered
                                  (return-from yield-to))
                                (:= already-triggered t)
                                (invoke-restart (find-restart 'resume)
                                                item))))
      (call generator))))
    
(defmacro force (generator-form)
  "Return the results of GENERATOR-FORM work as a list of items."
  (with-gensyms (item rez)
    `(let (,rez)
       (doing (,item ,generator-form)
         (push ,item ,rez))
       (reverse ,rez))))

(defmacro doing ((item generator-form &optional result) &body body)
  "Like DOLIST but for iterating GENERATOR-FORM."
  (with-gensyms (e)
    `(block nil
       (handler-bind ((generated (lambda (,e)
                                   (let ((,item (generated-item ,e)))
                                     ,@body
                                     (invoke-restart (find-restart 'resume))))))
         ,generator-form)
       ,result)))


(defun izip (&rest iterators)
  (yield (mapcar 'funcall iterators)))


(defmacro doing2 (((item1 generator-form1)
                   (item2 generator-form2))
                  &body body)
  (with-gensyms (e s s1 s2 items r restarts)
    `(block nil
       (let (,restarts
             ,items)
         (handler-bind
             ((generated
                (lambda (,e)
                  (push (generated-item ,e) ,items)
                  (print (pair :items ,items))
                  (when (= (length ,items) 2)
                    (with (((,item1 ,item2) (reverse ,items)))
                      (void ,items)
                      ,@body))
                  (handler-bind
                      ((generated
                         (lambda (,e)
                           (push (generated-item ,e) ,items)
                           (print (pair :items ,items))
                           (when (= (length ,items) 2)
                             (with (((,item1 ,item2) (reverse ,items)))
                               (void ,items)
                               ,@body))
                           (push (find-restart 'resume) ,restarts)
                           (let ((,r (last1 ,restarts)))
                             (:= ,restarts (butlast ,restarts))
                             (invoke-restart ,r)))))
                    (push (find-restart 'resume) ,restarts)
                    (if (> (length ,restarts) 1)
                        (let ((,r (last1 ,restarts)))
                          (:= ,restarts (butlast ,restarts))
                          (invoke-restart ,r))
                        ,generator-form2)))))
           ,generator-form1)))))
