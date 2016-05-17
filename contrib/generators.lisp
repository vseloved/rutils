;;; see LICENSE file for permissions

(cl:in-package #:rutilsx.generators)
(named-readtables:in-readtable rutils-readtable)
(declaim #.+default-opts+)


(define-condition generated ()
  ((item :initarg :item :reader generated-item)))

(defun yield (item)
  "Signal an ITEM from the generator with a possibility to resume computation."
  (restart-case (signal 'generated :item item)
    (resume () item)))

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
