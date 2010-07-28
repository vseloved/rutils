;;; RUTILS object handling
;;; see LICENSE file for permissions

(in-package #:reasonable-utilities.object)


;; OBJ-EQUAL

(defgeneric obj-equal-by-slots (obj1 obj2 &optional test)
  (:documentation "Slot-by-slot comparison of objects of one class.
If objs are of different classes the result is NIL.
Obviously, can be specialized on classes, and even for two objects of
diffenet classes, why not...")
  (:method (obj1 obj2 &optional (test 'equal))
    (let ((cls (class-of obj1)))
      (when (eq cls (class-of obj2))
        (handler-case
            (apply #'every test
                   (mapcar (lambda (obj)
                             (mapcar (lambda (slot)
                                       (slot-value
                                        obj
                                        (closer-mop:slot-definition-name slot)))
                                     (closer-mop:class-slots (class-of obj))))
                           (list obj1 obj2)))
          (unbound-slot () nil))))))

(defgeneric obj-equal (obj1 obj2 &optional test)
  (:documentation
   "Class-aware polymorphic equality with an optional <_:arg test />")
  (:method (obj1 obj2 &optional (test 'equal))
    "For non CLOS primitive types"
    (funcall test obj1 obj2))
  (:method ((obj1 sequence) (obj2 sequence) &optional (test 'equal))
    "For sequences -- recursively look inside the ordered variants"
    (when (= (length obj1) (length obj2))
      (every (lambda (obj1 obj2)
               (obj-equal obj1 obj2 test))
             obj1 obj2))) ; add lexicographical ordering
  (:method ((obj1 standard-class) (obj2 standard-class) &optional (test 'equal))
    "Slot-by-slot comparison for STANDARD-CLASSES.
If objs are of different classes the result is NIL."
    (obj-equal-by-slots obj1 obj2 test)))


;;; end