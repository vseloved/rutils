;;;;; Test suite for RUTILS CORE
;;;;; see LICENSE file for permissions

(in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)


(eval-always
(defun abbr-aux-fn (x) (car x))
(defsetf abbr-aux-fn rplaca)
(defun abbr-aux-mac (x) x)
(abbr abbr-aux-mac-abbr abbr-aux-mac)
(abbr abbr-aux-fn-abbr abbr-aux-fn (x))
)

(deftest abbr ()
  (should be = 42
          (abbr-aux-mac-abbr 42))
  (should be = 42
          (abbr-aux-fn-abbr '(42)))
  (should be equal '(42)
          (let ((x (list :foo)))
            (setf (abbr-aux-fn-abbr x) 42)
            x)))
