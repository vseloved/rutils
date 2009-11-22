;;; RUTILS implementation specific code
;;; see LICENSE file for permissions

(in-package "REASONABLE-UTILITIES.CORE")

(defun make-reader-error-fun (char)
  (lambda (stream char arg)
    (declare (ignore stream arg))
    #+sbcl (error 'sb-int:simple-reader-error :stream *error-output*
                  :format-control "no dispatch function defined for #\~c"
                  :format-arguments (list char))
    #-(or sbcl)
    (error 'simple-error :stream *error-output*
           :format-control "no dispatch function defined for #\~c"
           :format-arguments (list char))))

;;; end