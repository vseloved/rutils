;; For license see LICENSE

#+nil
(defpackage "REASONABLE-UTILITIES.DISTRO"
  (:nicknames "RUTILS.DISTRO")
  (:use :common-lisp "RUTILS.CORE" "RUTILS.SHORT" "RUTILS.FUNCTION"
        "RUTILS.OBJ")
  (:documentation "Systems, distributions and environment control suite")
  (:export #:*settings*
           #:*search-paths*
           #:*install-path*
           #:*distros*
           #:*platforms*

           ;; environment
           #:defenv
           #:envcond
           #:in-env
          
           ;; system definition
           #:asdf-system
           #:simple-system
           #:standard-system
           #:defsystem
           
           ;; operations
           #:op
           #:perform

           ;; extension API
           #:def-op-hook
           #:sort-parts-for-specific-order
           ;; system selection strategies


           ;; platform

           ;; condition
           #:distro-error))

(in-package "RUTILS.DISTRO")


;; settings

(defvar *settings* (make-hash-table)
  "Table of settings, which includes the following ones (default is marked with *):
:mode - operation mode - :auto* or :interactive
:silent - supress all warnings? - nil*
:allow-version-overwrite - action on the request to load the system with new version - t*
:v-mismatch-strategy - what to do, if the requested version(s) of the system 
                       is/are not found, but others are.
                       Options: :closest-bigger*, :first, :prompt, :download, ...
                       (:prompt will signal an error, if :mode is not :interactive)
")

;; :v-ambiguity-strategy - what to do, if several systems with one version are found? - :first*, :prompt


(defmacro warn-if (&rest args)
  "<_:fun Warn />, unless in :silent mode"
  `(unless (gethash :silent *settings*)
     (warn ,@args)))


;; util

(declaim (inline by-key sys-sym v-from-string sys< part< get-deps match-env-prefix ensure-env))

(defconstant +future+ 9999999999)

(defun by-key (node key &optional (test 'eql))
  "Like <_:code (getf plist key) /> but for the case, when <_:arg node /> is
not a proper <_:type plist />. As well, arbitrary <_:arg test /> can be used"
  (when-it (position key node :test test)
    (nth (1+ it) node)))

(defun sys-sym (name)
  "Make <_:arg name /> into the symbol from <_:pkg distros />"
  (mksym name :package 'distros))

(defun v-from-string (str)
  "From a string version spec, like 1.4.2 make a list one: (1 4 2)"
  (cl-ppcre:split "(\\.)|(:)|(~)|(-)|(_)|(,)|(\\+)" str))

(defun till/= (op lst1 lst2)
  "Like <_:fun <= /> or <_:fun >= /> (depending on <_:arg op />: '< or '>,
but operates on lists in thios manner: traverses them in parallel,
while consecutive elements are <_:fun = />;
then either one of the lists ends (if it's <_:arg lst1 />, result is nil,
otherwise -- t), or result is <_:arg op /> applied to the first mismatching
element pair"
  (block whole
    (let* ((l2 (length lst2))
           (i2 0))
      (dolist (e1 lst1)
        (let ((e2 (when (< i2 l2) (elt lst2 i2))))
          (if (eql e1 e2)
              (incf i2)
              (return-from whole
                (or (not e2)
                    (funcall op e1 e2)))))))
    t))

(defun match-v (v v-spec)
  "Match <_:arg v /> as a list (like (0 1 2)) against the <_:arg v-spec />
either as a plain list (in this case match by <_:fun equalp />), or as a
plist of a form (:from (0 1 2) :to (1 0 0)), where either :from or :to part
may be missing and both are considered inclusive"
  (if (keywordp (car v-spec))
      ;; it's a range spec
      (let ((from (getf v-spec :from))
            (to (getf v-spec :to)))
        (and (if from (till/= '> v from) t)
             (if to (till/= '< v to) t)))
      ;; otherwise it's an exact spec
      (equalp v v-spec))))

(defun sys< (a b)
  "System precedence order (by dependence)"
  (find (sys-name a) (sys-deps b) :test #'string=))

(defun part< (a b)
  "Part precedence order (by dependence)"
  (find (car (mklist a)) (get-deps b) :test #'string=))

(defun get-deps (lst)
  "From a list (<file> :on <deps> ...) get <deps> list"
  (by-key lst :on))

(defun match-env-prefix (key)
  "Does <_:arg key /> start with ENV- prefix?"
  (let ((key-str (princ-to-string key)))
    (string= (subseq key-str 0
                     (if (> (length key-str) 4)
                         4 0))
             "ENV-")))

(defun ensure-env (key)
  "Ensure, that <_:arg key /> starts with ENV- prefix"
  (if (match-env-prefix env) env  ; leave as is
      (mkey key :format "env-~a")))

(defun file-write-date (pathspec)
  "Determine"
  #+sbcl #+unix (+ (sb-posix:stat-mtime (sb-posix:stat pathspec)) 2209075200))  ; + 70 years and some



;; environment

(defgeneric in-env (env)
  (:documentation "Set-up the context of the environment <_:arg env />")
  (:method (env)
    (error (format nil "Environment ~a ot defined" env))))

(defmacro defenv (key (&key sys-paths root install-path platform)
                  &body body)
  "Define new environment (ephemeral object) by defining it's context
\(<_:args sys-path root install-path and platform />) and additional
actions, needed to be performed, when switching to it (as a <_:arg body />
of a method specialised on this environment designator).
The environment is designated by a keyword :ENV-<_:arg key />, and it's
method is defined so, as to exclude other environments with the designator,
constructed the same way"
  (let ((env (ensure-env env)))
    (once-only (sys-paths root install-path platform)
      `(defmethod in-env ((env (eql ,env)))
         (setf *features*
               (delete-if #'match-env-prefix *features*))
         (push ,env *features*)
         (when ,sys-paths
           (setf *search-paths* ,sys-paths)
           #+asdf (setf asdf:*central-registry* ,sys-paths))
         (when ,root
           (setf *default-pathname-defaults* ,root))
         (when ,install-path
           (setf *install-path* install-path))
         (when ,platform
           (setf *platform* ,platform))
         ,@body))))

(defmacro envcond (&body clauses)
  "Like <_:fun cond /> but operates on"
  `(cond
     ,@(loop :for clause-and-tail :on clauses
          :collect (if (and (eq (caar clause-and-tail) t)
                            (not (cdr clause-and-tail)))
                       `(t ,@(cdar clause-and-tail))
                       `((find (ensure-env ,(caar clause-and-tail)) *features*)
                         ,@(cdar clause-and-tail))))))


;; system definition

(defvar *search-paths* '("/usr/share/lisp")
  "Where to search for system definition files")

(defvar *install-path* "/usr/share/lisp"
  "Where to install downloaded DISTROS")

(defvar *distros* (make-hash-table)
  "Table of known system objects indexed by systems' name and aliases")

(defmacro defsystem (name (&optional (system-class 'standard-system)) &rest args)
  "Define a system of class <_:arg system-class />, add it to <_:var *distros* />
and export from <_:pkg distros />"
  (with-gensyms (ref sys aliases)
    `(eval-always
       (let* ((,ref (let ((,ref (sys-sym name)))
                                     (export ref 'distros)
                                     ref))
              (,sys (make-instance ,system-class
                                   :name ,ref
                                   ,@args))
              (,aliases (mapcar #`(export _ 'distros)
                                (getf ',args :aliases))))
         (mapc #`(setf (gethash _ *distros*) ,sys)
               (cons ,ref ,aliases))))))

(defclass simple-system ()
  ((name :reader sys-name :initarg :name
         :initform (error "Name is required for system"))
   (v :reader sys-v :initarg v
      :documentation "version-spec in list form, like (0 4 2)")
   (meta :reader sys-meta :initform (make-hash-table)
         :documentation "meta information, like author, maintainer, description etc")
   (path :accessor sys-path :initarg :path
         :documentation "path, from which the system was last loaded")
   (parts :accessor sys-parts :initarg :parts
          :documentation "list of parts and their dependencies")
   (deps :accessor sys-deps :initarg :deps :initform nil
         :documentation "list of dependencies on other systems")
   (ext-deps :accessor sys-ext-deps :initarg :ext-deps :initform nil
             :documentation "list of dependencies on external (non-Lisp) libraries")
   (stamp :reader sys-stamp :initarg :stamp
          :documentation "timestamp of the time of start of last load operation"))
  (:documentation "Information about the system, available to the management system"))

(defclass standard-system (simple-system)
  ((aliases :reader sys-aliases :initarg :aliases
            :documentation "list of alternative names, by which the
system can be found in <_:var *disros* />")
   (url :accessor sys-url :initarg :url
        :documentation "a canonical distro URL")
   (parts-order
    :documentation "tree of parts in order of precedence")
   (deps-order
    :documentation "full set of dependencies in order of precedence"))
  (:documentation "Information about the DISTRO, available to the management system"))

(defclass asdf-system (simple-system)
  ()
  (:documentation "Information about the ASDF system, available to the management system"))

(defmethod obj-equal ((a standard-system) (b standard-system))
  (and (string= (sys-name a) (sys-name b))
       (equalp (sys-v a) (sys-v b))))

(defmethod initialize-instance :after
    ((system standard-system) &key meta &allow-other-keys)
  "Add meta info and calculate precedence order for
<_:slot standard-system deps /> and <_:slot standard-system parts />"
  (setf (sys-meta system) (etypecase meta
                            (hash-table meta)
                            (list (hash-table-from-list meta)))

        (slot-value system 'deps-order)
        (sort (copy-seq (sys-deps system)) #'sys<)

        (slot-value system 'parts-order)
        (sort-parts (copy-seq (sys-parts system)))))

(defmethod (setf sys-deps) :after ((system standard-system))
  "Recompute dependency order for <_:slot standard-system deps />
after their redefinition"
  (setf (slot-value system 'deps-order)
        (sort (copy-seq (sys-deps system)) #'sys<)))

(defmethod (setf sys-parts) :after ((system standard-system))
  "Recompute dependency order for <_:slot standard-system parts />
after their redefinition"
  (setf (slot-value system 'parts-order)
        (sort-parts (copy-seq (sys-parts system)))))

(defmethod print-object ((sys simple-system) stream)
  (print-unreadable-object (sys stream :type t :identity t)
    (ignore-errors
      (prin1 (sys-name sys) stream))))


;; sorting parts & deps

(defun sort-parts (parts-spec)
  "Sort parts in dependency precedence order.
<_:arg Parts-spec /> is in general a tree of the following form:
spec = ([:random|:serial|...]
        <file>|(<file>)
        (<file> :on <file-list>)   ; dependencies
        (<dir> <spec>              ; subsystem definition
         :on <file-list>)          ; dependencies
        (<file>|<dir> <spec>       ; external dependencies -- i.e. handled not by DISTR
         :by <handler-object>      ; Lisp handler object, either performing some action
                                   ; in-house or calling external utils
         :with <handler-arglist>)) ; arguments fo handler
There can be multiple :ext subsystems."
  (let ((maybe-order (car parts-spec)))
    (if (keywordp maybe-order)
        (let ((parts (cdr parts-spec)))
          (case maybe-order
            (:serial (mapcar #`(let ((spec (mklist _)))
                                 (if (listp (cadr spec))  ; it's a subsystem (subdir)
                                     (setf (cadr spec) (sort-parts (cadr spec)))
                                     (car (mklist spec))))  ; single file
                             parts))
            (:random (sort parts #'part<))
            (otherwise (sort-parts-for-specific-order maybe-order parts))))
        ;; random order -- default
        (sort-parts (cons :random parts-spec)))))

(defgeneric sort-parts-for-specific-order (order parts-lst)
  (:documentation "API function. For extending system definitions with special orders.
In default case just signals a <_:cond distro-error />")
  (:method (order parts-list)
    (error 'distro-error
           :msg (format nil "Order :~a not implemented" order))))


;; find system

(defun find-system (designator &optional v)
  ""
  (let ((ref (sys-sym designator))
        (ref-str (string-downcase (ensure-string designator))))
    (or (gethash ref *distros*)
        (mv-bind (sys-d alternates-d) (find-on-disk ref-str)
          (or sys
              (mv-bind (sys-w alternates-w) (find-on-web ref-str)
                (or sys-w
                    (select-system (nconc alternates-d alternates-w)
                                   (gethash *settings* :v-mismatch-strategy))))))))) 

(defun find-on-disk (name)
  "Find on disk in <_:var *search-paths* /> the system directory
\(should start with <_:arg name />)"
  (labels ((last-part (regex string)
             (last1 (cl-ppcre:split regex string)))
           (get-subdirs (dir)
             (filter #`(when (cl-fad:directory-pathname-p _)
                         (last-part "/" (prin1-to-string
                                         (cl-fad:pathname-as-directory _))))
                     (cl-fad:list-directory dir))))
    (loop
       :with name := name
       :with paths := *search-paths*
       :while paths :nconc
       (let* ((dir (pop paths))
              (subdirs (get-subdirs dir)))
         (mapcar #`(pushnew _ paths :test #'string-equal) subdirs)
;         (remove-if-not #`(cl-ppcre:scan *known-extensions-scanner* (car _))
         (filter #`(when (cl-ppcre:scan (strcat "(?i)^" name) _)
                     (list (strcat dir "/" _)
                           (last-part "(-)|(_)" _)))
                 subdirs)))))

(defvar *known-extensions-scanner* (cl-ppcre:make-scanner "(?i).(dst)|(asd)$")
  "Scanner to find system definition files by extension")

;;;;;;;;;;???
(defun select-system (system &optional (strategy #'first-closest-version-system))
  "
Default <_:arg strategy /> is to select at least some system
with the <_:arg v />ersion, closest to the desired one.
Any other system selection strategy can be given"
  (funcall strategy (sys-name system) (sys-v system) (find-system name)))


;; system selection strategies

;; operation modes: :interactive and :automatic

(defun select-all-systems-by-versions (path-variants v name)
  "Interactive"
  (if-it (remove-if-not #`(string= (second _) v) path-variants :key #'second)
         (nconc it
                (sort (remove it path-variants #'string= :key #'second)
                      #'string> :key #'second))
         (sort path-variants #'string> :key #'second)))

(defun select-all-systems-matching-versions (path-variants v name)
  "Interactive"
  (remove-if-not #`(string= (second _) v) path-variants :key #'second))

(defun select-first-closest-version-system (name v path-variants)
  ""
  (car (or (find v path-variants :test #'string= :key #'second)
           (when-it (car (sort (remove-if-not #`(string> (second _) v) path-variants)
                               #'string< :key #'second))
             (warn-if (format nil "Couldn't find system ~a of version: ~a. Using version: ~a"
                              name v (second it)))
             it)
           (when-it (car (sort (remove-if-not #`(string< (second _) v) path-variants)
                               #'string> :key #'second))
             (warn-if (format nil "Couldn't find system ~a of version: ~a. Using version: ~a"
                              name v (second it)))
             it))))

(defun select-first-matching-version-system (name v path-variants)
  ""
  (or (find v path-variants :test #'string= :key #'second)
      (and (warn-if (format nil "No source for version ~a of system ~a found."
                            v name))
           nil)))


;; download


;; extract & install
;; taken from ASDF-INSTALL

(defparameter *tar-program*
  (progn
    "tar"
    #+darwin "gnutar"
    #+(or sunos netbsd) "gtar")
  "")

(defun get-tar-directory (packagename)
  ""
  (let* ((tar (with-output-to-string (o)
                (or (sb-ext:run-program *tar-program*
                                        (list "-tzf" (namestring packagename))
                                        :output o
                                        :search t
                                        :wait t)
                    (error "can't list archive"))))
         (first-line (subseq tar 0 (position #\Newline tar))))
    (if (find #\/ first-line)
        (subseq first-line 0 (position #\/ first-line))
        first-line)))

(defun untar-package (source packagename)
  ""
  (with-output-to-string (o)
    (or (sb-ext:run-program *tar-program*
                            (list "-C" (namestring source)
                                  "-xzvf" (namestring packagename)) ; j = bzip2
                            :output o
                            :search t
                            :wait t)
        (error "can't untar"))))

(defun install-package (source system packagename)
  "Returns a list of asdf system names for installed asdf systems"
  (ensure-directories-exist source)
  (ensure-directories-exist system)
  (let* ((tdir (get-tar-directory packagename))
         (*default-pathname-defaults*
          (merge-pathnames (make-pathname :directory `(:relative ,tdir))
                           source)))
    (princ (untar-package source packagename))
    (loop for asd in (directory
                      (make-pathname :name :wild :type "asd"))
          do (let ((target (merge-pathnames
                            (make-pathname :name (pathname-name asd)
                                           :type (pathname-type asd))
                            system)))
               (when (probe-file target)
                 (sb-posix:unlink target))
               #-win32
               (sb-posix:symlink asd target))
          collect (pathname-name asd))))


;; perform methods

(defgeneric perform (op system &key &allow-other-keys)
  (:documentation "Perform <_:arg op />erations on <_:arg system />s"))

(defmethod perform ((op (eql :check)) (system standard-system) &key &allow-other-keys)
  (labels ((rec-check (parts path)
             (every #'identity
                    (mapcar #`(and (probe-file (strcat path "/" (car part)))
                                   (if (listp (cadr part))  ; recurse into subsystems
                                       (rec-verify (cadr part))
                                       t))
                            parts))))
    (block check
      (let ((parts (sys-parts system)))
        (mapc #`(when (rec-check parts (car _))
                  (return-from check (car _)))
              system))
      nil)))

(macrolet ((processing-files (op stamp)
             `(let ((cur-path (or (sys-path system)
                                  (select-system system)))
                    (cur-stamp (sys-stamp system))
                    (new-stamp (get-universal-time)))
                (when (and cur-stamp (> cur-stamp new-stamp))
                  (warn-if "Previous timestamp exceeds current time"))
                (if (perform :check system)
                    (progn (process-files ,op
                                          (slot-value system 'parts-order)
                                          cur-path
                                          :stamp ,stamp)
                           (with-slots (stamp path) system
                             (setf stamp new-stamp
                                   path cur-path))
                           (values t
                                   cur-path))
                    (error 'distro-error
                           :msg (format nil "Verification failed for system: ~a"
                                        (sys-name system)))))))
  
  (defmethod perform ((op (eql :compile)) (system standard-system) &key &allow-other-keys)
    (processing-files 'compile-file (or cur-stamp +future+)))

  (defmethod perform ((op (eql :recompile)) (system standard-system) &key &allow-other-keys)
    (processing-files 'compile-file 0))

  (defmethod perform ((op (eql :load)) (system standard-system) &key &allow-other-keys)
    (processing-files 'load (or cur-stamp +future+)))

  (defmethod perform ((op (eql :reload)) (system standard-system) &key &allow-other-keys)
    (processing-files 'load 0)))

(defmethod perform ((op (eql :test)) (system standard-system) &key &allow-other-keys)
  (error "Need to define system-specific test method"))

#+nil
(defmethod perform ((op (eql :update)) (system standard-system) &key &allow-other-keys)
    "Update the system from canonical link. If <_:arg v /> is given,
update for that version, otherwise -- for latest"
    (download (sys-name system)


(defmethod perform ((op (eql :update-deps)) (system standard-system) &key &allow-other-keys)
    (dolist (dep (slot-value system 'deps-order))
   )

(defun op (op sys-name &rest args)
  "A shortcut to <_:fun perform />"
  (apply #'perform op (find-system sys-name) args))

(defmacro def-op-hook ((qualifier method system) &body body)
  "Define modifier methods (standard method combination)"
  `(defmethod ,qualifier ((op (eql ,method)) (system (eql ,system)) &key &allow-other-keys)
     ,@body))


;; files processing

(defun process-files (op file-list path &key stamp force ext ext-args)
  "Apply <_:arg op /> to files from <_:arg file-list /> list of names,
located by <_:arg path />, when the last modification time of the appropriate"
  (flet ((fullname (path name)
           (strcat path "/" name ".lisp")))
    (loop :with processed-nodes
       :for node :in file-list
       :for name := (car node)
       :for pathspec := (fullname path name)
       :for need := (or force (find (by-key node :on) processed-nodes))
       ;; after sorting we know, where to start, but since the order is incomplete, there may be files,
       ;; that should not be touched on the way: if their immediate deps are not in processed files

       :for ext := (or ext (by-key node :by)) :and ext-args := (or ext-args (by-key node :with))
       :do (cond ((listp (cadr node))  ; it's a subsystem
                  (when (process-files op
                                       (cadr node)
                                       (fullname path name)
                                       :stamp stamp :force need
                                       :ext ext :ext-args ext-args)
                    (push name processed-nodes)))
                 (ext  ; process with external processor
                  (handler-case
                      (progn (apply ext op pathspec stamp need ext-args)
                             (push name processed-nodes))
                    (error () (error 'distro-error
                                     :msg (format nil
                                                  "Error running external processor ~a with ~a"
                                                  ext ext-args)))))
                 ((or need  ; for processing complete subsystem
                      (< (file-modify-date (compile-file-pathname pathspec)) stamp))
                  (funcall op pathspec)
                  (push name processed-nodes))))))


#|
;; platforms

(defvar *platform* nil
  "The OS or VM, on which our Lisp runs")

(defvar *platforms* (make-hash-table)
  "Table of external package inquiry methods, specific to each platform")

(eval-always
  (setf (gethash :debian *platforms*)
        (lambda (pkg-str v)
          (let ((stream (get-output (strcat_ "dpkg -l" pkg-str))))
            (unless (and (equal (read stream) "ii")
                         (equal ())
|#


;; conditions

(define-condition distro-error (error)
  ((msg :initarg msg))
  (:documentation "A base class for distro related conditions")
  (:report (lambda (condition stream)
             (format stream "~a" msg))))


;;; end