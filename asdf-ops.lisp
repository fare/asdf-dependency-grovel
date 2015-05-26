#+xcvb (module (:depends-on ("variables" "grovel")))

(cl:in-package #:asdf-dependency-grovel)

(defmethod output-files :around ((op compile-op) (comp instrumented-cl-source-file))
  "Put instrumented FASL files in a temporary directory relative
to the base of the system."
  (let* ((output-file (car (call-next-method)))
         (system-base-dir (or *system-base-dir* *default-pathname-defaults*))
         (dir-component (subseq (pathname-directory output-file)
                                (or (mismatch (pathname-directory output-file)
                                              (pathname-directory system-base-dir)
                                              :test #'equal)
                                    (length (pathname-directory output-file))))))
    (list
     (if (or *current-constituent*
             (and (boundp '*old-macroexpand-hook*) *old-macroexpand-hook*))
         (merge-pathnames
          (make-pathname :directory `(,@(pathname-directory system-base-dir)
                                      ,(format nil "asdf-dependency-grovel-tmp-~A"
                                               *grovel-dir-suffix*)
                                      ,@dir-component)
                         :defaults output-file)
          system-base-dir)
         output-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dependency Tracking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Used only by with-dependency-tracking.
(defun call-with-dependency-tracking (comp thunk)
  (if *current-constituent*
      (operating-on-asdf-component-constituent (comp)
        (with-groveling-readtable
          (with-groveling-macroexpand-hook
            (funcall thunk))))
      (funcall thunk)))
;;       (if *current-dependency-state*
;;           (operating-on-component (comp)
;;             (let ((file (namestring (merge-pathnames
;;                                      (component-pathname comp))))
;;                   (*readtable* (make-instrumented-readtable)))
;;               (signal-user file 'file-component)
;;               (noticing-*feature*-changes
;;                (multiple-value-prog1
;;                    (funcall thunk)
;;                  (signal-new-internal-symbols)))))
;;           (funcall thunk))))

;; Used only by emit-perform-method.
(defmacro with-dependency-tracking (comp &body body)
  `(call-with-dependency-tracking ,comp #'(lambda () ,@body)))

#|
(macrolet ((emit-perform-method (op-type)
             `(defmethod perform :around
                  ((op ,op-type)
                   (comp instrumented-cl-source-file))
                (with-dependency-tracking comp (call-next-method)))))
  (emit-perform-method load-source-op)
  (emit-perform-method load-op)
  (emit-perform-method compile-op))
|#

(defmethod perform
    ((op load-source-op)
     (comp instrumented-cl-source-file))
  (wtf "Perform load-source-op ~S" comp)
  (let ((source (component-pathname comp)))
    (operating-on-asdf-component-constituent (comp)
      ;; do NOT grovel the same file more than once
      (unless (asdf::component-operation-time 'load-source-op comp)
        (#+sbcl fine-grain-instrumented-load #-sbcl instrumented-load
         source)))))

(defmethod perform :around
    ((op compile-op)
     (comp instrumented-cl-source-file))
  nil)

(defmethod perform :around
    ((op load-op)
     (comp instrumented-cl-source-file))
  (perform (make-instance 'load-source-op) comp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO for asdf-component/op:
;;; * ignore component-name. I have no idea what it /should/ indicate.

;; Used by XCVB:
(defclass component-file (source-file)
  ((last-grovel-state :initform nil)
   (load-system :initarg :load-systems)
   (merge-systems :initarg :merge-systems)
;;    (cull-redundant :initarg :cull-redundant :initform nil)
   (verbose :initarg :verbose :initform t)
   (output-file :initarg :output-file)
   (base-pathname :initarg :base-pathname)
;;    (debug-object-types :initarg :debug-object-types :initform nil)
   (base-asd-file :initarg :base-asd-file :initform nil)
   (additional-initargs
    :initarg :additional-initargs :initform nil
    :documentation
    #.(format nil "A list of mappings from ~
     components in systems to additional initargs that the ~
     instrumented components should receive. E.g.:
     ((:foo-system (\"module\" \"component-name\") :additional-dependencies ())
      (:foo-system (\"component2\") :data-files ())"))))

;; Used by XCVB.
(defclass dependency-op (load-source-op)
  ((selfward-operation :initform 'prepare-dependency-op)))
(defclass prepare-dependency-op (prepare-source-op)
  ((sideway-operation :initform 'dependency-op)))

(defun state-of (op component)
  (declare (ignore op))
  (slot-value component 'last-grovel-state))

(defun (setf state-of) (new-val op component)
  (declare (ignore op))
  (setf (slot-value component 'last-grovel-state) new-val))

(defmethod output-files ((op dependency-op) (c component-file))
  (list
   ;; XXX: base-pathname?
   (merge-pathnames (slot-value c 'output-file)
                    (component-pathname c))))

(defmethod input-files ((op dependency-op) (c component-file))
  ;; XXX: base-pathname?
  (list (component-pathname c)))

(defmethod operation-done-p ((op dependency-op) (comp component-file))
  nil)

(defvar *default-component-class* (find-class 'cl-source-file))

;;; XXX: nasty hack.
;;; Necessary to support asd files that weren't rewritten to use
;;; instrumented-module/instrumented-cl-source-file classes.
(defmethod asdf::module-default-component-class :around ((c module))
  (let ((what-would-asdf-do (call-next-method)))
    (if (member what-would-asdf-do `(nil cl-source-file
                                         ,(find-class 'cl-source-file)))
        *default-component-class*
        what-would-asdf-do)))

;; Used by perform.
(defun load-instrumented-systems (systems additional-initargs)
  (let ((*default-component-class* (find-class 'instrumented-cl-source-file)))
    (flet ((reload-system (system)
             (let ((system (find-system system)))
               (load (system-definition-pathname system)))))
      (mapc #'reload-system systems)))
  (labels ((find-component-in-module (module components)
             (if (null (rest components))
                 (find-component module (first components))
                 (find-component-in-module (find-component
                                            module (first components))
                                           (rest components))))
           (add-initargs (system compspec args)
             (let ((component (find-component-in-module
                               (find-system system)
                               compspec)))
               (assert component (component)
                       "Component spec ~S in ~S didn't find a component."
                       compspec system)
               (apply #'reinitialize-instance component args))))
    (loop :for (system compspec . initargs) in additional-initargs :do
      (assert (member (coerce-name system) systems
                      :key #'coerce-name
                      :test #'equal)
              ()
              "Component translation in System ~A which is not a member of the ~
               systems to merge." system)
      (add-initargs system compspec initargs))))

(defmethod perform ((op dependency-op) (c component-file))
  (let* ((destination-file (first (output-files op c)))
         (tmp-file-name (format nil "~A-~A"
                                 destination-file
                                 (get-universal-time))))
    (ensure-directories-exist tmp-file-name)
    (with-open-file (component-stream tmp-file-name
                                      :direction :output
                                      :if-does-not-exist :create
                                      :if-exists :supersede)
      (with-slots (load-system merge-systems
                   component-name-translation verbose ;cull-redundant
                   ; debug-object-types
                   additional-initargs base-pathname) c
         (let ((base-pathname
                (or (and (slot-boundp c 'base-pathname)
                         base-pathname)
                    (truename (make-pathname :name nil
                                             :type nil
                                             :defaults
                                             (component-pathname c))))))
           (load-instrumented-systems merge-systems additional-initargs)
           (setf (state-of op c)
                 (if (state-of op c)
                     (error "I refuse to re-grovel.")
;;                      (re-grovel-dependencies
;;                       (mapcar #'find-system
;;                               (if (consp load-system)
;;                                   load-system
;;                                   (list load-system)))
;;                       component-stream
;;                       (mapcar #'find-system merge-systems)
;;                       (state-of op c)
;;                       :verbose verbose
;;                       :debug-object-types debug-object-types
;;                       :base-pathname base-pathname)
                     (initially-grovel-dependencies
                      (mapcar #'find-system
                              (if (consp load-system)
                                  load-system
                                  (list load-system)))
                      component-stream
                      (mapcar #'find-system merge-systems)
                      :verbose verbose
;;                      :cull-redundant cull-redundant
;;                      :debug-object-types debug-object-types
                      :base-pathname base-pathname))))))
    #+clisp ;; But for a bug in CLISP 2.48, we should use :if-exists :overwrite and be atomic
    (posix:copy-file tmp-file-name destination-file :method :rename)
    #-clisp
    (rename-file tmp-file-name destination-file
                 #+clozure :if-exists #+clozure :rename-and-delete)))

;;; Reading the component list back into asdf defsystems

(defun %comp-file-reader (pathname system-names collector)
  (with-open-file (f pathname :direction :input)
    (let ((systems (read f))
          (done-systems nil))
      (labels ((do-1-system (system-name)
                 (unless (position system-name done-systems
                                   :test #'string-equal)
                   (let ((system (assoc system-name systems
                                              :test #'string-equal)))
                     (funcall collector system)
                     (push system-name done-systems)
                     (getf (cdr system) :depends-on)))))
        (loop :while system-names :do
          (let ((system-name (pop system-names)))
            (setf system-names (append system-names (do-1-system system-name)))))))))

(defun read-component-file (pathname &rest system-names)
  (while-collecting (c)
    (%comp-file-reader
     pathname system-names
     (lambda (system)
       (map () #'c (getf (cdr system) :components))))))

(defun systems-in-configuration (pathname &rest system-names)
  (let ((component-names nil))
    (%comp-file-reader
     pathname system-names
     (lambda (system)
       (when system
         (push (first system) component-names))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
;; Used by qres's lisp/make-qres-dependencies.lisp,
;; needs code deleted in 3df5023fc5ebae41141996861df44d471b1fb539 to be reinstated.
(defclass compare-dependency-op (dependency-op) ())

(defmethod input-files ((op compare-dependency-op) (c component-file))
  (append (call-next-method) (list (slot-value c 'base-asd-file))))

(defmethod output-files ((op compare-dependency-op) (c component-file))
  (append (call-next-method) (list (slot-value c 'output-file))))

(defmethod perform ((op compare-dependency-op) (c component-file))
  ;; not incremental yet (but it's mostly useless anyway for large systems?)
  (with-slots (load-system merge-systems base-asd-file output-file
               component-name-translation cull-redundant verbose
               additional-initargs base-pathname debug-object-types) c
    (let* ((base-pathname (or (and (slot-boundp c 'base-pathname)
                                   base-pathname)
                              (truename
                               (make-pathname :name nil
                                              :type nil
                                              :defaults
                                              (component-pathname c)))))
           (out-pathname (pathname (first (output-files op c))))
           (tmp-pathname (make-pathname
                           :name (format nil "~A-~A" (pathname-name out-pathname) (get-universal-time))
                           :defaults out-pathname)))
       (load-instrumented-systems merge-systems additional-initargs)
       (prog1
           (grovel-and-compare-dependencies (mapcar #'find-system
                                                    (if (consp load-system)
                                                      load-system
                                                      (list load-system)))
                                            base-asd-file
                                            (mapcar #'find-system merge-systems)
                                            :output tmp-pathname
                                            :verbose verbose
                                            :cull-redundant cull-redundant
                                            :debug-object-types debug-object-types
                                            :base-pathname base-pathname))
       (rename-file tmp-pathname out-pathname))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
