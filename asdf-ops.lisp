(cl:in-package #:asdf-dependency-grovel)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass instrumented-component ()
       ((translated-name :initarg :translated-name
                         :reader translated-name)
        (translated-pathname :initarg :translated-pathname-form
                             :reader translated-pathname)
        (output-file-type :initarg :output-file-type
                          :reader output-file-type)
        (additional-dependencies :initarg :additional-dependencies
                                 :initform nil
                                 :reader additional-dependencies)
        (overridden-dependencies :initarg :override-dependencies
                                 :reader overridden-dependencies)))
  (defclass instrumented-cl-source-file (asdf:cl-source-file
                                         instrumented-component)
       ((additional-initargs :initarg :additional-initargs
                             :initform nil
                             :reader additional-initargs)))
  (defclass instrumented-module (asdf:module instrumented-component)
       ()
    (:default-initargs :default-component-class 'instrumented-cl-source-file)))

(defmethod asdf:output-files :around ((op asdf:compile-op) (comp instrumented-cl-source-file))
  "Put instrumented FASL files in a temporary directory relative
to the base of the system."
  (let* ((output-file (car (call-next-method)))
         (system-base-dir *default-pathname-defaults*)
         (dir-component (subseq (pathname-directory output-file)
                                (or (mismatch (pathname-directory output-file)
                                              (pathname-directory system-base-dir)
                                              :test #'equal)
                                    (length (pathname-directory output-file))))))
    
    (list
     (if (and (boundp '*old-macroexpand-hook*)
              *old-macroexpand-hook*)
         (merge-pathnames (make-pathname :directory `(,@(pathname-directory system-base-dir)
                                                        ,(format nil "asdf-dependency-grovel-tmp-~A"
                                                                 *grovel-dir-suffix*)
                                                        ,@dir-component)
                                         :defaults output-file)
                          system-base-dir)
         output-file))))

(defmethod asdf:perform :around ((op asdf:load-op) (comp instrumented-cl-source-file))
  (let ((*current-component* comp)
        (file (namestring (merge-pathnames (asdf:component-pathname comp)))))
    (signal-macroexpansion *user-hook* file 'file-component)
    (call-next-method)))

(defmethod asdf:perform :around ((op asdf:compile-op) (comp instrumented-cl-source-file))
  (let* ((*current-component* comp)
         (file (namestring (merge-pathnames (asdf:component-pathname comp)))))
    (signal-macroexpansion *user-hook* file 'file-component)
    (call-next-method)))

;;; TODO for asdf-component/op:
;;; * ignore component-name. I have no idea what it /should/ indicate.

(defclass component-file (asdf:source-file)
     ((load-system :initarg :load-system)
      (merge-systems :initarg :merge-systems)
      (cull-redundant :initarg :cull-redundant :initform nil)
      (verbose :initarg :verbose :initform t)
      (output-file :initarg :output-file)
      (base-pathname :initarg :base-pathname)))

(defclass dependency-op (asdf:operation)
     ())

(defmethod asdf:source-file-type ((c component-file) (s asdf:module))
  "asd")

(defmethod asdf:output-files ((op dependency-op) (c component-file))
  (list
   ;; XXX: base-pathname?
   (merge-pathnames (slot-value c 'output-file)
                    (asdf:component-pathname c))))

(defmethod asdf:input-files ((op dependency-op) (c component-file))
  ;; XXX: base-pathname?
  (asdf:component-pathname c))

(defmethod asdf:operation-done-p ((op dependency-op) (comp component-file))
  nil)

(defvar *default-component-class* (find-class 'asdf:cl-source-file))

;;; XXX: nasty hack.
;;; Necessary to support asd files that weren't rewritten to use
;;; instrumented-module/instrumented-cl-source-file classes.
(defmethod asdf::module-default-component-class :around ((c asdf:module))
  *default-component-class*)

(defmethod asdf:perform ((op dependency-op) (c component-file))
  (with-open-file (component-stream (car (asdf:output-files op c))
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede)
    (with-slots (load-system merge-systems
                             component-name-translation cull-redundant verbose
                             additional-dependencies
                             base-pathname) c
       ;; we need none of the systems to be loaded.
       (flet ((system-not-loaded-p (system)
                (null
                 (gethash (asdf::coerce-name system) asdf::*defined-systems*))))
         (unless (every #'system-not-loaded-p merge-systems)
           (error "Systems ~A are already loaded."
                  (remove-if #'system-not-loaded-p merge-systems))))
       (grovel-dependencies load-system component-stream
                            :interesting (let ((*default-component-class* (find-class 'instrumented-cl-source-file)))
                                           (mapcar #'asdf:find-system merge-systems))
                            :cull-redundant cull-redundant
                            :verbose verbose
                            :base-pathname
                            (or
                             (and (slot-boundp c 'base-pathname)
                                  base-pathname)
                             (truename
                              (make-pathname :name nil
                                             :type nil
                                             :defaults
                                             (asdf:component-pathname c))))))))