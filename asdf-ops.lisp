(cl:in-package #:asdf-dependency-grovel)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass instrumented-cl-source-file (asdf:cl-source-file)
       ((output-file-type :initarg :output-file-type
                          :reader output-file-type))))

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
  (let ((*current-component* comp))
    (call-next-method)))

(defmethod asdf:perform :around ((op asdf:compile-op) (comp instrumented-cl-source-file))
  (let* ((*current-component* comp))
    (call-next-method)))

;;; TODO for asdf-component/op:
;;; * ignore component-name. I have no idea what it /should/ indicate.

(defclass component-file (asdf:source-file)
     ((load-system :initarg :load-system)
      (merge-systems :initarg :merge-systems)
      (additional-dependencies :initarg :additional-dependencies :initform nil)
      (component-name-translation :initarg :component-name-translation :initform nil)
      (cull-redundant :initarg :cull-redundant :initform nil)
      (verbose :initarg :verbose :initform t)
      (output-file :initarg :output-file)))

(defclass dependency-op (asdf:operation)
     ())

(defmethod asdf:source-file-type ((c component-file) (s asdf:module))
  "asd")

(defmethod asdf:output-files ((op dependency-op) (c component-file))
  (list
   (merge-pathnames (slot-value c 'output-file)
                    (asdf:component-pathname c))))

(defmethod asdf:input-files ((op dependency-op) (c component-file))
  (asdf:component-pathname c))

(defmethod asdf:operation-done-p ((op dependency-op) (comp component-file))
  nil)

(defmethod asdf:perform ((op dependency-op) (c component-file))
  (with-open-file (clim-components (merge-pathnames (slot-value c 'output-file)
                                                    (asdf:component-pathname c))
                                   :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede)
    (with-slots (load-system merge-systems
                             component-name-translation cull-redundant verbose
                             additional-dependencies) c
       (grovel-dependencies load-system clim-components
                            :interesting (mapcar #'asdf:find-system merge-systems)
                            :component-name-translation component-name-translation
                            :additional-dependencies additional-dependencies
                            :cull-redundant cull-redundant
                            :verbose verbose))))