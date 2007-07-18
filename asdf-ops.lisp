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
        (file (namestring (merge-pathnames (asdf:component-pathname comp))))
        (*readtable* (make-instrumented-readtable)))
    (signal-macroexpansion *user-hook* file 'file-component)
    (noticing-*feature*-changes
     (call-next-method))))

(defmethod asdf:perform :around ((op asdf:compile-op) (comp instrumented-cl-source-file))
  (let* ((*current-component* comp)
         (file (namestring (merge-pathnames (asdf:component-pathname comp))))
         (*readtable* (make-instrumented-readtable)))
    (signal-macroexpansion *user-hook* file 'file-component)
    (noticing-*feature*-changes
     (call-next-method))))

;;; TODO for asdf-component/op:
;;; * ignore component-name. I have no idea what it /should/ indicate.

(defclass component-file (asdf:source-file)
     ((load-system :initarg :load-systems)
      (merge-systems :initarg :merge-systems)
      (cull-redundant :initarg :cull-redundant :initform nil)
      (verbose :initarg :verbose :initform t)
      (output-file :initarg :output-file)
      (base-pathname :initarg :base-pathname)
      (inherit-from :initarg :inherit-from :initform nil)
      (additional-initargs :initarg :additional-initargs :initform nil
                           :documentation
                           #.(format nil "A list of mappings from ~
                           components in systems to additional initargs that the ~
                           instrumented components should receive. E.g.:
                           ((:foo-system (\"module\" \"component-name\") :additional-dependencies ())
                            (:foo-system (\"component2\") :data-files ())"))))

(defun ancestor-component (component)
  (asdf:find-component (asdf:component-parent component)
                       (slot-value component 'inherit-from)))

(defun inherited-slot-values (slot-name component)
  (when (slot-value component 'inherit-from)
    (let ((ancestor (ancestor-component component)))
      (append (slot-value ancestor slot-name)
              (inherited-slot-values slot-name ancestor)))))

(defclass dependency-op (asdf:operation)
     ((states :initform (make-hash-table :test #'equal)
              :reader states-of)))

(defmethod asdf:component-depends-on ((op dependency-op) (c component-file))
  (with-slots (inherit-from) c
     (when inherit-from
       `((dependency-op ,inherit-from)))))

(defun state-of (op component)
  (gethash component (states-of op)))

(defun (setf state-of) (new-val op component)
  (setf (gethash component (states-of op)) new-val))

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
    (with-slots (
                 load-system merge-systems
                 component-name-translation cull-redundant verbose
                 additional-initargs base-pathname) c
       ;; we require that none of the systems be loaded.
       (flet ((system-not-loaded-p (system)
                (null
                 (gethash (asdf::coerce-name system) asdf::*defined-systems*))))
         (unless (every #'system-not-loaded-p merge-systems)
           (cerror "Continue anyway." "Systems ~A are already loaded."
                   (remove-if #'system-not-loaded-p merge-systems))))
       (let ((*default-component-class* (find-class 'instrumented-cl-source-file)))
         (mapc #'asdf:find-system merge-systems))
       (labels ((find-component-in-module (module components)
                  (if (null (rest components))
                      (asdf:find-component module (first components))
                      (find-component-in-module (asdf:find-component module (first components))
                                                (rest components))))
                (add-initargs (system compspec args)
                  (let ((component (find-component-in-module
                                    (asdf:find-system system)
                                    compspec)))
                    (assert component (component)
                            "Component spec ~S in ~S didn't find a component."
                            compspec system)
                    (apply #'reinitialize-instance component
                           args))))
         (loop for (system compspec . initargs) in additional-initargs
               do (assert (member (asdf::coerce-name system) merge-systems :key #'asdf::coerce-name
                                  :test #'equal)
                          ()
                          "Component translation in System ~A which is not a member of the ~
                           systems to merge." system)
               do (add-initargs system compspec initargs)))
       
       (let ((merge-systems (append merge-systems (inherited-slot-values 'merge-systems c)))
             (initial-state (when (ancestor-component c)
                              (state-of op (ancestor-component c)))))
         (setf (state-of op c)
               (grovel-dependencies load-system component-stream
                                    :interesting (mapcar #'asdf:find-system merge-systems)
                                    :cull-redundant cull-redundant
                                    :verbose verbose
                                    :initial-state initial-state
                                    :base-pathname
                                    (or
                                     (and (slot-boundp c 'base-pathname)
                                          base-pathname)
                                     (truename
                                      (make-pathname :name nil
                                                     :type nil
                                                     :defaults
                                                     (asdf:component-pathname c))))))))))

;;; Reading the component list back into asdf defsystems


(macrolet
    ((define-comp-file-reader (fname (1-system-var return-var)
                                     &body 1-system-body)
         
         (let ((system-name (gensym))
               (system-names (gensym))
               (done-systems (gensym))
               (systems (gensym)))
           `(defun ,fname (pathname &rest ,system-names)
              (with-open-file (f pathname :direction :input)
                (let ((,systems (read f))
                      ,done-systems
                      ,return-var)
                  (labels ((do-1-system (,system-name)
                             (unless (position ,system-name ,done-systems
                                               :test #'string-equal)
                               (let ((,1-system-var (assoc ,system-name ,systems
                                                           :test #'string-equal)))
                                 (progn ,@1-system-body)
                                 (push ,system-name ,done-systems)
                                 (getf (cdr ,1-system-var) :depends-on)))))
                    (loop while ,system-names
                          for ,system-name = (pop ,system-names)
                          do (setf ,system-names (append ,system-names
                                                        (do-1-system ,system-name))))
                    ,return-var)))))))
  (define-comp-file-reader read-component-file (system component-list)
    (setf component-list 
          (append component-list (getf (cdr system) :components))))
  (define-comp-file-reader systems-in-configuration (system component-names)
    (when system
      (push (first system) component-names))))