#+xcvb (module (:depends-on ("variables" "grovel")))

(cl:in-package #:asdf-dependency-grovel)

(defmethod asdf:output-files :around ((op asdf:compile-op) (comp instrumented-cl-source-file))
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
        ;((merge-pathnames (asdf:component-pathname comp)))
        (with-groveling-readtable
          (with-groveling-macroexpand-hook
            (funcall thunk))))
      (funcall thunk)))
;;       (if *current-dependency-state*
;;           (operating-on-component (comp)
;;             (let ((file (namestring (merge-pathnames
;;                                      (asdf:component-pathname comp))))
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
             `(defmethod asdf:perform :around
                  ((op ,op-type)
                   (comp instrumented-cl-source-file))
                (with-dependency-tracking comp (call-next-method)))))
  (emit-perform-method asdf:load-source-op)
  (emit-perform-method asdf:load-op)
  (emit-perform-method asdf:compile-op))
|#

(defmethod asdf:perform
    ((op asdf:load-source-op)
     (comp instrumented-cl-source-file))
  (wtf "Perform asdf:load-source-op ~S" comp)
  (let ((source (asdf:component-pathname comp)))
    ;; do NOT grovel the same file more than once
    (unless (asdf:component-property comp 'last-loaded-as-source)
      (setf (asdf:component-property comp 'last-loaded-as-source)
            (and (fine-grain-instrumented-load ;;load
                  source)
                 (get-universal-time))))))

(defmethod asdf:perform :around
    ((op asdf:compile-op)
     (comp instrumented-cl-source-file))
  nil)

(defmethod asdf:perform :around
    ((op asdf:load-op)
     (comp instrumented-cl-source-file))
  (asdf:perform (make-instance 'asdf:load-source-op) comp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO for asdf-component/op:
;;; * ignore component-name. I have no idea what it /should/ indicate.

;; Used by XCVB:
(defclass component-file (asdf:source-file)
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
(defclass dependency-op (asdf:operation) ())

(defun state-of (op component)
  (declare (ignore op))
  (slot-value component 'last-grovel-state))

(defun (setf state-of) (new-val op component)
  (declare (ignore op))
  (setf (slot-value component 'last-grovel-state) new-val))

(defmethod asdf:source-file-type ((c component-file) (s asdf:module))
  "asd")

(defmethod asdf:output-files ((op dependency-op) (c component-file))
  (list
   ;; XXX: base-pathname?
   (merge-pathnames (slot-value c 'output-file)
                    (asdf:component-pathname c))))

(defmethod asdf:input-files ((op dependency-op) (c component-file))
  ;; XXX: base-pathname?
  (list (asdf:component-pathname c)))

(defmethod asdf:operation-done-p ((op dependency-op) (comp component-file))
  nil)

(defvar *default-component-class* (find-class 'asdf:cl-source-file))

;;; XXX: nasty hack.
;;; Necessary to support asd files that weren't rewritten to use
;;; instrumented-module/instrumented-cl-source-file classes.
(defmethod asdf::module-default-component-class :around ((c asdf:module))
  (let ((what-would-asdf-do (call-next-method)))
    (if (member what-would-asdf-do `(nil asdf:cl-source-file
                                         ,(find-class 'asdf:cl-source-file)))
        *default-component-class*
        what-would-asdf-do)))

;; Used by asdf:perform.
(defun load-instrumented-systems (systems additional-initargs)
  (let ((*default-component-class* (find-class 'instrumented-cl-source-file)))
    (flet ((reload-system (system)
             (let ((system (asdf:find-system system)))
               (load (asdf:system-definition-pathname system)))))
      (mapc #'reload-system systems)))
  (labels ((find-component-in-module (module components)
             (if (null (rest components))
                 (asdf:find-component module (first components))
                 (find-component-in-module (asdf:find-component
                                            module (first components))
                                           (rest components))))
           (add-initargs (system compspec args)
             (let ((component (find-component-in-module
                               (asdf:find-system system)
                               compspec)))
               (assert component (component)
                       "Component spec ~S in ~S didn't find a component."
                       compspec system)
               (apply #'reinitialize-instance component args))))
    (loop for (system compspec . initargs) in additional-initargs
          do (assert (member (asdf::coerce-name system) systems
                             :key #'asdf::coerce-name
                             :test #'equal)
                     ()
                     "Component translation in System ~A which is not a member of the ~
                           systems to merge." system)
          do (add-initargs system compspec initargs))))

(defmethod asdf:perform ((op dependency-op) (c component-file))
  (let ((tmp-file-name (format nil "~A-~A"
                               (first (asdf:output-files op c))
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
                                             (asdf:component-pathname c))))))
           (load-instrumented-systems merge-systems additional-initargs)
           (setf (state-of op c)
                 (if (state-of op c)
                     (error "I refuse to re-grovel.")
;;                      (re-grovel-dependencies
;;                       (mapcar #'asdf:find-system
;;                               (if (consp load-system)
;;                                   load-system
;;                                   (list load-system)))
;;                       component-stream
;;                       (mapcar #'asdf:find-system merge-systems)
;;                       (state-of op c)
;;                       :verbose verbose
;;                       :debug-object-types debug-object-types
;;                       :base-pathname base-pathname)
                     (initially-grovel-dependencies
                      (mapcar #'asdf:find-system
                              (if (consp load-system)
                                  load-system
                                  (list load-system)))
                      component-stream
                      (mapcar #'asdf:find-system merge-systems)
                      :verbose verbose
;;                      :cull-redundant cull-redundant
;;                      :debug-object-types debug-object-types
                      :base-pathname base-pathname))))))
    (rename-file tmp-file-name (first (asdf:output-files op c)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Unused.
;; (defclass compare-dependency-op (dependency-op) ())

;; ;; Unused.
;; (defmethod asdf:input-files ((op compare-dependency-op) (c component-file))
;;   (append (call-next-method) (list (slot-value c 'base-asd-file))))

;; ;; Unused.
;; (defmethod asdf:output-files ((op compare-dependency-op) (c component-file))
;;   (append (call-next-method) (list (slot-value c 'output-file))))

;; ;; Unused.
;; (defmethod asdf:perform ((op compare-dependency-op) (c component-file))
;;   ;; not incremental yet (but it's mostly useless anyway for large systems?)
;;   (with-slots (load-system merge-systems base-asd-file output-file
;;                component-name-translation cull-redundant verbose
;;                additional-initargs base-pathname debug-object-types) c
;;     (let* ((base-pathname (or (and (slot-boundp c 'base-pathname)
;;                                    base-pathname)
;;                               (truename
;;                                (make-pathname :name nil
;;                                               :type nil
;;                                               :defaults
;;                                               (asdf:component-pathname c)))))
;;            (out-pathname (pathname (first (asdf:output-files op c))))
;;            (tmp-pathname (make-pathname
;;                            :name (format nil "~A-~A" (pathname-name out-pathname) (get-universal-time))
;;                            :defaults out-pathname)))
;;        (load-instrumented-systems merge-systems additional-initargs)
;;        (prog1
;;            (grovel-and-compare-dependencies (mapcar #'asdf:find-system
;;                                                     (if (consp load-system)
;;                                                       load-system
;;                                                       (list load-system)))
;;                                             base-asd-file
;;                                             (mapcar #'asdf:find-system merge-systems)
;;                                             :output tmp-pathname
;;                                             :verbose verbose
;;                                             :cull-redundant cull-redundant
;;                                             :debug-object-types debug-object-types
;;                                             :base-pathname base-pathname))
;;        (rename-file tmp-pathname out-pathname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
