#+xcvb (module (:depends-on ("variables")))

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
                                 :reader overridden-dependencies)
        (additional-initargs :initarg :additional-initargs
                             :initform nil
                             :reader additional-initargs)))
  (defclass instrumented-cl-source-file (asdf:cl-source-file
                                         instrumented-component)
       ())
  (defclass instrumented-module (asdf:module instrumented-component)
       ()
    (:default-initargs :default-component-class 'instrumented-cl-source-file)))

(defmethod additional-initargs :around ((comp instrumented-component))
  (flet ((slot-when-bound (slot-name initarg)
           (when (slot-boundp comp slot-name)
             `(,initarg ,(slot-value comp slot-name)))))
    `(,@(call-next-method)
        ,@(slot-when-bound 'translated-name :translated-name)
        ,@(slot-when-bound 'translated-pathname :translated-pathname-form))))

