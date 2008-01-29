;;; -*- Mode: Lisp -*-

(cl:defpackage #:asdf-dependency-grovel.system
  (:use :asdf :cl))
(cl:in-package #:asdf-dependency-grovel.system)

(defclass grovel-handlers (module)
     ((%components :accessor %handler-components)))

(defun handler-input-file-list (pathname parent)
  (map 'list
       (lambda (f)
         (make-instance 'cl-source-file
            :name (pathname-name f)
            :parent parent
            :pathname f))
       (sort (directory (make-pathname :defaults pathname
                                       :name :wild
                                       :type "lisp"
                                       :version :newest))
             #'string<
             :key #'namestring)))

(defmethod module-components ((c grovel-handlers))
  (if (slot-boundp c '%components)
      (%handler-components c)
      (setf (%handler-components c)
            (handler-input-file-list (component-pathname c) c))))

(defsystem asdf-dependency-grovel
  :components ((:file "package")
               (:file "variables" :depends-on ("package"))
               (:file "grovel" :depends-on ("package" "variables"))
               (grovel-handlers "handlers" :pathname #p"handlers/"
                                :depends-on ("grovel"))
               (:file "asdf-ops" :depends-on ("package" "variables" "grovel"))))

(defmethod perform :after ((op load-op) (c (eql (find-system :asdf-dependency-grovel))))
  (push :asdf-dependency-grovel *features*))
