;;; -*- Mode: Lisp -*-

(cl:in-package :asdf)

(unless (or #+asdf2 (asdf:version-satisfies (asdf:asdf-version) "2.014.8"))
  (error "Not only is your ASDF version is too old for ASDF-DEPENDENCY-GROVEL,
	you must upgrade it *before* you try to load any system."))

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

(defsystem :asdf-dependency-grovel
  :depends-on ((:version :asdf "2.017"))
  :components ((:file "package")
               (:file "variables" :depends-on ("package"))
               (:file "classes" :depends-on ("package" "variables"))
               (:file "asdf-classes" :depends-on ("package"))
               (:file "grovel" :depends-on ("package" "variables" "classes" "asdf-classes"))
               (:file "asdf-ops" :depends-on ("package" "variables" "grovel"))
               (:grovel-handlers "handlers" :pathname #p"handlers/"
                                :depends-on ("grovel"))))

(defmethod perform :after ((op load-op) (c (eql (find-system :asdf-dependency-grovel))))
  (push :asdf-dependency-grovel *features*))
