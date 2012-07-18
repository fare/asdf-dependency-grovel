;;; -*- Mode: Lisp -*-

(cl:in-package :asdf)

#-asdf2
(error "ASDF-DEPENDENCY-GROVEL requires ASDF2.")

(unless (version-satisfies (asdf-version) "2.014.8")
  (error "Not only is your ASDF version is too old for ASDF-DEPENDENCY-GROVEL,
	you must upgrade it *before* you try to load any system."))

(defclass grovel-handlers (module)
     ((%components :accessor %handler-components)))

(defun handler-input-file-list (pathname parent)
  (mapcar
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
  :description "Analyse the dependencies in an ASDF system"
  :long-description "ASDF-DEPENDENCY-GROVEL will analyse the actual dependencies in an ASDF system.
Based on an analysis with file granularity ,
it can output an optimized system definition,
based on which compilation can be parallelized.
Based on an analysis with form granularity,
it can output a summary from which you can untangle
the circularities in your build."
  :version "1.106"
  :depends-on ((:version :asdf "2.21")) ;; for :encoding support
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
