;; -*- Mode: Lisp -*-

(cl:in-package :cl-user)

;; load groveler, for the component type, etc.
(eval-when (:load-toplevel :execute)
  (asdf:oos 'asdf:load-op :asdf-dependency-grovel))

(defpackage :asdf-dependency-grovel-test.system
  (:use :cl :asdf :asdf-dependency-grovel))

(in-package :asdf-dependency-grovel-test.system)

(defsystem :asdf-dependency-grovel-test/generator
  :components ((component-file :asdf-dependency-grovel-test/serial
                               :load-system :asdf-dependency-grovel-test/serial
                               :merge-systems (:asdf-dependency-grovel-test/serial)
                               :output-file "groveled-components.lisp"
                               :cull-redundant nil
                               :verbose nil)))

(defsystem :asdf-dependency-grovel-test/serial
  :default-component-class instrumented-cl-source-file
  :serial t
  :components (
               (:file "package")
               ;; simple stuff
               (:file "p-defmacro")
               (:file "p-define-method-combination")

               ;; setf
               (:file "p-define-setf-expander")
               (:file "p-defsetf")

               ;; clos
               (:file "p-defclass")
               (:file "p-defstruct")
               (:file "p-defgeneric")
               (:file "p-defmethod")
               (:file "p-define-condition")

               ;; package system
               (:file "p-defpackage")
               
               ;; defunoids / instrumented stuff
               (:file "p-defun")
               (:file "p-define-symbol-macro")
               (:file "p-defconstant")


               ;; users
               
               ;; simple stuff
               (:file "u-defmacro")
               (:file "u-define-method-combination")

               ;; setf
               (:file "u-define-setf-expander")
               (:file "u-defsetf")

               ;; clos
               (:file "u-defclass.defclass")
               (:file "u-defclass.defmethod")
               (:file "u-defstruct.defmethod")
               (:file "u-defgeneric")
               (:file "u-defmethod")
               (:file "u-define-condition")

               ;; package system
               (:file "u-defpackage")
               (:file "u-defpackage.1")
               
               ;; defunoids / instrumented stuff
               (:file "u-defun")
               (:file "u-define-symbol-macro")
               (:file "u-defconstant")))