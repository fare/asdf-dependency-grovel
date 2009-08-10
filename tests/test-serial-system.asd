;;; -*- lisp -*-

(asdf:defsystem :test-serial-system
  :serial t
  :components ((:file "package")

               ;; simple stuff
               (:file "p-defmacro")
               (:file "p-define-method-combination")
               (:file "p-feature")

               ;; defvar
               (:file "p-defvar")

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

               ;; deftype
               (:file "p-deftype")

;;; users

               ;; simple stuff
               (:file "u-defmacro")
               (:file "u-defmacro.indirect")
               (:file "u-define-method-combination")
               (:file "u-feature")

               ;; defvar
               (:file "u-defvar")
               (:file "u-defvar.vector")
               (:file "u-defvar.setf")
               (:file "u-defvar.push")

               ;; setf
               (:file "u-define-setf-expander")
               (:file "u-defsetf")

               ;; clos
               (:file "u-defclass.defclass")
               (:file "u-defclass.defmethod")
               (:file "u-defgeneric")
               (:file "u-defmethod")
               (:file "u-define-condition")

               ;; structs
               (:file "u-defstruct")
               (:file "u-defstruct.type")
               (:file "u-defstruct.defmethod")
               (:file "u-defstruct.functions")
               (:file "u-defstruct.functions.sharpquote")
               (:file "u-defstruct.include")

               ;; package system
               (:file "u-defpackage")
               (:file "u-defpackage.1")
               (:file "u-defpackage.2")

               ;; defunoids / instrumented stuff
               (:file "u-defun")
               (:file "u-defun.sharpquote")
               (:file "u-define-symbol-macro")
               (:file "u-defconstant")

               ;; deftypes
               (:file "u-deftype.foo")
               (:file "u-deftype.bar")
               (:file "u-deftype.satisfies")
               (:file "u-deftype.etypecase")
               (:file "u-deftype.typecase")))
