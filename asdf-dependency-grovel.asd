;;; -*- Mode: Lisp -*-

(cl:defpackage #:asdf-dependency-grovel.system
  (:use :asdf :cl))
(cl:in-package #:asdf-dependency-grovel.system)

(defsystem asdf-dependency-grovel
  :components ((:file "package")
               (:file "variables" :depends-on ("package"))
               (:file "grovel" :depends-on ("package" "variables"))
               (:file "asdf-ops" :depends-on ("package" "variables" "grovel"))))