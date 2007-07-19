;; -*- Mode: Lisp -*-

(cl:in-package :cl-user)

;; load groveler, for the component type, etc.
(eval-when (:load-toplevel :execute)
  (asdf:oos 'asdf:load-op :asdf-dependency-grovel))

(defpackage :asdf-dependency-grovel-test.system
  (:use :cl :asdf :asdf-dependency-grovel))

(in-package :asdf-dependency-grovel-test.system)

(defsystem :test-serial
  :components ((component-file :asdf-dependency-grovel-test/serial
                               :load-systems (:test-serial-system)
                               :merge-systems (:test-serial-system)
                               :output-file "groveled-components.lisp"
                               :cull-redundant nil
                               :verbose nil)))

