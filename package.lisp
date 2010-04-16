#+xcvb (module ())

(cl:in-package :asdf)

(defpackage #:asdf-dependency-grovel
  (:use #:cl)
  (:export #:reload
           #:component-file
           #:dependency-op
           #:instrumented-cl-source-file
           #:instrumented-module
           #:instrumented-component
           #:additional-initargs
           #:read-component-file
           #:systems-in-configuration
           #:define-macroexpand-handlers
           #:signal-user
           #:signal-provider
           #:components-in-traverse-order
           #:with-constituent-groveling
           #:instrumented-load
           #:instrumented-compile-file
           #:fine-grain-instrumented-load
           #:print-big-ol-dependency-report))

(defpackage #:asdf-dependency-grovel.packages
  (:use))

(defpackage #:asdf-dependency-grovel.lambdas
  (:use))

(in-package #:asdf-dependency-grovel)

(defparameter *asdf-dependency-grovel-version* "1.101")
(defparameter *asdf-version-required-by-adg* "1.702")

#-asdf2
(error "ASDF-DEPENDENCY-GROVEL requires ASDF2.")

#+asdf2
(unless (asdf:version-satisfies (asdf:asdf-version) *asdf-version-required-by-adg*)
  (error "POIU ~A requires ASDF ~A or later."
         *asdf-dependency-grovel-version*
         *asdf-version-required-by-adg*))
