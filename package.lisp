#+xcvb (module ())

(cl:defpackage #:asdf-dependency-grovel
  (:use #:cl)
  (:export #:reload
           #:component-file #:dependency-op ;#:compare-dependency-op
           #:instrumented-cl-source-file
           #:instrumented-module
           #:instrumented-component #:additional-initargs
           #:read-component-file #:systems-in-configuration
           #:handle-macroexpansion #:define-macroexpand-handlers
           #:define-simple-macroexpand-handlers
           #:signal-user #:signal-provider
           #:with-dependency-tracking
           ;; feature set when groveling:
           #:groveling
           ;; Added by msteele:
           ;#:with-non-asdf-groveling
           #:instrumented-load #:instrumented-compile-file
           #:print-big-ol-dependency-report))

(cl:defpackage #:asdf-dependency-grovel.packages
  (:use))

(cl:defpackage #:asdf-dependency-grovel.lambdas
  (:use))
