(cl:defpackage #:asdf-dependency-grovel
  (:use #:cl)
  (:export #:component-file #:dependency-op #:instrumented-cl-source-file
           #:define-symbol-alias #:instrumented-module))

(cl:defpackage #:asdf-dependency-grovel.packages
  (:use))