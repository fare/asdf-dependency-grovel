;;; This file defines the package in which the tests are defined (do
;;; not confuse this with the package in which the test framework
;;; resides; I don't want to deal with accidental spillover from
;;; there)
(cl:defpackage :asdf-dependency-grovel-test
  (:use :cl))