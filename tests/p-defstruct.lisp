(cl:in-package :asdf-dependency-grovel-test)

(defstruct test-defstruct)

(defstruct test-defstruct/function
  "This is a docstring."
  (a 0 :type integer) b c)

(defstruct (test-defstruct/quux (:conc-name test-defstruct/q-))
  x y z)
