(cl:in-package :asdf-dependency-grovel-test)

(defstruct (test-defstruct/function-extended
             (:include test-defstruct/function))
  "This is a docstring."
  d e)
