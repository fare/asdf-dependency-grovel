(cl:in-package :asdf-dependency-grovel-test)

(etypecase (list 1 2 3)
  (test-defstruct/function t)
  (cons nil))
