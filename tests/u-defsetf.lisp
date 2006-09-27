(cl:in-package :asdf-dependency-grovel-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (test-defsetf.1 (list 'a 'b)) 1))