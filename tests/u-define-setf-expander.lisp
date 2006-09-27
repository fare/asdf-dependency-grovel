(cl:in-package :asdf-dependency-grovel-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar a (list 'a 'b 'c 'd))
  (setf (test-define-setf-expander.1 a) 3))