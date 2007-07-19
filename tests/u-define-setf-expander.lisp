(cl:in-package :asdf-dependency-grovel-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar a-setf (list 'a 'b 'c 'd))
  (setf (test-define-setf-expander.1 a-setf) 3))
