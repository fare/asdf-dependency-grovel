(cl:in-package :asdf-dependency-grovel-test)

(eval-when (:compile-toplevel)
  (defvar *function-defstruct-2* (make-test-defstruct/function :a 2 :b 3 :c 4))
  (test-defstruct/function-a *function-defstruct-2*))
