(cl:in-package :asdf-dependency-grovel-test)

(defvar *function-defstruct-1* (make-test-defstruct/function :a 2 :b 3 :c 4))

(test-defstruct/function-a *function-defstruct-1*)
