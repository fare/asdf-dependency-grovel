(cl:in-package :asdf-dependency-grovel-test)

(defgeneric test-specialized-method.on-struct (a))

(defmethod test-specialized-method.on-struct ((a test-defstruct)))
