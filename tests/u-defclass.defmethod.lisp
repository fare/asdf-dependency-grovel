(cl:in-package :asdf-dependency-grovel-test)

(defgeneric test-specialized-method (a))

(defmethod test-specialized-method ((a test-defclass)))