(cl:in-package :asdf-dependency-grovel-test)

(defgeneric test-defmethod.1 (x))

(defmethod test-defmethod.1 (x) (declare (ignore x)))