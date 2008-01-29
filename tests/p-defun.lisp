(cl:in-package :asdf-dependency-grovel-test)

(defun test-defun.1 (x) (declare (ignore x)))

(defun test-defun.2 (x) (declare (ignore x)) "foo" (declare (special x)))