(cl:in-package :asdf-dependency-grovel-test)

(defun test-defsetf.1 (l)
  (first l))

(defsetf test-defsetf.1 rplaca)