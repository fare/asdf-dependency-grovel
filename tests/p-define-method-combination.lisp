(cl:in-package :asdf-dependency-grovel-test)

(defun test-and (&rest operands)
  (every #'identity operands))

(define-method-combination test-and.1 :identity-with-one-argument t)
