(cl:in-package :asdf-dependency-grovel-test)

;;; from the define-setf-expander clhs example

(defun test-define-setf-expander.1 (x) (car (last x)))
(define-setf-expander test-define-setf-expander.1 (x &environment env)
  "Set the last element in a list to the given value."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion x env)
    (declare (ignore newval setter))
    (let ((store (gensym)))
      (values dummies
              vals
              `(,store)
              `(progn (rplaca (last ,getter) ,store) ,store)
              `(test-define-setf-expander.1 ,getter)))))
