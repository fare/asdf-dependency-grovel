(cl:in-package :asdf-dependency-grovel-test)

(defvar *things* (list 'alpha 'beta 'gamma))

(define-symbol-macro test-symbol-macro.1 (first *things*))