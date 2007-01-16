(cl:in-package :asdf-dependency-grovel-test)

(defmacro test-defmacro.1 ()
  `(with-output-to-string (stream)
     (print 'yay stream)))
