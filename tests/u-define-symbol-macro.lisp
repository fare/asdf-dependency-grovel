(cl:in-package :asdf-dependency-grovel-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (with-output-to-string (stream)
    (print test-symbol-macro.1 stream)))
