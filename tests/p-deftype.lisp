(in-package :asdf-dependency-grovel-test)

(deftype foo () 'string)
(deftype bar () 'integer)

(defun satisfies-something (#1=#:a)
  (declare (ignore #1#))
  t)