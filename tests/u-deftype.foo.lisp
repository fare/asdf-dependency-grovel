(in-package :asdf-dependency-grovel-test)

(deftype uses-foo ()
  '(and foo base-string))