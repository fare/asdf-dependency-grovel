(in-package :asdf-dependency-grovel-test)

(deftype uses-bar ()
  `(or (not bar)
       (and bar rational)))