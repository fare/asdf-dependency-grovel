(in-package :asdf-dependency-grovel-test)

(deftype uses-satisfies ()
  `(satisfies satisfies-something))