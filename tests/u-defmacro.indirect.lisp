(cl:in-package :asdf-dependency-grovel-test)

(defmacro test-defmacro-indirect.1 (bool)
  `(when ,bool (test-defmacro.1)))

(test-defmacro-indirect.1 t)
