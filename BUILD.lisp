;;; ASDF dependency grovel

#+xcvb
(module
 (:fullname "asdf-dependency-grovel"
  :supersedes-asdf ("asdf-dependency-grovel")
  :author ("Andreas Fuchs" "Matthew Steele")
  :description "dependency groveler for Common Lisp software"
  :depends-on ("grovel" "asdf-ops" "handlers/00-standard-handlers")
  :build-image nil))
