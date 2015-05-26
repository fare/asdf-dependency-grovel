;; -*- Mode: Lisp -*-

(defsystem "test-serial"
  :author "Andreas Fuchs, Matthew Steele and Francois-Rene Rideau"
  :description "Test system for asdf-dependency-grovel"
  :defsystem-depends-on ("asdf-dependency-grovel")
  :version "1"
  :components (("asdf-dependency-grovel::component-file" "asdf-dependency-grovel-test/serial"
                :pathname ""
                :load-systems (:test-serial-system)
                :merge-systems (:test-serial-system)
                :output-file "groveled-components.lisp"
                :base-asd-file "groveled-components.output.asd"
                ;;:cull-redundant t
                :verbose t)))
