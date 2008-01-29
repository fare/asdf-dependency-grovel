(cl:in-package #:asdf-dependency-grovel)

(defvar *macroexpansion-handlers* (make-hash-table :test #'equal))

(defvar *old-macroexpand-hook* nil
  "When the override macroexpand hook is active, this is bound to
  the old value of *macroexpand-hook*.")

(defvar *current-component* nil
  "The currently loaded/compiled ASDF component")

(defvar *symbol-translations* nil
  "Hash table containing patched-symbol -> ansi-symbol translations")

(defvar *suspected-variables* nil
  "Hash table containing the symbols that should be treated as if they
were variables when used in forms.")

(defvar *current-dependency-state* nil
  "The state of dependency information for the current groveling run.")

(defvar *previous-package* nil
  "The package that was current previous to this run. Allows the 
symbol-interning heuristic.")

(defvar *previously-interned-symbols* (make-hash-table :test #'eql)
  "The symbols that were interned in *previous-package* before the 
current in-package stretch.")

(defvar *grovel-dir-suffix* nil
  "Bound to a value that is unique once per operating on each
  dependency-groveling system.")

(defvar *debug-object-types* nil
  "List containing object type names (as string designators) for which
dependency information should be printed.")

(defvar *component-dependency-op-times* (make-hash-table)
  "Maps components to the last universal-time they were groveled.")