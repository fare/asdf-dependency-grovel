(cl:in-package #:asdf-dependency-grovel)

(defvar *provider-hook* nil)
(defvar *user-hook* nil)

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

(defvar *grovel-dir-suffix* nil
  "Bound to a value that is unique once per operating on each
  dependency-groveling system.")