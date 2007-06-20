(cl:in-package #:asdf-dependency-grovel)

(defvar *provider-hook* nil)
(defvar *user-hook* nil)

(defvar *old-macroexpand-hook* nil
  "When the override macroexpand hook is active, this is bound to
  the old value of *macroexpand-hook*.")

(defvar *current-component* nil
  "The currently loaded/compiled ASDF component")

(defvar *current-package-contents* nil
  "Is bound to a datastructure which resolves symbols contained in
packages at one time.")

(defvar *symbol-translations* nil
  "Hash table containing patched-symbol -> ansi-symbol translations")

(defvar *grovel-dir-suffix* nil
  "Bound to a value that is unique once per operating on each
  dependency-groveling system.")