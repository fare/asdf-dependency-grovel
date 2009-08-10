#+xcvb (module (:depends-on ("package")))

(in-package #:asdf-dependency-grovel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Debugging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *debug-trace* nil
  "When non-nil, enables all kinds of output for debugging ADG.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Macro Expansion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *macroexpansion-handlers* (make-hash-table :test #'equal))

(defvar *old-macroexpand-hook* nil
  "When the override macroexpand hook is active, this is bound to the old value
   of *macroexpand-hook*.")

(defvar *preprocess-form-p* t
  "Used to let the groveling-macroexpand-hook know when it needs to preprocess
   a form before macroexpanding it.  It starts out as t, but is bound to nil
   within the instrumented-eval function.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constituent Stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *current-constituent* nil
  "The lowest-level constituent that we are currently inside.")

(defvar *constituent-table* nil
  "Hash table mapping constituent designators to constituent objects
   (initialized by with-constituent-groveling).")

;;;;;;;;;;;;;;;;;;;;;;;;;; Global Variable Tracking ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *suspected-variables* nil
  "Hash table containing the symbols that should be treated as if they were
   variables (as if by defvar/defparameter) when used in forms.")

(defvar *suspected-constants* nil
  "Hash table containing the symbols that should be treated as if they were
   constants (as if by defconstant) when used in forms.")

(defvar *global-mutations* nil
  "Experimental -- likely to be removed in the future.  (msteele)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ASDF Stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar *current-component* nil
;;   "The currently loaded/compiled ASDF component")

;; (defvar *current-dependency-state* nil
;;   "The state of dependency information for the current groveling run.")

(defvar *grovel-dir-suffix* nil
  "Bound to a value that is unique once per operating on each
   dependency-groveling system.")

;; (defvar *debug-object-types* nil
;;   "List containing object type names (as string designators) for which
;;    dependency information should be printed.")

;; (defvar *component-dependency-op-times* (make-hash-table)
;;   "Maps components to the last universal-time they were groveled.")

;; Used by XCVB:
(defvar *system-base-dir* nil
  "Designates the directory under which ADG will create its temporary directory
   for instrumented dependency-discovery FASLs.
   When nil, ADG will use the base-pathname of the target system;
   when non-nil, ADG will use that value as a pathname designator for it.")

;;;;;;;;;;;;;;;;;;;;;;;;;; Internal Symbol Checking ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These variables are for the machinery for noticing dependencies due to
;; importing symbols from one package into another.  When a defpackage uses
;; :import-from or :shadowing-import-from to import a symbol from another
;; package, that symbol must already exist.  Thus, the defpackage depends on
;; whatever constituent provided that symbol.
;;
;; The machinery essentially works by keeping a hashset called
;; *previously-interned-symbols* of interned symbols in the current package,
;; and after each constituent, it checks over the symbols in the package to see
;; if it now contains any that aren't already in the set; if so, it signals
;; provisions for those symbols, and updates the set.  We must also, of course,
;; notice when the package changes, so when we hit an in-pckage form, we signal
;; the provisions immediately, and then reset the *previously-interned-symbols*
;; set to hold symbols from the newly selected package.
;;
;; If you want more details about how this works, you should look at the
;; in-package handler, the signal-new-internal-symbols function, the
;; operating-on-file-constituent and operating-on-form-constituent macros, and
;; the with-constituent-groveling macro, noticing uses of the
;; *previous-package* and *previously-interned-symbols* variables.  Be careful
;; if you want to make changes -- care must be taken to ensure that the
;; in-package handler does the right thing whether or not we are groveling at
;; the granularity of top-level forms.
;;
;; Note that there's no need to instrument the import and shadowing import
;; _functions_, because they don't seem to require the symbol to already exist.
;;
;; Cases that we don't currently handle correctly:
;;
;;   - Someone might setf or setq *package* manually instead of using
;;     in-package.  For setf, this could possibly be handled within the setf
;;     handler.  Handling setq would take more work.  I'm not sure if either
;;     case is worth the effort.

(defvar *previous-package* nil
  "The most recent package to be set by in-package.  We use *previous-package*
   instead of *package* directly because after loading a file, we need to know
   which package we were in just before the load finished, but the load
   function resets *package* when the load finishes.")

(defvar *previously-interned-symbols* nil
  "The symbols that were already interned in *previous-package*.  This gets
   updated each time we finish a constituent, and each time we hit an
   in-package form.  Note that there's subtlety involved in making sure this
   works regardless of whether or not we are groveling at the granularity of
   top-level forms.")

;; Added by msteele:
(defvar *check-internal-symbols-p* nil
  "If non-nil, then pay attention to dependencies that might be caused by
   an :import-from or :shadowing-import-from in a defpackage.  If nil, then
   signal-internal-symbols becomes a no-op, and the defpackage and in-package
   handlers do less work.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
