
#+xcvb (module (:depends-on ("variables" "classes" "asdf-classes")))

(cl:in-package #:asdf-dependency-grovel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Used in a number of places, but not exported.
(eval-when (:compile-toplevel :load-toplevel)
  (defun canonical-package-name (package-designator)
    "Return the name of a package as a symbol, given either a package object or
     a string or symbol naming the package.  Note that given a package nickname
     as a string or symbol, it returns a symbol for that nickname, not for the
     primary name of that package."
    (intern (typecase package-designator
              (package (package-name package-designator))
              (t (string package-designator)))
            :asdf-dependency-grovel.packages)))

;; Currently used only by the defconstant handler.
(defmacro symbol-macroify (operator name &rest args &environment env)
  (let ((new-name (gentemp (format nil "ASDF-DEPENDENCY-GROVEL-~A--~A"
                                   operator name))))
    `(progn
       (define-symbol-macro ,name ,new-name)
       ,(macroexpand `(,operator ,new-name ,@args) env))))

;; Currently used only by instrumenting-macroexpand-hook.
(defmacro walk-symbols ((sym form) &body body)
  "Visit each symbol in `form' one at a time, bind `sym' to that symbol, and
   execute the body."
  (with-gensyms (visit walk node car cdr)
    `(labels ((,visit (,sym) ,@body)
              (,walk (,node)
                (cond ((symbolp ,node)
                       (,visit ,node))
                      ((consp ,node)
                       (loop :for (,car . ,cdr) :on ,node
                             :do (,walk ,car)
                             :if (symbolp ,cdr) :do (,visit ,cdr))))))
       (,walk ,form))))

;; Exists only to be exported.
(defun reload ()
  "Reload the asdf-dependency-grovel system safely."
  (let ((*macroexpand-hook* (if (boundp '*old-macroexpand-hook*)
                                *old-macroexpand-hook*
                                *macroexpand-hook*)))
    (asdf:oos 'asdf:load-op :asdf-dependency-grovel)))

;; Exists only to be exported; used by XCVB.
(defun components-in-traverse-order (system compspecs
                                     &optional (traverse-type 'asdf:load-op))
  (let* ((op (make-instance traverse-type))
         (opspecs (asdf::traverse op system))
         (order-table (make-hash-table)))
    (loop :for (nil . component) :in opspecs
          :for component-index :from 0
          :do (setf (gethash component order-table) component-index))
    (sort compspecs #'<
          :key (lambda (c) (gethash (first c) order-table -1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Signaling Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Used all over the place.
(defun signal-provider (name form-type)
;;                         &optional (*current-dependency-state*
;;                                    *current-dependency-state*))
  "Signal that symbol `name' of kind `form-type' is being provided by the
   current component.  For example, (defun foo ...) would signal with `name'
   foo and `form-type' defun."
  ;; If we're using SBCL, we may need to filter out occasional bogus provisions
  ;; of symbols from the SB-IMPL package.  (msteele)
  #+sbcl
  (when (and (symbolp name)
             (eql (symbol-package name) (find-package :sb-impl)))
    (return-from signal-provider (values)))
;;   (if *using-constituents*
      (when (and *current-constituent*
                 ;; FIXME defvar is problematic; turn it off for now
                 (not (eql form-type 'defvar)))
        (constituent-add-provision (list name form-type)
                                   *current-constituent*))
;;       (when (and *current-dependency-state* *current-component*)
;;         (with-slots (providers component-counter) *current-dependency-state*
;;           (push (list component-counter *current-component*)
;;                 (gethash (list name form-type) providers)))))
  (values))

;; Used all over the place.
(defun signal-user (name form-type)
;;                     &optional (*current-dependency-state*
;;                                *current-dependency-state*))
  "Signal that symbol `name' of kind `form-type' is being used by the
   current component.  For example, (with-foo ...) might signal with `name'
   with-foo and `form-type' defmacro."
;;   (if *using-constituents*
      (when (and *current-constituent*
                 ;; FIXME defvar is problematic; turn it off for now
                 (not (eql form-type 'defvar)))
        (constituent-add-use (list name form-type)
                             *current-constituent*))
;;       (when (and *current-dependency-state* *current-component*
;;                ;; FIXME defvar is problematic; turn it off for non-ASDF for now
;;                  (not (and *non-asdf-p* (eql form-type 'defvar))))
;;         (with-slots (users component-counter) *current-dependency-state*
;;           (setf (gethash *current-component* users)
;;                 (gethash *current-component* users
;;                          (make-hash-table :test #'equal)))
;;           (setf (gethash (list name form-type)
;;                          (gethash *current-component* users))
;;                 component-counter))))
  (values))

;; This function is part of the machinery for noticing dependencies due to
;; importing symbols.  For details, refer to the "Internal Symbol Checking"
;; section of variables.lisp.
(defun signal-new-internal-symbols (&key (populate nil))
  "This should be called by the in-package handler as well as at the end of
   each constituent (at every level).  It looks for symbols that have been
   added to *previous-package* since the last time
   *previously-interned-symbols* was populated, and signals that the current
   constituent provided them.  Uses of these symbols are picked up in
   the :import-from and :shadowing-import-from clauses of a defpackage.
   The :populate keyword, if non-nil, tells signal-new-internal-symbols to
   populate *previously-interned-symbols* with any missing symbols (after
   signaling provisions)."
  (when *check-internal-symbols-p*
    (do-symbols (sym *previous-package*)
      (unless (hashset-contains-p sym *previously-interned-symbols*)
        (signal-provider sym 'internal-symbol)
        (when populate
          (hashset-add sym *previously-interned-symbols*))))))

;; These two provide support for symbol macros.  The former is used by the
;; define-symbol-macro handler, and the second must exist in case someone uses
;; setf on an instrumented symbol macro.
(defun signal-symbol-macroexpansion (name expansion)
  (signal-user name 'define-symbol-macro)
  expansion)
(defsetf signal-symbol-macroexpansion (name expression) (new-value)
  `(progn
     (signal-user ',name 'define-symbol-macro)
     (setf ,expression ,new-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Instrumentation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Used only by instrument-defun-body.
(defun parse-body (body &key (ignore-multiple-docstrings t) whole)
  ;; Used with kind permission by Tobias C. Rittweiler.
  ;; C.f. CLHS 1.4.1.2.1 and 3.4.11.
  (prog (current decls doc)
   :scan-body
     (setf current (car body))
     (cond ((stringp current)       (go :scan-string))
           ((eql 'declare (and (listp current) (first current)))
            (push (pop body) decls) (go :scan-body))
           (t (go :finish)))
   :scan-string
     (cond ((null (cdr body))       (go :finish))    ; string is form
           ((not doc)
            (setf doc (pop body))   (go :scan-body))
           (ignore-multiple-docstrings
            (pop body)              (go :scan-body)) ; skip redundant docstring
           (t (error "Too many documentation strings in ~S." (or whole body))))
   :finish
     (return (values body (nreverse decls) doc))))

;; Used by several handlers (but not exported).
(defun instrument-defun-body (body form)
  "Insert FORM into list of defun body forms BODY such that the
   return value of BODY is the same as it would be without FORM,
   keeping declarations intact."
  (multiple-value-bind (body decls doc)
      (parse-body body :ignore-multiple-docstrings nil)
    `(,@decls ,doc ,form ,@(or body (list nil)))))

(defmacro noticing-*feature*-changes (&rest body)
  ;; naive implementation, doesn't really deal with removed features
  (with-gensyms (old-features feature)
    `(let ((,old-features (copy-list *features*)))
       (multiple-value-prog1
           (progn ,@body)
         (dolist (,feature (set-difference *features* ,old-features))
           (signal-provider ,feature 'feature))
         (dolist (,feature (set-difference ,old-features *features*))
           (signal-provider ,feature 'removed-feature))))))

(labels ((signal-feature (presentp feature)
           (signal-user feature
                        (if presentp 'feature 'removed-feature)))
         (featurep (x)
           (if (consp x)
               (case (car x)
                 ((:not not)
                  (if (cddr x)
                      (error "too many subexpressions in feature expression: ~S" x)
                      (not (featurep (cadr x)))))
                 ((:and and) (every #'featurep (cdr x)))
                 ((:or or) (some #'featurep (cdr x)))
                 (t
                  (error "unknown operator in feature expression: ~S." x)))
               (let ((feature-presentp (not (null (member x *features*)))))
                 (signal-feature feature-presentp x)
                 feature-presentp)))
         (guts (stream not-p)
           (unless (if (let ((*package* (find-package :keyword))
                             (*read-suppress* nil))
                         (featurep (read stream t nil t)))
                       (not not-p)
                       not-p)
             (let ((*read-suppress* t))
               (read stream t nil t)))
           (values)))
  (defun instrumented-sharp+ (stream subchar numarg)
    (declare (ignore numarg subchar))
    (guts stream nil))
  (defun instrumented-sharp- (stream subchar numarg)
    (declare (ignore numarg subchar))
    (guts stream t)))

(defun instrumented-sharpquote (stream subchar numarg)
  (declare (ignore subchar numarg))
  (let ((function (read stream t nil t)))
    (when (or (symbolp function)
              (and (consp function) (eql 'setf (first function))))
      (signal-user function 'defun))
    `(function ,function)))

(defun make-instrumented-readtable (&optional (readtable *readtable*))
  (setf readtable (copy-readtable readtable))
  (set-dispatch-macro-character #\# #\+ #'instrumented-sharp+ readtable)
  (set-dispatch-macro-character #\# #\- #'instrumented-sharp- readtable)
  (set-dispatch-macro-character #\# #\' #'instrumented-sharpquote readtable)
  ;; TODO: #.
  readtable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Macro Expansion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun does-not-macroexpand ()
  "A handler can return (does-not-macroexpand) to indicate that does not wish
   to provide any special expansion, and will thus defer to the normal
   expansion for the macro."
  (values nil nil))

(defmacro does-macroexpand ((function env &key (macroexpand-hook
                                                '*old-macroexpand-hook*))
                            new-macro-body)
  `(values t (let ((*macroexpand-hook* ,macroexpand-hook))
               (funcall *old-macroexpand-hook* ,function
                        ,new-macro-body ,env))))

;; Much like `does-macroexpand', but takes an additional `epilogue' parameter
;; that should be a list of forms.  These forms will be inserted after the
;; expanded `new-macro-body' within a progn.  The caller must make sure that
;; the last form in `epilogue' returns whatever the macro would have returned.
;; We'd like to just use multiple-value-prog1 instead of progn, but that
;; doesn't necessarily preserve top-level-ness.  (msteele)
(defmacro does-macroexpand-with-epilogue
    ((function env &key (macroexpand-hook '*old-macroexpand-hook*))
     new-macro-body epilogue)
  `(values t `(progn
                ,(let ((*macroexpand-hook* ,macroexpand-hook))
                   (funcall *old-macroexpand-hook* ,function
                            ,new-macro-body ,env))
                ,@,epilogue)))

;; Used only by instrumenting-macroexpand-hook.
(defun handle-macroexpansion (name form function environment)
  "Handle macroexpansion of a macro called `name' on `form' with macro function
   `function' in `environment'.  Either dispatch to the appropriate handler, or
   else simply signal the use of the macro, and provide no special expansion.
   Return two values: first, a boolean indicating whether any special expansion
   was done, and second, the special expansion form (or nil if none)."
  (let ((handler (gethash (list (canonical-package-name (symbol-package name))
                                (string (symbol-name name)))
                          *macroexpansion-handlers*)))
    (if handler
        (funcall handler form :function function :environment environment)
        (progn (signal-user (first form) 'defmacro)
               (does-not-macroexpand)))))

;; The hook itself.
(defun instrumenting-macroexpand-hook (fun form env)
  "A substitute for `*macroexpand-hook*' that provides the entry into the magic
   of asdf-dependency-grovel."
  (if (listp form)
      ;; If the form is a list, we're going to do some magic.
      (progn
        ;; Step 1: Look over the symbols in the original form, in case we need
        ;;         to signal a use of defpackage or defvar.
        (walk-symbols (sym form)
          ;; If the symbol is in a package other than CL or KEYWORD, we should
          ;; signal that we're using that package.
          (let ((package (symbol-package sym)))
            (unless (or (null package)
                        (eql package (find-package :cl))
                        (eql package (find-package :keyword)))
              (signal-user (canonical-package-name package) 'defpackage)))
          ;; If the symbol is a variable, we should signal that we're using
          ;; that variable.
          ;; XXX: heuristic, doesn't catch everything:
;;           (when (and (not *using-constituents*)
;;                      (gethash sym *suspected-variables*))
;;             (signal-user sym 'defvar))
          )
        ;; Step 2: Hand the form over to handle-macroexpansion, which will
        ;;         dispatch to the appropriate handler.
        (multiple-value-bind (replacep new-form)
            (handle-macroexpansion (first form) form fun env)
          ;; If the handler provided a replacement, use that, otherwise defer
          ;; to the old macroexpand hook.
          (if replacep new-form
              (funcall *old-macroexpand-hook* fun form env))))
      ;; If the form is not a list, defer to the old macroexpand hook.
      (funcall *old-macroexpand-hook* fun form env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Groveling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-groveling-macroexpand-hook (&body body)
  ;; Needs to happen later, else we get problems with CLOS:
  `(let ((*old-macroexpand-hook* *macroexpand-hook*)
         (*macroexpand-hook* #'instrumenting-macroexpand-hook))
     ,@body))

;; ;; Currently used only by with-new-groveling-environment.
;; (defmacro with-groveling-environment ((state verbose debug-object-types
;; 					     base-pathname)
;;                                             &body body)
;;   `(let* ((*current-dependency-state* ,state)
;;           (*features* (adjoin 'groveling *features*))
;;           (*compile-print* ,verbose)
;;           (*compile-verbose* ,verbose)
;;           (*debug-object-types* ,debug-object-types)
;;           (*default-pathname-defaults* ,base-pathname)
;;           (*grovel-dir-suffix* (get-universal-time))
;;           ;;(*break-on-signals* 'error)
;;           (*suspected-variables* (slot-value *current-dependency-state*
;;                                              'suspected-variables)))
;;      ,@body))

;; ;; Currently used only by initially-grovel-dependencies.
;; (defmacro with-new-groveling-environment ((verbose debug-object-types
;;                                                    base-pathname)
;;                                           &body body)
;;   `(with-groveling-environment ((make-instance 'dependency-state)
;;                                 ,verbose ,debug-object-types ,base-pathname)
;;      ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dependency State ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Quick hack: some of these hash tables have been set to use equal instead of
;; ;; eql in order to support non-ASDF groveling.  This makes things slower.
;; ;; However, I have in mind some ideas for how to reorganize how
;; ;; dependency-state works that will allow us to switch back to using eql;
;; ;; hopefully I can do that a little later on.  (msteele)
;; (defclass dependency-state ()
;;      ((order-table
;;        :initform (make-hash-table :test #'equal) ;; was eql
;;        :documentation "maps components to their component-counter")
;;       (providers
;;        :initform (make-hash-table :test #'equal)
;;        :documentation "maps form types and names to the defining
;;        component and their component-counter.")
;;       (users
;;        :initform (make-hash-table :test #'equal) ;; was eql
;;        :documentation "maps components to the forms they need.")
;;       (component-counter
;;        :initform 0
;;        :documentation "counter to order the dependencies")
;;       (component-dependencies
;;        :initform (make-hash-table :test #'equal) ;; was eql
;;        :documentation "Maps components to their dependencies (recomputed after
;; operating on a component).")
;;       (system-dependencies
;;        :initform (make-hash-table :test #'eql)
;;        :documentation "Maps systems to their system dependencies (recomputed
;; after operating on a component).")
;;       (suspected-variables :initform (make-hash-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Operating on Component ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Everything below is used only by operating-on-component.

;; ;; Currently used only by operating-on-component.
;; (defun note-operating-on-component (component &optional
;;                                     (*current-dependency-state*
;;                                      *current-dependency-state*))
;;   (when *current-dependency-state*
;;     (with-slots (component-counter order-table) *current-dependency-state*
;;        (unless (gethash component order-table)
;;          (setf (gethash component order-table) (incf component-counter))))))

;; ;; Used only by generate-form-providers and compute-dependencies-for-component.
;; (defun get-counter-of (component state)
;;   (with-slots (order-table) state
;;      (gethash component order-table)))

;; ;; Used only by compute-dependencies-for-component.
;; (defun generate-form-providers (form state &key before generator)
;;   (with-slots (providers) state
;;     (loop :for (unreliable-count component) :in (gethash form providers)
;;           :for count = (or (get-counter-of component state) -1)
;;           :when (and component (or (null before) (> before count)))
;;             :do (funcall generator component))))

;; ;; Used only by compute-dependencies-for-component.
;; (defun get-usage-of (component state)
;;   (with-slots (users) state
;;      (or (gethash component users)
;;          (make-hash-table))))

;; ;; Used only by recompute-dependencies-for.
;; (defun compute-dependencies-for-component (component state
;;                                    &key generator)
;;   (let ((component-counter (get-counter-of component state)))
;;     (loop :for form :being :the :hash-keys :of (get-usage-of component state)
;;           :do (generate-form-providers
;;                    form state
;;                    :before (if *non-asdf-p* nil component-counter)
;;                    :generator generator))
;;     (values)))

;; ;; Used only by operating-on-component.
;; (defun recompute-dependencies-for (component)
;;   (unless *current-dependency-state*
;;     (return-from recompute-dependencies-for (values nil :no-state)))
;;   (let* ((state *current-dependency-state*)
;;          (c-deps (slot-value state 'component-dependencies))
;;          (s-deps (if *non-asdf-p* nil
;;                      (setf (gethash (asdf:component-system component)
;;                                     (slot-value state 'system-dependencies))
;;                            (gethash (asdf:component-system component)
;;                                     (slot-value state 'system-dependencies)
;;                                     (make-hash-table)))))
;;          (c-dep-uniqueness (make-hash-table)))
;;     (if *non-asdf-p*
;;         ;; For non-ASDF groveling, do c-deps.setdefault(component, nil).
;;         (setf (gethash component c-deps) (gethash component c-deps nil))
;;         ;; For ASDF groveling, do c-deps[component] = nil.
;;         (setf (gethash component c-deps) nil))
;;     (compute-dependencies-for-component
;;      component state
;;      :generator (lambda (dep-c)
;;                   (if *non-asdf-p*
;;                       (unless (equal dep-c component)
;;                         (pushnew dep-c (gethash component c-deps)
;;                                  :test #'equal))
;;                       ;; poor (in computation time) man's pushnew:
;;                       (unless (gethash dep-c c-dep-uniqueness)
;;                         (setf (gethash dep-c c-dep-uniqueness) t)
;;                         (push dep-c (gethash component c-deps))
;;                         ;; XXX: not sure if this will DTRT for re-groveling,
;;                         ;; but that's secondary right now.
;;                         (setf (gethash (asdf:component-system dep-c) s-deps)
;;                               t)))))))

;; ;; Currently used only by call-with-dependency-tracking.
;; (defmacro operating-on-component ((component &key (update-counter-p t))
;;                                   &body body)
;;   `(let ((*current-component* ,component))
;;      (when ,update-counter-p
;;        (note-operating-on-component *current-component*))
;;      (multiple-value-prog1
;;        (progn ,@body)
;;        (setf (gethash ,component *component-dependency-op-times*)
;;              (get-universal-time))
;;        (recompute-dependencies-for ,component))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Output Component File ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Everything below is needed only by output-component-file.

;; Used by maybe-translated-component-name and output-component-file.
(defun enough-component-spec (c &optional pn-p)
  (flet ((strip/ (name)
           (subseq name (1+ (or (position #\/ name :from-end t) -1)))))

    (if (equal (parse-namestring (enough-namestring (asdf:component-pathname c)))
               (make-pathname :name (asdf:component-name c) :type "lisp"))
        (format nil "~S" (asdf:component-name c))
        (let ((pn (parse-namestring (enough-namestring (asdf:component-pathname c)))))
          ;; XXX: make-pathname forms are more portable, but namestrings
          ;; are more readable.
          (format nil "~S~:[~; :pathname #p~S~]"
                  (enough-namestring (make-pathname :name (strip/ (asdf:component-name c))
                                                    :type nil :defaults (asdf:component-pathname c)))
                  pn-p
                  (enough-namestring pn))))))

;; Used by additional-dependencies* and overridden-dependencies*.
(defun map-over-instrumented-component-and-parents (component slot-name)
  (loop :for c = component :then (asdf:component-parent c)
        :until (null c)
        :when (and (typep c 'instrumented-component)
                   (slot-boundp c slot-name))
        :append (slot-value c slot-name)))

;; Used by additional-dependencies*, overridden-dependencies*, and
;; maybe-translated-component-name.
(defun dwim-stringify-component-spec (component-spec)
  (case (char component-spec 0)
    ((#\# #\") ; is a subform, insert as-is
     ;; of course, this fails if the first char of a pathname should
     ;; be " or #. Let's hope nobody does that, ever.
     component-spec)
    (otherwise ; should be a string, make it one.
     (concatenate 'string (string #\") component-spec (string #\")))))

;; Currently used only by output-component-file.
(defun additional-dependencies* (component)
  "Walk the tree up through all parent components and collect
   their :additional-dependencies."
  (mapcar #'dwim-stringify-component-spec
          (map-over-instrumented-component-and-parents
           component 'additional-dependencies)))

;; Currently used only by output-component-file.
(defun overridden-dependencies* (component)
  (mapcar #'dwim-stringify-component-spec
          (map-over-instrumented-component-and-parents
           component 'overridden-dependencies)))

;; Currently used only by output-component-file.
(defun maybe-translated-component-name (component &key include-pathname)
  (if (and (typep component 'instrumented-component)
           (slot-boundp component 'translated-name))
      (format nil "~A~@[ :pathname #.~S~]"
              (dwim-stringify-component-spec
               (slot-value component 'translated-name))
              (and include-pathname
                   (slot-value component 'translated-pathname)))
      (enough-component-spec component include-pathname)))

;; Used only by maybe-translated-component-class.
(defun coerce-name (maybe-class)
  (if (typep maybe-class 'standard-class)
      (class-name maybe-class)
      maybe-class))

;; Currently used only by output-component-file.
(defun maybe-translated-component-class (component)
  (cond
    ;; standard instrumented component with no output
    ;; file type, and default-component-type files are
    ;; emitted as :file.
    ((and (typep component 'instrumented-cl-source-file)
          (not (slot-boundp component 'output-file-type)))
     :file)
    ((and (eql (class-of component)
               (find-class (or
                            (coerce-name
                             (asdf::module-default-component-class
                              (asdf:component-parent component)))
                            'asdf:cl-source-file))))
     :file)
    ;; instrumented components with output file types emit
    ;; their output file type
    ((typep component 'instrumented-component)
     (class-name (find-class (output-file-type component))))
    ;; other types get their class name.
    (t
     (class-name (class-of component)))))

;; Currently used only by initially-grovel-dependencies.
(defun output-component-file (stream dependencies &key
                              (output-systems-and-dependencies-p t))
  (let ((*print-case* :downcase))
    (format stream ";;; This file contains -*- lisp -*- expressions.~%")
    (format stream "~@<;;; ~@;AUTO-GENERATED file from system definition ~
                              of system ~A.  Instead of directly editing this ~
                              file, please edit the system definition~P ~
                              and re-generate this file.~:@>"
            (mapcar #'asdf:component-name (mapcar #'first dependencies))
            (length dependencies))
    (format stream "~&(~%")
    (dolist (system dependencies)
      (destructuring-bind (system &key depends-on components) system
        (when output-systems-and-dependencies-p
          (format stream "~& (~S~@[ :depends-on ~:S~] :components~%  ("
                  (asdf:component-name system)
                  (delete (asdf:component-name system)
                          (mapcar #'asdf:component-name depends-on)
                          :test #'equal)))
        (dolist (compspec components) ;; to sort: (components-in-traverse-order system components)
          (destructuring-bind (component &key depends-on) compspec
            (let ((*package* (find-package :keyword)))
              (format
               stream
               "~&   (~S ~A~@[~&    :depends-on ~:A~]~@[~&    ~{~S~^ ~}~])"
               ;; component class:
               (maybe-translated-component-class component)
               ;; component names:
               (maybe-translated-component-name
                component :include-pathname t)
               ;; component dependencies:
               (remove-duplicates
                `(,@(sort
                     `(,@(or (overridden-dependencies* component)
                             (mapcar #'enough-component-spec depends-on))
                         ,@(additional-dependencies* component))
                     #'string<))
                :test #'equal)
               ;; component initargs
               (and (typep component 'instrumented-component)
                    (additional-initargs component)))))))
      (when output-systems-and-dependencies-p
        (format stream "))~%")))
    (format stream "~&)~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constituent Support ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-constituent-groveling (&body body)
  `(let (
;;          (*non-asdf-p* t) ;; TODO remove this eventually
;;          (*using-constituents* t) ;; TODO remove this eventually
         ;; Initialize an fresh constituent environment.
         (*constituent-table* (make-hash-table :test #'equal))
         (*current-constituent* (make-instance 'top-constituent :parent nil))
         ;; Set up machinery for checking internal symbols.
         (*previous-package* *package*)
         (*previously-interned-symbols*
          (let ((hashset (make-hashset :test 'eql)))
            (do-symbols (sym *package*)
              (hashset-add sym hashset))
            hashset))
         (*check-internal-symbols-p* t)
         ;; Indicate that we are groveling.
         (*features* (adjoin 'groveling *features*)))
     ,@body))

(defmacro operating-on-asdf-component-constituent ((component) &body body)
  "Used internally; not exported."
  (with-gensyms (component! designator existing-con present-p con)
    `(let* ((,component! ,component)
            (,designator (cons (asdf:component-name ,component!)
                               (constituent-designator *current-constituent*)))
            (,con (multiple-value-bind (,existing-con ,present-p)
                      (gethash ,designator *constituent-table*)
                    (if ,present-p ,existing-con
                        (setf (gethash ,designator *constituent-table*)
                              (make-instance 'asdf-component-constituent
                                             :parent *current-constituent*
                                             :component ,component!)))))
            (*current-constituent* ,con))
       (assert (equal (constituent-designator ,con) ,designator))
       (noticing-*feature*-changes ,@body))))

(defmacro operating-on-file-constituent ((path) &body body)
  "Used internally; not exported."
  (with-gensyms (path! designator existing-con present-p con)
    `(let* ((,path! ,path)
            (,designator (cons ,path! (constituent-designator
                                       *current-constituent*)))
            (,con (multiple-value-bind (,existing-con ,present-p)
                      (gethash ,designator *constituent-table*)
                    (if ,present-p ,existing-con
                        (setf (gethash ,designator *constituent-table*)
                              (make-instance 'file-constituent
                                             :parent *current-constituent*
                                             :path ,path!)))))
            (*current-constituent* ,con)
            (*previous-package* *package*)) ;; See Note [prev-package] below
       (assert (equal (constituent-designator ,con) ,designator))
       (multiple-value-prog1
           (noticing-*feature*-changes ,@body)
         (signal-new-internal-symbols :populate t)))))

;; Note [prev-package]: Notice that operating-on-file-constituent binds
;; *previous-package* to *package*, and operating-on-form-constituent doesn't.
;; This is to ensure that the in-package handler behaves correctly.  Recall
;; that the in-package handler will setf *previous-package* to the newly
;; selected package.  Thus, operating-on-file-constituent must ensure that
;; *previous-package* gets reset back to the old value of *package* after
;; loading the file, because *package* itself will get reset by the load
;; function.  However, operating-on-form-constituent must _not_ bind
;; *previous-package*, because otherwise later forms in the same file wouldn't
;; observe the change to *previous-package* made by the in-package handler.

(defmacro operating-on-form-constituent ((index pos summary) &body body)
  "Used internally; not exported."
  (with-gensyms (index! designator existing-con present-p con)
    `(let* ((,index! ,index)
            (,designator (cons ,index! (constituent-designator
                                        *current-constituent*)))
            (,con (multiple-value-bind (,existing-con ,present-p)
                      (gethash ,designator *constituent-table*)
                    (if ,present-p ,existing-con
                        (setf (gethash ,designator *constituent-table*)
                              (make-instance 'form-constituent
                                             :parent *current-constituent*
                                             :position ,pos
                                             :summary ,summary)))))
            (*current-constituent* ,con))
       (assert (equal (constituent-designator ,con) ,designator))
       (multiple-value-prog1
           (noticing-*feature*-changes ,@body)
         (signal-new-internal-symbols :populate t)))))

;;;;;;;;;;;;;;;;;;;;;;;; Initially Grovel Dependencies ;;;;;;;;;;;;;;;;;;;;;;;;

;; Everything below is used only by initially-grovel-dependencies.

;; ;; Used only by cull-dependencies and dependency-forms.
;; (defun cached-component-dependencies (state component)
;;   (gethash component (slot-value state 'component-dependencies)))

;; ;; Used only by cull-dependencies.
;; (defun make-dependency-space (component state dependency-space)
;;   (dolist (dep (gethash component
;;                         (slot-value state 'component-dependencies)))
;;     (if (= 1 (incf (gethash dep dependency-space 0)))
;;         (make-dependency-space dep
;;                                state dependency-space))))

;; ;; Used only by dependency-forms.
;; (defun cull-dependencies (component state)
;;   (let ((dependency-space (make-hash-table)))
;;     (make-dependency-space component state dependency-space)
;;     (loop :for dep :in (cached-component-dependencies state component)
;;           :if (= 1 (gethash dep dependency-space))
;;             :collect dep)))

;; Currently used only by dependency-forms and constituent-dependency-forms.
(defun asdf-system-file-components (system)
  "Flatten the tree of modules/components into a list that
   contains only the non-module components."
  (loop :for component :in (asdf:module-components system)
        :if (typep component 'asdf:module)
          :append (asdf-system-file-components component)
        :else
          :collect component))

;; ;; Currently used only by initially-grovel-dependencies.
;; (defun dependency-forms (state interesting-systems &key cull-redundant)
;;   (let ((s-deps (slot-value state 'system-dependencies)))
;;     (loop :for system :in interesting-systems
;;           :collect `(,system
;;                      :depends-on
;;                      ,(loop :for d-sys :being :the :hash-keys
;;                             :of (gethash system s-deps (make-hash-table))
;;                             :collect d-sys)
;;                      :components
;;                      ,(loop :for comp :in (asdf-system-file-components system)
;;                             :for deps = (if cull-redundant
;;                                             (cull-dependencies comp state)
;;                                             (cached-component-dependencies
;;                                              state comp))
;;                             :collect `(,comp :depends-on ,deps))))))

;; Used only by initially-grovel-dependencies.
(defun constituent-dependency-forms (top interesting-systems)
  (let ((constituent-deps (constituent-dependency-table top))
        (component-deps (make-hash-table :test 'eql))
        (system-deps (make-hash-table :test 'eql)))
    ;; Populate the component-deps and system-deps tables.
    (loop :for con1 :being :each :hash-key :of constituent-deps
          :using (:hash-value deps)
          :when (typep con1 'asdf-component-constituent) :do
       (let ((comp1 (asdf-component-constituent-component con1)))
         (loop :for con2 :being :each :hash-key :of deps
               :when (typep con2 'asdf-component-constituent) :do
            (let ((comp2 (asdf-component-constituent-component con2)))
              (pushnew comp2 (gethash comp1 component-deps) :test 'eql)
              (pushnew (asdf:component-system comp2)
                       (gethash (asdf:component-system comp1) system-deps)
                       :test 'eql)))))
    ;; Build and return the dependency forms.
    (loop :for system :in interesting-systems
          :collect `(,system
                     :depends-on ,(gethash system system-deps)
                     :components
                     ,(loop :for comp :in (asdf-system-file-components system)
                            :for deps = (gethash comp component-deps)
                            :collect `(,comp :depends-on ,deps))))))

;; Used once in asdf-ops, but nowhere else.
(defun initially-grovel-dependencies (systems
                                      stream
                                      interesting-systems
                                      &key verbose
;;                                      debug-object-types
;;                                      cull-redundant
                                      (base-pathname
                                       (error "must supply a base-pathname")))
  ;; TODO: debug-object-types.
;;   (if (or t *using-constituents*)
      (let ((*compile-print* verbose)
            (*compile-verbose* verbose)
            (*load-verbose* verbose)
            (*default-pathname-defaults* base-pathname)
            (*grovel-dir-suffix* (get-universal-time))
            (*readtable* (make-instrumented-readtable)))
        (with-constituent-groveling
          (dolist (system systems)
            (operating-on-asdf-component-constituent (system)
              (asdf:oos 'asdf:load-op system :verbose verbose)))
          (let ((deps (constituent-dependency-forms *current-constituent*
                                                    interesting-systems)))
            (output-component-file stream deps))
          *current-constituent*)))
;;       (with-new-groveling-environment (verbose debug-object-types
;;                                        base-pathname)
;;         (let ((state *current-dependency-state*))
;;           (dolist (system systems)
;;             (with-groveling-macroexpand-hook
;;               (asdf:oos 'asdf:load-op system :verbose verbose)))
;;           (let ((deps (dependency-forms state interesting-systems
;;                                         :cull-redundant cull-redundant)))
;;             (output-component-file stream deps))
;;           state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Consituent Reporting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-constituent-dependency-report (&key (stream t))
  (let ((constituent *current-constituent*))
    (propagate-constituent constituent)
    (let ((*print-pretty* nil) ;; Don't insert newlines when formatting sexps!
          (dependency-table (constituent-dependency-table constituent)))
      (loop :for con :being :the :hash-keys :in dependency-table
            :using (:hash-value dep-table) :do
         (progn
           (format stream "~&c~S~%" (constituent-summary con))
           (loop :for dep :being :the :hash-keys :in dep-table
                 :using (:hash-value reasons) :do
              (progn
                (format stream "    d~S~%" (constituent-summary dep))
                (dolist (reason reasons)
                  (format stream "        ~{~S  (~S)~}~%" reason)))))))))

(defun print-constituent-file-splitting-strategy (&key (stream t))
  (let ((graph (build-merged-graph *current-constituent*))
        (parent-map (make-hash-table :test 'eql))
        (*print-pretty* nil))
    (do-hashset (dnode graph)
      (push dnode (gethash (dnode-parent dnode) parent-map)))
    (loop :for parent :being :each :hash-key :of parent-map
          :using (:hash-value dnodes)
          :when (> (length dnodes) 1) :do
       (format stream "~&~S~%" (constituent-summary parent))
       (dolist (dnode dnodes)
         (format stream "  dnode:~%")
         (do-hashset (con (dnode-constituents dnode))
           (format stream "    ~S~%" (constituent-summary con)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Non-ASDF Support ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro instrumented-load (file &rest args)
  (with-gensyms (file!)
    `(let ((,file! ,file))
       (operating-on-file-constituent (,file!)
         (with-groveling-macroexpand-hook
           (load ,file! ,@args))))))

(defmacro instrumented-compile-file (file &rest args)
  (with-gensyms (file!)
    `(let ((,file! ,file))
       (operating-on-file-constituent (,file!)
         (with-groveling-macroexpand-hook
           (compile-file ,file! ,@args))))))

(defun print-big-ol-dependency-report (&key ;(state *current-dependency-state*)
                                            (stream t))
;;   (when *using-constituents*
    (print-constituent-dependency-report :stream stream)
    (print-constituent-file-splitting-strategy :stream stream))
;;     (return-from print-big-ol-dependency-report))
;;   (let ((*print-pretty* nil) ;; Don't insert newlines when formatting sexps!
;;         (comp-deps (slot-value state 'component-dependencies))
;;         (providers (slot-value state 'providers))
;;         (users (slot-value state 'users)))
;;     ;; Print a summary of which files depend on which other files and why.
;;     (loop :for comp :being :the :hash-keys :in comp-deps
;;           :using (:hash-value deps) :do
;;        (progn
;;          (format stream "~&File ~S depends on:~%" comp)
;;          (dolist (dep deps)
;;            (format stream "    file ~S because of:~%" dep)
;;            (loop :for thing :being :the :hash-keys :in (gethash comp users)
;;                  :if (loop :for (counter provider) :in (gethash thing providers)
;;                            :if (equal provider dep) :do (return t)
;;                            :finally (return nil)) :do
;;               (format stream "        ~{~S  (~S)~}~%" thing)))))
;;     ;; Print a summary of any cyclic dependencies, using DFS to find cycles.
;;     (format stream "~&Dependency cycles:~%")
;;     (loop :with expanded = nil
;;           :with stack = (loop :for comp :being :the :hash-keys :in comp-deps
;;                               :collecting (list comp))
;;           :until (null stack) :do
;;           (let* ((chain (pop stack))
;;                  (comp (car chain)))
;;             (unless (member comp expanded :test #'equal)
;;               (push comp expanded)
;;               (dolist (dep (gethash comp comp-deps))
;;                 (if (member dep chain :test #'equal)
;;                     (format stream "    ~S~%" (reverse chain))
;;                     (push (cons dep chain) stack))))))))

;;;;;;;;;;;;;;;;;; Fine-Grain Instrumentation (Experimental) ;;;;;;;;;;;;;;;;;;

(defun summarize-form (form)
  (cond ((symbolp form) form)
        ((and (consp form) (symbolp (car form)))
         (cond ((null (cdr form)) (format nil "(~S)" (car form)))
               ((and (consp (cdr form)) (symbolp (cadr form)))
                (if (null (cddr form))
                    (format nil "(~S ~S)" (car form) (cadr form))
                    (format nil "(~S ~S ...)" (car form) (cadr form))))
               (t (format nil "(~S ...)" (car form)))))
        (t "(...)")))

#-sbcl
(defun fine-grain-instrumented-load (&rest args)
  (declare (ignore args))
  (error "fine-grain-instrumented-load is only implemented for SBCL"))

;; The below code was copied wholesale from the SBCL source code for the load
;; and load-as-source functions, and then modified.  It's definitely still in
;; "quick hack" status.

#+sbcl
(defun fine-grain-instrumented-load (pathspec &key
                                     (verbose *load-verbose*)
                                     (print *load-print*)
                                     (if-does-not-exist t)
                                     (external-format :default))
  (labels ((load-stream (stream filename)
             (let* ((*readtable* *readtable*)
                    (*package* (sb-int:sane-package))
                    (*load-pathname* (handler-case (pathname stream)
                                       (error () nil)))
                    (*load-truename* (when *load-pathname*
                                       (handler-case (truename stream)
                                         (file-error () nil))))
                    (sb-fasl::*load-depth* (1+ sb-fasl::*load-depth*))
                    (sb-c::*policy* sb-c::*policy*))
               (return-from fine-grain-instrumented-load
                 (if (equal (stream-element-type stream) '(unsigned-byte 8))
                     (sb-fasl::load-as-fasl stream verbose print)
                     (fine-grain-instrumented-load-as-source stream
                                                             filename)))))
           (fine-grain-instrumented-load-as-source (stream filename)
             (macrolet ((do-sexprs ((sexpr index pos stream) &body body)
                          (sb-int:aver (symbolp sexpr))
                          (sb-int:aver (symbolp index))
                          (sb-int:with-unique-names (source-info)
                            (sb-int:once-only ((stream stream))
                              `(if (handler-case (pathname stream)
                                     (error () nil))
                                   (let ((,source-info
                                          (sb-c::make-file-source-info
                                           (pathname ,stream)
                                           (stream-external-format ,stream))))
                                     (setf (sb-c::source-info-stream
                                            ,source-info) ,stream)
                                     (sb-c::do-forms-from-info
                                         ((,sexpr current-index) ,source-info)
                                       (let ((,index current-index)
                                             (,pos (nth-value 1
                                                     (sb-c::find-source-root
                                                      current-index
                                                      ,source-info))))
                                         ,@body)))
                                   (do ((,index 0 (1+ ,index))
                                        (,pos nil) ;; TODO handle this case
                                        (,sexpr
                                         (read ,stream nil sb-int:*eof-object*)
                                         (read ,stream nil sb-int:*eof-object*)))
                                       ((eq ,sexpr sb-int:*eof-object*))
                                     ,@body))))))
;;                (if *using-constituents*
                   (operating-on-file-constituent (filename)
                     (do-sexprs (sexpr i p stream)
                       (operating-on-form-constituent
                           (i p (summarize-form sexpr))
                         (with-groveling-macroexpand-hook
                           (eval sexpr)))))
;;                    (do-sexprs (sexpr i stream)
;;                      (error "fine-grain instrumentation must use constituents")))
;;                      (operating-on-component ((list filename i))
;;                        (with-groveling-macroexpand-hook
;;                          (eval sexpr)))))
               t)))
    (when (streamp pathspec)
      (return-from fine-grain-instrumented-load
        (load-stream pathspec "<stream>")))
    (let ((pathname (pathname pathspec)))
      (with-open-stream
          (stream (or (open pathspec :element-type '(unsigned-byte 8)
                            :if-does-not-exist nil)
                      (when (null (pathname-type pathspec))
                        (let ((defaulted-pathname
                               (sb-fasl::probe-load-defaults pathspec)))
                          (if defaulted-pathname
                              (progn (setq pathname defaulted-pathname)
                                     (open pathname
                                           :if-does-not-exist
                                           (if if-does-not-exist :error nil)
                                           :element-type '(unsigned-byte 8))))))
                      (if if-does-not-exist
                          (error 'simple-file-error
                                 :pathname pathspec
                                 :format-control
                                 "~@<Couldn't load ~S: file does not exist.~@:>"
                                 :format-arguments (list pathspec)))))
        (unless stream
          (return-from fine-grain-instrumented-load nil))

        (let* ((header-line (make-array
                             (length sb-fasl::*fasl-header-string-start-string*)
                             :element-type '(unsigned-byte 8))))
          (read-sequence header-line stream)
          (if (mismatch header-line sb-fasl::*fasl-header-string-start-string*
                        :test #'(lambda (code char) (= code (char-code char))))
              (let ((truename (probe-file stream)))
                (when (and truename
                           (string= (pathname-type truename)
                                    sb-fasl::*fasl-file-type*))
                  (error 'fasl-header-missing
                         :stream (namestring truename)
                         :fhsss header-line
                         :expected sb-fasl::*fasl-header-string-start-string*)))
              (progn
                (file-position stream :start)
                (return-from fine-grain-instrumented-load
                  (load-stream stream pathname))))))
      (with-open-file (stream pathname :external-format external-format)
        (load-stream stream pathname)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dependency Comparison ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun enough-component-name-from-reader (c)
;;   (read-from-string (maybe-translated-component-name c)))

;; (defun read-definition-from-asd (pathname)
;;   (with-open-file (f pathname)
;;     (let ((package (asdf::make-temporary-package)))
;;       (loop :for expr = (let ((*package* package))
;;                          (read f nil nil))
;;             :until (null expr)
;;             :do (cond ((consp expr)
;;                       (case (first expr)
;;                         (cl:in-package
;;                          (setf *package* (find-package (second expr))))
;;                         (asdf:defsystem
;;                          (return-from read-definition-from-asd
;;                            (getf (nthcdr 2 expr) :components))))))))))

;; ;; Used only by print-dependencies-comparison.
;; (defun dependency-forms-to-asdoid (systems)
;;   (apply #'append
;;          (mapcar
;;           (lambda (sys)
;;             (loop :for (comp nil deps) :in (getf (cdr sys) :components)
;;                   :collect `(,(maybe-translated-component-class comp)
;;                                    ,(enough-component-name-from-reader comp)
;;                                    :depends-on ,(remove-duplicates
;;                                                        `(,@(or (mapcar #'read-from-string
;;                                                                        (overridden-dependencies* comp))
;;                                                                (mapcar #'enough-component-name-from-reader deps))
;;                                                            ,@(mapcar #'read-from-string
;;                                                                      (additional-dependencies* comp)))
;;                                                        :test #'equal))))
;;           systems)))

;; ;; Used only by asdf:perform(compare-dependency-op, component-file).
;; (defun grovel-and-compare-dependencies (systems base-asd-file
;;                                         interesting-systems
;;                                         &key verbose debug-object-types
;;                                         cull-redundant
;;                                         output
;;                                         (base-pathname
;;                                          (truename
;;                                           (make-pathname
;;                                            :type nil
;;                                            :name nil
;;                                            :defaults *load-truename*))))
;;   (with-new-groveling-environment (verbose debug-object-types base-pathname)
;;     (dolist (system systems)
;;       (with-groveling-macroexpand-hook
;;           (asdf:oos 'asdf:load-op system :verbose verbose)))
;;     (multiple-value-bind (depforms error-p)
;;         (print-dependencies-comparison *standard-output* ;; also print to a file?
;;                                        base-asd-file interesting-systems :cull-redundant cull-redundant)
;;       (with-open-file (component-stream output
;;                        :direction :output
;;                        :if-does-not-exist :create
;;                        :if-exists :supersede)
;;         (output-component-file component-stream depforms :output-systems-and-dependencies-p nil))
;;       error-p)))

;; ;; Used only by grovel-and-compare-dependencies.
;; (defun print-dependencies-comparison (stream base-asd-file interesting-systems
;;                                       &key cull-redundant)
;;     (symbol-macrolet ((state *current-dependency-state*))
;;       (let* (missing-dependencies
;;              redundant-deps
;;              (depforms (dependency-forms state interesting-systems :cull-redundant cull-redundant))
;;              (deps (dependency-forms-to-asdoid depforms))
;;             ;; read the base asd file, assuming it contains one system:
;;             (current-deps (read-definition-from-asd base-asd-file)))
;;         ;; find missing/new components
;;         (let* ((c-in-both (intersection deps current-deps :key #'second :test #'equal))
;;                (c-missing-in-result (set-difference current-deps c-in-both :key #'second :test #'equal))
;;                (c-missing-in-base (set-difference deps c-in-both :key #'second :test #'equal))
;;                ;; component specs from both lists:
;;                (my-comps (sort (remove-if-not (lambda (cs)
;;                                                 (member (second cs) c-in-both
;;                                                         :key #'second :test #'equal))
;;                                               deps)
;;                                #'string<
;;                                :key #'second))
;;                (cur-comps (sort (remove-if-not (lambda (cs)
;;                                                  (member (second cs) c-in-both
;;                                                          :key #'second :test #'equal))
;;                                                current-deps)
;;                                 #'string<
;;                                 :key #'second)))
;;           (loop :for my-comp :in my-comps
;;                 :for cur-comp :in cur-comps
;;                 :for mc-deps = (getf my-comp :depends-on)
;;                 :for cc-deps = (getf cur-comp :depends-on)
;;                 :for cc-redundant-deps = (set-difference cc-deps mc-deps :test #'equal)
;;                 :for cc-missing-deps = (set-difference mc-deps cc-deps :test #'equal)
;;                 :do (assert (equal (second cur-comp) (second my-comp))
;;                             (cur-comp my-comp))
;;                 :do (unless (null cc-redundant-deps)
;;                       (push `(,(second cur-comp) ,@cc-redundant-deps) redundant-deps))
;;                 :do (unless (null cc-missing-deps)
;;                       (push `(,(second cur-comp) ,@cc-missing-deps) missing-dependencies)))
;;           (format stream "~:[~;~&Components only in ~A:~{~&     ~S~}~]"
;;                   c-missing-in-result base-asd-file c-missing-in-result)
;;           (format stream "~:[~;~&Components missing in ~A:~{~&     ~S~}~]"
;;                   c-missing-in-base base-asd-file c-missing-in-base)
;;           (format stream "~:[~;~&Unnecessary dependencies in ~A:~:{~&     ~S =>~@{ ~S~}~}~]"
;;                   redundant-deps base-asd-file redundant-deps)
;;           (format stream "~:[~;~&Missing dependencies in ~A:~&~:{~&     ~S =>~@{ ~S~}~}~]~%"
;;                   missing-dependencies base-asd-file missing-dependencies)
;;           (values depforms missing-dependencies)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Regroveling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This section is stuff that's only used for re-grovel-dependencies, which
;; exists only to be exported.

;; (defun dependency-op-done-p (component)
;;   (let* ((input-files (asdf:input-files
;;                        (make-instance 'asdf:compile-op)
;;                        component))
;;          (file (and input-files (probe-file (first input-files)))))
;;     (cond ((null input-files)
;;            t)
;;           ((not
;;             (or (null file)
;;                 (null (gethash component *component-dependency-op-times*))))
;;            (< (file-write-date file)
;;               (gethash component *component-dependency-op-times*))))))

;; (defun update-component-order (state load-systems
;;                                interesting-systems)
;;   (with-slots (component-counter order-table) state
;;      (setf component-counter 0)
;;      (clrhash order-table)

;;      (dolist (system interesting-systems)
;;        ;; undo all load-ops for files:
;;        (dolist (c (asdf-system-file-components system))
;;          (remhash 'asdf:load-op (asdf::component-operation-times c))))
;;      (delete-duplicates
;;       (loop :for system :in load-systems
;;             :append
;;             (loop :for (op . c) :in (asdf::traverse (make-instance 'asdf:load-op)
;;                                        system)
;;                   :do (note-operating-on-component c state)
;;                   :unless (dependency-op-done-p c)
;;                   :collect c)))))

;;; Re-groveling works like this:
;;;  We keep a dependency state, which contains:
;;;   * A table that maps forms to provider components
;;;   * One that maps components to forms they use
;;;   * One that assigns a compilation order to the components
;;;  The last table is necessary to avoid cyclic dependencies: components
;;;  can depend only on components that came before them in the compile order.
;;;
;;;  ADG finds out the new order of components (by tricking
;;;  ADSF::TRAVERSE into thinking all operations on a system are
;;;  un-done, and walking the emitted series of operations), then
;;;  compiles and loads only those files that changed, updating the
;;;  dependency state.
;;;
;;;  From that state, it generates the component list like the regular
;;;  groveler.

;; (defun re-grovel-dependencies (systems stream interesting-systems state
;;                                &key verbose debug-object-types
;;                                (base-pathname
;;                                 (truename
;;                                  (make-pathname
;;                                   :type nil
;;                                   :name nil
;;                                   :defaults *load-truename*))))
;;   (with-groveling-environment (state verbose debug-object-types base-pathname)
;;     (loop :with load-op = (make-instance 'asdf:load-op)
;;           :with compile-op = (make-instance 'asdf:compile-op)
;;           :for component :in (update-component-order state systems
;;                                                      interesting-systems)
;;           :do (with-groveling-macroexpand-hook
;;                (handler-bind ((error
;;                                (lambda (c)
;;                                  (format *error-output*
;;                                          "Caught error \"~A\" while re-groveling on ~
;;                                    ~A. Continuing anyway.~%"
;;                                          c component)
;;                                  (invoke-restart 'continue-groveling))))

;;                  (with-simple-restart (continue-groveling "Continue groveling with the next component after ~A" component)
;;                    (operating-on-component (component :update-counter-p nil)
;;                      (asdf:perform compile-op component)
;;                      (asdf:perform load-op component))))))
;;     (let ((deps (;; compute-dependencies
;;                  dependency-forms state interesting-systems)))
;;       (output-component-file stream deps))
;;     state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
