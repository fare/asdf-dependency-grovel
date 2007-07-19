;;; ADSF dependency groveler using macroexpand-hook. Fairly precise,
;;; at least for the mcclim system.

(cl:in-package #:asdf-dependency-grovel)

;;; macroexpand hook and helper functions/macros

(defun canonical-package-name (package-designator)
  (intern (typecase package-designator
            (package (package-name package-designator))
            (t (string package-designator)))
          :asdf-dependency-grovel.packages))

(defun signal-macroexpansion (hook name macro-type &optional (component *current-component*))
  (when (and hook component)
    (funcall hook name macro-type component)))

(defun signal-user (name form-type)
  (signal-macroexpansion *user-hook* name form-type)
  (values))

(defun signal-provider (name form-type)
  (signal-macroexpansion *provider-hook* name form-type)
  (values))

(defun signal-symbol-use-in-form (form)
  (labels ((signal-for-symbol (sym)
             (unless (or (null (symbol-package sym))
                         (member (symbol-package sym)
                                 (mapcar #'find-package '(:keyword :cl))))
               (signal-user (canonical-package-name (symbol-package sym)) 'defpackage))))
    (cond ((symbolp form) (signal-for-symbol form))
          ((consp form)
           (loop for (car . cdr) on form
                 do (signal-symbol-use-in-form car)
                 if (symbolp cdr)
                   do (signal-for-symbol cdr))))))

(defun signal-possible-special-variable-use-in-form (form)
  (labels ((signal-for-symbol (sym)
             (when (gethash sym *suspected-variables*)
               (signal-user sym 'defvar))))
    (cond ((symbolp form)
           (signal-for-symbol form))
          ((consp form)
           (loop for (car . cdr) on form
                 do (signal-possible-special-variable-use-in-form car)
                 if (symbolp cdr)
                   do (signal-for-symbol cdr))))))

(defun signal-symbol-macroexpansion (name expansion)
  (signal-user name 'define-symbol-macro)
  expansion)

(defsetf signal-symbol-macroexpansion (name expression) (new-value)
  `(progn
     (signal-user ',name 'define-symbol-macro)
     (setf ,expression ,new-value)))

(defmacro symbol-macroify (operator name &rest args &environment env)
  (let ((new-name (gentemp (format nil "asdf-dependency-grovel-~A-" operator))))
    `(progn
       (define-symbol-macro ,name ,new-name)
       ,(macroexpand `(,operator ,new-name ,@args) env))))

(defmacro define-symbol-alias (new-symbol ansi-symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (when (and (boundp '*symbol-translations*) (hash-table-p *symbol-translations*))
       (setf (gethash ',new-symbol *symbol-translations*) ',ansi-symbol))))

(defun unalias-symbol (form)
  (if (boundp '*symbol-translations*)
      (or (gethash form *symbol-translations*)
          form)
      form))

(defun instrument-defun-body (body form)
  "Insert FORM into list of defun body forms BODY such that the
return value of BODY is the same as it would be without FORM,
keeping declarations intact."
  `(,@
    (when (stringp (first body))
      (list (pop body)))
    ,@(loop for (elt . body) on (or body (list nil))
            if (or (and (listp elt)
                        (not (eql (first elt) 'declare)))
                   (not (listp elt)))
              collect form
              and collect elt
              and append body
              and do (loop-finish)
            if (null body)
              collect elt
              and collect form
              and collect nil
              and do (loop-finish)
            collect elt)))

(defmacro noticing-*feature*-changes (&rest body)
  ;; naive implementation, doesn't really deal with removed features
  (let ((old-features (gensym))
        (new-feature (gensym))
        (removed-feature (gensym)))
    `(let ((,old-features (copy-list *features*)))
       (prog1 (progn ,@body)
              (dolist (,new-feature (set-difference *features* ,old-features))
                (signal-provider ,new-feature 'feature))
              (dolist (,removed-feature (set-difference ,old-features *features*))
                (signal-provider ,removed-feature 'removed-feature))))))


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

(defun make-instrumented-readtable (&optional (readtable *readtable*))
  (setf readtable (copy-readtable readtable))
  (set-dispatch-macro-character #\# #\+ #'instrumented-sharp+ readtable)
  (set-dispatch-macro-character #\# #\- #'instrumented-sharp- readtable)
  ;; TODO: #.
  readtable)

(defun does-not-macroexpand ()
  (values nil nil))

(defmacro does-macroexpand ((function env &key (macroexpand-hook '*old-macroexpand-hook*))
                            &body new-macro-body)
  `(values t
           (let ((*macroexpand-hook* ,macroexpand-hook))
             (funcall *old-macroexpand-hook* ,function
                      (progn ,@new-macro-body) ,env))))

(defun handle-macroexpansion (translated-name form function environment)
  (let ((handler (gethash translated-name *macroexpansion-handlers*)))
    (if  handler
         (funcall handler
                  form :function function :environment environment)
         (signal-user (first form) 'defmacro))))

;;; The hook itself
(defun instrumenting-macroexpand-hook (fun form env)
  (when (listp form)
    (multiple-value-bind (replacep new-form)
        (handle-macroexpansion (unalias-symbol (first form)) form fun env)
      (signal-symbol-use-in-form form)
      ;; XXX: heuristic, doesn't catch everything:
      (signal-possible-special-variable-use-in-form form)
      (if replacep
          new-form
          (let ((expanded (funcall *old-macroexpand-hook* fun form env)))
            expanded)))))

;;; The actual groveling part.

(defun enough-component-spec (c &optional pn-p)
  (flet ((strip/ (name)
           (subseq name (or (position #\/ name :from-end t) 0))))
    (if (equal (parse-namestring (enough-namestring (asdf:component-pathname c)))
               (make-pathname :name (asdf:component-name c) :type "lisp"))
        (format nil "~S" (asdf:component-name c))
        (let ((pn (parse-namestring (enough-namestring (asdf:component-pathname c)))))
          (format nil "~S~:[~; :pathname #.(make-pathname :directory '~S :name ~S :type \"lisp\")~%~]"
                  (enough-namestring (make-pathname :name (strip/ (asdf:component-name c))
                                                    :type nil :defaults (asdf:component-pathname c)))
                  pn-p
                  (pathname-directory pn)
                  (pathname-name pn))))))

(defun system-file-components (system)
  "Flatten the tree of modules/components into a list that
contains only the non-module components."
  (loop for component in (asdf:module-components system)
        if (typep component 'asdf:module)
          append (system-file-components component)
        else
          collect component))

(defun map-over-instrumented-component-and-parents (component slot-name)
  (loop for c = component then (asdf:component-parent c)
        until (null c)
        when (and (typep c 'instrumented-component)
                  (slot-boundp c slot-name))
          append (slot-value c slot-name)))

(defun dwim-stringify-component-spec (component-spec)
  (case (char component-spec 0)
    ((#\# #\") ; is a subform, insert as-is
     component-spec)
    (otherwise ; should be a string, make it one.
     (concatenate 'string (string #\") component-spec (string #\")))))

(defun additional-dependencies* (component)
  "Walk the tree up through all parent components and collect
their :additional-dependencies."
  (mapcar #'dwim-stringify-component-spec
          (map-over-instrumented-component-and-parents component 'additional-dependencies)))

(defun overridden-dependencies* (component)
  (mapcar #'dwim-stringify-component-spec
          (map-over-instrumented-component-and-parents component 'overridden-dependencies)))

(defun maybe-translated-component-name (component &key include-pathname)
  
  (if (and (typep component 'instrumented-component)
                   (slot-boundp component 'translated-name))
      (format nil "~A~@[ :pathname #.~S~]"
              (slot-value component 'translated-name)
              (and include-pathname
                   (slot-value component 'translated-pathname)))
      (enough-component-spec component include-pathname)))

(defclass grovel-state ()
     ((dir-suffix :initform (get-universal-time)
                  :initarg :dir-suffix
                  :reader dir-suffix)
      (providers :initform (make-hash-table :test #'equal)
                 :initarg :providers
                 :reader providers)
      (dependencies :initform (make-hash-table :test #'eql)
                    :initarg :dependencies
                    :reader dependencies)
      (suspected-variables :initform (make-hash-table :test #'eql)
                           :initarg :suspected-variables
                           :reader suspected-variables)
      (symbol-translations :initform  (make-hash-table)
                           :initarg :symbol-translations
                           :reader symbol-translations)))

(defun simple-copy-hash-table (hash-table &key (test #'eql) &aux (return (make-hash-table :test test)))
  (loop for key being the hash-keys in hash-table using (hash-value value)
        do (setf (gethash key return) value))
  return)

(defun copy-state (state)
  (make-instance 'grovel-state
     :dir-suffix (dir-suffix state)
     :providers (simple-copy-hash-table (providers state) :test #'equal)
     :dependencies (simple-copy-hash-table (dependencies state) :test #'eql)
     :suspected-variables (simple-copy-hash-table (suspected-variables state) :test #'eql)
     :symbol-translations (simple-copy-hash-table (symbol-translations state)
                                                  :test #'eql)))

(defun grovel-dependencies (system-or-systems stream &key interesting verbose cull-redundant
                            (base-pathname (truename
                                            (make-pathname
                                             :type nil
                                             :name nil
                                             :defaults *load-truename*)))
                            initial-state)
  (let* ((state (if initial-state
                    (copy-state initial-state)
                    (make-instance 'grovel-state)))
         (systems (mapcar #'asdf:find-system
                          (if (consp system-or-systems)
                              system-or-systems
                              (list system-or-systems))))
         (providers (providers state))
         (dependencies (dependencies state))

         (*old-macroexpand-hook* *macroexpand-hook*)
         (*macroexpand-hook* #'instrumenting-macroexpand-hook)
         (*symbol-translations* (symbol-translations state))
         (*default-pathname-defaults* base-pathname)
         (*grovel-dir-suffix* (dir-suffix state))
         (*suspected-variables* (suspected-variables state)))
    (labels ((interestingp (component)
               (member (asdf:component-system component) interesting :test #'eql))
             (redundantp (from to)
               (labels ((redundantp-1 (from-1)
                          (let ((from-deps (gethash from-1 dependencies)))
                            (cond ((and (member to from-deps) (not (eql from from-1))) t)
                                  (t (dolist (from-dep from-deps)
                                       (when (redundantp-1 from-dep)
                                         (return-from redundantp t)))
                                     nil)))))
                 (redundantp-1 from)))
             (new-dep (from to-components)
               (when (and (interestingp from)
                          (some #'interestingp to-components))
                 (setf (gethash from dependencies)
                       (delete-duplicates (append (remove-if-not #'interestingp to-components)
                                                  (gethash from dependencies))))
                 (loop for component in to-components
                       do (pushnew (asdf:component-system component)
                                   (gethash (asdf:component-system from) dependencies)))))
             (coerce-name (maybe-class)
               (if (typep maybe-class 'standard-class)
                   (class-name maybe-class)
                   maybe-class)))
      (let ((*provider-hook*
             (lambda (name type component)
               (pushnew component (gethash (list type name) providers))))
            (*user-hook*
             (lambda (name type component)
               (cond ((and (gethash (list type name) providers)
                           (not (member component (gethash (list type name) providers))))
                               
                      (new-dep component (gethash (list type name) providers)))))))
        (let ((*compile-print* verbose)
              (*compile-verbose* verbose)
              ;; (*break-on-signals* 'warning)
              )
          (dolist (system systems)
            (asdf:oos 'asdf:load-op system :verbose verbose)))
        (let ((*print-case* :downcase))
          (format stream ";;; This file contains -*- lisp -*- expressions.~%")
          (format stream "~@<;;; ~@;AUTO-GENERATED file from system definition of system ~A ~
                                   Instead of directly editing this file, please edit the system definition~P of~
                                   ~{ ~A~^ or~}, then re-generate this file.~:@>"
                  (mapcar #'asdf:component-name systems)
                  (length interesting) (mapcar #'asdf:component-name interesting))
          (format stream "~&(~%")
          (dolist (system interesting)
            (format stream "~& (~S~@[ :depends-on ~:S~] :components~%  (" (asdf:component-name system)
                    (delete (asdf:component-name system)
                            (mapcar #'asdf:component-name (gethash system dependencies))
                            :test #'equal))
            (loop for component in (system-file-components system)
                  for 1-dependencies = (gethash component dependencies)
                  do (let ((*package* (find-package :keyword)))
                       (format stream "~&   (~S ~A~@[~&    :depends-on ~:A~]~@[~&    ~{ ~S~}~])"
                               ;; component class:
                               (cond
                                 ;; standard instrumented component with no output file type, and
                                 ;; default-component-type files are emitted as :file.
                                 ((and (typep component 'instrumented-cl-source-file)
                                       (not (slot-boundp component 'output-file-type)))
                                  :file)
                                 ((and (not (typep component 'instrumented-cl-source-file))
                                       (eql (class-of component)
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
                                  (class-name (class-of component))))

                               ;; component names:
                               (maybe-translated-component-name component :include-pathname t)

                               ;; component dependencies:
                               (remove-duplicates
                                `(,@(sort
                                     `(,@(or (overridden-dependencies* component)
                                             (mapcar #'maybe-translated-component-name
                                                     (remove-if (lambda (comp)
                                                                  (and cull-redundant (redundantp component comp)))
                                                                1-dependencies)))
                                         ,@(additional-dependencies* component))
                                     #'string<))
                                :test #'equal)

                               ;; component initargs
                               (and (typep component 'instrumented-cl-source-file)
                                    (additional-initargs component)))))
            (format stream "))~%"))
          (format stream "~&)~%"))))
    state))


