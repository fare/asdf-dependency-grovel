;;; ADSF dependency groveler using macroexpand-hook. Fairly precise,
;;; at least for the mcclim system.

;;; TODO:
;; * handle more def*:
;;  * defvar & defparameter - fails because of special declarations.
;;    if it worked, it would be the same as defconstant.
;;  * define-compiler-macro - argh. seriously, no idea. if you use
;;    this at compile/load time, sorry.
;;  * deftype - we can signal that it was provided, but have to walk
;;    declarations in def* and generally almost /everwhere/. not fun.

;; * more package games: :use, :import-from, :shadow?

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

(defun signal-symbol-use-in-form (form)
  (labels ((signal-for-symbol (sym)
             (unless (or (null (symbol-package sym))
                         (member (symbol-package sym)
                                 (mapcar #'find-package '(:keyword :cl))))
               (signal-macroexpansion *user-hook* (canonical-package-name (symbol-package sym)) 'defpackage))))
    (cond ((symbolp form) (signal-for-symbol form))
          ((consp form)
           (loop for (car . cdr) on form
                 do (signal-symbol-use-in-form car)
                 if (symbolp cdr)
                   do (signal-for-symbol cdr))))))

(defun signal-symbol-macroexpansion (name expansion)
  (signal-macroexpansion *user-hook* name 'define-symbol-macro)
  expansion)

(defsetf signal-symbol-macroexpansion (name expression) (new-value)
  `(progn
     (signal-macroexpansion *user-hook* ',name 'define-symbol-macro)
     (setf ,expression ,new-value)))

(defmacro symbol-macroify (operator name &rest args &environment env)
  (let ((new-name (gentemp)))
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

(defun instrumenting-macroexpand-hook (fun form env)
  (when (listp form)
    (case (unalias-symbol (first form))
      ((defmacro define-method-combination)
       (signal-macroexpansion *provider-hook* (second form) (first form)))
      ((defsetf define-setf-expander)
       (signal-macroexpansion *provider-hook* (second form) 'setf))
      ((setf)
       (when (listp (second form))
         (signal-macroexpansion *user-hook* (first (second form)) 'setf)))
                  
      ((defgeneric)
       (signal-macroexpansion *provider-hook* (second form) 'defgeneric)
       (let ((method-combination (second (assoc :method-combination (nthcdr 3 form)))))
         (when method-combination
           (signal-macroexpansion *user-hook* method-combination 'define-method-combination))))
      ((defmethod)
       (signal-macroexpansion *user-hook* (second form) 'defgeneric)
       ;; walk arg list and signal use of specialized-on
       ;; classes, and instrument function body.
       (let* ((*macroexpand-hook* *old-macroexpand-hook*)
              (name (second form))
              (new-expansion
               `(defmethod ,name
                    ,@(loop for (elt . body) on (nthcdr 2 form)
                            if (not (listp elt))
                              collect elt into modifiers
                            else
                              do (signal-macroexpansion *provider-hook* `(,name ,@modifiers ,elt) 'defmethod)
                              and do
                                (loop for arg in elt
                                      when (and (listp arg)
                                                (symbolp (second arg)))
                                        do (signal-macroexpansion *user-hook* (second arg) 'defclass))
                              and collect elt into modifiers
                              and collect elt
                              and append (instrument-defun-body body
                                                    `(signal-macroexpansion
                                                            *user-hook*
                                                            '(,name ,@modifiers)
                                                            'defmethod))
                              and do (loop-finish)
                            collect elt))))
         ;; (format *debug-io* "~&~S becomes:~&~S~%~%" form new-expansion)
         (return-from instrumenting-macroexpand-hook
           (funcall *old-macroexpand-hook* fun new-expansion env))))
      ((defclass define-condition)
       (signal-macroexpansion *provider-hook* (second form) (first form))
       ;; signal use of direct
       ;; superclasses/superconditions. Note that we
       ;; declare a dependency only if the direct
       ;; superclass is already defined through the
       ;; current system definition.
       (loop for superclass in (third form)
             do (signal-macroexpansion *user-hook* superclass (first form))))
      ((defstruct)
       (signal-macroexpansion *provider-hook* (second form) 'defclass))
      ((defpackage)
       (signal-macroexpansion *provider-hook*
                              (canonical-package-name (second form))
                              'defpackage)
       (labels ((clause-contents (clause-name &optional (filter #'rest))
                  (mapcar #'canonical-package-name
                          (reduce #'append
                                  (mapcar filter
                                          (remove-if-not (lambda (clause)
                                                           (eql clause-name
                                                                (first clause)))
                                                         (nthcdr 2 form))))))
                (clause-second-element (clause)
                  (list (second clause))))
         (loop for nickname in (clause-contents :nickname)
               do (signal-macroexpansion *provider-hook* nickname 'defpackage))
         ;; signal :uses of packages
         (loop for use in (append (clause-contents :use)
                                  (clause-contents :import-from
                                         #'clause-second-element)
                                  (clause-contents :shadowing-import-from
                                         #'clause-second-element))
               do (signal-macroexpansion *user-hook*
                                         (canonical-package-name use)
                         'defpackage))))
      ((in-package)
       (signal-macroexpansion *user-hook* (canonical-package-name (second form)) 'defpackage))

      ((defconstant)
       (signal-macroexpansion *provider-hook* (second form) (first form))
       (return-from instrumenting-macroexpand-hook
         (let ((*macroexpand-hook* *old-macroexpand-hook*))
           (funcall *old-macroexpand-hook* (macro-function 'symbol-macroify)
                    `(symbol-macroify ,@form) env))))

      ((define-symbol-macro)
       (destructuring-bind (def name expansion) form
         (signal-macroexpansion *provider-hook* name def)
         (return-from instrumenting-macroexpand-hook
           (let ((*macroexpand-hook* *old-macroexpand-hook*))
             (funcall *old-macroexpand-hook* fun
                      `(,def ,name (signal-symbol-macroexpansion ',name ,expansion))
                      env)))))                  
      ((defun)
       (destructuring-bind (defun name arg-list &rest maybe-body) form
         (signal-macroexpansion *provider-hook* name (first form))
         (let ((*macroexpand-hook* *old-macroexpand-hook*))
           (return-from instrumenting-macroexpand-hook
             (funcall *old-macroexpand-hook* fun
                      `(,defun ,name ,arg-list
                         ,@(instrument-defun-body maybe-body
                                                  `(signal-macroexpansion *user-hook* ',name 'defun)))
                      env)))))
      (otherwise (signal-macroexpansion *user-hook* (first form) 'defmacro))))
  (signal-symbol-use-in-form form)
  (funcall *old-macroexpand-hook* fun form env))


;;; The actual groveling part.

(defun enough-component-spec (c &optional pn-p)
  (if (equal (parse-namestring (enough-namestring (asdf:component-pathname c)))
             (make-pathname :name (asdf:component-name c) :type "lisp"))
      (format nil "~S" (asdf:component-name c))
      (let ((pn (parse-namestring (enough-namestring (asdf:component-pathname c)))))
        (format nil "~S~:[~; :pathname #.(make-pathname :directory '~S :name ~S :type \"lisp\")~%~]"
                (enough-namestring (make-pathname :type nil :defaults (asdf:component-pathname c)))
                pn-p
                (pathname-directory pn)
                (pathname-name pn)))))

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

(defun additional-dependencies* (component)
  "Walk the tree up through all parent components and collect
their :additional-dependencies."
  (map-over-instrumented-component-and-parents component 'additional-dependencies))

(defun overridden-dependencies* (component)
  (map-over-instrumented-component-and-parents component 'overridden-dependencies))

(defun maybe-translated-component-name (component &key include-pathname)
  
  (if (and (typep component 'instrumented-cl-source-file)
                   (slot-boundp component 'translated-name))
      (format nil "~A~@[ :pathname #.~S~]"
              (slot-value component 'translated-name)
              (and include-pathname
                   (slot-value component 'translated-pathname)))
      (enough-component-spec component include-pathname)))

(defun grovel-dependencies (system stream &key interesting verbose cull-redundant
                            (base-pathname (truename
                                            (make-pathname
                                             :type nil
                                             :name nil
                                             :defaults (asdf:component-pathname system)))))
  (let* ((system (asdf:find-system system))
         (providers (make-hash-table :test #'equal))
         (dependencies (make-hash-table :test #'eql))

         (*old-macroexpand-hook* *macroexpand-hook*)
         (*macroexpand-hook* #'instrumenting-macroexpand-hook)
         (*symbol-translations* (make-hash-table))
         (*default-pathname-defaults* base-pathname)
         (*grovel-dir-suffix* (get-universal-time)))
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
                       (remove-duplicates (append (remove-if-not #'interestingp to-components)
                                                  (gethash from dependencies)))))))
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
          (asdf:oos 'asdf:load-op system :verbose verbose))
        (let ((*print-case* :downcase))
          (format stream ";;; This file contains -*- lisp -*- expressions.~%")
          (format stream "~@<;;; ~@;AUTO-GENERATED file from system definition of system ~A in ~
                                   ~A.
                                   Instead of directly editing this file, please edit the system definition~P of~
                                   ~{ ~A~^ or~}, then re-generate this file.~:@>"
                  (asdf:component-name system)
                  (make-pathname :directory nil :defaults (asdf:component-pathname system))
                  (length interesting) (mapcar #'asdf:component-name interesting))
          (format stream "~&(~%")
          (dolist (system interesting)
            (format stream "~&;; ex ~A~%" (asdf:component-name system))
            (loop for component in (system-file-components system)
                  for 1-dependencies = (gethash component dependencies)
                  do (let ((*package* (find-package :keyword)))
                       (format stream "~& (~S ~A :depends-on ~:A)~%"
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
                                                         (asdf::module-default-component-class
                                                          (asdf:component-parent component))
                                                         'asdf:cl-source-file))))
                                  :file)
                                 ;; instrumented components with output file types emit
                                 ;; their output file type
                                 ((typep component 'instrumented-cl-source-file)
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
                                :test #'equal)))))
          (format stream "~&)~%"))))))


