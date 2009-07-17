;;; Handlers for functions defined in the CLHS.
;;;
;;; TODO:
;; * handle more def*:
;;  * defvar & defparameter - works for symbols, but thinks any use
;;    /somewhere/ in a macro is a dependency.
;;  * define-compiler-macro - argh. seriously, no idea. if you use
;;    this at compile/load time, sorry.
;;  * deftype - we can signal that it was provided, but have to walk
;;    declarations in def* and generally almost /everwhere/. not fun.

#+xcvb (module (:depends-on ("grovel")))

(in-package :asdf-dependency-grovel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro define-macroexpand-handlers ((form &key
                                        (environment (gensym) environmentp)
                                        (function (gensym) functionp))
                                       (&rest form-names) &body body)
  ;;; TODO: destructuring-bind form?
  `(progn
     ,@(loop :for form-name :in form-names
             :for symbol-name = (if (symbolp form-name)
                                    (symbol-name form-name)
                                    (string (second form-name)))
             :for pkg-name = (canonical-package-name
                              (if (symbolp form-name)
                                  (symbol-package form-name)
                                  (first form-name)))
             :for fun-name = (intern (format nil "HANDLE-MACROEXPANSION-~A/~A"
                                             pkg-name symbol-name)
                                     :asdf-dependency-grovel)
             :collect `(defun ,fun-name
                        (,form &key
                         ,@(and functionp `(((:function ,function))))
                         ,@(and environmentp `(((:environment ,environment))))
                         &allow-other-keys)
                        ,@body)
             :collect `(setf (gethash (list ',pkg-name ',symbol-name)
                              *macroexpansion-handlers*)
                        ',fun-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;; Variable-Related Handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macroexpand-handlers (form)
    (defvar defparameter)
  (signal-provider (second form) 'defvar)
;;   (unless *using-constituents*
;;     (setf (gethash (second form) *suspected-variables*) t))
  (does-not-macroexpand))


(define-macroexpand-handlers (form :environment env)
    (defconstant)
  (let ((symbol (second form)))
    (signal-provider (second form) (first form))
    (does-macroexpand-with-epilogue
     ((macro-function 'symbol-macroify) env)
     `(symbol-macroify ,@form)
     `((eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (symbol-value ',symbol) ,symbol))
       ',symbol))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Macro-Related Handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macroexpand-handlers (form)
    (defmacro define-method-combination)
  (signal-provider (second form) (first form))
  (does-not-macroexpand))


(define-macroexpand-handlers (form :function fun :environment env)
    (define-symbol-macro)
  (destructuring-bind (def name expansion) form
    (signal-provider name def)
    (does-macroexpand (fun env)
      `(,def ,name (signal-symbol-macroexpansion ',name ,expansion)))))


;;;;;;;;;;;;;;;;;;;;;;;;;; Function-Related Handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macroexpand-handlers (form :function fun :environment env)
    (defun)
  (destructuring-bind (defun name arg-list &rest maybe-body) form
    (signal-provider name (first form))
    (does-macroexpand (fun env)
      `(,defun ,name ,arg-list
         ,@(instrument-defun-body maybe-body
                                  `(signal-user ',name 'defun))))))


;; I think instrumenting lambdas is overkill.  Most of the time, such as when
;; the lambda is an argument to e.g. mapcar, the instrumentation accomplishes
;; nothing.  The only time it would seem to help is if one constituent stores a
;; lambda somewhere, and other uses it, but that case ought to be caught by
;; e.g. the defparameter handler.  Having lambda instrumentation makes QPX take
;; a very long time to build.  (msteele)

;; (define-macroexpand-handlers (form :function fun :environment env)
;;     (lambda)
;;   (let ((name (gentemp "ASDF-DEPENDENCY-GROVEL-LAMBDA"
;;                        '#:asdf-dependency-grovel.lambdas)))
;;     (destructuring-bind (l arg-list &rest maybe-body) form
;;       (signal-provider name 'defun)
;;       (does-macroexpand (fun env)
;;         `(,l ,arg-list
;;              ,@(instrument-defun-body maybe-body
;;                                       `(signal-user ',name 'defun)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Method-Related Handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macroexpand-handlers (form)
    (defgeneric)
  (signal-provider (second form) 'defgeneric)
  (let ((method-combination (second (assoc :method-combination
                                           (nthcdr 3 form)))))
    (when method-combination
      (signal-user method-combination 'define-method-combination)))
  (does-not-macroexpand))


(define-macroexpand-handlers (form :function fun :environment env)
    (defmethod)
  (signal-user  (second form) 'defgeneric)
  ;; walk arg list and signal use of specialized-on classes, and
  ;; instrument function body.
  (let* ((name (second form))
         (new-expansion
          `(defmethod ,name
               ,@(loop :for (elt . body) :on (nthcdr 2 form)
                       :if (not (listp elt))
                         :collect elt :into modifiers
                       :else
                         :do (signal-provider `(,name ,@modifiers ,elt)
                                              'defmethod)
                         :and :do
                           (loop :for arg :in elt
                                 :when (and (listp arg)
                                            (symbolp (second arg)))
                                   :do (signal-user (second arg) 'defclass))
                         :and :collect elt :into modifiers
                         :and :collect elt
                         :and :append (instrument-defun-body
                                       body
                                       `(signal-user
                                         '(,name ,@modifiers)
                                         'defmethod))
                         :and :do (loop-finish)
                       :collect elt))))
    ;; (format *debug-io* "~&~S becomes:~&~S~%~%" form new-expansion)
    (does-macroexpand (fun env :macroexpand-hook *macroexpand-hook*)
      new-expansion)))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Class-Related Handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macroexpand-handlers (form)
    (defclass define-condition)
  (signal-provider (second form) (first form))
  (signal-provider (second form) 'deftype)
  ;; signal use of direct superclasses/superconditions. Note that we
  ;; declare a dependency only if the direct superclass is already
  ;; defined through the current system definition.
  (loop :for superclass :in (third form)
        :do (signal-user superclass (first form))
        :do (signal-user (second form) 'deftype))
  (loop :for slot :in (fourth form)
        :for slot-type = (and (consp slot)
                              (getf (cdr slot) :type))
        :do (when slot-type
              (signal-user slot-type 'deftype)))
  (does-not-macroexpand))


(define-macroexpand-handlers (form :function fun :environment env)
    (defstruct)
  (destructuring-bind (header &rest body) (cdr form)
    (destructuring-bind (name &rest struct-options)
        (if (listp header) header (list header))
      ;; Provide the class and type for this struct.
      (signal-provider name 'defclass)
      (signal-provider (second form) 'deftype)
      ;; Deal with accessor functions.  The defstruct macro automatically
      ;; defines a whole bunch of accessor functions, which we must
      ;; "retroactively" instrument.  In particular, we must "manually"
      ;; determine the names of the accessors, provide them (using
      ;; signal-provider), and then wrap them so that they will call
      ;; signal-user when called.
      (let (;; Determine the prefix used to create accessor function names.
            (prefix (let ((conc-name-option (assoc :conc-name struct-options)))
                      (if conc-name-option
                          (string (second conc-name-option))
                          (concatenate 'string (symbol-name name) "-"))))
            ;; Get the list of slot descriptions by chopping off the docstring
            ;; (if any) from `body'.
            (slot-descriptions (if (and (consp body) (stringp (car body)))
                                   (cdr body) body)))
        (loop :for slot-desc :in slot-descriptions
              ;; Get the symbol for this slot's accessor.
              :as accessor-name =
                (destructuring-bind (slot-name &optional slot-init-value
                                          &rest slot-options)
                    (if (listp slot-desc) slot-desc (list slot-desc))
                  (declare (ignore slot-init-value slot-options))
                  (let* ((slot-name-str (symbol-name slot-name))
                         (accessor-name-str
                          (concatenate 'string prefix slot-name-str)))
                    (intern accessor-name-str)))
              ;; Provide this accessor.
              :do (signal-provider accessor-name 'defun)
              ;; Generate instrumentation for the accessor.
              :collect (let ((temp (gensym)))
                         `(let ((,temp (function ,accessor-name)))
                            ;; Fun fact: this code _won't_ work properly:
                            ;;     (defun ,accessor-name (&rest args)
                            ;;       (signal-user ',accessor-name 'defun)
                            ;;       (apply ,temp args))
                            ;; ...but this code does.  See CLHS 3.2.2.3.
                            (setf (symbol-function ',accessor-name)
                                  (lambda (&rest args)
                                    (signal-user ',accessor-name 'defun)
                                    (apply ,temp args)))))
                :into redefinitions
              ;; Finally, put all the instrumentation into place.
              :finally (return (does-macroexpand-with-epilogue (fun env)
                                 form
                                 (append redefinitions (list `',name)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Type-Related Handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(labels ((traverse-subtypes (typespec)
           (cond
             ((consp typespec)
              (case (first typespec)
                (quote            ; typespecs often come quoted. argh.
                 (traverse-subtypes (second typespec)))
                ((and or not) (mapcar #'traverse-subtypes (rest typespec)))
                (satisfies (signal-user (second typespec) 'defun))
                ((vector array) (traverse-subtypes (second typespec)))
                (function (and (second typespec)
                               (traverse-subtypes (second typespec))
                               (and (third typespec)
                                    (traverse-subtypes (third typespec)))))
                (:otherwise nil)))
             ((not (null typespec))
              (signal-user typespec 'deftype)))))

  (define-macroexpand-handlers (form)
      (handler-bind handler-case)
    (loop :for (condition . stuff) :in (if (eql 'handler-bind (first form))
                                           (second form)
                                           (cddr form))
          :unless (eql condition :no-error)
            :do (traverse-subtypes condition))
    (does-not-macroexpand))

  (define-macroexpand-handlers (form)
      (deftype)
    (signal-provider (second form) 'deftype)
    (traverse-subtypes (fourth form))
    (does-not-macroexpand))

  (define-macroexpand-handlers (form)
      (typecase etypecase)
    (loop :for (typespec . rest) :in (cddr form)
          :do (traverse-subtypes typespec))
    (does-not-macroexpand))

  (define-macroexpand-handlers (form)
      (check-type)
    (traverse-subtypes (third form))
    (does-not-macroexpand)))


;;;;;;;;;;;;;;;;;;;;;;;;;; Package-Related Handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macroexpand-handlers (form)
    (defpackage)
  ;; signal a use for the package first, to do the package
  ;; redefinition dance right.
  (signal-user (canonical-package-name (second form))
               'defpackage)
  ;; Provide the package.
  (signal-provider (canonical-package-name (second form))
                   'defpackage)
  (labels ((clause-contents (clause-name &optional (filter #'rest))
             (mapcar #'canonical-package-name
                     (reduce #'append
                             (mapcar filter
                                     (remove-if-not (lambda (clause)
                                                      (eql clause-name
                                                           (first clause)))
                                                    (cddr form))))))
           (clause-second-element (clause)
             (list (second clause))))
    ;; Provide each nickname of the package.
    (loop :for nickname :in (clause-contents :nickname)
          :do (signal-provider nickname 'defpackage))
    ;; Signal uses of packages.
    (loop :for use :in (append (clause-contents :use)
                               (clause-contents :import-from
                                                #'clause-second-element)
                               (clause-contents :shadowing-import-from
                                                #'clause-second-element))
          :do (signal-user (canonical-package-name use) 'defpackage))
    ;; signal imports of symbols (they need to exist before they can
    ;; be imported)
    (when *check-internal-symbols-p*
      (loop :for (clause-name pkg . symbols)
            :in (remove-if-not (lambda (clause)
                                 (member (first clause)
                                         '(:import-from
                                           :shadowing-import-from)))
                               (cddr form))
            :do (loop :for sym :in symbols
                      :do (signal-user (find-symbol (string sym) pkg)
                                       'internal-symbol)))))
  (does-not-macroexpand))


(define-macroexpand-handlers (form)
    (in-package)
  ;; If we're checking internal symbols, we need to account for the fact that
  ;; we're switching packages.
  (when *check-internal-symbols-p*
    (signal-new-internal-symbols :populate nil)
    (clrhash *previously-interned-symbols*)
    (setf *previous-package* (find-package (second form)))
    (do-symbols (symbol *previous-package*)
      (hashset-add symbol *previously-interned-symbols*)))
  ;; Signal that we're using the package.
  (signal-user (canonical-package-name (second form)) 'defpackage)
  (does-not-macroexpand))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Miscellaneous Handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macroexpand-handlers (form)
    (defsetf define-setf-expander)
  (signal-provider (second form) 'setf)
  (does-not-macroexpand))


(define-macroexpand-handlers (form)
    (setf)
  (loop :for (setee value) :on (cdr form) :by #'cddr
        :do (cond
              ((consp setee)
               (signal-user (first setee) 'setf))
              ((and (symbolp setee) (boundp setee))
               (signal-user setee 'defvar)
               (signal-provider setee 'defvar))))
  (does-not-macroexpand))


(define-macroexpand-handlers (form)
    (pushnew push)
  (let ((setee (third form)))
    (when (and (symbolp setee) (boundp setee))
      (signal-user setee 'defvar)
      (signal-provider setee 'defvar))))


(define-macroexpand-handlers (form :function fun :environment env)
    (with-open-file)
  (destructuring-bind (stream pathname &key (direction :input)
                              &allow-other-keys)
      (second form)
    (declare (ignore stream))
    (if (eql direction :output)
        (does-macroexpand (fun env)
            `(with-open-file ,(second form)
               ,@(instrument-defun-body
                  (cddr form)
                  `(signal-provider
                    (namestring (merge-pathnames ,pathname))
                    'file-component))))
        (does-not-macroexpand))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
