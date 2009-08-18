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


;; (defvar *adg-specials* (make-hash-table :test 'eql))
;; (defun special-substitute-name (name)
;;   (or (gethash name *adg-specials*)
;;       (setf (gethash name *adg-specials*)
;;             (gentemp (format nil "ASDF-DEPENDENCY-GROVEL-SPECIAL--~A" name)))))


;; ;; When we hit a declaim, we need to watch out for special variable
;; ;; declarations (all other declarations can be left as-is).  Once a name is
;; ;; declared special, it can no longer be defined as a symbol macro; thus,
;; ;; without this handler, things will break if someone declares a name special
;; ;; before defvar-ing that name.
;; (define-macroexpand-handlers (form :function fun :environment env)
;;     (declaim)
;;   (destructuring-bind (dec &rest claims) form
;;     (loop :for claim :in claims :with new-names := nil
;;        :if (equal (string (car claim)) "SPECIAL")
;;          :do (setf new-names (mapcar #'special-substitute-name (cdr claim)))
;;          :and :collect (cons (car claim) new-names)
;;               :into new-claims
;;          :and :append (let ((*macroexpand-hook* *old-macroexpand-hook*))
;;                         (loop :for name :in (cdr claim)
;;                               :for new-name :in new-names
;;                            :collect (macroexpand-1
;;                                      `(define-symbol-macro ,name
;;                                           (signal-variable-use ',name 'defvar
;;                                                                ',new-name)))))
;;               :into epilogue
;;        :else
;;          :collect claim :into new-claims
;;        :finally (return (does-macroexpand-with-epilogue (fun env)
;;                           (cons dec new-claims)
;;                           (nconc epilogue (list '(values))))))))


(define-macroexpand-handlers (form)
    (defvar defparameter)
  (destructuring-bind (def name &rest rest) form
    (hashset-add name *suspected-variables*)
    (signal-provider name 'defvar)
;;     (let ((new-name (special-substitute-name name)))
;; ;;           (new-name (gentemp (format nil "ASDF-DEPENDENCY-GROVEL-DEFVAR--~A"
;; ;;                                      name))))
;;       (does-macroexpand-with-prologue (fun env)
;;         ;; Do a little dance to prevent ADG from thinking that this form is
;;         ;; providing a symbol macro.  Probably a better solution would be to
;;         ;; change does-macroexpand-with-prologue (and
;;         ;; does-macroexpand-with-epilogue, while we're at it) to not apply
;;         ;; handlers to the prologue/epilogue.
;;         (let ((*macroexpand-hook* *old-macroexpand-hook*))
;;           (list (macroexpand-1
;;                  `(define-symbol-macro ,name
;;                       (signal-variable-use ',name 'defvar ',new-name)))))
;;         ;; Do the original defvar/defparameter, but with the new name.
;;         `(,def ,new-name ,@rest)))))
    (does-not-macroexpand)))

(define-macroexpand-handlers (form :environment env)
    (defconstant)
  (let ((symbol (second form)))
    (signal-provider (second form) 'defconstant)
    (hashset-add (second form) *suspected-constants*)
;;         (does-macroexpand-with-epilogue
;;             ((macro-function 'symbol-macroify) env)
;;           `(symbol-macroify ,@form)
;;           `((eval-when (:compile-toplevel :load-toplevel :execute)
;;               (setf (symbol-value ',symbol) ,symbol))
;;             ',symbol))
    (does-not-macroexpand)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Macro-Related Handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macroexpand-handlers (form)
    (defmacro define-method-combination)
  (signal-provider (second form) (first form))
  (does-not-macroexpand))


(define-macroexpand-handlers (form :function fun :environment env)
    (define-symbol-macro)
  (destructuring-bind (def name expansion) form
    (signal-provider name 'define-symbol-macro)
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
      (signal-provider name 'defstruct)
      (signal-provider name 'deftype)
      ;; Check for an :include struct option.
      (let ((include-option (assoc :include struct-options)))
        (when include-option
          (signal-user (second include-option) 'defstruct)))
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
        (loop :for (slot-name slot-init-value . slot-options)
              :in (loop :for desc :in slot-descriptions :collect
                     (if (listp desc) desc (list desc)))
              ;; Get the symbol for this slot's accessor.
              :as accessor-name :=
                      (let* ((slot-name-str (symbol-name slot-name))
                             (accessor-name-str
                              (concatenate 'string prefix slot-name-str)))
                        (intern accessor-name-str))
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
              ;; Check for a type option on the slot.
              :do (let ((typespec (getf slot-options :type)))
                    (when typespec
                      (signal-typespec typespec)))
              ;; Finally, put all the instrumentation into place.
              :finally (return (does-macroexpand-with-epilogue (fun env)
                                 form
                                 (append redefinitions (list `',name)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Type-Related Handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macroexpand-handlers (form)
    (handler-bind handler-case)
  (loop :for (condition . nil) :in (if (eql 'handler-bind (first form))
                                       (second form)
                                       (cddr form))
     :unless (eql condition :no-error)
     :do (signal-typespec condition))
  (does-not-macroexpand))


(define-macroexpand-handlers (form)
    (deftype)
  (signal-provider (second form) 'deftype)
  (signal-typespec (fourth form))
  (does-not-macroexpand))


(define-macroexpand-handlers (form)
    (typecase etypecase)
  (loop :for (typespec . nil) :in (cddr form)
     :do (signal-typespec typespec))
  (does-not-macroexpand))


(define-macroexpand-handlers (form)
    (check-type)
  (signal-typespec (third form))
  (does-not-macroexpand))


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
    ;; Signal imports of symbols (they need to exist before they can
    ;; be imported).
    (when *check-internal-symbols-p*
      (loop :for (nil pkg . symbols)
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
    (loop)
  ;; Note that at the moment, this handler doesn't actually parse the loop
  ;; macro, and could break if someone, say, used of-type as a name of a loop
  ;; variable.  Hopefully, that won't happen?
  (loop :for (car . cdr) :on form
     :when (and (symbolp car) (equal (string car) "OF-TYPE"))
       :do (signal-typespec (car cdr)))
  (does-not-macroexpand))


(define-macroexpand-handlers (form)
    (defsetf define-setf-expander)
  (signal-provider (second form) 'setf)
  (does-not-macroexpand))


(define-macroexpand-handlers (form)
    (setf)
  (loop :for (setee nil) :on (cdr form) :by #'cddr
        :do (cond
              ((consp setee)
               (signal-user (first setee) 'setf))
              #|((and (symbolp setee) (boundp setee))
               (signal-user setee 'defvar)
               (signal-provider setee 'defvar))|#)
            (when (hashset-contains-p setee *suspected-variables*)
              (push (list (first form)
                          setee
                          (constituent-summary *current-constituent*))
                    *global-mutations*)))
  (does-not-macroexpand))


(define-macroexpand-handlers (form :function fun :environment env)
    (pushnew push)
  (let ((setee (third form)))
    (when (hashset-contains-p setee *suspected-variables*)
      (push (list (first form)
                  setee
                  (constituent-summary *current-constituent*))
            *global-mutations*))
    (does-not-macroexpand)))


;; (define-macroexpand-handlers (form :function fun :environment env)
;;     (pushnew push)
;;   (let ((setee (third form)))
;;     (if (hashset-contains-p setee *suspected-variables*)
;;         (with-gensyms (old-version new-version)
;;           (assert (symbolp setee))
;;           (does-macroexpand-with-prologue (fun env)
;;             `((let* ((,old-version (gethash ,setee *variable-versions*))
;;                      (,new-version (1+ ,old-version)))
;;                 (signal-user (cons ,setee ,old-version) 'var-version)
;;                 (signal-provider (cons ,setee ,new-version) 'var-version)
;;                 (setf (gethash ,setee *variable-versions*) ,new-version)))
;;             form))
;;         (let* ((old-version (gethash setee *variable-versions*))
;;                (new-version (1+ old-version)))
;;           (signal-user (cons setee old-version) 'var-version)
;;           (signal-provider (cons setee new-version) 'var-version)
;;           (setf (gethash setee *variable-versions*) new-version)
;;           )
;;         (does-not-macroexpand))))

;; (define-macroexpand-handlers (form)
;;     (pushnew push)
;;   (let ((setee (third form)))
;;     (when (and (symbolp setee) (boundp setee))
;;       (signal-user setee 'defvar)
;;       (signal-provider setee 'defvar))))


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
