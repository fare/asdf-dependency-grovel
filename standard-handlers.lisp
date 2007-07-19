;;; Handlers for functions defined in the CLHS.
;;;
;;; TODO:
;; * handle more def*:
;;  * defvar & defparameter - works for symbols, but think any use
;;    /somewhere/ in a macro is a dependency.
;;  * define-compiler-macro - argh. seriously, no idea. if you use
;;    this at compile/load time, sorry.
;;  * deftype - we can signal that it was provided, but have to walk
;;    declarations in def* and generally almost /everwhere/. not fun.

;; * more package games: :import-from, :shadow

(in-package :asdf-dependency-grovel)

(defmacro define-macroexpand-handlers ((form &key
                                             (environment (gensym) environmentp)
                                             (function (gensym) functionp))
                                       (&rest form-names) &body body)
  `(progn
     ,@(loop for form-name in form-names
             for fun-name = (intern (format nil "HANDLE-MACROEXPANSION-~A"
                                            form-name))
             collect `(defun ,fun-name
                          (,form &key
                           ,@(and functionp `(((:function ,function))))
                           ,@(and environmentp `(((:environment ,environment))))
                           &allow-other-keys)
                        ,@body)
             collect `(setf (gethash ',form-name *macroexpansion-handlers*)
                            ',fun-name))))

(defmacro define-simple-macroexpand-handlers (form-var identifier-form
                                              signal-type signal-form-type
                                              (&rest form-names))
  
  (let ((identifier (gensym)))
    `(define-macroexpand-handlers (,form-var) (,@form-names)
       (let ((,identifier ,identifier-form))
         (,(ecase signal-type
             (:user 'signal-user)
             (:provider 'signal-provider))
           ,identifier ,signal-form-type)
         (does-not-macroexpand)))))

(define-macroexpand-handlers (form) (defmacro define-method-combination)
  (signal-provider (second form) (first form))
  (does-not-macroexpand))

(define-simple-macroexpand-handlers form
    (second form) :provider 'setf
    (defsetf define-setf-expander))

(define-macroexpand-handlers (form) (setf)
  (when (consp (second form))
    (signal-user (first (second form)) 'setf))
  (does-not-macroexpand))

(define-macroexpand-handlers (form) (defgeneric)
  (signal-provider (second form) 'defgeneric)
  (let ((method-combination (second (assoc :method-combination
                                           (nthcdr 3 form)))))
    (when method-combination
      (signal-user method-combination 'define-method-combination)))
  (does-not-macroexpand))

(define-macroexpand-handlers (form) (defvar defparameter)
  (signal-provider (second form) 'defvar)
  (setf (gethash (second form) *suspected-variables*) t)
  (does-not-macroexpand))

(define-macroexpand-handlers (form :function fun :environment env)
    (defmethod)
  (signal-user  (second form) 'defgeneric)
  ;; walk arg list and signal use of specialized-on
  ;; classes, and instrument function body.
  (let* ((name (second form))
         (new-expansion
          `(defmethod ,name
               ,@(loop for (elt . body) on (nthcdr 2 form)
                       if (not (listp elt))
                         collect elt into modifiers
                       else
                         do (signal-provider  `(,name ,@modifiers ,elt) 'defmethod)
                         and do
                           (loop for arg in elt
                                 when (and (listp arg)
                                           (symbolp (second arg)))
                                   do (signal-user (second arg) 'defclass))
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
    (does-macroexpand (fun env :macroexpand-hook *macroexpand-hook*)
      new-expansion)))

(define-macroexpand-handlers (form) (defclass define-condition)
  (signal-provider (second form) (first form))
  ;; signal use of direct
  ;; superclasses/superconditions. Note that we
  ;; declare a dependency only if the direct
  ;; superclass is already defined through the
  ;; current system definition.
  (loop for superclass in (third form)
        do (signal-user superclass (first form)))
  (does-not-macroexpand))

(define-macroexpand-handlers (form) (defstruct)
  (let ((name (etypecase (second form)
                (symbol (second form))
                (cons (first (second form))))))
    (signal-provider name 'defclass))  
  (does-not-macroexpand))

(define-macroexpand-handlers (form) (defpackage)
  ;; signal a use for the package first, to do the package
  ;; redefinition dance right.
  (signal-user
   (canonical-package-name (second form))
   'defpackage)
  (signal-provider
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
          do (signal-provider nickname 'defpackage))
    ;; signal :uses of packages
    (loop for use in (append (clause-contents :use)
                             (clause-contents :import-from
                                    #'clause-second-element)
                             (clause-contents :shadowing-import-from
                                    #'clause-second-element))
          do (signal-user
              (canonical-package-name use)
                    'defpackage)))
  (does-not-macroexpand))

(define-macroexpand-handlers (form) (in-package)
  (signal-user (canonical-package-name (second form)) 'defpackage)
  (does-not-macroexpand))

(define-macroexpand-handlers (form :environment env)
    (defconstant)
  (signal-provider (second form) (first form))
  (does-macroexpand ((macro-function 'symbol-macroify) env)
    `(symbol-macroify ,@form)))

(define-macroexpand-handlers (form :function fun :environment env)
    (define-symbol-macro)
  (destructuring-bind (def name expansion) form
    (signal-provider name def)
    (does-macroexpand (fun env)
      `(,def ,name (signal-symbol-macroexpansion ',name ,expansion)))))

(define-macroexpand-handlers (form :function fun :environment env) (defun)
  (destructuring-bind (defun name arg-list &rest maybe-body) form
    (signal-provider name (first form))
    (does-macroexpand (fun env)
      `(,defun ,name ,arg-list
         ,@(instrument-defun-body maybe-body
                                  `(signal-user ',name 'defun))))))

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