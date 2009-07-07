;; Classes used by asdf-dependency-grovel.

(cl:in-package #:asdf-dependency-grovel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass constituent ()
  ((parent
    :initarg :parent
    :initform (error "must supply a parent")
    :reader constituent-parent
    :documentation "The parent constituent directly containing this one,
    or nil if the constituent has no parent.")
   (children
    :initform nil
    :reader constituent-children
    :documentation "A list of direct children of this constituent.")
   (index
    :initform 0
    :reader constituent-index
    :documentation "The index of this constituent within its parent (if any).")
   (uses
    :initform nil
    :accessor constituent-uses
    :documentation "A list of things used by this constituent.")
   (provisions
    :initform nil
    :accessor constituent-provisions
    :documentation "A list of things provided by this constituent.")))

;; The top-level constituent.  This is instantiated by
;; with-constituent-groveling, and will have a nil parent.  The only reason to
;; have this as a separate class is so that we can overload
;; constituent-designator to return the empty list for top-level constituents.
(defclass top-constituent (constituent)
  ())

;; A constituent representing a file.
(defclass file-constituent (constituent)
  ((path
    :initarg :path
    :initform (error "must supply a path")
    :reader file-constituent-path
    :documentation "The path of the file.")))

;; A constituent representing a Lisp form.  The constituent for a top-level
;; form would generally have a file-constituent as a parent.
(defclass form-constituent (constituent)
  ((summary
    :initarg :summary
    :initform nil
    :reader form-constituent-summary
    :documentation "A human-readable sexp summarizing the form.")))

(defmethod initialize-instance :after ((con constituent) &key)
  (let ((parent (slot-value con 'parent)))
    (when parent
      (setf (slot-value con 'index)
            (length (slot-value parent 'children)))
      (push con (slot-value parent 'children)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric constituent-designator (con)
  (:documentation "Return the unique designator of the constituent."))

(defmethod constituent-designator ((con constituent))
  (cons (constituent-index con)
        (constituent-designator (constituent-parent con))))

(defmethod constituent-designator ((con top-constituent))
  nil)

(defmethod constituent-designator ((con file-constituent))
  (cons (file-constituent-path con)
        (constituent-designator (constituent-parent con))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric constituent-summary (con)
  (:documentation "Return a summary of the identity constituent."))

(defmethod constituent-summary ((con constituent))
  (constituent-designator con))

(defmethod constituent-summary ((con form-constituent))
  (list (constituent-designator con)
        (form-constituent-summary con)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun constituent-descendant-p (con1 con2)
  "Return t if con1 is a descendant of con2, nil otherwise."
  (and con1 (or (eql con1 con2)
                (constituent-descendant-p (constituent-parent con1) con2))))

(defun constituent-add-use (use con)
  (pushnew use
           (constituent-uses con)
           :test #'equal))

(defun constituent-add-provision (provision con)
  (pushnew provision
           (constituent-provisions con)
           :test #'equal))

(defun propagate-constituent (con)
  (dolist (child (constituent-children con))
    (propagate-constituent child)
    (dolist (provision (constituent-provisions child))
      (constituent-add-provision provision con))
    (dolist (use (constituent-uses child))
      (constituent-add-use use con))))

(defun constituent-provision-table (constituent)
  (let ((table (make-hash-table :test #'equal)))
    (labels ((populate (con)
               (dolist (provision (constituent-provisions con))
                 (push con (gethash provision table)))
               (dolist (child (constituent-children con))
                 (populate child))))
      (populate constituent))
    table))

(defun constituent-dependency-table (constituent)
  (let ((provisions (constituent-provision-table constituent))
        (table (make-hash-table :test #'eql)))
    (labels ((populate (con)
               (let ((subtable (make-hash-table :test #'eql)))
                 (setf (gethash con table) subtable)
                 (dolist (use (constituent-uses con))
                   (dolist (dep (gethash use provisions))
                     (unless (or (constituent-descendant-p con dep)
                                 (constituent-descendant-p dep con))
                       (push use (gethash dep subtable)))))
                 (dolist (child (constituent-children con))
                   (populate child)))))
      (populate constituent))
    table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
