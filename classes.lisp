;; Classes used by asdf-dependency-grovel.

#+xcvb (module (:depends-on ("variables")))

(cl:in-package #:asdf-dependency-grovel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Used in a few macros; not exported.  Why isn't this a standard CL macro?
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(mapcar #'(lambda (name) `(,name (gensym))) names) ,@body))

;; Another function that I don't know why CL doesn't seem to have.
(defun pophash (hash-table)
  (loop :for k :being :the :hash-key :of hash-table :using (:hash-value v)
     :do (remhash k hash-table) (return (values k v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defgeneric constituent-summary (con)
  (:documentation "Return a summary of the identity of the constituent."))

(defmethod constituent-summary ((con constituent))
  (constituent-designator con))

(defmethod constituent-summary ((con form-constituent))
  (list (constituent-designator con)
        (form-constituent-summary con)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro walk-constituents-preorder ((constituent top) &body body)
  (with-gensyms (visit walk con child)
    `(labels ((,visit (,constituent) ,@body)
              (,walk (,con)
                (,visit ,con)
                (dolist (,child (constituent-children ,con))
                  (,walk ,child))))
       (,walk ,top))))

(defmacro walk-constituents-postorder ((constituent top) &body body)
  (with-gensyms (visit walk con child)
    `(labels ((,visit (,constituent) ,@body)
              (,walk (,con)
                (dolist (,child (constituent-children ,con))
                  (,walk ,child))
                (,visit ,con)))
       (,walk ,top))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun constituent-descendant-p (con1 con2)
  "Return t if con1 is a descendant of con2, nil otherwise."
  (and con1 (or (eql con1 con2)
                (constituent-descendant-p (constituent-parent con1) con2))))

(defun constituent-add-use (use con)
  (pushnew use
           (constituent-uses con)
           :test 'equal))

(defun constituent-add-provision (provision con)
  (pushnew provision
           (constituent-provisions con)
           :test 'equal))

(defun propagate-constituent (con)
  (dolist (child (constituent-children con))
    (propagate-constituent child)
    (dolist (provision (constituent-provisions child))
      (constituent-add-provision provision con))
    (dolist (use (constituent-uses child))
      (constituent-add-use use con))))

(defun constituent-provision-table (top)
  (let ((table (make-hash-table :test 'equal)))
    (walk-constituents-preorder (con top)
      (dolist (provision (constituent-provisions con))
        (push con (gethash provision table))))
    table))

(defun constituent-dependency-table (top)
  (let ((provisions (constituent-provision-table top))
        (table (make-hash-table :test 'eql)))
    (walk-constituents-preorder (con top)
      (let ((subtable (make-hash-table :test 'eql)))
        (setf (gethash con table) subtable)
        (dolist (use (constituent-uses con))
          (dolist (dep (gethash use provisions))
            (unless (or (constituent-descendant-p con dep)
                        (constituent-descendant-p dep con))
              (push use (gethash dep subtable)))))))
    table))

(defun get-file-constituents (top)
  (let ((file-constituents nil))
    (walk-constituents-preorder (con top)
      (typecase con (file-constituent (push con file-constituents))))
    (nreverse file-constituents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Graph ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass dnode ()
  ((parent
    :initarg :parent
    :initform (error "must supply a parent")
    :reader dnode-parent
    :documentation "The parent of all constituents in this node.")
   (constituents
    :initform (make-hash-table :test 'eql)
    :reader dnode-constituents
    :documentation "The set of constituents in this node.")
   (that-depend-on
    :initform (make-hash-table :test 'eql)
    :reader dnodes-that-depend-on
    :documentation "The set of nodes that depend on this node.")
   (needs
    :initform (make-hash-table :test 'eql)
    :reader dnodes-needed-by
    :documentation "The set of nodes that this node depends on.")))

(defun make-dnode (constituent)
  (let ((dnode (make-instance 'dnode
                              :parent (constituent-parent constituent))))
    (setf (gethash constituent (dnode-constituents dnode)) t)
    dnode))

(defun cyclic-reachable-p (start-node end-node)
  (let* ((parent (dnode-parent start-node))
         (start-state (list start-node nil))
         (stack (list start-state))
         (visited (make-hash-table :test 'equal)))
    (assert (eql parent (dnode-parent end-node)))
    (setf (gethash start-state visited) t)
    (do () ((null stack) nil) ;; If the stack is emptied, the answer is "no".
      (destructuring-bind (dnode tainted) (pop stack)
        (cond (tainted
               (when (eql dnode end-node)
                 ;; If this assertion fails, then the graph must be cyclic.
                 (assert (not (eql start-node end-node)))
                 (return t))) ;; The answer is "yes".
              ((not (eql (dnode-parent dnode) parent))
               (setf tainted t)))
        (loop :for child :being :the :hash-keys :in (dnodes-needed-by dnode)
           :with new-state = (list child tainted)
           :unless (gethash new-state visited)
             :do (setf (gethash new-state visited) t)
                 (push new-state stack))))))

(defun try-to-merge-dnodes (graph dnode1 dnode2)
  (assert (eql (dnode-parent dnode1) (dnode-parent dnode2)))
  (assert (gethash dnode1 graph))
  (assert (gethash dnode2 graph))
  (when (or (cyclic-reachable-p dnode1 dnode2)
            (cyclic-reachable-p dnode2 dnode1))
    (return-from try-to-merge-dnodes nil))
  (loop :for con :being :the :hash-keys :in (dnode-constituents dnode2)
     :do (setf (gethash con (dnode-constituents dnode1)) t))
  (loop :for dnode3 :being :the :hash-keys :in (dnodes-needed-by dnode2)
     :do (remhash dnode2 (dnodes-that-depend-on dnode3))
         (setf (gethash dnode1 (dnodes-that-depend-on dnode3)) t)
         (setf (gethash dnode3 (dnodes-needed-by dnode1)) t))
  (loop :for dnode3 :being :the :hash-keys :in (dnodes-that-depend-on dnode2)
     :do (remhash dnode2 (dnodes-needed-by dnode3))
         (setf (gethash dnode1 (dnodes-needed-by dnode3)) t)
         (setf (gethash dnode3 (dnodes-that-depend-on dnode1)) t))
  (remhash dnode2 graph)
  (remhash dnode1 (dnodes-needed-by dnode1))
  (remhash dnode1 (dnodes-that-depend-on dnode1))
  t)

(defun build-merged-graph (top-constituent)
  (let ((graph (make-hash-table :test 'eql))
        (parent-tables nil))
    ;; Populate the graph.
    (dolist (file-con (get-file-constituents top-constituent))
      (let ((parent-table (make-hash-table :test 'eql)))
        (dolist (child (constituent-children file-con))
          (let ((dnode (make-dnode child)))
            (setf (gethash dnode parent-table) t)
            (setf (gethash dnode graph) t)))
        (push parent-table parent-tables)))
    ;; Try to merge nodes from the same parent.
    (dolist (parent-table (nreverse parent-tables))
      (do () ((= 0 (hash-table-count parent-table)))
        (let* ((dnode (pophash parent-table)))
          (loop :for other :being :each :hash-key :of parent-table
             :if (try-to-merge-dnodes graph dnode other)
               :do (remhash other parent-table)))))
    graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
