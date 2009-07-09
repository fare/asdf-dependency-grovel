;; Classes used by asdf-dependency-grovel.

(cl:in-package #:asdf-dependency-grovel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Used in a few macros; not exported.  Why isn't this a standard CL macro?
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(mapcar #'(lambda (name) `(,name (gensym))) names) ,@body))

;; Hash-set abstraction, since CL doesn't seem to have a set datatype.
(defun make-hashset (&key (test 'eql))
  "Create a new hashset with the specified test function."
  (make-hash-table :test test))
(defun hashset-count (hashset)
  "Return the number of items in the hashset."
  (hash-table-count hashset))
(defun hashset-empty-p (hashset)
  "Return t if the hashset is empty, nil otherwise."
  (= 0 (hashset-count hashset)))
(defun hashset-contains-p (item hashset)
  "Return t if the item is in the hashset, nil otherwise."
  (gethash item hashset))
(defun hashset-add (item hashset)
  "Add an item to the hashset."
  (setf (gethash item hashset) t))
(defun hashset-remove (item hashset)
  "Remove an item from the hashset."
  (remhash item hashset))
(defmacro do-hashset ((item hashset) &body body)
  "Like dolist, but for hashsets."
  `(loop :for ,item :being :the :hash-keys :in ,hashset
      :do (progn ,@body)))
(defun hashset-pop (hashset)
  "Remove and return an arbitrary item from the hashset."
  (do-hashset (k hashset)
    (hashset-remove k hashset)
    (return-from hashset-pop k)))

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
  "Like dolist, but visits constituents in a preorder traversal."
  (with-gensyms (visit walk con child)
    `(labels ((,visit (,constituent) ,@body)
              (,walk (,con)
                (,visit ,con)
                (dolist (,child (constituent-children ,con))
                  (,walk ,child))))
       (,walk ,top))))

(defmacro walk-constituents-postorder ((constituent top) &body body)
  "Like dolist, but visits constituents in a postorder traversal."
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
    :initform (make-hashset :test 'eql)
    :reader dnode-constituents
    :documentation "The set of constituents in this node.")
   (that-depend-on
    :initform (make-hashset :test 'eql)
    :reader dnodes-that-depend-on
    :documentation "The set of nodes that depend on this node.")
   (needs
    :initform (make-hashset :test 'eql)
    :reader dnodes-needed-by
    :documentation "The set of nodes that this node depends on.")))

(defun make-dnode (constituent)
  "Create a new dnode containing a single constituent."
  (let ((dnode (make-instance 'dnode
                              :parent (constituent-parent constituent))))
    (hashset-add constituent (dnode-constituents dnode))
    dnode))

(defun cyclic-reachable-p (start-node end-node)
  "Return t if `end-node' is cyclic-reachable from `start-node', nil othewise."
  (let* ((parent (dnode-parent start-node))
         (start-state (list start-node nil))
         (stack (list start-state))
         (visited (make-hashset :test 'equal)))
    (assert (eql parent (dnode-parent end-node)))
    (hashset-add start-state visited)
    (do () ((null stack) nil) ;; If the stack is emptied, the answer is "no".
      (destructuring-bind (dnode tainted) (pop stack)
        (cond (tainted
               (when (eql dnode end-node)
                 ;; If this assertion fails, then the graph must be cyclic.
                 (assert (not (eql start-node end-node)))
                 (return t))) ;; The answer is "yes".
              ((not (eql (dnode-parent dnode) parent))
               (setf tainted t)))
        (do-hashset (child (dnodes-needed-by dnode))
          (let ((new-state (list child tainted)))
            (unless (hashset-contains-p new-state visited)
              (hashset-add new-state visited)
              (push new-state stack))))))))

(defun try-to-merge-dnodes (graph dnode1 dnode2)
  "Either merge the nodes and return t, or do nothing and return nil."
  (assert (eql (dnode-parent dnode1) (dnode-parent dnode2)))
  (assert (hashset-contains-p dnode1 graph))
  (assert (hashset-contains-p dnode2 graph))
  (when (or (cyclic-reachable-p dnode1 dnode2)
            (cyclic-reachable-p dnode2 dnode1))
    (return-from try-to-merge-dnodes nil))
  (do-hashset (con (dnode-constituents dnode2))
    (hashset-add con (dnode-constituents dnode1)))
  (do-hashset (con (dnodes-needed-by dnode2))
    (hashset-remove dnode2 (dnodes-that-depend-on dnode3))
    (hashset-add dnode1 (dnodes-that-depend-on dnode3))
    (hashset-add dnode3 (dnodes-needed-by dnode1)))
  (do-hashset (con (dnodes-that-depend-on dnode2))
    (hashset-remove dnode2 (dnodes-needed-by dnode3))
    (hashset-add dnode1 (dnodes-needed-by dnode3))
    (hashset-add dnode3 (dnodes-that-depend-on dnode1)))
  (hashset-remove dnode2 graph)
  (hashset-remove dnode1 (dnodes-needed-by dnode1))
  (hashset-remove dnode1 (dnodes-that-depend-on dnode1))
  t)

(defun build-merged-graph (top-constituent)
  (let ((graph (make-hashset :test 'eql))
        (dnode-sets nil))
    ;; Populate the graph.
    (dolist (file-con (get-file-constituents top-constituent))
      (let ((dnode-set (make-hashset :test 'eql)))
        (dolist (child (constituent-children file-con))
          (let ((dnode (make-dnode child)))
            (hashset-add dnode dnode-set)
            (hashset-add dnode graph)))
        (push dnode-set dnode-sets)))
    ;; Try to merge nodes from the same parent.
    (dolist (dnode-set (nreverse dnode-sets))
      (do () ((hashset-empty-p dnode-set))
        (let* ((dnode1 (hashset-pop dnode-set)))
          (do-hashset (dnode2 dnode-set)
            (when (try-to-merge-dnodes graph dnode1 dnode2)
              (hashset-remove dnode2 dnode-set))))))
    graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
