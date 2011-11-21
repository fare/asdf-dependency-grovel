;; Classes used by asdf-dependency-grovel.

;; Originally, this file just held some classes.  Now it holds a bunch of
;; stuff.  It should probably be renamed, or reorganized.

#+xcvb (module (:depends-on ("variables")))

(cl:in-package #:asdf-dependency-grovel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Used in a few macros; not exported.  Why isn't this a standard CL macro?
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(mapcar #'(lambda (name) `(,name (gensym))) names) ,@body))

(defmacro do-until (condition &body body)
  `(do () (,condition) ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Heaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A binary minheap, which can be used as a priority queue.
(flet ((heap-key-function (heap) (car heap))
       (heap-vector (heap) (cdr heap))
       (parent-index (index) (values (floor (1- index) 2)))
       (child-indices (index)
         (let ((left (1+ (* 2 index))))
           (values left (1+ left))))
       (item-key (item) (car item))
       (item-obj (item) (cdr item)))
  (defun make-heap (&key key)
    "Make an empty min-heap.  The :key argument should be a function mapping
     elements of the heap to a priority value (typically a number) by which
     that item will be sorted.  If no :key function is given, the element
     itself is used as the priority value."
    (unless key
      (setf key (lambda (x) x)))
    (cons key (make-array 0 :adjustable t :fill-pointer t)))
  (defun heap-insert (obj heap)
    "Add an element to the heap."
    (let* ((key (funcall (heap-key-function heap) obj))
           (item (cons key obj))
           (vec (heap-vector heap))
           (index (vector-push-extend item vec)))
      (do-until (= index 0)
        (let ((pindex (parent-index index)))
          (if (>= key (item-key (elt vec pindex)))
              (return)
              (psetf (elt vec index) (elt vec pindex)
                     (elt vec pindex) (elt vec index)
                     index pindex)))))
    (values))
  (defun heap-pop (heap)
    "Remove the element with the smallest priority-value from the heap and
     return two values: the element removed (or nil if the heap was already
     empty), and a boolean that is nil if the heap was indeed already empty or
     t otherwise."
    (when (heap-empty-p heap)
      (return-from heap-pop (values nil nil)))
    (let* ((vec (heap-vector heap))
           (min-obj (item-obj (elt vec 0)))
           (item (vector-pop vec))
           (length (fill-pointer vec)))
      (when (> length 0)
        (let ((key (item-key item))
              (index 0))
          (loop
             (multiple-value-bind (lindex rindex) (child-indices index)
               (cond ((>= lindex length)
                      (return))
                     ((>= rindex length)
                      (if (<= key (item-key (elt vec lindex)))
                          (return)
                          (setf (elt vec index) (elt vec lindex)
                                index lindex)))
                     (t (let ((lkey (item-key (elt vec lindex)))
                              (rkey (item-key (elt vec rindex))))
                          (cond ((and (<= key lkey)
                                      (<= key rkey))
                                 (return))
                                ((<= lkey rkey)
                                 (setf (elt vec index) (elt vec lindex)
                                       index lindex))
                                (t
                                 (setf (elt vec index) (elt vec rindex)
                                       index rindex))))))))
          (setf (elt vec index) item)))
      (values min-obj t)))
  (defun heap-count (heap)
    "Return the number of elements in the heap."
    (fill-pointer (heap-vector heap)))
  (defun heap-empty-p (heap)
    "Return t if the heap is empty, nil otherwise."
    (= 0 (heap-count heap))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hashsets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(defmacro loop-hashset ((item hashset) &rest rest)
  "Loop macro for hashsets."
  `(loop :for ,item :being :the :hash-keys :in ,hashset
      ,@rest))
(defmacro do-hashset ((item hashset) &body body)
  "Like dolist, but for hashsets."
  `(loop-hashset (,item ,hashset)
      :do (progn ,@body)))
(defun hashset-pop (hashset)
  "Remove and return an arbitrary item from the hashset."
  (do-hashset (k hashset)
    (hashset-remove k hashset)
    (return-from hashset-pop k)))
(defun hashset-subset-p (set1 set2)
  "Return t if `set1' is a subset of `set2', nil otherwise."
  (do-hashset (item set1)
    (unless (hashset-contains-p item set2)
      (return-from hashset-subset-p nil)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass simple-print-object-mixin ()
  ())

(defun collect-slots (object slots)
  (loop :for slot-spec :in slots :nconc
    (if (functionp slot-spec)
      (funcall slot-spec object)
      (destructuring-bind (slot &optional
                                (fun #'identity)
                                (keyword (intern (symbol-name slot) :keyword)))
          (if (consp slot-spec) slot-spec (list slot-spec))
        (when (slot-boundp object slot)
          (list keyword (funcall fun (slot-value object slot))))))))

(defun simple-print-object (object stream &key identity (slots (slots-to-print object)))
  (print-unreadable-object (object stream :type t :identity identity)
    (write (collect-slots object slots) :stream stream)))

(defgeneric slots-to-print (object)
  (:method-combination append))

(defmethod print-object ((object simple-print-object-mixin) stream)
  (simple-print-object object stream :slots (slots-to-print object)))

;; Base class for constituents.
(defclass constituent (simple-print-object-mixin)
  ((parent
    :initarg :parent
    :initform nil
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
    :initform (make-hashset :test 'equal)
    :accessor constituent-uses
    :documentation "A hashset of things used by this constituent.")
   (provisions
    :initform (make-hashset :test 'equal)
    :accessor constituent-provisions
    :documentation "A hashset of things provided by this constituent.")))

(defmethod slots-to-print append ((con constituent))
  `(,#'(lambda (x) (list :designator (constituent-designator x)))
    (children ,#'length)
    index
    (uses ,#'hash-table-count)
    (provisions ,#'hash-table-count)))

(defmethod initialize-instance :after ((con constituent) &key)
  (let ((parent (slot-value con 'parent)))
    (when parent
      (setf (slot-value con 'index)
            (length (slot-value parent 'children)))
      (push con (slot-value parent 'children)))))

;; The top-level constituent.  This is instantiated by
;; with-constituent-groveling, and will have a nil parent.  The only reason to
;; have this as a separate class is so that we can overload
;; constituent-designator to return the empty list for top-level constituents.
(defclass top-constituent (constituent)
  ())

;; A constituent representing an ASDF component.
(defclass asdf-component-constituent (constituent)
  ((component
    :initarg :component
    :initform (error "must supply a component")
    :reader asdf-component-constituent-component
    :documentation "The ASDF component.")))

(defmethod slots-to-print append ((con asdf-component-constituent))
  '(component))

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
  ((position
    :initarg :position
    :initform nil
    :reader form-constituent-position
    :documentation "The position in the file at which the form starts.")
   (summary
    :initarg :summary
    :initform nil
    :reader form-constituent-summary
    :documentation "A human-readable string summarizing the form.")))

;; A temporary constituent, typically made with a nil parent.  This is used to
;; collect uses and provisions into one place (e.g. for sharpdot
;; instrumentation) before adding them to another constituent using the
;; transfer-constituent function.
(defclass temp-constituent (constituent)
  ((label
    :initarg :label
    :initform (error "must supply a label")
    :reader temp-constituent-label
    :documentation "The label to use as the summary of this constituent.")))

(defmethod slots-to-print append ((con temp-constituent))
  '(label))

(defun new-temp-constituent ()
  "Create a new temp-constituent with an automatically generated label."
  (make-instance 'temp-constituent
                 :label (cons (gensym) (and *current-constituent*
                                            (constituent-designator
                                             *current-constituent*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric constituent-designator (con)
  (:documentation "Return the unique designator of the constituent."))

(defmethod constituent-designator ((con null))
  nil)

(defmethod constituent-designator ((con top-constituent))
  t)

(defmethod constituent-designator ((con constituent))
  (list* (class-of con) (constituent-index con)
         (constituent-designator (constituent-parent con))))

(defmethod constituent-designator ((con form-constituent))
  (list* :form (constituent-index con)
         (constituent-designator (constituent-parent con))))

(defmethod constituent-designator ((con asdf-component-constituent))
  (list* :asdf
         (asdf::component-find-path (asdf-component-constituent-component con))
         (constituent-designator (constituent-parent con))))

(defmethod constituent-designator ((con file-constituent))
  (list* :file
         (file-constituent-path con)
         (constituent-designator (constituent-parent con))))


(defgeneric constituent-summary (con)
  (:documentation "Return a summary of the identity of the constituent."))

(defmethod constituent-summary ((con constituent))
  (constituent-designator con))

(defmethod constituent-summary ((con form-constituent))
  (list (constituent-designator con)
        (form-constituent-position con)
        (form-constituent-summary con)))

(defmethod constituent-summary ((con temp-constituent))
  (temp-constituent-label con))


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
  "Add a use to the constituent.  The use should be a list of two elements: the
   name of the thing used, and a symbol indicating the kind of thing used,
   e.g. '(*foo* defvar)."
  (hashset-add use (constituent-uses con)))

(defun constituent-add-provision (provision con)
  "Add a provision to the constituent.  The provision should be a list of two
   elements: the name of the thing provided, and a symbol indicating the kind
   of thing provided, e.g. '(*foo* defvar)."
  (hashset-add provision (constituent-provisions con)))

(defun propagate-constituent-downward (con)
  "Copy all uses and provisions from the constituent to all its descendants,
   and do the same for each descendant."
  (dolist (child (constituent-children con))
    (do-hashset (provision (constituent-provisions con))
      (constituent-add-provision provision child))
    (do-hashset (use (constituent-uses con))
      (constituent-add-use use child))
    (propagate-constituent-downward child)))

(defun propagate-constituent-upward (con)
  "Copy all uses and provisions from the constituent's descendants to it,
   and do the same for each descendant."
  (dolist (child (constituent-children con))
    (propagate-constituent-upward child)
    (do-hashset (provision (constituent-provisions child))
      (constituent-add-provision provision con))
    (do-hashset (use (constituent-uses child))
      (constituent-add-use use con))))

(defun constituent-provision-table (top)
  "Create a hash table mapping provisions to constituents that provide them."
  (let ((table (make-hash-table :test 'equal)))
    (walk-constituents-preorder (con top)
      (do-hashset (provision (constituent-provisions con))
        (push con (gethash provision table))))
    table))

(defun constituent-dependency-table (top)
  "Create a table mapping constituents to constituents to lists of reasons."
  (let ((provisions (constituent-provision-table top))
        (table (make-hash-table :test 'eql)))
    (walk-constituents-preorder (con top)
      (let ((subtable (make-hash-table :test 'eql)))
        (setf (gethash con table) subtable)
        (do-hashset (use (constituent-uses con))
          ;; You don't depend on something you yourself provide (this is
          ;; important for similar declarations that sometimes appear in
          ;; multiple files, e.g. defvars).
          (unless (hashset-contains-p use (constituent-provisions con))
            (dolist (dep (gethash use provisions))
              ;; You don't depend on something provided by one of your
              ;; descendants or ancestors.
              (unless (or (constituent-descendant-p con dep)
                          (constituent-descendant-p dep con))
                (push use (gethash dep subtable))))))))
    table))

(defun get-file-constituents (top)
  "Return a list of file-constituents that are descendants of the given
   constituent."
  (let ((file-constituents nil))
    (walk-constituents-postorder (con top)
      (typecase con (file-constituent (push con file-constituents))))
    file-constituents))

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

(defun cyclic-reachable-p (graph start-node end-node)
  "Return t if `end-node' is cyclic-reachable from `start-node', nil othewise.
   If nodes A and B have the same parent, then node B is said to be
   cyclic-reachable from a node A if there is a way to get from A to B that
   passes through a node with a different parent (this implies that merging
   nodes A and B would create a cycle between two nodes with different
   parents)."
  (assert (hashset-contains-p start-node graph))
  (assert (hashset-contains-p end-node graph))
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
          (assert (hashset-contains-p child graph))
          (let ((new-state (list child tainted)))
            (unless (hashset-contains-p new-state visited)
              (hashset-add new-state visited)
              (push new-state stack))))))))

(defun try-to-merge-dnodes (graph dnode1 dnode2)
  "Either merge the nodes and return t, or do nothing and return nil."
  (assert (not (eql dnode1 dnode2)))
  (assert (eql (dnode-parent dnode1) (dnode-parent dnode2)))
  (assert (hashset-contains-p dnode1 graph))
  (assert (hashset-contains-p dnode2 graph))
  ;; If dnode2 is cyclic-reachable from dnode1 (or vice-versa), we can't merge.
  (when (or (cyclic-reachable-p graph dnode1 dnode2)
            (cyclic-reachable-p graph dnode2 dnode1))
    (return-from try-to-merge-dnodes nil))
  ;; Merge constituents of dnode2 into dnode1.
  (do-hashset (con (dnode-constituents dnode2))
    (hashset-add con (dnode-constituents dnode1)))
  ;; Nodes that needed dnode2 now need dnode1 instead.
  (do-hashset (dnode3 (dnodes-that-depend-on dnode2))
    (hashset-remove dnode2 (dnodes-needed-by dnode3))
    (hashset-add dnode1 (dnodes-needed-by dnode3))
    (hashset-add dnode3 (dnodes-that-depend-on dnode1)))
  ;; Nodes that dnode2 needed are now needed by dnode1 instead.
  (do-hashset (dnode3 (dnodes-needed-by dnode2))
    (hashset-remove dnode2 (dnodes-that-depend-on dnode3))
    (hashset-add dnode1 (dnodes-that-depend-on dnode3))
    (hashset-add dnode3 (dnodes-needed-by dnode1)))
  ;; Remove any self edge on dnode1, in case the merging created one.
  (hashset-remove dnode1 (dnodes-needed-by dnode1))
  (hashset-remove dnode1 (dnodes-that-depend-on dnode1))
  ;; Remove dnode2 from the graph.
  (hashset-remove dnode2 graph)
  t)

(defun build-merged-graph (top-constituent)
  (let ((graph (make-hashset :test 'eql))
        (dnode-lookup (make-hash-table :test 'eql))
        (dnode-sets nil)
        (dependencies (constituent-dependency-table top-constituent)))
    ;; Populate the graph with nodes.
    (dolist (file-con (get-file-constituents top-constituent))
      (let ((dnode-set (make-hashset :test 'eql)))
        (dolist (child (constituent-children file-con))
          (let ((dnode (make-dnode child)))
            (setf (gethash child dnode-lookup) dnode)
            (hashset-add dnode dnode-set)
            (hashset-add dnode graph)))
        (push dnode-set dnode-sets)))
    ;; Populate the nodes with dependencies.
    (loop :for con1 :being :each :hash-key :in dnode-lookup
          :using (:hash-value dnode1) :do
       (loop :for con2 :being :each :hash-key :in (gethash con1 dependencies)
             :for dnode2 := (gethash con2 dnode-lookup)
             :when dnode2 :do
          (hashset-add dnode1 (dnodes-that-depend-on dnode2))
          (hashset-add dnode2 (dnodes-needed-by dnode1))))
    (fail-if-not-acyclic graph "during build-merged-graph")
    ;; Try to merge nodes from the same parent.
    (dolist (dnode-set (nreverse dnode-sets))
      (do-until (hashset-empty-p dnode-set)
        (let* ((dnode1 (hashset-pop dnode-set)))
          (do-hashset (dnode2 dnode-set)
            (when (try-to-merge-dnodes graph dnode1 dnode2)
              (hashset-remove dnode2 dnode-set))))))
    (fail-if-not-acyclic graph "after build-merged-graph")
    graph))

(defun find-a-cycle-if-any (graph)
  "If the graph contains a cycle, return a list of the dnodes involved; if
   there is no cycle, return nil."
  (let ((expanded (make-hashset :test 'eql))
        (stack nil))
    (do-hashset (dnode graph)
      (push (list dnode) stack))
    (do-until (null stack)
      (let* ((chain (pop stack))
             (head (car chain)))
        (hashset-add head expanded)
        (do-hashset (next (dnodes-needed-by head))
          (when (member next chain :test 'eql)
            (return-from find-a-cycle-if-any chain))
          (unless (hashset-contains-p next expanded)
            (push (cons next chain) stack)))))
    nil))

(defun fail-if-not-acyclic (graph message)
  (let ((cycle (find-a-cycle-if-any graph)))
    (when cycle
      (error (format nil "There was a cycle (~A):~%~{  dnode:~%~{    ~S~%~}~}"
                     message
                     (loop :for dnode :in cycle :collect
                        (loop-hashset (con (dnode-constituents dnode))
                           :collect (constituent-summary con))))))))

(defun topologically-stable-sort-graph (graph parents)
  (fail-if-not-acyclic graph "before topologically-stable-sort-graph")
  (let* ((sorted-list nil)
         (finished (make-hashset :test 'eql)) ;; nodes in sorted-list
         (enqueued (make-hashset :test 'eql)) ;; nodes ever to be in heap
         (key-table (make-hash-table :test 'eql)) ;; maps dnodes to keys
         (heap (make-heap :key (lambda (dnode) (gethash dnode key-table)))))
    ;; Populate the key-table.
    (let ((parents-table (make-hash-table :test 'eql)))
      (loop :for parent :in parents :for index :from 1
         :do (setf (gethash parent parents-table) index))
      (do-hashset (dnode graph)
        (let ((index (gethash (dnode-parent dnode) parents-table)))
          (assert index)
          (setf (gethash dnode key-table)
                (- (+ (* 10000 index) ;; this is a gross hack
                      (loop-hashset (con (dnode-constituents dnode))
                         :minimize (constituent-index con))))))))
    ;; Initialize heap to all nodes that no other node depends on -- such
    ;; nodes can safely come at the _end_ of the list, and thus can be _pushed_
    ;; onto sorted-list _first_.
    (do-hashset (dnode graph)
      (when (hashset-empty-p (dnodes-that-depend-on dnode))
        (hashset-add dnode enqueued)
        (heap-insert dnode heap)))
    ;; Work until the heap has been emptied.
    (do-until (heap-empty-p heap)
      ;; Pop the next item off the heap, and push it onto sorted-list.
      (let ((dnode (heap-pop heap)))
        (assert (hashset-contains-p dnode enqueued))
        (assert (not (hashset-contains-p dnode finished)))
        (hashset-add dnode finished)
        (push dnode sorted-list)
        ;; Now that this node is in sorted-list, examine nodes that depend on
        ;; this one.  Enqueue any such nodes that 1) have never been enqueued,
        ;; and 2) are not depended on by anything that isn't already in
        ;; sorted-list.  Each node in the graph will thus be enqueued just
        ;; after the last node that depends on it is enqueued.
        (do-hashset (other (dnodes-needed-by dnode))
          (when (and (not (hashset-contains-p other enqueued))
                     (hashset-subset-p (dnodes-that-depend-on other) finished))
            (hashset-add other enqueued)
            (heap-insert other heap)))))
    ;; When we're done, every node from the graph should now be in sorted-list,
    ;; in topological order.
    (assert (= (length sorted-list) (hashset-count graph)))
    (assert (or (null sorted-list)
                (hashset-empty-p (dnodes-needed-by (first sorted-list)))))
    sorted-list))

#|
(defun topologically-sort-graph (graph)
  "Given an acyclic graph of nodes, return a list of the nodes in topological
   order (that is, each node comes before all nodes that depend on it)."
  (fail-if-not-acyclic graph "before topologically-sort-graph")
  (let ((sorted-list nil)
        (finished (make-hashset :test 'eql)) ;; nodes in sorted-list
        (enqueued (make-hashset :test 'eql)) ;; nodes ever to be in stack
        (stack nil))
    ;; Initialize stack to all nodes that no other node depends on -- such
    ;; nodes can safely come at the _end_ of the list, and thus can be _pushed_
    ;; onto sorted-list _first_.
    (do-hashset (dnode graph)
      (when (hashset-empty-p (dnodes-that-depend-on dnode))
        (hashset-add dnode enqueued)
        (push dnode stack)))
    ;; Work until the stack has been emptied.
    (do-until (null stack)
      ;; Pop the next item off the stack, and push it onto sorted-list.
      (let ((dnode (pop stack)))
        (assert (hashset-contains-p dnode enqueued))
        (assert (not (hashset-contains-p dnode finished)))
        (hashset-add dnode finished)
        (push dnode sorted-list)
        ;; Now that this node is in sorted-list, examine nodes that depend on
        ;; this one.  Enqueue any such nodes that 1) have never been enqueued,
        ;; and 2) are not depended on by anything that isn't already in
        ;; sorted-list.  Each node in the graph will thus be enqueued just
        ;; after the last node that depends on it is enqueued.
        (do-hashset (other (dnodes-needed-by dnode))
          (when (and (not (hashset-contains-p other enqueued))
                     (hashset-subset-p (dnodes-that-depend-on other) finished))
            (hashset-add other enqueued)
            (push other stack)))))
    ;; When we're done, every node from the graph should now be in sorted-list,
    ;; in topological order.
    (assert (= (length sorted-list) (hashset-count graph)))
    (assert (or (null sorted-list)
                (hashset-empty-p (dnodes-needed-by (first sorted-list)))))
    sorted-list))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
