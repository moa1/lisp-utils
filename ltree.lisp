;; labelled tree structure.
;; In this implementation, every node in the tree has a value.
;; The children are not ordered. 
;; If a circle is constructed, the behavior of the functions are undefined.

(defpackage :ltree
  (:use :common-lisp)
  (:export :ltree-label
	   :ltree-value
	   :ltree-parent ;; I really wouldn't want to export the SETF-function of ltree-parent, b/c changing the parent of a node can make the whole data structure inconsistent.
	   :ltree-reduce
	   :ltree->list
	   :list->ltree
	   :ltree-set-new-child
	   :ltree-follow-path
	   :ltree-add/update-path
	   :make-ltree
	   :ltree-follow-parent
	   :ltree-path-from-root
	   :define-ltree-type
	   :ltree-hashtable ;structure
	   :make-ltree-hashtable-root
	   :define-ltree-type-with-children-array-storage
	   ))

(in-package :ltree)

;;;; ltree functions, to be exported.

(defgeneric ltree-label (ltree)
  (:documentation "Return the label of ltree."))

(defgeneric (setf ltree-label) (label ltree)
  (:documentation "Set the LABEL of ltree."))

(defgeneric ltree-value (ltree)
  (:documentation "Return the value of ltree."))

(defgeneric (setf ltree-value) (value ltree)
  (:documentation "Set the VALUE of ltree."))

(defgeneric ltree-parent (ltree)
  (:documentation "Return the parent of ltree."))

(defgeneric (setf ltree-parent) (parent ltree)
  (:documentation "Set the PARENT of ltree."))

;; internal accessor methods, not exported.

(defgeneric ltree-children (ltree)
  (:documentation "Return the children of ltree."))

(defgeneric (setf ltree-children) (children ltree)
  (:documentation "Set the CHILDREN of ltree."))

;; The 5 methods related to children handling. These functions must be overwritten by DEFINE-LTREE-TYPE, if the children storage should be a custom data structure.
       
;; I somewhat artificially made this function depend on its only argument NODE, to allow converting the function to a method which depends on its type of NODE, which would not be possible in the beforehand setting, where the function didn't have a parameter and only returned the value representing an empty children slot.
(defgeneric set-empty-children (node)
  (:documentation "Set the children slot of NODE to the value representing no children attached to NODE."))
       
(defgeneric add/update-child-in-children (node child-label child-node)
  (:documentation "add/update CHILD-NODE under the label CHILD-LABEL in NODE's children.
Returns :UPDATE or :ADD, depending on whether CHILD-LABEL was present or not, respectively.
Does _not_ modify CHILD-LABEL or CHILD-VALUE."))

(defgeneric get-child-from-children (node child-label)
  (:documentation "Get the child node stored under label CHILD-LABEL in the children of NODE.
Returns as secondary value whether CHILD-LABEL was found."))

(defgeneric remove-child-from-children (node child-label)
  (:documentation "Removes CHILD-LABEL from the children of NODE.
Returns T if CHILD-LABEL was present, NIL otherwise."))

(defgeneric list-childs-in-children (node)
  (:documentation "Return a list with all children nodes of NODE."))

(defgeneric new-node-from-ltree-type (ltree)
  (:documentation "Return a newly created node with the same type as LTREE and all slots initialized to their default values."))

;;;; Functions in this section use the accessor functions and 5 functions handling children passed to DEFINE-LTREE-TYPE to modify or query node-children. For the hash-table children type see below.

(defun ltree-reduce (ltree children-reduce-function children-initial-value node-function &key sort-children-predicate)
  "Visit LTREE in depth-first order and reduce it to one value.
This means that LTREE-REDUCE calls NODE-FUNCTION with all the leaves and CHILDREN-INITIAL-VALUE.
A node with children calculates its value by starting with CHILDREN-INITIAL-VALUE and then iteratively reducing it and the result of its children with CHILDREN-REDUCE-FUNCTION to CHILDREN-RESULT.
The node's value as a whole is calculated by calling NODE-FUNCTION with the node and its children's value.
The node's value is then used in its parent as a child's value in a CHILDREN-REDUCE-FUNCTION call and so on until the whole tree is processed.
The last call is to NODE-FUNCTION with the root of LTREE and its children's value.

This also means that NODE-FUNCTION is called with CHILDREN-INITIAL-VALUE on only the leaves of the tree.
All other calls to NODE-FUNCTION are with a reduced CHILDREN-RESULT.
The other place where CHILDREN-INITIAL-VALUE is used is as the initial value in the reduction of the child's values to the childrens' value.

The algorithm goes as follows:
CHILDREN-REDUCE-FUNCTION is first called with CHILDREN-INITIAL-VALUE and the result (returned by NODE-FUNCTION, see below) of the first child of LTREE.
Its returned value is stored as CHILDREN-RESULT.
CHILDREN-REDUCE-FUNCTION is called with it and the result of the second child of LTREE.
Its returned result is stored as the new CHILDREN-RESULT, and so on until the result of the last child has been reduced to the CHILDREN-RESULT.
If a node doesn't have any children, CHILDREN-INITIAL-VALUE is used as CHILDREN-RESULT unchanged.
When the childrens' values are thus reduced to one value, NODE-FUNCTION is called with the node and the CHILDREN-RESULT (childrens' value).
When the whole tree was visited, the resulting value of the root is returned.

If SORT-CHILDREN-PREDICATE is non-NIL, it is used in a call to SORT to sort the children before their reduction.
An example of SORT-CHILDREN-PREDICATE is (lambda (a b) (< (ltree-value a) (ltree-value b)))."
  ;; TODO: specialize on sort-children-predicate.
  (labels ((rec (ltree)
	     ;; TODO: don't use a list for child-list, use a vector (for faster sort).
	     (let ((child-list (list-childs-in-children ltree))
		   (children-result children-initial-value))
	       (when (not (null sort-children-predicate))	       
		 (setf child-list (sort child-list sort-children-predicate)))
	       (dolist (child child-list)
		 (setf children-result
		       (funcall children-reduce-function
				children-result
				(rec child))))
	       (funcall node-function ltree children-result))))
    (rec ltree)))

;;TODO (defun ltree-breadth-first-reduce ...
;;TODO: Is there a difference between ltree-depth-first-reduce and ltree-breadth-first-reduce? Right now, I don't think there is one, because the children must be computed before their parent.

(defun ltree->list (ltree &key sort-children-predicate)
  (flet ((children-reduce-function (initial-value child-value)
	   (cons child-value initial-value))
	 (node-function (node children-result)
	   (list (ltree-label node) (ltree-value node) (nreverse children-result)))) ;;nreverse b/c we cons'd
    (ltree-reduce ltree #'children-reduce-function nil #'node-function :sort-children-predicate sort-children-predicate)))

(defun ltree-set-new-child (ltree child-label child-value)
  "Set the child with label CHILD-LABEL and value CHILD-VALUE under the ltree LTREE to an empty ltree.
Returns the newly created child node, and :UPDATE or :ADD as second value, depending on whether CHILD-LABEL was present or not, respectively."
  (let ((child-node (new-node-from-ltree-type ltree)))
    (setf (ltree-label child-node) child-label)
    (setf (ltree-value child-node) child-value)
    (setf (ltree-parent child-node) ltree)
    (set-empty-children child-node)
    (values child-node
	    (add/update-child-in-children ltree child-label child-node))))

(defun list->ltree (list make-ltree-root-function)
  "Convert the list representation LIST to its corresponding ltree.
MAKE-LTREE-ROOT-FUNCTION must be the function without parameters that returns a new root of a LTREE. (see parameter MAKE-LTREE-TYPENAME-ROOT-FUNCTION-NAME of DEFINE-LTREE.)
If a label occurs more than once in a children list, the last occurrence is kept and the others are thrown away."
  (labels ((rec (list ltree)
	     (if (null list)
		 ltree
		 (destructuring-bind (label value children-list) list
		   (let ((new-ltree (ltree-set-new-child ltree label value)))
		     (dolist (child-list children-list)
		       (rec child-list new-ltree)))
		   ltree))))
    (let ((ltree-root (funcall make-ltree-root-function))
	  (root-label (car list))
	  (root-value (cadr list))
	  (children-list (caddr list)))
      (setf (ltree-label ltree-root) root-label)
      (setf (ltree-value ltree-root) root-value)
      (dolist (child-list children-list)
	(rec child-list ltree-root))
      ltree-root)))

(defun ltree-follow-path (ltree path &optional (function nil))
  "Find a path from the top of the ltree, LTREE, with the path specified by the list PATH, its elements specifying child's labels.
Return the first ltree that doesn't have a child with a label like the CAR of the remaining path.
Return as secondary value the remaining path that was not found (i.e. NIL if PATH is stored in LTREE completely).
At each matching child along the path, call FUNCTION with the child and the remaining path, if path is present and not NIL yet. The first invocation of FUNCTION is with the node that has (car PATH) as label."
  ;; TODO: specialize on FUNCTION being NIL.
  (if (null path)
      (values ltree nil)
      (multiple-value-bind (child-node present-p)
	  (get-child-from-children ltree (car path))
	(if (null present-p)
	    (values ltree path)
	    (let ((cdr-path (cdr path)))
	      (when (not (null function))
		(funcall function child-node cdr-path))
	      (ltree-follow-path child-node cdr-path))))))

(defun ltree-add/update-path (ltree labels-along-path values-along-path)
  "Add/update under LTREE all child nodes along the label list LABELS-ALONG-PATH with the values VALUES-ALONG-PATH.
The first added/updated node is the one with label (car LABELS-ALONG-PATH) under LTREE.
Return the (maybe newly created) ltree at the end of LABELS-ALONG-PATH."
  (flet ((visit-node (ltree path)
	   (declare (ignore path))
	   (setf labels-along-path (cdr labels-along-path))
	   (setf (ltree-value ltree) (car values-along-path))
	   (setf values-along-path (cdr values-along-path))))
    (multiple-value-bind (ltree remaining-path)
	(ltree-follow-path ltree labels-along-path #'visit-node)
      ;;(print (list "ltree" ltree "remaining-path" remaining-path "labels-along-path" labels-along-path))
      (assert (equal remaining-path labels-along-path))
      (labels ((create-node (ltree labels values)
		 (if (null labels)
		     ltree
		     (create-node (ltree-set-new-child ltree (car labels) (car values))
				  (cdr labels)
				  (cdr values)))))
	(create-node ltree labels-along-path values-along-path)))))

(defun ltree-set-value (ltree path-label value)
  "In LTREE, look up the child ltree adressed by PATH-LABEL, which is a list of child labels.
If the path described by PATH-LABEL doesn't exist, then an UNBOUND-SLOT error is raised.
Otherwise set the looked-up ltree's value to VALUE."
  (multiple-value-bind (ltree path-rest) (ltree-follow-path ltree path-label)
    (if (null path-rest)
	(setf (ltree-value ltree) value)
	(error (make-condition 'unbound-slot :instance ltree :name path-label)))))

(defun make-ltree-from-paths (ltree-root path-labels path-values)
  "Make a new ltree from PATH-LABELS and PATH-VALUES, and store it in LTREE-ROOT, see function MAKE-LTREE."
  (assert (= (length path-labels) (length path-values)))
  (do ((l path-labels (cdr l))
       (v path-values (cdr v)))
      ((null l))
    (let* ((label (car l))
	   (value (car v)))
      (multiple-value-bind (ltree-here path-rest)
	  (ltree-follow-path ltree-root label)
	(case (length path-rest)
	  (0 (setf (ltree-value ltree-here) value))
	  (1 (ltree-set-new-child ltree-here (car path-rest) value))
	  (otherwise (error "Either PATH-LABELS are not ordered correctly or are missing a connecting label-path, e.g. PATH-LABELS is '((A A)) but should be '((A) (A A))."))))))
  ltree-root)

(defun make-ltree (make-ltree-root-function &key (path-labels nil) (path-values nil) (pairs nil))
  "Return an newly constructed ltree.
MAKE-LTREE-ROOT-FUNCTION must be the function without parameters that returns a new root of a LTREE. (see parameter MAKE-LTREE-TYPENAME-ROOT-FUNCTION-NAME of DEFINE-LTREE.)
Either PATH-LABELS and PATH-VALUES, or PAIRS is used in newly constructing the ltree.
* if PATH-LABELS (e.g. '((a) (b) (a a) (a b) (a a))) and PATH-VALUES (e.g. '(1 2 11 12 -11)) are specified, then they are used to initialize the ltree.
* if PAIRS (e.g. '(((a) 1) ((b) 2) ((a a) 11) ((a b) 12)) ) is specified, they are used to initialize the ltree.
* otherwise, an empty ltree is returned.
Both PATH-LABELS and the path-labels in PAIRS must be ordered by their occurrence in the tree, so that entries closer to the root of the tree come before entries further away from the root."
  (assert (or (and path-labels path-values) pairs))
  (let ((ltree-root (funcall make-ltree-root-function)))
    (cond
      (path-labels (make-ltree-from-paths ltree-root path-labels path-values))
      (pairs (make-ltree-from-paths ltree-root (mapcar #'car pairs) (mapcar #'cadr pairs)))
      )))

;; TODO: add (defun ltree-merge-ltree (ltree1 ltree2)), which merges the two ltrees (on child-label collision, ltree1 is preferred). This should merge '(((a) 1) ((b) 2) ((a a) 11) ((a b) 12)) and '(((a) -1) ((a c) 13)) so that '(((a) 1) ((b) 2) ((a a) 11) ((a b) 12) ((a c) 13)) results (i.e. ((a) -1) is overridden by ((a) 1)).

(defun ltree-follow-parent (ltree function &key root)
  "Follow LTREE's parent, until we reach a node that is EQ to ROOT, or the root of the tree.
At each node (including ROOT or the root), call FUNCTION with the ltree at this node.
The FUNCTION is first called with LTREE's parent, i.e. if LTREE is ROOT, the function is not called at all.
Returns ROOT."
  (labels ((rec (ltree)
	     (let ((parent (ltree-parent ltree)))
	       (if (eq parent root)
		   (if (null root)
		       ltree
		       (progn
			 (funcall function parent)
			 parent))
		   (progn
		     (funcall function parent)
		     (rec parent))))))
    (rec ltree)))

(defun ltree-path-from-root (ltree &key root)
  "Follow the parents of LTREE recursively until ROOT or the root of the tree is reached.
Return the labels along the path, including the ROOT."
  (let ((path (list (ltree-label ltree))))
    (ltree-follow-parent ltree
			 (lambda (node)
			   (push (ltree-label node) path))
			 :root root)
    path))

(defmacro define-ltree-type (ltree-typename
			     make-ltree-root-function-name
			     set-empty-children-function
			     add/update-child-in-children-function
			     get-child-from-children-function
			     remove-child-from-children-function
			     list-childs-in-children-function)
  "Define a new type of LTREE, which has node type LTREE-TYPENAME and the 5 functions modifying and accessing the children of a node.
Also defines the function named MAKE-LTREE-ROOT-FUNCTION-NAME, which returns the empty LTREE upon invocation.
The descriptions of the 5 functions modifying and accessing children are as follows:

  (defun set-empty-children (node)
    'Set the children slot of NODE to the value representing no children attached to NODE.'
    ... )

  (defun add/update-child-in-children (node child-label child-node)
    'add/update CHILD-NODE under the label CHILD-LABEL in NODE's children.
  Returns :UPDATE or :ADD, depending on whether CHILD-LABEL was present or not, respectively.
  Does _not_ modify CHILD-LABEL or CHILD-VALUE.'
    ... )

  (defun get-child-from-children (node child-label)
    'Get the child node stored under label CHILD-LABEL in the children of NODE.
  Returns as secondary value whether CHILD-LABEL was found.'
    ... )

  (defun remove-child-from-children (node child-label)
    'Removes CHILD-LABEL from the children of NODE.
  Returns T if CHILD-LABEL was present, NIL otherwise.'
    ... )

  (defun list-childs-in-children (node)
    'Return a list with all children nodes of NODE.'
    ... )

When implementing a new ltree type, you might want to use function LTREE-TEST to test some basic functionality of the type."
  (declare (type symbol ltree-typename))
	   ;;(ftype (function (t) t) set-empty-children-function)
	   ;;(ftype (function (t symbol t) (or :add :update)) add/update-child-in-children-function)
	   ;;(ftype (function (t symbol) (values t (or t nil))) get-child-from-children-function)
	   ;;(ftype (function (t symbol) (or t nil)) remove-child-from-children-function)
	   ;;(ftype (function (t) list) list-childs-in-children-function))
  (let* ((type-string (string ltree-typename))
	 (ltree-typename+-label (intern (concatenate 'simple-string type-string "-LABEL")))
	 (ltree-typename+-value (intern (concatenate 'simple-string type-string "-VALUE")))
	 (ltree-typename+-parent (intern (concatenate 'simple-string type-string "-PARENT")))
	 (ltree-typename+-children (intern (concatenate 'simple-string type-string "-CHILDREN")))
	 (ltree-typename+make-node (gensym (concatenate 'simple-string "MAKE-" type-string))))
    `(progn
       ;; the structure

       (defstruct (,ltree-typename
		    ;; don't create potentially already otherwhere defined symbols
		    ;; TODO: How to name selector functions with symbols made by gensym?
		    (:constructor ,ltree-typename+make-node)
		    (:copier nil)
		    (:predicate nil))
	 (label nil) ; the node's label; can have any type.
	 (value nil) ; the node's value; can have any type.
	 (parent nil) ;; I would like to add ":type (or nil node)" here, but that doesn't work. nil means that this node represents the root of the ltree.
	 (children nil) ;; The mapping from a child's label to its node. The specific structure depends on the underlying implementation.
	 )

       ;; the accessor methods

       (defmethod ltree-label ((ltree ,ltree-typename))
	 "Return the label of LTREE."
	 (,ltree-typename+-label ltree))

       (defmethod (setf ltree-label) (label (ltree ,ltree-typename))
	 "Set the LABEL of LTREE."
	 (setf (,ltree-typename+-label ltree) label))

       (defmethod ltree-value ((ltree ,ltree-typename))
	 "Return the value of LTREE."
	 (,ltree-typename+-value ltree))

       (defmethod (setf ltree-value) (value (ltree ,ltree-typename))
	 "Set the VALUE of LTREE."
	 (setf (,ltree-typename+-value ltree) value))

       ;; internal accessor methods, not exported.

       (defmethod ltree-parent ((ltree ,ltree-typename))
	 "Return the parent of LTREE."
	 (,ltree-typename+-parent ltree))

       (defmethod (setf ltree-parent) (parent (ltree ,ltree-typename))
	 "Set the PARENT of LTREE."
	 (setf (,ltree-typename+-parent ltree) parent))

       (defmethod ltree-children ((ltree ,ltree-typename))
	 "Return the children of LTREE."
	 (,ltree-typename+-children ltree))

       (defmethod (setf ltree-children) (children (ltree ,ltree-typename))
	 "Set the CHILDREN of LTREE."
	 (setf (,ltree-typename+-children ltree) children))

       (defun ,make-ltree-root-function-name ()
	 "Return an empty ltree."
	 (let ((root (,ltree-typename+make-node :label nil
						:value nil
						:parent nil
						:children nil)))
	   (funcall ,set-empty-children-function root)
	   root))

       ;; The 5 methods related to children handling.
       ;; Previous comment was: The following 5 functions must be replaced if CHILDREN (in structure NODE) should not be a hash-table of default type anymore, or not be a hash-table at all anymore.
       
       ;; I somewhat artificially made this function depend on its only argument NODE, to allow converting the function to a method which depends on its type of NODE, which would not be possible in the beforehand setting, where the function didn't have a parameter and only returned the value representing an empty children slot.
       (defmethod set-empty-children ((node ,ltree-typename))
	 (funcall ,set-empty-children-function node))
       
       (defmethod add/update-child-in-children ((node ,ltree-typename) child-label child-node)
	 (funcall ,add/update-child-in-children-function node child-label child-node))

       (defmethod get-child-from-children ((node ,ltree-typename) child-label)
	 (funcall ,get-child-from-children-function node child-label))

       (defmethod remove-child-from-children ((node ,ltree-typename) child-label)
	 (funcall ,remove-child-from-children-function node child-label))

       (defmethod list-childs-in-children ((node ,ltree-typename))
	 (funcall ,list-childs-in-children-function node))

       ;; auxiliary functions

       (defmethod new-node-from-ltree-type ((ltree ,ltree-typename))
	 "Return a newly created node with the same type as LTREE and all slots initialized to their default values."
	 (,ltree-typename+make-node))
       (defmethod print-object ((object ,ltree-typename) stream)
	 ;; TODO: find a way to sort the children by label, possibly using the sort-children parameter of ltree->list.
	 (print-unreadable-object (object stream :type nil :identity t)
	   (format stream "~A ~s" ,type-string (ltree->list object))))
       )))

;;;; Different sub-tree storage implementation types
;; All of the following should be possible:
;; * Store the sub-trees as a hash-table
;; * Store the sub-trees as an alist or plist.
;; * Store the sub-trees in an array, where each index corresponds to a predefined atom

;;;; Using a hash-table as children storage method.

(defun htchilds-set-empty-children (node)
  (setf (ltree-children node) (make-hash-table :test 'eq)))

(defun htchilds-add/update-child-in-children (node child-label child-node)
  "add/update CHILD-NODE under the label CHILD-LABEL in NODE's children.
Returns :UPDATE or :ADD, depending on whether CHILD-LABEL was present or not, respectively.
Does _not_ modify CHILD-LABEL or CHILD-VALUE."
  (let* ((children (ltree-children node))
	 (operation (if (null (nth-value 1 (gethash child-label children)))
			:add
			:update)))
    (setf (gethash child-label children) child-node)
    operation))

(defun htchilds-get-child-from-children (node child-label)
  "Get the child node stored under label CHILD-LABEL in the children of NODE.
Returns as secondary value whether CHILD-LABEL was found."
  (gethash child-label (ltree-children node)))

(defun htchilds-remove-child-from-children (node child-label)
  "Removes CHILD-LABEL from the children of NODE.
Returns T if CHILD-LABEL was present, NIL otherwise."
  (remhash child-label (ltree-children node)))

(defun htchilds-list-childs-in-children (node)
  "Return a list with all children nodes of NODE."
  (let ((children nil))
    (maphash (lambda (k v) 
	       (declare (ignore k))
	       (push v children))
	     (ltree-children node))
    children))

;; Define the ltree type ltree-hashtable.

(define-ltree-type ltree-hashtable
    make-ltree-hashtable-root
  #'htchilds-set-empty-children
  #'htchilds-add/update-child-in-children
  #'htchilds-get-child-from-children
  #'htchilds-remove-child-from-children
  #'htchilds-list-childs-in-children)

;;;; Using an array of symbols as children storage method.

(defmacro define-ltree-type-with-children-array-storage (ltree-typename init-possible-children-symbols-function-name make-ltree-root-function-name)
  "Define an ltree that uses an array of objects (except NIL) to store children.
LTREE-TYPENAME is the name of the struct that makes up the ltree.
INIT-POSSIBLE-CHILDREN-SYMBOLS-FUNCTION-NAME is the name of the function that receives the list of symbols that can be stored as children labels in the ltree. It must be used exactly once, before creating any ltree of this type using MAKE-LTREE-ROOT-FUNCTION-NAME.
MAKE-LTREE-ROOT-FUNCTION-NAME is the name of the function that returns an empty root."
  ;; with init-possible-children-symbols-function-name it is possible to init the possible children symbols during run-time.
  (let ((set-empty-children-fn (gensym "set-empty-children"))
	(add/update-child-in-children-fn (gensym "add/update-child-in-children"))
	(get-child-from-children-fn (gensym "get-child-from-children"))
	(remove-child-from-children-fn (gensym "remove-child-from-children"))
	(list-childs-in-children-fn (gensym "list-childs-in-children"))
	(child-to-index (gensym "child-to-index"))
	(number-children (gensym "number-children")))
    `(let ((,child-to-index nil)
	   (,number-children nil))
       (defun ,init-possible-children-symbols-function-name (possible-children-symbols)
	 "Initialize the list of symbols that may be used as the labels of children with POSSIBLE-CHILDREN-SYMBOLS.
This function must be called exactly once, before the first ltree root of this type is created."
	 (assert (listp possible-children-symbols))
	 ;; TODO: better error message than the cryptic one returned by this assert.
	 (assert (or (and (null ,child-to-index) (null ,number-children))
		     (let ((stored nil)
			   (passed (loop for s in possible-children-symbols collect s)))
		       (maphash (lambda (k v) (declare (ignore v)) (push k stored)) ,child-to-index)
		       (flet ((symbol-lessp (a b)
				(string-lessp (string a) (string b))))
			 (setf passed (sort passed #'symbol-lessp))
			 (setf stored (sort stored #'symbol-lessp))
			 (equalp passed stored))))) ;if the init-* function is called multiple times, it must be called with the same parameter.
	 (setf ,child-to-index (make-hash-table :test 'eq :size (length possible-children-symbols)))
	 (setf ,number-children (length possible-children-symbols))
	 (loop
	    for s in possible-children-symbols
	    for index from 0 do
	      (assert (not (null s)))
	      (setf (gethash s ,child-to-index) index)))
       (flet ((,set-empty-children-fn (node)
		(setf (ltree-children node) (make-array ,number-children :element-type 'symbol :initial-element nil :adjustable nil :fill-pointer nil)))
	      (,add/update-child-in-children-fn (node child-label child-node)
		(let* ((i (gethash child-label ,child-to-index))
		       (children (ltree-children node))
		       (operation (if (null (svref children i))
				      :add
				      :update)))
		  (setf (svref children i) child-node)
		  operation))
	      (,get-child-from-children-fn (node child-label)
		(let* ((i (gethash child-label ,child-to-index))
		       (children (ltree-children node))
		       (child (svref children i)))
		  (values child (not (null child)))))
	      (,remove-child-from-children-fn (node child-label)
		(let* ((i (gethash child-label ,child-to-index))
		       (children (ltree-children node))
		       (child (svref children i)))
		  (setf (svref children i) nil)
		  (not (null child))))
	      (,list-childs-in-children-fn (node)
		(let* ((children (ltree-children node))
		       (childs nil))
		  (maphash (lambda (symbol index)
			     (declare (ignore symbol))
			     (let ((child-node (svref children index)))
			       (when child-node
				 (push child-node childs))))
			   ,child-to-index)
		  childs)))
	 (define-ltree-type ,ltree-typename
	     ,make-ltree-root-function-name
	   #',set-empty-children-fn
	   #',add/update-child-in-children-fn
	   #',get-child-from-children-fn
	   #',remove-child-from-children-fn
	   #',list-childs-in-children-fn)))))

;; Test define-ltree-type-with-children-array-storage:
;;(progn
;;  (ltree::define-ltree-type-with-children-array-storage ltree-array-1 init-ltree-array-1 make-ltree-array-1)
;;  (init-ltree-array-1 '(ltree::a ltree::a/a ltree::a/a/a ltree::a/b ltree::a/c ltree::b))
;;  (ltree::ltree-test #'make-ltree-array-1)
;;  )

;;;; Test functions:

(defun symbol-label-lessp (a b)
  "Returns T, if the label of node A is lexicographically less than the label of node B, NIL otherwise."
  (let* ((sa (string (ltree-label a)))
	 (sb (string (ltree-label b)))
	 (r (string-lessp sa sb)))
    r))

(defun ltree-test (make-ltree-root-function)
  "Perform tests.
MAKE-LTREE-ROOT-FUNCTION is a function with no parameters that returns a new LTREE root."
  (flet ((assert-equal (ltree list-representation)
	   (let ((actual-list (ltree->list ltree
					  :sort-children-predicate
					  #'symbol-label-lessp)))
	     (assert (equal actual-list list-representation)))))
    (let ((ltree (funcall make-ltree-root-function)))
      (assert-equal ltree '(nil nil nil))
      (setf (ltree-label ltree) :root)
      (setf (ltree-value ltree) :bla)
      ;; this tests LTREE->LIST, and LTREE-LABEL, LTREE-VALUE, LTREE-REDUCE in the process.
      (assert-equal ltree '(:root :bla nil))
      ;; this tests LTREE-ADD/UPDATE-PATH, and LTREE-FOLLOW-PATH in the process.
      (ltree-add/update-path ltree '(a a/a a/a/a) '(1 11 111))
      (assert-equal ltree '(:root :bla ((a 1 ((a/a 11 ((a/a/a 111 nil))))))))
      (ltree-add/update-path ltree '(a a/c) '(-1 -13))
      (ltree-add/update-path ltree '(a a/b) '(-1 -12)) 
      (assert-equal ltree '(:root :bla ((a -1 ((a/a 11 ((a/a/a 111 nil))) (a/b -12 nil) (a/c -13 nil))))))
      ;; test LIST->LTREE.
      (let* ((l1 '(:root :bla ((a -1 ((a/a 11 ((a/a/a 111 nil))) (a/b -12 nil) (a/c -13 nil))))))
	     (l2 (ltree->list (list->ltree l1 make-ltree-root-function)
			     :sort-children-predicate #'symbol-label-lessp)))
	(assert (equal l1 l2)))
      (let ((ltree-a/a/a (ltree-follow-path ltree '(a a/a a/a/a)))
	    (ltree-a (ltree-follow-path ltree '(a))))
	;; test LTREE-PATH-FROM-ROOT and LTREE-FOLLOW-PARENT in the process.
	(assert (equal (ltree-path-from-root ltree-a/a/a) '(:root a a/a a/a/a)))
	(assert (equal (ltree-path-from-root ltree-a/a/a :root ltree-a) '(a a/a a/a/a)))))
    (let ((ltree-paths (make-ltree make-ltree-root-function :path-labels '((a) (b) (a a) (a a) (a b)) :path-values '(1 2 11 -11 12)))
	  (ltree-pairs (make-ltree make-ltree-root-function :pairs '(((a) 1) ((b) 2) ((a a) 11) ((a b) 12) ((a a) -11))))
	  (correct '(NIL NIL ((A 1 ((A -11 NIL) (B 12 NIL))) (B 2 NIL)))))
      (assert-equal ltree-paths correct)
      (assert-equal ltree-pairs correct))))

(ltree-test #'make-ltree-hashtable-root)
