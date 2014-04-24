;; DAWG (directed acyclic word graph) structure.
;; In this implementation, every node in the tree has a value.
;; The children are not ordered. 
;; If a circle is constructed, the behavior of the functions are undefined.

;; A big note: I suspect that I implemented a normal tree, not a DAWG. (Especially in the functions DAWG->LIST, and DAWG.) Just rename the package in this case. But basically I want a tree anyways.

(defpackage :dawg
  (:use :common-lisp)
  (:export :dawg-label
	   :dawg-value
	   :dawg-reduce
	   :dawg-list
	   :dawg-set-new-child
	   :dawg-follow-path
	   :dawg-add/update-path
	   :dawg
	   :dawg-follow-parent))

(in-package :dawg)

;; the structure

(defstruct node
  (label nil) ; the node's label; can have any type.
  (value nil) ; the node's value; can have any type.
  (parent nil) ;; I would like to add ":type (or nil node)" here, but that doesn't work. nil means that this node represents the root of the dawg.
  (children nil) ;; The mapping from a children's label to its node. The specific structure depends on the underlying implementation.
  )

;;;; The following 5 functions must be replaced if CHILDREN (in structure NODE) should not be a hash-table of default type anymore, or not be a hash-table at all anymore.

;; TODO: make it possible to define a new type plus functions operating on this new type, which use user-specified versions of the following 5 functions. (see macro DEFINE-HASHTRIE of package DARTSCLHASHTREE for an example.)

;; I somewhat artificially made this function depend on its only argument NODE, to allow converting the function to a method which depends on its type of NODE, which would not be possible in the beforehand setting, where the function didn't have a parameter.
(defun set-empty-children (node)
  (setf (node-children node) (make-hash-table :test 'eq)))

(defun add/update-child-in-children (node child-label child-node)
  "add/update CHILD-NODE under the label CHILD-LABEL in NODE's children.
Returns :UPDATE or :ADD, depending on whether CHILD-LABEL was present or not, respectively.
Does _not_ modify CHILD-LABEL or CHILD-VALUE."
  (let* ((children (node-children node))
	 (operation (if (null (nth-value 1 (gethash child-label children)))
			:add
			:update)))
    (setf (gethash child-label children) child-node)
    operation))

(defun get-child-from-children (node child-label)
  "Get the child node stored under label CHILD-LABEL in the children of NODE.
Returns as secondary value whether CHILD-LABEL was found."
  (gethash child-label (node-children node)))

(defun remove-child-from-children (node child-label)
  "Removes CHILD-LABEL from the children of NODE.
Returns T or NIL, depending on whether CHILD-LABEL was present."
  (remhash child-label node))

(defun list-childs-in-children (node)
  "Return a list with all children nodes of NODE."
  (let ((children nil))
    (maphash (lambda (k v) 
	       (declare (ignore k))
	       (push v children))
	     (node-children node))
    children))

;;;; All functions below this line use the functions above to modify or query node-children.

;;;; DAWG functions, to be exported.

(defun dawg-label (dawg)
  "Return the label of DAWG."
  (node-label dawg))

(defun (setf dawg-label) (label dawg)
  "Set the LABEL of DAWG."
  (setf (node-label dawg) label))

(defun dawg-value (dawg)
  "Return the value of DAWG."
  (node-value dawg))

(defun (setf dawg-value) (value dawg)
  "Set the VALUE of DAWG."
  (setf (node-value dawg) value))

(defun make-dawg-root ()
  "Return an empty dawg."
  (let ((root (make-node :label nil
			 :value nil
			 :parent nil
			 :children nil)))
    (set-empty-children root)
    root))

(defun dawg-reduce (dawg children-reduce-function children-initial-value node-function &key sort-children-predicate)
  "Visit DAWG in depth-first order and reduce it to one value.
This means that CHILDREN-REDUCE-FUNCTION is first called with CHILDREN-INITIAL-VALUE and the result of the left-most child of DAWG.
Its returned value is then used as new CHILDREN-INITIAL-VALUE and CHILDREN-REDUCE-FUNCTION is called with it and the second-left-most child of DAWG, and so on until the right-most child is reached.
When the childrens' values are thus reduced to one value, NODE-FUNCTION is called with the node's label, value, and the previously calculated childrens' value.
When the whole tree was visited, the resulting value is returned.
If SORT-CHILDREN-PREDICATE is non-NIL, it is used in a call to SORT to sort the children before their reduction.
An example of SORT-CHILDREN-PREDICATE is (lambda (a b) (< (dawg-value a) (dawg-value b)))."
  ;; TODO: specialize on a non-NIL sort-children-predicate.
  (labels ((rec (dawg)
	     ;; TODO: don't use a list for child-list, use a vector (for faster sort).
	     (let ((child-list (list-childs-in-children dawg))
		   (children-result children-initial-value))
	       (when (not (null sort-children-predicate))	       
		 (setf child-list (sort child-list sort-children-predicate)))
	       (dolist (child child-list)
		 (setf children-result
		       (funcall children-reduce-function
				children-result
				(rec child))))
	       (funcall node-function
			(node-label dawg)
			(node-value dawg)
			children-result))))
    (rec dawg)))

;;TODO (defun dawg-breadth-first-reduce ...
;;TODO: Is there a difference between dawg-depth-first-reduce and dawg-breadth-first-reduce? Right now, I don't think there is one, because the children must be computed before their parent.

(defun dawg->list (dawg &key sort-children-predicate)
  (flet ((children-reduce-function (initial-value child-value)
	   (cons child-value initial-value))
	 (node-function (label value children-result)
	   (list label value (nreverse children-result)))) ;;nreverse b/c we cons'd
    (dawg-reduce dawg #'children-reduce-function nil #'node-function :sort-children-predicate sort-children-predicate)))

;; TODO: (defun list->dawg (list))

(defmethod print-object ((object node) stream)
  ;; TODO: find a way to sort the children by label, possibly using the sort-children parameter of dawg->list.
  (print-unreadable-object (object stream :type nil :identity t)
    (format stream "DAWG ~s" (dawg->list object))))

(defun dawg-set-new-child (dawg child-label child-value)
  "Set the child with label CHILD-LABEL and value CHILD-VALUE under the dawg DAWG to an empty dawg.
Returns the newly created child node, and :UPDATE or :ADD as second value, depending on whether CHILD-LABEL was present or not, respectively."
  (let ((child-node (make-node :label child-label
			       :value child-value
			       :parent dawg
			       :children nil)))
    (set-empty-children child-node)
    (values child-node
	    (add/update-child-in-children dawg child-label child-node))))

(defun dawg-follow-path (dawg path &optional (function nil))
  "Find a path from the top of the dawg, DAWG, with the path specified by the list PATH, its elements specifying child's labels.
Return the first dawg that doesn't have a child with a label like the CAR of the remaining path.
Return as secondary value the remaining path that was not found (i.e. NIL if PATH is stored in DAWG completely).
At each matching child along the path, call FUNCTION with the child and the remaining path, if path is present and not NIL yet."
  ;; TODO: specialize on FUNCTION being non-NIL.
  (if (null path)
      (values dawg nil)
      (multiple-value-bind (child-node present-p)
	  (get-child-from-children dawg (car path))	
	(if (null present-p)
	    (values dawg path)
	    (let ((cdr-path (cdr path)))
	      (when (not (null function))
		(funcall function child-node cdr-path))
	      (dawg-follow-path child-node cdr-path))))))

(defun dawg-add/update-path (dawg labels-along-path values-along-path)
  "Add/update under DAWG all child nodes along the label list LABELS-ALONG-PATH with the values VALUES-ALONG-PATH.
Return the (maybe newly created) dawg at the end of LABELS-ALONG-PATH."
  (flet ((visit-node (dawg path)
	   (declare (ignore path))
	   (setf labels-along-path (cdr labels-along-path))
	   (setf (node-value dawg) (car values-along-path))
	   (setf values-along-path (cdr values-along-path))))
    (multiple-value-bind (dawg remaining-path)
	(dawg-follow-path dawg labels-along-path #'visit-node)
      ;;(print (list "dawg" dawg "remaining-path" remaining-path "labels-along-path" labels-along-path))
      (assert (equal remaining-path labels-along-path))
      (labels ((create-node (dawg labels values)
		 (if (null labels)
		     dawg
		     (create-node (dawg-set-new-child dawg (car labels) (car values))
				  (cdr labels)
				  (cdr values)))))
	(create-node dawg labels-along-path values-along-path)))))

(defun dawg-set-value (dawg path-label value)
  "In DAWG, look up the child dawg adressed by PATH-LABEL, which is a list of child labels.
If the path described by PATH-LABEL doesn't exist, then an UNBOUND-SLOT error is raised.
Otherwise set the looked-up dawg's value to VALUE."
  (multiple-value-bind (dawg path-rest) (dawg-follow-path dawg path-label)
    (if (null path-rest)
	(setf (dawg-value dawg) value)
	(error (make-condition 'unbound-slot :instance dawg :name path-label)))))

(defun make-dawg-from-paths (path-labels path-values)
  "Make a new dawg from PATH-LABELS and PATH-VALUES, see function DAWG."
  (assert (= (length path-labels) (length path-values)))
  (let ((dawg (make-dawg-root)))
    (do ((l path-labels (cdr l))
	 (v path-values (cdr v)))
	((null l))
      (let* ((label (car l))
	     (value (car v)))
	(multiple-value-bind (dawg-here path-rest)
	    (dawg-follow-path dawg label)
	  (case (length path-rest)
	    (0 (setf (dawg-value dawg-here) value))
	    (1 (dawg-set-new-child dawg-here (car path-rest) value))
	    (otherwise (error "Either PATH-LABELS are not ordered correctly or are missing a connecting label-path, e.g. PATH-LABELS is '((A A)) but should be '((A) (A A))."))))))
    dawg))

(defun dawg (&key (path-labels nil) (path-values nil) (pairs nil))
  "Return an newly constructed dawg.
Either PATH-LABELS and PATH-VALUES, or PAIRS is used in newly constructing the dawg.
* if PATH-LABELS (e.g. '((a) (b) (a a) (a b) (a a))) and PATH-VALUES (e.g. '(1 2 11 12 -11)) are specified, then they are used to initialize the dawg.
* if PAIRS (e.g. '(((a) 1) ((b) 2) ((a a) 11) ((a b) 12)) ) is specified, they are used to initialize the dawg.
* otherwise, an empty dawg is returned.
Both PATH-LABELS and the path-labels in PAIRS must be ordered by their occurrence in the tree, so that entries closer to the root of the tree come before entries further away from the root."
  ;; TODO: add a key option LIST which is used to construct the dawg by passing it to LIST->DAWG.
  (assert (or (and path-labels path-values) pairs))
  (cond
    (path-labels (make-dawg-from-paths path-labels path-values))
    (pairs (make-dawg-from-paths (mapcar #'car pairs) (mapcar #'cadr pairs)))
    ))

;; TODO: add (defun dawg-merge-dawg (dawg1 dawg2)), which merges the two dawgs (on child-label collision, dawg1 is preferred). This should merge '(((a) 1) ((b) 2) ((a a) 11) ((a b) 12)) and '(((a) -1) ((a c) 13)) so that '(((a) 1) ((b) 2) ((a a) 11) ((a b) 12) ((a c) 13)) results (i.e. ((a) -1) is overridden by ((a) 1)).

(defun dawg-follow-parent (dawg function)
  "Follow DAWG's parent, until we reach the root.
At each node, call FUNCTION with the dawg at this node.
The function is first called with DAWG's parent, i.e. if DAWG is the root, the function is not called at all.
Returns the root."
  (let ((parent (node-parent dawg)))
    (if (null parent)
	dawg
	(progn
	  (funcall function parent)
	  (dawg-follow-parent parent function)))))

;;;; Test functions:
(defun symbol-label-lessp (a b)
  "Returns T, if the label of node A is lexicographically less than the label of node B, NIL otherwise."
  (let* ((sa (string (dawg-label a)))
	 (sb (string (dawg-label b)))
	 (r (string-lessp sa sb)))
    r))

(defun dawg-test ()
  (flet ((assert-equal (dawg list-representation)
	   (let ((actual-list (dawg->list dawg
					  :sort-children-predicate
					  #'symbol-label-lessp)))
	     (assert (equal actual-list list-representation)))))
    (let ((dawg (make-dawg-root)))
      (assert-equal dawg '(nil nil nil))
      (setf (dawg-label dawg) :root)
      (setf (dawg-value dawg) :bla)
      ;; this tests DAWG->LIST, and DAWG-LABEL, DAWG-VALUE, DAWG-REDUCE in the process.
      (assert-equal dawg '(:root :bla nil))
      ;; this tests DAWG-ADD/UPDATE-PATH, and DAWG-FOLLOW-PATH in the process.
      (dawg-add/update-path dawg '(a a/a a/a/a) '(1 11 111))
      (assert-equal dawg '(:root :bla ((a 1 ((a/a 11 ((a/a/a 111 nil))))))))
      (dawg-add/update-path dawg '(a a/c) '(-1 -13))
      (dawg-add/update-path dawg '(a a/b) '(-1 -12)) 
      (assert-equal dawg '(:root :bla ((a -1 ((a/a 11 ((a/a/a 111 nil))) (a/b -12 nil) (a/c -13 nil))))))
      (let ((dawg-a/a/a (dawg-follow-path dawg '(a a/a a/a/a)))
	    (visited nil))
	(dawg-follow-parent dawg-a/a/a (lambda (dawg) (push (dawg-label dawg) visited)))
	(assert (equal visited '(:root a a/a)))))
    (let ((dawg-paths (dawg :path-labels '((a) (b) (a a) (a a) (a b)) :path-values '(1 2 11 -11 12)))
	  (dawg-pairs (dawg :pairs '(((a) 1) ((b) 2) ((a a) 11) ((a b) 12) ((a a) -11))))
	  (correct '(NIL NIL ((A 1 ((A -11 NIL) (B 12 NIL))) (B 2 NIL)))))
      (assert-equal dawg-paths correct)
      (assert-equal dawg-pairs correct))))

(dawg-test)

;;;; Different sub-tree storage implementation types
;; All of the following should be possible:
;; * Store the sub-trees as a hash-table
;; * Store the sub-trees as an alist.
;; * Store the sub-trees in an array, where each index corresponds to a predefined atom
