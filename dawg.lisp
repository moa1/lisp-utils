;; DAWG (directed acyclic word graph) structure.
;; In this implementation, every node in the tree has a value.

(defpackage :dawg
  (:use :common-lisp)
  (:export :dawg-label
	   :dawg-value
	   :dawg
	   :dawg-add-child
	   :dawg-follow-childs-in-path
	   :dawg-follow-parent-until-root))

(in-package :dawg)

;; the structure

(defstruct node
  (label nil) ; the node's label; can have any type.
  (value nil) ; the node's value; can have any type.
  (parent nil) ;; I would like to add ":type (or nil node)" here, but that doesn't work. nil means that this node represents the root of the dawg.
  (children nil :type hash-table))

;; the following 3 functions must be replaced if CHILDREN (in structure NODE) should not be a hash-table of default type anymore, or not be a hash-table at all anymore.

(defun make-empty-children ()
  (make-hash-table :test 'eq))

(defun add/update-child-in-children (node child-label child-node)
  "add/update CHILD-NODE under the label CHILD-LABEL in NODE's children.
Returns :UPDATE or :ADD, depending on whether CHILD-LABEL was present or not, respectively."
  (let* ((children (node-children node))
	 (operation (if (null (nth-value 1 (gethash child-label children)))
			:add
			:update)))
    (setf (gethash child-label children) child-node)
    operation))

(defun get-child-from-children (node child-label)
  "get the child node stored under label CHILD-LABEL in the children of NODE.
Returns as secondary value whether CHILD-LABEL was found."
  (gethash child-label (node-children node)))

;; DAWG functions, to be exported.

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
  (setf (node-label dawg) value))

;; TODO: make it possible to specify label-paths and values which are used in newly constructing the dawg.
;; like this: (defun dawg &key (label-paths nil) (values nil) (pairs nil)))
;; * if label-paths and values are specified, then they are used to initialize.
;; * if pairs (e.g. '(((a) 1) ((b) 2) ((a a) 11) ((a b) 12)) ) is specified, they are used to initialize.
;; * otherwise, an empty dawg is returned.
(defun dawg ()
  "Return an empty DAWG."
  (make-node :label nil
	     :value nil
	     :parent nil
	     :children (make-empty-children)))

(defun dawg-add-child (dawg child-label child-value)
  "Add a child with label CHILD-LABEL and value CHILD-VALUE under the dawg DAWG."
  (let ((child-node (make-node :label child-label
			       :value child-value
			       :parent dawg
			       :children (make-empty-children))))
    (add/update-child-in-children dawg child-label child-node)))

;; TODO: add (defun dawg-add-dawg (dawg child-label child-dawg)), which adds a whole dawg as a child.

(defun dawg-follow-childs-in-path (dawg path)
  "Return the dawg stored in DAWG under the children path PATH, or NIL if this path was not found."
  (if (null path)
      dawg
      (multiple-value-bind (child-node present-p)
	  (get-child-from-children dawg (car path))
	(if (null present-p)
	    nil
	    (dawg-follow-childs-in-path child-node (cdr path))))))

(defun dawg-follow-parent-until-root (dawg function)
  "Follow DAWG's parent, until we reach the root.
At each node, call FUNCTION with the dawg at this node.
The function is first called with DAWG's parent, i.e. if DAWG is the root, the function is not called at all."
  (let ((parent (node-parent dawg)))
    (when (not (null parent))
      (funcall function dawg)
      (dawg-follow-parent-until-root parent function))))
