(load "~/quicklisp/setup.lisp")
(ql:quickload :alexandria)
(use-package :alexandria)

;;;; This package is like package dlist, but implements the structure dlist a bit differently. For example, when wanting to store (dlist 1 2 3), instead of having the first dcons like this (make-dcons :prev nil :data 1 :next DCONS-2), the first dcons is DCONS-START == (make-dcons :prev nil :data nil :next DCONS-1) with DCONS-1 == (make-dcons :prev DCONS-START :data 1 :next DCONS-2). Likewise, the end of the dcons is capped by an "empty" dcons as well. Finally, (dlist-first D) returns DCONS-START instead of DCONS-1, and (dlist-last D) returns DCONS-END instead of DCONS-3. This has the advantage that an empty dlist does not need to be NIL. Therefore the user can always be sure about the type of the object at hand. It also simplifies code that is using dlist, since it doesn't have to check the special case of NIL before recursing on a dcons.

(defstruct dcons
  "An element of a doubly linked list, with data DATA, previous element PREV, and next element NEXT."
  (prev nil :type (or null dcons))
  (data nil :type t)
  (next nil :type (or null dcons)))

;; TODO: instead of (declaim (inline XY)), write compiler-macros for the following functions which are callable from outside the package.
(declaim (inline dconsp))
(defun dconsp (object)
  "Return whether OBJECT is a dcons."
  (dcons-p object))

;; For compatibility with package dlist.
;; TODO: Do I need DECLARE, DECLAIM, or PROCLAIM here if I want PREV to be inlined everywhere?
(declaim (inline prev))
(defun prev (dcons)
  (dcons-prev dcons))

(declaim (inline (setf prev)))
(defun (setf prev) (val dcons)
  (setf (dcons-prev dcons) val))

(declaim (inline next))
(defun next (dcons)
  (dcons-next dcons))

(declaim (inline (setf next)))
(defun (setf next) (val dcons)
  (setf (dcons-next dcons) val))

(declaim (inline next))
(defun data (dcons)
  (dcons-data dcons))

(declaim (inline (setf data)))
(defun (setf data) (val dcons)
  (setf (dcons-data dcons) val))

(defun print-dcons (dcons stream depth)
  "Print a doubly linked list.
Doesn't yet print whether DCONS has any circularities in it (but detects them already)."
  (declare (ignore depth))
  ;; FIXME: rewrite this function to print a reader-readable format (or define a reader function or whatever is supposed work).
  (let ((visited nil))
    ;; FIXME: only detect loops if *PRINT-CIRCLE* is true.
    ;; FIXME: print where the loop occurs (using "#1=OBJ ...more-objs... #1#" syntax).
    (format stream " O:~A" (data dcons))
    (push dcons visited)
    (do ((cur (next dcons) (next cur))) ((or (null cur) (find cur visited)))
      (push cur visited)
      (format stream " ~A" (data cur)))
    (do ((cur (prev dcons) (prev cur))) ((or (null cur) (find cur visited)))
      (push cur visited)
      (format stream " B:~A" (data cur)))))

(defmethod print-object ((l dcons) stream)
  (print-unreadable-object (l stream :type nil :identity nil)
    (format stream "DCONS")
    (print-dcons l stream 0)))

;; TODO: prepend 'sequence to class precedence list
(defclass dlist (standard-object)
  ((first :initarg :first :accessor dlist-first)
   (last :initarg :last :accessor dlist-last))
  (:documentation "A doubly-linked list, defined by the elements between FIRST and LAST."))

(defun dlistp (object)
  "Returns whether OBJECT is a dlist."
  (typep object 'dlist))

(defmethod print-object ((l dlist) stream)
  (print-unreadable-object (l stream :type nil :identity nil)
    (format stream "DLIST")
    (print-dcons (dlist-first l) stream 0)))

(defun dcons-list (&rest args)
  (let* ((first (make-dcons :prev nil :data nil :next nil))
	 (cur first))
    (loop for arg in args do
	 (let ((new (make-dcons :prev cur :data arg :next nil)))
	   (setf (next cur) new)
	   (setf cur new)))
    (let ((last (make-dcons :prev cur :data nil :next nil)))
      (setf (next cur) last)
      (values first last))))

;; TODO: instead of (declaim (inline XY)), write compiler-macros for the following functions which are callable from outside the package.
(declaim (inline dcons-prepend))
(defun dcons-prepend (data dcons)
  "Replace PREV of DCONS with a newly-created dcons with data DATA and next DCONS."
  (let ((new (make-dcons :prev nil :data data :next dcons)))
    (setf (dcons-prev dcons) new)
    new))

(declaim (inline dcons-append))
(defun dcons-append (data dcons)
  "Replace NEXT of DCONS with a newly-created dcons with data DATA and prev DCONS."
  (let ((new (make-dcons :prev dcons :data data :next nil)))
    (setf (dcons-next dcons) new)
    new))

(declaim (inline dcons-insert-between))
(defun dcons-insert-between (prev data next)
  "Connect the dconses PREV, a newly created DCONS, and NEXT, in that order. Return the newly created DCONS."
  (let ((new (make-dcons :prev prev :data data :next next)))
    (setf (dcons-next prev) new)
    (setf (dcons-prev next) new)
    new))

(declaim (inline dcons-delete-return-prev))
(defun dcons-delete-return-prev (dcons)
  "Remove DCONS from the doubly linked list and return (dcons-prev DCONS)."
  (let ((prev (dcons-prev dcons))
	(next (dcons-next dcons)))
    (setf (dcons-next prev) next)
    (setf (dcons-prev next) prev)))

(declaim (inline dcons-delete-return-next))
(defun dcons-delete-return-next (dcons)
  "Remove DCONS from the doubly linked list and return (dcons-next DCONS)."
  (let ((prev (dcons-prev dcons))
	(next (dcons-next dcons)))
    (setf (dcons-prev next) prev)
    (setf (dcons-next prev) next)))

(defmacro dlist-push (data dlist &key at-end)
  "If AT-END evaluates to NIL, replaces pushes OBJ onto the beginning of DLIST.
Otherwise, pushes OBJ onto the end of DLIST.
Returns the newly created DCONS."
  (once-only (data dlist at-end)
    (with-gensyms (end cap)
      `(if ,at-end
	   (let* ((,cap (dlist-last ,dlist))
		  (,end (dcons-prev ,cap)))
	     (dcons-insert-between ,end ,data ,cap))
	   (let* ((,cap (dlist-first ,dlist))
		  (,end (dcons-next ,cap)))
	     (dcons-insert-between ,cap ,data ,end))))))

(defmacro dlist-pop (dlist &key from-end)
  "If FROM-END is NIL, delete the first element of DLIST and return the new first element.
Otherwise, delete the last element of DLIST and return the new last element."
  (once-only (dlist from-end)
    `(if ,from-end
	 (dcons-delete-return-next (prev (dlist-last ,dlist)))
	 (dcons-delete-return-prev (next (dlist-first ,dlist))))))

;; DLIST-FIRST and DLIST-LAST for DLISTs are already defined by defclass.

(defmethod dlist-first ((dlist dcons))
  (do ((cur dlist (prev cur))) ((null (prev cur)) cur)))

(defmethod dlist-last ((dlist dcons))
  (do ((cur dlist (next cur))) ((null (next cur)) cur)))

(defmacro dodcons ((var dlist &optional result-form from-end) &body body)
  "Iterate over the dconses between the 'prev'-most dcons of DLIST and the 'next'-most dcons of DLIST, and in each iteration, bind VAR to the current dcons and then evaluate BODY.
If FROM-END is non-NIL, instead iterate over the dconses in reverse."
  (once-only (dlist from-end)
    `(if ,from-end
	 (do ((,var (prev (dlist-last ,dlist)) (prev ,var)))
	     ((null (prev ,var)) ,result-form)
	   ,@body)
	 (do ((,var (next (dlist-first ,dlist)) (next ,var)))
	     ((null (next ,var)) ,result-form)
	   ,@body))))

(defmacro dodlist ((var dlist &optional result-form from-end) &body body)
  "Iterate over the dconses between the 'prev'-most dcons of DLIST and the 'next'-most dcons of DLIST, and in each iteration, bind VAR to the data of the current dcons and then evaluate BODY.
If FROM-END is non-NIL, instead iterate over the dconses in reverse.
Note that this is like dodcons, but instead of binding VAR to the current dcons it binds to the data of the current dcons."
  (with-gensyms (dcons-var)
    `(dodcons (,dcons-var ,dlist ,result-form ,from-end)
       (let ((,var (dcons-data ,dcons-var)))
	 ,@body))))

(defmacro dodcons-between ((var first-dcons last-dcons &optional result-form from-end) &body body)
  "Iterates over the dconses delimited by FIRST-DCONS and LAST-DCONS, both excluded."
  (once-only (first-dcons last-dcons from-end)
    `(if ,from-end
	 (do ((,var (prev ,last-dcons) (prev ,var)))
	     ((eq ,var ,first-dcons) ,result-form)
	   ,@body)
	 (do ((,var (next ,first-dcons) (next ,var)))
	     ((eq ,var ,last-dcons) ,result-form)
	   ,@body))))

(defun copy-dlist (dlist &key deep-copy from-end)
  (let* ((first (dcons-list))
	 (cur first)
	 (last (dcons-next first)))
    (dodcons (dcons dlist nil from-end)
      (let* ((data (dcons-data dcons))
	     (new (if (and deep-copy (or (dlistp data) (dconsp data)))
		      (copy-dlist data)
		      data)))
	(setf cur (dcons-insert-between cur new last))))
    (etypecase dlist
      (dcons first)
      (dlist (make-instance 'dlist :first first :last last)))))

(defun dlist (&rest elements)
  "Create a doubly-linked list consisting of the ELEMENTS."
  (multiple-value-bind (first last) (apply #'dcons-list elements)
    (make-instance 'dlist :first first :last last)))

(defun dlist->list (dlist &key deep)
  "Convert DLIST to a list.
If DEEP is true, dlists inside of DLIST are also converted to a list."
  (let ((l nil))
    (dodlist (var dlist l t)
      (push (if (and deep (or (dlistp var) (dconsp var)))
		(dlist->list var :deep t)
		var)
	    l))))
