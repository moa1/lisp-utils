;;(load "~/quicklisp/setup.lisp")
;;(ql:quickload :alexandria)
;;(use-package :alexandria)

;;;; This package is like package dlist, but implements the structure dlist a bit differently. For example, when wanting to store (dlist 1 2 3), instead of having the first dcons like this (make-dcons :prev nil :data 1 :next DCONS-2), the first dcons is DCONS-START == (make-dcons :prev nil :data nil :next DCONS-1) with DCONS-1 == (make-dcons :prev DCONS-START :data 1 :next DCONS-2). Likewise, the end of the dcons is capped by an "empty" dcons as well. Finally, (dlist-first D) returns DCONS-START instead of DCONS-1, and (dlist-last D) returns DCONS-END instead of DCONS-3. This has the advantage that an empty dlist does not need to be NIL. Therefore the user can always be sure about the type of the object at hand. It also simplifies code that is using dlist, since it doesn't have to check the special case of NIL before recursing on a dcons. Finally, some programs become faster (for example pushing onto and popping off a dlist), some slower (programs that create a lot of small dlists).

(defpackage :dlist2
  (:use :common-lisp :alexandria)
  (:export :dcons
	   :dconsp
	   :prev
	   :next
	   :data
	   :dlist
	   :dlistp
	   :dcons-list
	   :dcons-prepend
	   :dcons-append
	   :dcons-insert-between
	   :dcons-delete-return-prev
	   :dcons-delete-return-next
	   :dlist-push
	   :dlist-pop
	   :dlist-first
	   :dlist-last
	   :dodcons
	   :dodlist
	   :dodcons-between
	   :copy-dlist
	   ;; :dlist is defined above already
	   :dlist->list
	   :list->dlist
	   :dlist-nconc
	   :dlist-append
	   :dlist-length
	   :nthdcons
	   :dlist-nth
	   :dlist-nreverse
	   :dlist-reverse
	   :dlist-equal
	   :dlist=
	   :make-dlist
	   :mapdcons
	   :mapdcon
	   :mapdlist))

(in-package :dlist2)


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

(define-compiler-macro dconsp (object)
  `(dcons-p ,object))

;; For compatibility with package dlist.
;; TODO: Do I need DECLARE, DECLAIM, or PROCLAIM here if I want PREV to be inlined everywhere?
(declaim (inline prev))
(defun prev (dcons)
  (dcons-prev dcons))

(define-compiler-macro prev (dcons)
  `(dcons-prev ,dcons))

(declaim (inline (setf prev)))
(defun (setf prev) (val dcons)
  (setf (dcons-prev dcons) val))

(define-compiler-macro (setf prev) (val dcons)
  `(setf (dcons-prev ,dcons) ,val))

(declaim (inline next))
(defun next (dcons)
  (dcons-next dcons))

(define-compiler-macro next (dcons)
  `(dcons-next ,dcons))

(declaim (inline (setf next)))
(defun (setf next) (val dcons)
  (setf (dcons-next dcons) val))

(define-compiler-macro (setf next) (val dcons)
  `(setf (dcons-next ,dcons) ,val))

(declaim (inline data))
(defun data (dcons)
  (dcons-data dcons))

(define-compiler-macro data (dcons)
  `(dcons-data ,dcons))

(declaim (inline (setf data)))
(defun (setf data) (val dcons)
  (setf (dcons-data dcons) val))

(define-compiler-macro (setf data) (val dcons)
  `(setf (dcons-data ,dcons) ,val))

(defun print-dcons (dcons stream depth)
  "Print a doubly linked list.
Doesn't yet print whether DCONS has any circularities in it (but detects them already)."
  (declare (ignore depth))
  ;; TODO: rewrite this function to not print the initial and ending NIL.
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

(declaim (inline dlistp))
(defun dlistp (object)
  "Returns whether OBJECT is a dlist."
  (typep object 'dlist))

(define-compiler-macro dlistp (object)
  `(typep ,object 'dlist))

(defmethod print-object ((l dlist) stream)
  (print-unreadable-object (l stream :type nil :identity nil)
    (format stream "DLIST")
    (let ((first (dlist-first l))
	  (last (dlist-last l))
	  (visited nil))
      ;; FIXME: only detect loops if *PRINT-CIRCLE* is true.
      ;; FIXME: print where the loop occurs (using "#1=OBJ ...more-objs... #1#" syntax).
      (do ((cur (next first) (next cur))) ((or (eq cur last) (null cur) (find cur visited)))
	(push cur visited)
	(format stream " ~A" (data cur))))))

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
    (with-gensyms (p)
      `(if ,from-end
	   (let ((,p (prev (dlist-last ,dlist))))
	     (prog1 (data ,p) (dcons-delete-return-next ,p)))
	   (let ((,p (next (dlist-first ,dlist))))
	     (prog1 (data ,p) (dcons-delete-return-prev ,p)))))))

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

(defmacro dodcons-between ((var first-dcons last-dcons &key result-form from-end) &body body)
  "Iterates over the dconses delimited by FIRST-DCONS and LAST-DCONS, both excluded."
  (let ((do-from-last (once-only (first-dcons last-dcons)
			`(do ((,var (prev ,last-dcons) (prev ,var)))
			     ((eq ,var ,first-dcons) ,result-form)
			   ,@body)))
	(do-from-first (once-only (first-dcons last-dcons)
			 `(do ((,var (next ,first-dcons) (next ,var)))
			      ((eq ,var ,last-dcons) ,result-form)
			    ,@body))))
      (case from-end
	((t) do-from-last)
	((nil) do-from-first)
	(t (once-only (from-end)
	     `(if ,from-end ,do-from-last ,do-from-first))))))

(defun copy-dlist (dlist &key deep-copy from-end)
  (let* ((first (dcons-list))
	 (cur first)
	 (last (dcons-next first)))
    (dodcons-between (dcons (dlist-first dlist) (dlist-last dlist) :from-end from-end)
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
  ;; TODO: specialize on DEEP == NIL and DEEP != NIL.
  (let ((l nil))
    (dodcons-between (dcons (dlist-first dlist) (dlist-last dlist) :from-end t)
      (let ((data (dcons-data dcons)))
	(push (if (and deep (or (dlistp data) (dconsp data)))
		  (dlist->list data :deep t)
		  data)
	      l)))
    l))

(defun list->dlist (l)
  "Deeply convert list L to a dlist."
  ;; Note: this hangs on converting circular lists:
  ;; (let ((*PRINT-CIRCLE* t))
  ;;   (let ((l (list 1 2 3)))
  ;;     (setf (cdr (last l)) l)
  ;;     (print l)
  ;;     (list->dlist l)))
  (declare (type list l))
  (let ((content (loop for e in l collect
		      (if (listp e)
			  (list->dlist e)
			  e))))
    (apply #'dlist content)))

(defun dlist-nconc (&rest dlists)
  "Append DLISTS by modifying their first(last) dconses to point to the last(first) dcons of the previous(next) dlist.
If all DLISTS are dlists, runtime is proportional to the number of DLISTS. If some DLISTS are dconses, runtime will be proportional to the number of elements in the dconses plus the number of dlists in DLISTS."
  (if (null dlists)
      (dlist)
      (labels ((rec (dlist-1 dlists)
		 (if (null dlists)
		     dlist-1
		     (let ((dlist-2 (car dlists)))
		       ;;(print (dlist-last dlist-2))
		       (setf (next (prev (dlist-last dlist-1))) (next (dlist-first dlist-2)))
		       (setf (prev (next (dlist-first dlist-2))) (prev (dlist-last dlist-1)))
		       (setf (dlist-last dlist-1) (dlist-last dlist-2)) ;this takes O(n) if dlist-2 is a dcons.
		       ;;(print dlist-1)
		       (rec dlist-1 (cdr dlists))))))
	(let ((dlist-1 (car dlists)))
	  (when (dconsp dlist-1)
	    (setf dlist-1 (make-instance 'dlist :first (dlist-first dlist-1) :last (dlist-last dlist-1))))
	  (rec dlist-1 (cdr dlists))))))

(let* ((d1 (dcons-list 1 2 (dcons-list 3 4 5) 6))
       (l1 (make-instance 'dlist :first (next d1) :last (prev (dlist-last d1))))
       (l2 (dlist 7 8 9))
       (d3 (dcons-list 10 11)))
  (assert (equal (dlist->list (dlist-nconc d1 l2 d3) :deep t) '(1 2 (3 4 5) 6 7 8 9 10 11)))
  (assert (equal (dlist->list (dlist-nconc l1 l2 d3) :deep t) '(2 (3 4 5) 7 8 9 10 11))))

(defun dlist-append (&rest dlists)
  "Like dlist-nconc, but shallow-copies DLISTS before appending."
  (let ((dlists (loop for dlist in dlists collect (copy-dlist dlist :deep-copy nil))))
    (apply #'dlist-nconc dlists)))

(let* ((d1 (dcons-list 1 2 (dcons-list 3 4 5) 6))
       (l1 (make-instance 'dlist :first (next d1) :last (prev (dlist-last d1))))
       (l2 (dlist 7 8 9))
       (d3 (dcons-list 10 11)))
  (assert (equal (dlist->list (dlist-append d1 l2 d3) :deep t) '(1 2 (3 4 5) 6 7 8 9 10 11)))
  (assert (equal (dlist->list (dlist-append l1 l2 d3) :deep t) '(2 (3 4 5) 7 8 9 10 11))))

(defun dlist-length (dlist)
  (let ((i 0))
    (dodcons-between (var (dlist-first dlist) (dlist-last dlist))
      (incf i))
    i))

(let* ((d1 (dcons-list 1 2 (dcons-list 3 4 5) 6))
       (l1 (make-instance 'dlist :first (next d1) :last (prev (dlist-last d1))))
       (l2 (dlist 7 8 9)))
  (assert (eq (dlist-length d1) 4))
  (assert (eq (dlist-length l1) 2))
  (assert (eq (dlist-length l2) 3)))

(defun nthdcons (n dlist &key from-end)
  "If FROM-END is NIL, return the Nth dcons of DLIST.
If FROM-END is T, return the Nth dcons from the end of DLIST."
  (let ((i 0))
    (dodcons-between (dcons (dlist-first dlist) (dlist-last dlist) :from-end from-end)
      (when (= i n)
	(return-from nthdcons (values dcons t)))
      (incf i)))
  (values nil nil))

(defun dlist-nth (n dlist &key from-end)
  "If FROM-END is NIL, return the Nth element of DLIST.
If FROM-END is T, return the Nth element from the end of DLIST."
  (multiple-value-bind (dcons inside-dlist-p)
      (nthdcons n dlist :from-end from-end)
    (if inside-dlist-p
	(values (data dcons) t)
	(values nil nil))))

(let* ((d1 (dcons-list 1 2 (dcons-list 2.3 2.6) 3))
       (l1 (make-instance 'dlist :first (next d1) :last (prev (dlist-last d1)))))
  (assert (equal (dlist-nth 0 d1) 1))
  (assert (equal (dlist-nth 1 d1) 2))
  (assert (equal (dlist-nth 0 d1 :from-end t) 3))
  (assert (equal (dlist-nth 0 l1) 2))
  (assert (equal (dlist-nth 1 l1 :from-end t) 2)))

(defun (setf dlist-nth) (val n dlist &key from-end)
  (multiple-value-bind (dcons inside-dlist-p)
      (nthdcons n dlist :from-end from-end)
    (declare (ignore inside-dlist-p))
    (setf (data dcons) val) ;signals an error if dcons is no dcons (i.e. if it was outside the range)
    ))

(defun dlist-nreverse (dlist)
  "Reverse dlist destructively."
  (let ((front (dlist-first dlist))
	(back (dlist-last dlist)))
    (tagbody
       start
       (when (eq front back)
	 (return-from dlist-nreverse dlist))
       (setf front (next front))
       (when (eq front back)
	 (return-from dlist-nreverse dlist))
       (setf back (prev back))
       (psetf (data front) (data back)
	      (data back) (data front))
       (go start))))

(let* ((d1 (dcons-list 1 2 3 4))
       (l1 (make-instance 'dlist :first (next d1) :last (prev (dlist-last d1))))
       (d2 (dlist 1 2 3)))
  (assert (equal (dlist->list (dlist-nreverse d1)) '(4 3 2 1)))
  (dlist-nreverse l1)
  (assert (equal (dlist->list d1) '(4 2 3 1)))
  (assert (equal (dlist->list (dlist-nreverse d2)) '(3 2 1))))

(defun dlist-reverse (dlist)
  "Reverse dlist non-destructively."
  (dlist-nreverse (copy-dlist dlist)))

(let* ((d1 (dcons-list 1 2 3 4))
       (l1 (make-instance 'dlist :first (next d1) :last (prev (dlist-last d1))))
       (d2 (dlist 1 2 3)))
  (assert (equal (dlist->list (dlist-reverse d1)) '(4 3 2 1)))
  (assert (equal (dlist->list d1) '(1 2 3 4)))
  (dlist-reverse l1)
  (assert (equal (dlist->list d1) '(1 2 3 4)))
  (assert (equal (dlist->list (dlist-reverse d2)) '(3 2 1))))

(defun dlist-equal (dlist1 dlist2 &key (test #'equal))
  "Recursively compares DLIST1 and DLIST2 and returns T if they are equal, and NIL if not."
  (declare (type (or dlist dcons) dlist1 dlist2))
  (flet ((compare (dlist1 dlist2 dlist1-dlist-p dlist2-dlist-p)
	   (do ((dcons1 (dlist-first dlist1) (next dcons1))
		(dcons2 (dlist-first dlist2) (next dcons2)))
	       ((or (if dlist1-dlist-p (eq dcons1 (dlist-last dlist1)) (null (next dcons1)))
		    (if dlist2-dlist-p (eq dcons2 (dlist-last dlist2)) (null (next dcons2))))
		(and (null (next dcons1)) (null (next dcons2)))) ;both at end?

	     (let ((data1 (data dcons1)) (data2 (data dcons2)))
	       (if (and (or (dconsp data1) (dlistp data1))
			(or (dconsp data2) (dlistp data2)))
		   (when (not (dlist-equal data1 data2))
		     (return-from dlist-equal nil))
		   (when (not (funcall test data1 data2))
		     (return-from dlist-equal nil)))))))
    ;; When compiling this function, SBCL gives warnings, but only when the following inline-declaration is present.
    (declare (inline compare))
    (if (dconsp dlist1)
	(if (dconsp dlist2)
	    (compare dlist1 dlist2 nil nil)
	    (compare dlist1 dlist2 nil t))
	(if (dconsp dlist2)
	    (compare dlist1 dlist2 t nil)
	    (compare dlist1 dlist2 t t)))))
	    
(defun dlist= (&rest dlists)
  "Compares all MORE-DLISTS, recursively descending into sub-lists, and returns T if they are all equal, and NIL if not."
  (if (null dlists)
      t
      (let ((dlist1 (car dlists)))
	(loop for dlist in (cdr dlists) always
	     (dlist-equal dlist1 dlist)))))

(defun make-dlist (size &key initial-element)
  (let ((d (dlist)))
    (loop for i below size do (dlist-push initial-element d))
    d))

(defun mapdcons (function dlist &rest more-dlists-and-from-end)
  "Creates a new dlist by applying FUNCTION to the 1st dconses of DLIST and MORE-DLISTS-AND-FROM-END, then the 2nd dconses etc..
Returns the newly created dlist.
If MORE-DLISTS-AND-FROM-END contains the keyword :FROM-END, the next element in MORE-DLISTS-AND-FROM-END is taken as the direction indicator, and :FROM-END and the direction indicator are removed from MORE-DLISTS-AND-FROM-END before creating the new dlist. If the direction indicator is non-NIL, DLIST and MORE-DLISTS-AND-FROM-END are iterated over from the end."
  ;;(print (list "mapdcons dlist" dlist "more-dlists-and-from-end" more-dlists-and-from-end))
  (labels ((rec (args from-end dlists)
	     (if (null args)
		 (values from-end (nreverse dlists))
		 (let ((a (car args)))
		   (if (eq a :from-end)
		       (rec (cddr args) (cadr args) dlists)
		       (rec (cdr args) from-end (cons a dlists)))))))
    (multiple-value-bind (from-end dlists)
	(rec more-dlists-and-from-end nil (list dlist))
      ;;(print (list "mapdcons from-end" from-end "dlists" dlists))
      (if (null (cdr dlists))
	  (let ((result (dlist)))
	    (dodcons (var (car dlists) result from-end)
	      (dlist-push (funcall function var) result :at-end t)))
	  (let ((result (dlist))
		(pointers (mapcar (if from-end #'dlist-last #'dlist-first)
				  dlists))
		(ends (mapcar (if from-end #'dlist-first #'dlist-last) dlists)))
	    (loop do
		 (setf pointers (mapcar (if from-end #'dcons-prev #'dcons-next)
					pointers))
		 (loop for p in pointers for e in ends do
		      (when (eq p e)
			(return-from mapdcons result)))
		 (dlist-push (apply function pointers) result :at-end t)))))))

(defun mapdcon (function dlist &rest more-dlists-and-from-end)
  "Like MAPDCONS, but returns DLIST."
  ;; TODO: implement without using mapdcons, because it conses up the result, which is the thrown away (when returning dlist). 
  (apply #'mapdcons function dlist more-dlists-and-from-end)
  dlist)

(defun mapdlist (function dlist &rest more-dlists-and-from-end)
  "Like MAPDCONS, but applies function to the DATA of DLIST and MORE-DLISTS-AND-FROM-END."
  ;; TODO: implement without using a (slow) lambda.
  (apply #'mapdcons
	 (lambda (&rest rest) (apply function (mapcar #'dcons-data rest)))
	 dlist
	 more-dlists-and-from-end))

(let ((d1 (dlist 1 2 3))
      (d2 (dlist 3 4)))
  (assert (equal (dlist->list (mapdcons (lambda (a b) (+ (data a) (data b))) d1 d2))
		 '(4 6)))
  (assert (equal (dlist->list (mapdlist #'+ d1 d2 :from-end t))
		 '(7 5)))
  (assert (equal (dlist->list (mapdcon (lambda (a b) (setf (data a) (+ (data a) (data b)))) d1 :from-end 1 d2))
		 '(1 5 7))))
