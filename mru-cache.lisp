(defpackage mru-cache
  (:documentation "A most-recently-used cache.
Normal usage is to create a MRU cache using #'MAKE, add key-value-pairs using #'(SETF GET), and get elements using #'GET.")
  (:shadow :get :set :count)
  (:use :cl)
  (:export
   ;; lsxhash
   :lsxhash
   :make-lsxhash-equalp-hash-table
   :make-lsxhash-equal-hash-table
   :make-sxhash-equal-hash-table
   :make-sxhash-eq-hash-table
   ;; MRU cache
   :mru-cache ;do not export slots or accessors
   :make
   :count
   :key-present-p
   :get
   :get*
   :set
   :set*
   :key-not-found
   :del
   :clear
   :iterate
   ;; Function cacher
   :make-function-cacher))

(in-package :mru-cache)

;;(load "~/quicklisp/setup.lisp")
;;(ql:quickload :cl-custom-hash-table)
;;(ql:quickload :dlist2)

;;;; Implementation of a better sxhash for lists, which regards all elements of the list, not just the first couple of elements.

;; this is copied from SBCL
;; originally this was "(ftype (sfunction". what's an sfunction? 
(declaim (ftype (function ((and fixnum unsigned-byte)
			   (and fixnum unsigned-byte))
			  (and fixnum unsigned-byte))
                mix))
(declaim (inline mix))
(defun mix (x y)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (and fixnum unsigned-byte) x y))
  (let* ((xy (+ (* x 3) y)))
    (logand most-positive-fixnum
            (logxor 441516657
                    xy
                    (ash xy -5)))))

;; cannot make LSXHASH inline, since it is recursive.
(defun lsxhash (x)
  "Return a hash value for value X.
X, (car X), and (cdr X) may be a list, a symbol, or a number."
  (declare (optimize (speed 3) (safety 0) (compilation-speed 0) (space 0)))
  (declare (values (and fixnum unsigned-byte))) ;inferred automatically (see (DESCRIBE 'LSXHASH))
  ;; in SBCL, etypecase takes 80% of the time of defmethod-ing on the different types of X: (let ((h (make-hash-table))) (timediff (lsxhash h) (mlsxhash h) :showtimes t))
  (etypecase x
    (single-float (sxhash x))
    (double-float (sxhash x))
    (ratio (sxhash x))
    (fixnum (sxhash x)) ;;in SBCL, close numbers seem to have close hashes.
    (string (sxhash x)) ;;in SBCL, (sxhash "ABC") == (sxhash 'abc).
    ;;(number (sxhash x))
    (symbol (sxhash x))
    ;; here, X can't be nil since (symbolp nil) == T.
    ;; FIXME: handle circular lists.
    (list (mix (lsxhash (car x)) (lsxhash (cdr x))))
    ;; FIXME: handle circular hash-tables.
    (hash-table (let ((ret 448291823))
		  (declare (type (and fixnum unsigned-byte) ret))
		  (setf ret (mix (sxhash (hash-table-count x))
				 (mix ret (sxhash (hash-table-test x)))))
		  ;; use logxor for speed and so that the order of key/value pairs does not matter
		  (loop for k being the hash-key of x using (hash-value v) do
		       (setf ret (logxor ret (mix (lsxhash k) (lsxhash v)))))
		  ret))
    ;; FIXME: handle circular arrays.
    (simple-array (let* ((size (array-total-size x))
			 (dim (array-dimensions x))
			 (type (array-element-type x))
			 (ret 518591303))
		    (declare (type (and fixnum unsigned-byte) ret))
		    (setf ret (mix (mix ret (sxhash type))
				   (lsxhash dim)))
		    (ecase type
		      ((fixnum)
		       (loop for i below size do
			    (let ((e (row-major-aref x i)))
			      (declare (type fixnum e))
			      (setf ret (mix ret (sxhash e))))))
		      ((t)
		       (loop for i below size do
			    (let ((e (row-major-aref x i)))
			      (setf ret (mix ret (lsxhash e))))))
		      )
		    ret))))

;; for caching mutable objects (including hash-tables) supported by LSXHASH.
(cl-custom-hash-table:define-custom-hash-table-constructor make-lsxhash-equalp-hash-table
    :test equalp :hash-function lsxhash)

;; for caching mutable objects supported by LSXHASH.
(cl-custom-hash-table:define-custom-hash-table-constructor make-lsxhash-equal-hash-table
    :test equal :hash-function lsxhash)

(cl-custom-hash-table:define-custom-hash-table-constructor make-sxhash-equal-hash-table
    :test equal :hash-function sxhash)

;; for quickly caching immutable objects.
(cl-custom-hash-table:define-custom-hash-table-constructor make-sxhash-eq-hash-table
    :test eq :hash-function sxhash)

;;;; A MRU (most recently used) cache.

(defclass mru-cache ()
  ((slots :initarg :slots :accessor mru-cache-slots :type (and fixnum unsigned-byte))
   (ht :initarg :ht :accessor mru-cache-ht :type hash-table)
   (dlist :initarg :dlist :accessor mru-cache-dlist :type dlist2))
  (:documentation "A most-recently-used-cache class."))

(defun make (slots &key (make-hash-table-fn #'make-lsxhash-equal-hash-table))
  "Make a most-recently-used cache, being able to cache up to SLOTS items."
  (declare (type unsigned-byte slots))
  (let* ((ht (funcall make-hash-table-fn :size slots :rehash-threshold 1.0))
	 (dlist (dlist2:dlist))
	 (c (make-instance 'mru-cache
			   :slots slots
			   :ht ht
			   :dlist dlist)))
    c))

(defun count (mru)
  "Return the number of key-value pairs stored in most-recently-used cache MRU."
  (hash-table-count (mru-cache-ht mru)))

(defun key-present-p (mru key)
  "Return whether the key KEY is present in the most-recently-used-cache MRU.
This does not modify the order of the key-value-pairs stored in MRU."
  (declare (type mru-cache mru))
  (cl-custom-hash-table:with-custom-hash-table
    (nth-value 1 (gethash key (mru-cache-ht mru)))))

(declaim (inline mru-dlist-hit-mru-dcons))
(defun mru-dlist-hit-mru-dcons (dlist mru-dcons)
  (declare (type dlist2:dlist dlist)
	   (type dlist2:dcons mru-dcons))
  "Bring the dcons MRU-DCONS to the front of the dlist DLIST."
  ;;(print "mru hit") (describe-object dlist t)
  ;; slice out mru-dcons from its current position.
  (dlist2:dcons-delete mru-dcons)
  ;; bring mru-dcons to the beginning of the dlist.
  (let ((first (dlist2:dlist-first dlist)))
    ;; need to re-use existing dcons mru-dcons, because the hash-table points to it.
    (dlist2:dcons-existing-insert-between first mru-dcons (dlist2:next first)))
  ;; updating the hash-table is not necessary, because its key still points to MRU-DCONS.
  )

(defun get (mru key &optional (default nil))
  "If the key KEY is present in mru-cache MRU, return the values for key KEY and as secondary value T, and bring KEY to the front of MRU.
If KEY is not present return the value DEFAULT and NIL as secondary value."
  (let ((ht (mru-cache-ht mru))
	(dlist (mru-cache-dlist mru)))
    (cl-custom-hash-table:with-custom-hash-table
      (multiple-value-bind (mru-dcons p) (gethash key ht)
	(if p
	    (progn
	      ;; an entry is present, therefore bring it to front in dlist.
	      (mru-dlist-hit-mru-dcons dlist mru-dcons)
	      ;;(describe-object dlist t)
	      (values (cdr (dlist2:data mru-dcons)) t))
	    (values default nil))))))

(defun get* (mru key &optional (default nil))
  "If the key KEY is present in mru-cache MRU, return the values for key KEY and as secondary value T, but do not bring KEY to the front of MRU.
If KEY is not present return the value DEFAULT and NIL as secondary value.
This function is like #'GET, but does not bring KEY to the front of the list."
  (let ((ht (mru-cache-ht mru)))
    (cl-custom-hash-table:with-custom-hash-table
      (multiple-value-bind (mru-dcons p) (gethash key ht)
	(if p
	    (values (cdr (dlist2:data mru-dcons)) t)
	    (values default nil))))))

(declaim (inline mru-add-entry-to-front))
(defun mru-add-entry-to-front (ht dlist slots key value)
  ;; push new cache entry to front of dlist.
  (let (front-dcons)
    (if (>= (hash-table-count ht) slots)
	(progn
	  ;; the cache is already full. Remove the last element, and re-use its dcons for the new front element.
	  (destructuring-bind (old-key . old-value) (dlist2:data (dlist2:prev (dlist2:dlist-last dlist)))
	    (declare (ignorable old-value))
	    ;;(print "mru full") (describe-object dlist t)
	    (setf front-dcons (dlist2:dlist-pop-dcons dlist :from-end t))
	    (remhash old-key ht))
	  (setf (dlist2:data front-dcons) (cons key value))
	  (let ((first (dlist2:dlist-first dlist)))
	    ;; need to re-use existing dcons mru-dcons, because the hash-table points to it.
	    (dlist2:dcons-existing-insert-between first front-dcons (dlist2:next first)))
	  ;;(describe-object dlist t)
	  )
	(setf front-dcons (dlist2:dlist-push-return-new-dcons (cons key value) dlist)))
    ;; insert front-dcons into ht
    (setf (gethash key ht) front-dcons)
    ;;(print (list "hash-table-count" (hash-table-count ht) slots))
    ))

(declaim (inline set))
(defun set (mru key value)
  "Set the value VALUE for the key KEY in mru-cache MRU.
Bring the KEY-VALUE-pair to the front of the mru-cache.
Remove the least-recently-used entry if the MRU had already been full."
  (declare (type mru-cache mru))
  (let* ((ht (mru-cache-ht mru))
	 (dlist (mru-cache-dlist mru)))
    (cl-custom-hash-table:with-custom-hash-table
      (multiple-value-bind (mru-dcons p) (gethash key ht)
	(if p
	    (progn
	      (mru-dlist-hit-mru-dcons dlist mru-dcons)
	      (setf (cdr (dlist2:data mru-dcons)) value))
	    (progn
	      (mru-add-entry-to-front ht dlist (mru-cache-slots mru) key value)
	      value))))))
(declaim (notinline set))

(defun (setf get) (value mru key)
  "Set the value VALUE for the key KEY in mru-cache MRU.
Bring the KEY-VALUE-pair to the front of the mru-cache.
Remove the least-recently-used entry if the MRU had already been full."
  (declare (inline set))
  (set mru key value))

(declaim (inline set*))
(defun set* (mru key value)
  "Set the value VALUE for the key KEY in mru-cache MRU.
If KEY had been present, set the new VALUE, but do not bring the KEY-VALUE-pair to the front of the mru-cache.
If KEY had not been present, add it to the front of the mru-cache, and remove the least-recently-used entry if the MRU had already been full.
This function is like #'SET, but does not change the relative order of already present entries."
  (declare (type mru-cache mru))
  (let* ((ht (mru-cache-ht mru)))
    (cl-custom-hash-table:with-custom-hash-table
      (multiple-value-bind (mru-dcons p) (gethash key ht)
	(if p
	    (setf (cdr (dlist2:data mru-dcons)) value)
	    (progn
	      (mru-add-entry-to-front ht (mru-cache-dlist mru) (mru-cache-slots mru) key value)
	      value))))))
(declaim (notinline set*))

(defun (setf get*) (value mru key)
  "Set the value VALUE for the key KEY in mru-cache MRU.
If KEY had been present, set the new VALUE, but do not bring the KEY-VALUE-pair to the front of the mru-cache.
If KEY had not been present, add it to the front of the mru-cache, and remove the least-recently-used entry if the MRU had already been full.
This function is like #'(SETF GET), but does not change the relative order of already present entries."
  (declare (inline set*))
  (set* mru key value))

(defun del (mru key)
  "Remove the entry with key KEY from mru-cache MRU. Returns non-NIL if KEY had been present, and NIL if not."
  (declare (type mru-cache mru))
  (let* ((ht (mru-cache-ht mru)))
    (cl-custom-hash-table:with-custom-hash-table
      (multiple-value-bind (mru-dcons p) (gethash key ht)
	(when p
	  (dlist2:dcons-delete mru-dcons)
	  (remhash key ht))
	p))))

(defun clear (mru)
  "Remove all entries from mru-cache MRU."
  (declare (type mru-cache mru))
  (let* ((ht (mru-cache-ht mru)))
    (cl-custom-hash-table:with-custom-hash-table
      (clrhash ht)))
  (setf (mru-cache-dlist mru) (dlist2:dlist)))

(defun iterate (mru fun &key from-end)
  "Iterate over the key-value pairs stored in mru cache MRU, without changing their order.
If FROM-END is NIL, MRU will be iterated first-to-last entry, and last-to-frist otherwise.
Calls function FUN with two values: the KEY and the VALUE. MRU may be modified in FUN, but the items prepended to the cache will not be iterated over if FROM-END=NIL."
  (let ((dlist (mru-cache-dlist mru)))
    (dlist2:dodcons-between (pair (dlist2:dlist-first dlist) (dlist2:dlist-last dlist) :from-end from-end)
      (let ((data (dlist2:data pair)))
	(funcall fun (car data) (cdr data))))))

;; test
(let ((mru (make 3)))
  (flet ((to-list (mru)
	   (let ((l nil))
	     (iterate mru (lambda (k v) (push (cons k v) l)) :from-end t)
	     l)))
    (setf (get mru 1) 'uno)
    (assert (eq (get mru 1) 'uno))
    (setf (get mru 1) 'one)
    (assert (eq (get mru 1) 'one))
    (assert (key-present-p mru 1))
    (setf (get mru 2) 'two)
    (setf (get mru 3) 'three)
    (assert (equal (to-list mru) '((3 . three) (2 . two) (1 . one))))
    (assert (and (eq (get* mru 1) 'one) (eq (get* mru 2) 'two) (eq (get* mru 3) 'three)))
    (setf (get mru 4) 'four)
    (assert (eq (get mru 4) 'four))
    (assert (not (key-present-p mru 1)))
    (assert (equal (to-list mru) '((4 . four) (3 . three) (2 . two))))
    (del mru 3)
    (assert (= (count mru) 2))
    (get mru 2)
    (get* mru 4)
    (assert (equal (to-list mru) '((2 . two) (4 . four))))
    (setf (get* mru 4) 'quadro)
    (assert (equal (to-list mru) '((2 . two) (4 . quadro))))
    (clear mru)
    (assert (= (count mru) 0))
    (assert (equal (to-list mru) nil))))

;;;; Function cacher

(defun make-function-cacher (fun slots &key (make-hash-table-fn #'make-lsxhash-equal-hash-table))
  "Make and return a function that has the same arguments as function FUN, but caches the SLOTS most-recently passed arguments and values.
The cached version returns values from the cache if FUN was already evaluated with those arguments. If FUN returns multiple values, the cached function version will do so as well.
FUN must be determinitic and side-effect-free."
  (let* ((mru (make slots :make-hash-table-fn make-hash-table-fn)))
    (lambda (&rest rest)
      ;;(print (list "make-function-cacher ht" (let (l) (maphash (lambda (key value) (push (list key value) l)) (mru-cache-ht mru)) l) "mru" (mru-cache-dlist mru)))
      (if (key-present-p mru rest)
	  (apply #'values (get mru rest))
	  ;; the function call is unkown
	  (let* ((values (multiple-value-list (apply fun rest))))
	    (set mru rest values)
	    (apply #'values values))))))

(let ((last nil))
  (flet ((add (a b)
	   ;;(print (list "add" a b))
	   (setf last (list a b))
	   (+ a b)))
    (let ((cadd (make-function-cacher #'add 2)))
      (setf last nil)
      (assert (= (funcall cadd 1 2) 3))
      (assert (equal last '(1 2)))
      (setf last nil)
      (assert (= (funcall cadd 3 4) 7))
      (assert (equal last '(3 4)))
      (setf last nil)
      (assert (= (funcall cadd 1 2) 3))
      (assert (equal last nil))
      (setf last nil)
      (assert (= (funcall cadd 1 2) 3))
      (assert (equal last nil))
      ;; now the mru has items '((1 2) (3 4)). test pushing the last mru-cached item, i.e. '(3 4), out.
      (setf last nil)
      (assert (= (funcall cadd 5 6) 11))
      (assert (equal last '(5 6)))
      ;; check that '(3 4) was not cached in the mru anymore.
      (setf last nil)
      (assert (= (funcall cadd 3 4) 7))
      (assert (equal last '(3 4)))
      (setf last nil)
      (assert (= (funcall cadd 1 2) 3))
      (assert (equal last '(1 2))))))

#|
(ql:quickload :utils)
(flet ((add (a b)
	 (+ a b)))
  (let ((cadd (make-function-cacher #'add 2)))
    (defun time-mru ()
      (time
       (utils:timesec (lambda ()
			(funcall cadd 1 2)
			(funcall cadd 3 4)
			(funcall cadd 1 2)
			(funcall cadd 1 2)
			(funcall cadd 5 6)
			(funcall cadd 3 4)
			(funcall cadd 1 2)))))))
;; with mru implementation using dlist: CL-USER> (time-mru) = 19/1638400 1.15966796875d-5 8192
;; with mru implementation using dlist2: CL-USER> (time-mru) = 7/1024000 6.8359375d-6 8192
|#
