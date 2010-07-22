(defun repeat (&rest rest)
  "Repeat the i-th argument the number of times specified by argument i+1"
  (declare (type list rest))
  (labels ((repeat-help (rest l)
	     (if (null rest)
		 l
		 (let* ((o (car rest))
			(n (cadr rest))
			(on (loop for i from 1 to n collect o)))
		   (repeat-help (nthcdr 2 rest) (append l on))))))
    (repeat-help rest nil)))

(defun flatten (l &optional (acc nil))
  (declare (type list l acc))
  "Return the flattened list (i.e. no more lists in the list)"
  (cond ((null l) (nreverse acc))
	((consp (car l)) (flatten (cdr l) (nconc (flatten (car l)) acc)))
	(t (flatten (cdr l) (nconc (list (car l)) acc)))))

(defun flatten-1 (l &optional (acc nil))
  "Return the list with one level of list structure removed,
e.g. (((1)) 2 (3 4)) -> ((1) 2 3 4)"
  (if (null l)
      (nreverse acc)
      (flatten-1 (cdr l) (nconc (if (consp (car l))
				    (copy-list (car l))
				    (list (car l))) acc))))

(defun unique-list-p (l &key (test #'equal))
  (declare (optimize (debug 3)))
  (let ((h (make-hash-table :test test)))
    (labels ((helper (l)
	       (if (consp l)
		   (symbol-macrolet ((entry (gethash (car l) h)))
		     (if entry
			 nil
			 (progn
			   (setf entry t)
			   (helper (cdr l)))))
		   t)))
      (helper l))))

;;(defmacro compile-deftype
(deftype unique-list ()
    `(and list (satisfies unique-list-p)))

(defmacro with-gensyms (symbols &body body)
  (declare (type unique-list symbols))
  `(let ,(loop for symbol in symbols collect `(,symbol (gensym)))
     ,@body))

(defmacro timeit (repeat &body body)
  (with-gensyms (start stop)
    `(progn
       (let ((,start (get-internal-run-time)))
	 ,@(flatten-1 (append (repeat body repeat)))
	 (let ((,stop (get-internal-run-time)))
	   (/ (- ,stop ,start) internal-time-units-per-second))))))

;; vielleicht sollte das ein compiler-macro sein: define-compiler-macro
;; man muss leider dieses macro 2x laden, damit die warnung wegen range weggeht
(defmacro range (start &rest rest)
  "Return an as(des)cending number list like python range"
  (let ((incl (if (find :incl rest) t)))
    (declare (type boolean incl))
    (setq rest (delete :incl rest))
    (if (> (length rest) 2)
	(error "range [start] stop [step] [:incl] (~A)" rest))
    (let ((start (if (= (length rest) 0) 0 start))
	  (stop (if (= (length rest) 0) start (car rest)))
	  (step (if (<= (length rest) 1) 1 (cadr rest))))
      (with-gensyms (i stepsym)
	(let* ((up (if incl 'upto 'below))
	       (down (if incl 'downto 'above)))
	  (cond ((and (numberp start) (numberp stop) (numberp step))
		 (if incl
		     `(quote ,(range start stop step :incl))
		     `(quote ,(range start stop step))))
		((not (numberp step))
		 `(let ((,stepsym ,step))
		    (if (> ,stepsym 0)
			(loop for ,i from ,start ,up ,stop by ,stepsym
			   collect ,i)
			(loop for ,i from ,start ,down ,stop by (- ,stepsym)
			   collect ,i))))
		((> step 0)
		 `(loop for ,i from ,start ,up ,stop by ,step collect ,i))
		((< step 0)
		 `(loop for ,i from ,start ,down ,stop by (- ,step) collect ,i))
		(t (error "range step must not be 0"))))))))

(defun string->number (str)
  (declare (type string str))
  (let ((i (read-from-string str)))
    (and (numberp i) i)))

(defun string->integer (str)
  (declare (type string str))
  (let ((i (read-from-string str)))
    (and (integerp i) i)))

(defun cmp (x y)
  (declare (type number x y))
  (if (< x y)
      -1
      (if (> x y)
	  1
	  0)))

(defun at-least (val min)
  (declare (type real val min))
  (if (< val min)
      min
      val))

(defun at-most (val max)
  (declare (type real val max))
  (if (> val max)
      max
      val))

(defun within-p (val min max)
  (declare (type real val min max))
;  (typep val '(real min max)))
  (and (>= val min) (<= val max)))

(defun within (val min max)
  (declare (type real val min max))
  (at-most (at-least val min) max))

(defun binary-search (obj sorted-sequence &key (predicate #'cmp) (exact t))
  (declare (type sequence sorted-sequence)
	   (type (function (t t) (integer -1 1)) predicate)
	   (type boolean exact))
  "Search for obj in the sorted-sequence according to the predicate.
predicate must accept two arguments, obj and an element of sorted-sequence, and
return -1, 0, 1 depending on obj being <, =, > than the element, respectively.
If exact is t and obj is not found, return nil, the closest element otherwise."
  (let ((len (length sorted-sequence)))
    (labels ((rec (a b)
	       (declare (type fixnum a b))
	       (format t "a:~A b:~A~%" a b)
	       (if (= b a)
		   (let ((a-elt (elt sorted-sequence (within a 0 (1- len)))))
		     (if (or (not exact)
			     (= 0 (funcall predicate obj a-elt)))
			 a-elt
			 nil))
		   (let* ((middle (+ (floor (- b a) 2) a))
			  (m-elt (elt sorted-sequence middle)))
		     (declare (type fixnum middle))
		     (ecase (funcall predicate obj m-elt)
		       (-1 (rec a (within (1- middle) a b)))
		       (0 m-elt)
		       (1 (rec (within (1+ middle) a b) b)))))))
      (if (null sorted-sequence)
	  nil
	  (rec 0 len)))))

(defun compile-binary-search (sorted-sequence &key (predicate #'cmp) (exact t))
  (let ((len (length sorted-sequence)))
    (labels ((rec (a b)
	       (declare (type fixnum a b))
	       (format t "a:~A b:~A~%" a b)
	       (if (= b a)
		   (let ((a-elt (elt sorted-sequence (within a 0 (1- len)))))
		     (if (not exact)
			 (lambda (obj) (declare (ignore obj)) a-elt)
			 (lambda (obj)
			   (if (= 0 (funcall predicate obj a-elt))
			       a-elt
			       nil))))
		   (let* ((middle (+ (floor (- b a) 2) a))
			  (m-elt (elt sorted-sequence middle))
			  (lower-node (rec a (within (1- middle) a b)))
			  (higher-node (rec (within (1+ middle) a b) b))
			  (node (lambda (obj)
				  (ecase (funcall predicate obj m-elt)
				    (-1 (funcall lower-node obj))
				    (0 m-elt)
				    (1 (funcall higher-node obj))))))
		       node))))
      (if (null sorted-sequence)
	  (lambda (obj) (declare (ignore obj)) nil)
	  (rec 0 len)))))

(defun emit-compile-binary-search (sorted-sequence
				   predicate-emitter
				   win-emitter
				   emitted-fail
				   &key
				   (body-emitter nil)
				   (exact t))
  (let ((len (length sorted-sequence)))
    (labels ((rec (a b)
	       (declare (type fixnum a b))
	       (if (= b a)
		   (let* ((a-elt (elt sorted-sequence (within a 0 (1- len))))
			  (a-elt-win (funcall win-emitter a-elt)))
		     (if (not exact)
			 a-elt-win
			 (funcall predicate-emitter a-elt
				  emitted-fail a-elt-win emitted-fail)))
		   (let* ((middle (+ (floor (- b a) 2) a))
			  (m-elt (elt sorted-sequence middle))
			  (lower-emitted (rec a (within (1- middle) a b)))
			  (higher-emitted (rec (within (1+ middle) a b) b))
			  (m-elt-win (funcall win-emitter m-elt)))
		     (format t "a:~A b:~A~%" a b)
		     (funcall predicate-emitter m-elt
			      lower-emitted m-elt-win higher-emitted)))))
      (let ((bsearch (if (null sorted-sequence)
			 emitted-fail
			 (rec 0 len))))
	(if body-emitter
	    (funcall body-emitter bsearch)
	    bsearch)))))

(defun const-fun (value)
  "Return a function which accepts any parameters and always returns value."
  (lambda (&rest rest) (declare (ignore rest)) value))

(defun list-nth (l n)
  "Create a new list by taking every n-th element of l."
  (labels ((rec (l acc)
	     (let ((rest (nthcdr n l)))
	       (if (consp l)
		   (rec rest (cons (car l) acc))
		   (nreverse acc)))))
    (rec l nil)))

(defun gmapcar (g function list)
  "Like mapcar, but the g function arguments are taken successively from list."
  (let ((lists (loop for i below g collect (list-nth (nthcdr i list) g))))
    (apply #'mapcar function lists)))

;;(defun sreplace (sequence-1 sequence-2 &key (start1 0) end1 (start2 0) end2)
;;  "like replace, but insert/delete chars, i.e. sequence-1 length may change"
;;  (adjust-array ...

;;(defun sequence-assemble (sequences starts ends)
;;  "creates a sequence of type "
  

;(defun foldl  == reduce
; unfold p f g seed == (loop for x = seed then (g x) until (p x) collect (f x))



