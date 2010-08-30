;; this in-package is neccessary so that symbols :use-d in defpackage
;; (see package.lisp) will be visible in this file
(in-package :utils)

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

(defun nflatten (l &optional (acc nil))
  (declare (type list l acc))
  "Return a flattened list (i.e. no tree structure in the list)"
  (cond ((null l) (nreverse acc))
	((consp (car l)) (nflatten (cdr l) (nconc (nflatten (car l)) acc)))
	(t (nflatten (cdr l) (nconc (list (car l)) acc)))))

(defun flatten-1 (l &optional acc)
  "Return a tree with the outermost level of tree structure removed, only
the modified structure is copied.
e.g. (((1)) 2 (3 4)) -> ((1) 2 3 4), the (1) in both is eq.
"
  (cond
    ((null l) (nreverse acc))
    (t (flatten-1 (cdr l) (if (consp (car l))
			      (nconc (reverse (car l)) acc)
			      (cons (car l) acc))))))


(defun has-key (key h)
  (multiple-value-bind (val p) (gethash key h)
    (declare (ignore val))
    p))

(defun unique-list-p (l &key (test #'equal))
  (let ((h (make-hash-table :test test)))
    (labels ((helper (l)
	       (symbol-macrolet ((item (gethash (car l) h)))
		 (if (multiple-value-bind (i p) item (declare (ignore i)) p)
		     nil
		     (if (consp l)
			 (progn
			   (setf item t)
			   (helper (cdr l)))
			 t)))))
      (helper l))))

;;(defmacro compile-deftype
(deftype unique-list ()
    `(and list (satisfies unique-list-p)))

;; is defined by alexandria
;;(defmacro with-gensyms (symbols &body body)
;;  ;;(declare (type unique-list symbols))
;;  `(let ,(loop for symbol in symbols collect `(,symbol (gensym)))
;;     ,@body))

(defmacro asetf (place value-form)
  "Setf place to value. The symbol it in value-form means the initial value."
  `(let ((it ,place))
     (setf ,place ,value-form)))

(defmacro aif (test-form then-form &optional else-form)
  "Like IF, but TEST-FORM value is accessible as symbol IT in the other forms."
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defun subseqbut (sequence n &optional (nl n))
  "Return a copy of SEQUENCE, omitting the element numbers from N up to NL."
  (nconc (subseq sequence 0 n) (subseq sequence (1+ nl))))

(defmacro defanaphoric (ana-name
			&optional form
			&key (it-parameter 1) ana-documentation)
  "(Re)Define the macro ANA-NAME, which first binds the parameter number
IT-PARAMETER (starting at 1) to the symbol it, and then evaluates and uses all
other parameters in FORM, which gets documentation ANA-DOCUMENTATION."
  (setf form (if (null form)
		     (read-from-string (subseq (string ana-name) 1))
		     form))
  (setf ana-documentation
	(if (null ana-documentation)
	    (let ((old-doc (or
			    (documentation form 'function)
			    (format nil "~A has no documentation." form))))
	      (format nil
		      "Like ~A, but the ~:R parameter is evaluated before all others are~%evaluated, and its value is available as symbol it in all other parameter forms.~%~%Documentation for ~A:~%~A"
		      form it-parameter form old-doc))))
  ;; improve with:
  ;; (defun describe-object-parameters (f)), which returns a lambda list of f
  ;;   use sb-introspect:function-arglist /:function-lambda-list for this
  ;; (defun lambda-list-enumerate (ll) return (0 (nil 1)) for (a &key (b 0) c)
  ;; (defun lambda-list-enumlist (ll)), return (a b c) for (a &key (b 0) c)
  ;;    !!consider macro lambda list, like (a b (c &optional d) &key (x 1) y)!!
  ;; (let* ((ll (describe-object-parameters (ana-name)))
  ;;        (ll-enum (lambda-list-enumerate ll))
  ;;        (all-args (lambda-list-enumlist ll))
  ;;        (args-head (subseq all-args 0 ,(1- it-parameter))
  ;;        (args-tail (subseq all-args ,it-parameter))
  ;;    ... etc ...
  `(defmacro ,ana-name (&rest rest)
     ,ana-documentation
     (let ((rest-head (subseq rest 0 ,(1- it-parameter)))
	   (rest-tail (subseq rest ,it-parameter)))
       `(let ((it (,@(nth ,(1- it-parameter) rest))))
	  (,',form ,@rest-head it ,@rest-tail)))))

(defanaphoric arplacd)

(defun variables-in-varlist (varlist)
  (labels ((rec (varlist variables)
	     (cond
	       ((null varlist)
		variables)
	       ((symbolp (car varlist))
		(rec (cdr varlist) (cons (car varlist) variables)))
	       ((and (listp (car varlist)) (symbolp (caar varlist)))
		(rec (cdr varlist) (cons (caar varlist) variables)))
	       (t (error "not a valid varlist")))))
    (rec varlist nil)))

(defun unroll-do-makebodies (varlist endlist body)
  (if (> (length endlist) 1)
      (error "result-forms not yet implemented, get result of do to implement"))
  ;; maybe implement:
  ;; result form: to do this, let the do return the list of variable values at
  ;; the end of the loops, and then use this list to construct a let form, in
  ;; which all the variables are bound to their values. The body of the let form
  ;; are the result-forms.
  ;; An unrolled do semantically equivalent to do: At the moment, modifying the
  ;; loop variables in the body does not influence the next step, or the end
  ;; condition (because the variables are bound again). Unrolling the variable
  ;; updates and end checking forms would mean slower code, and would save
  ;; just one jmp (to the beginning of the loop). It's probably not worth it.
  ;; 
  ;; A note: substituting the variables with their values at each loop does not
  ;; work, because then all variables will get substituted, also the shadowing
  ;; ones, e.g. a local lambda form using the same variable names. Therefore
  ;; the let at each step necessary (which is hopefully compiled away, because
  ;; the variables are constant).
  (let* ((variables (variables-in-varlist varlist))
	 (v-let-list `(list ,@(loop for v in variables
				 collect `(list ',v ,v)))))
    ;;(format t "v-ll:~A~%" v-let-list)
    (with-gensyms (list)
      `(let ((,list '(progn)))
	 (do ,varlist (,(car endlist) (list ,@variables))
	   ;;(format t "l:~A~%" ,list)
	   (setq ,list
		 (append ,list
			 `((let ,,v-let-list
			     (declare (ignorable ,',@variables))
			     (tagbody ,@',body))
			   ))))
	 ,list))))

(defmacro unroll-do (varlist endlist &body body)
  "Almost like do, but unrolls the loop. Difference is, all variables in varlist
are bound in the unrolled bodies to their respective values for each step.
Therefore modifying a variable in the body does not influence the next loop
step.
varlist: ({var|(var [init-form [step-form]])}*)
endlist: (end-test-form result-form*)
result-form is not yet implemented."
  (eval (unroll-do-makebodies varlist endlist body)))

(defmacro do-unrollable (unroll varlist endlist &body body)
  "Like do, but unrolls loop if UNROLL is T. "
  (if unroll
      `(unroll-do ,varlist ,endlist ,@body)
      `(do ,varlist ,endlist ,@body)))

(defmacro timeit ((repeat &key stats unroll) &body body)
  (declare (type integer repeat))
  "Return the total time of running BODY (number REPEAT) times. If STATS is T,
measure (with possible overhead) and additionally return statistics (min, mean,
max) of each execution duration.
If UNROLL is T, unroll the repetitions."
  ;;(princ "(time")
  (with-gensyms (start stop times i)
    `(progn
       ;;(princ "it")
       (let (,start ,stop ,@(if stats `((,times (make-array ,repeat)))))
	 (setq ,start (get-internal-real-time))
	 (do-unrollable ,unroll
	     ((,i 0 (1+ ,i))) ((>= ,i ,repeat))
	   ,@(append (if stats
			 `((setf (elt ,times ,i)
				 (get-internal-real-time))))
		     body))
	 (setq ,stop (get-internal-real-time))
	 ;;(format t ")~%")
	 (setf ,start (float (/ (- ,stop ,start)
				internal-time-units-per-second)))
	 ,(if stats
	      `(progn
		 (loop for ,i below ,(1- repeat)
		    do (asetf
			(elt ,times ,i)
			(float (/ (- (elt ,times (1+ ,i)) it)
				  internal-time-units-per-second))))
		 (asetf (elt ,times (1- ,repeat))
			(float (/ (- ,stop it)
				  internal-time-units-per-second)))
		 (values ,start
			 (reduce #'min ,times)
			 (/ (reduce #'+ ,times)
			    ,repeat)
			 (reduce #'max ,times)))
	      `,start)))))

(defun range (start &key (stop nil stop-p) (step 1) (incl nil))
  (if (not stop-p)
      (progn (setq stop start) (setq start 0)))
  (if (> step 0)
      (if incl
	  (loop for i from start upto stop by step collect i)
	  (loop for i from start below stop by step collect i))
      (if incl
	  (loop for i from start downto stop by (- step) collect i)
	  (loop for i from start above stop by (- step) collect i))))

(define-compiler-macro range (&whole form start &key (stop nil stop-p)
				     (step 1) (incl nil))
  (if (not stop-p)
      (progn (setq stop start) (setq start 0)))
  (let* ((up (if incl 'upto 'below))
	 (down (if incl 'downto 'above)))
    (cond ((and (numberp start) (numberp stop) (numberp step))
	   `(quote ,(range start :stop stop :step step :incl incl)))
	  ((not (numberp step))
	   form)
	  ((> step 0)
	   (with-gensyms (i)
	     `(loop for ,i from ,start ,up ,stop by ,step collect ,i)))
	  ((< step 0)
	   (with-gensyms (i)
	     `(loop for ,i from ,start ,down ,stop by (- ,step) collect ,i)))
	  (t form))))

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
  (warn "deprecated #'within-p: use #'<= instead")
  (<= min val max))

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
  (warn "deprecated #'const-fun: use #'constantly instead")
  (lambda (&rest rest) (declare (ignore rest)) value))

(defun list-nth (list n)
  "Create a new list by taking every N-th element of LIST."
  (labels ((rec (list acc)
	     (let ((rest (nthcdr n list)))
	       (if (consp list)
		   (rec rest (cons (car list) acc))
		   (nreverse acc)))))
    (rec list nil)))

(defun lists-nth (list n)
  "Create a list of N lists, where the i-th list is made of all the ith-elements
in LIST. (LIST-NTH '(1 2 3 4 5 6) 3) == '((1 4) (2 5) (3 6))"
  (loop for i below n collect (list-nth (nthcdr i list) n)))

(defun gmapcar (g function list)
  (declare (type integer g))
  "Like mapcar, but the g function arguments are taken successively from list."
  (let ((lists (lists-nth list g)))
    (apply #'mapcar function lists)))

(defun sreplace (sequence-1 sequence-2
		 &key (start1 0) (end1 (length sequence-1))
		 (start2 0) (end2 (length sequence-2)))
  "like replace, but insert/delete chars, i.e. sequence-1 length may change"
  (let* ((len-2 (+ start1 (- end2 start2) (- (length sequence-1) end1)))
	 (buf (adjust-array (make-array (length sequence-1)
					:displaced-to sequence-1
					:element-type
					(array-element-type sequence-1)
					:fill-pointer (length sequence-1))
			    len-2
			    :fill-pointer len-2))
	 (rep-end1 (+ start1 (- end2 start2))))
;;    (format t "~A ~A~%" buf len-2)
;;    (format t "start1:~A end1:~A :start2:~A end2:~A~%" start1 rep-end1 start2
;;	    end2)
    (let* ((buf2 (replace buf sequence-2
			  :start1 start1 :end1 rep-end1
			  :start2 start2 :end2 end2))
	   (start-b2 (+ start1 (- end2 start2)))
	   (start1-tailstart (- (length sequence-1) (- len-2 rep-end1))))
;;      (format t "buf:~A~%" buf)
;;      (format t "end1:~A length1:~A~%" rep-end1 (length sequence-1))
;;      (format t "start1:~A start2:~A~%" start-b2 start1-tailstart)
      (if (/= start-b2 len-2)
	  (replace buf2 sequence-1
		   :start1 start-b2
		   :start2 start1-tailstart)
	  buf2))))

(defun string-tolower (string)
  (map 'string 'char-downcase string))

(defun string-toupper (string)
  (map 'string 'char-upcase string))

(defun compose-1 (last-function &rest functions)
  (let* ((allf (cons last-function functions)) 
	 (fs-1 (butlast allf))
	 (f1 (car (last allf)))
	 (doc (reduce (lambda (x y) (format nil "(~A ~A)" x y))
		      fs-1
		      :from-end t
		      :initial-value (format nil "(~A &rest args)" f1))))
    (let ((composite
	   (lambda (&rest args)
	     (let ((val (apply f1 args)))
	       (reduce #'funcall fs-1 :from-end t :initial-value val)))))
      (setf (documentation composite 'function) doc)
      composite)))

(defun maptree (function tree)
  "Recreate the TREE from the results of FUNCTION called with each element."
  (labels ((rec (function tree acc)
	     (cond
	       ((null tree) (reverse acc))
	       ((consp (car tree)) (rec function (cdr tree)
					(nconc (cons (rec function (car tree)
							  nil)
						     nil)
					       acc)))
	       (t (rec function (cdr tree)
		       (cons (funcall function (car tree))
			     acc))))))
    (rec function tree nil)))

(defmacro compose* (sexp)
  "Return a function 
Create a lambda list from symbols 'a to 'z, and 'rest occurring in  sexp,
and use it in a lambda expression, which is returned"
  ;; was originally started as function... is easier as macro
  ;; keywords as argument places wont work, they might be normal arguments
  (labels ((variable? (x)
	     (and (typep x 'symbol)
		  (or (eq x 'rest)
		      (let ((s (string x)))
			(and (= (length s) 1)
			     (string>= s "A")
			     (string<= s "Z"))))
		  x))
	   (compose-free-variables (sexp)
	     "return a list of symbols 'a to 'z, and rest in sexp"
	     (let* ((l (delete-duplicates
			(sort
			 (mapcar #'string
				 (remove-if (complement #'variable?)
					    (nflatten sexp)))
			 #'string-lessp)))
		    (s (mapcar #'intern l)))
	       (if (find 'rest s)
		   (nconc (delete 'rest s) (list '&rest 'rest))
		   s))))
    (let ((lambdalist (compose-free-variables sexp)))
      `(lambda (,@lambdalist) ,sexp))))

(defun mapc-array-major (function array)
  "Consider array as a vector by viewing its elements in row-major order, and
call function with all indices of the vector."
  (loop for i below (array-total-size array) do
       (funcall function i))
  array)

(defun mapc-array (function array)
  "Call function on all elements of array in row-major order."
  (mapc-array-major (lambda (i)
		      (funcall function (row-major-aref array i)))
		    array))

(defun map-array (function array)
  "Return a copy of array with all elements replaced with the return value
of calling the function with the original element value in row-major order."
  (let ((copy (copy-array array)))
    (mapc-array-major (lambda (i)
			(setf (row-major-aref copy i)
			      (funcall function (row-major-aref copy i))))
		      array)))

(defun unique (list &key (only nil only-p) (not nil not-p) (test 'eql))
  "Return a list with all double elements in list removed. Equality is tested
with test. The order of the returned elements is not specified.
If only is T, return only the unique elements in list.
If not is T, return only the elements that have at least one duplicate in list.
Either only or not may be specified."
  (assert (not (and only-p not-p)))
  (let ((ht (make-hash-table :test test))
	(result))
    (if (or only not)
	(loop for e in list do (incf (gethash e ht 0)))
	(loop for e in list do (setf (gethash e ht) t)))
    (cond
      (only
       (maphash (lambda (k v)
		  (if (= v 1) (setf result (cons k result))))
		ht))
      (not
       (maphash (lambda (k v)
		  (if (> v 1) (setf result (cons k result))))
		ht))
      (t
       (maphash (lambda (k v)
		  (declare (ignore v))
		  (setf result (cons k result)))
		ht)))
    result))

(defun andf (&rest list)
  "Like AND, but as function."
  (labels ((rec (l last)
	     (if (null l)
		 last
		 (if (eq nil (car l))
		     nil
		     (rec (cdr l) (car l))))))
    (rec list t)))

(defun orf (&rest list)
  "Like OR, but as function."
  (labels ((rec (l)
	     (if (null l)
		 nil
		 (if (car l)
		     (car l)
		     (rec (cdr l))))))
    (rec list)))

(defun equal-array (a b &key (test 'equal))
  "Return A if the arrays A and B have equal contents, NIL otherwise."
  (and (equal (array-dimensions a)
	      (array-dimensions b))
       (mapc-array-major (lambda (i)
			   (if (not (funcall test
					     (row-major-aref a i)
					     (row-major-aref b i)))
			       (return-from equal-array nil)))
			 a)))

(defun all (item sequence &key from-end (start 0) end key (test #'eql))
  "Check if all items in SEQUENCE are equal to ITEM under function TEST.
Is equivalent to (not (position item sequence :test (complement test)))."
  (not (position item sequence :from-end from-end :start start :end end :key key
		:test (complement test))))

(defun all-if (predicate sequence &key from-end (start 0) end key)
  "Check if all items in SEQUENCE satisfy PREDICATE.
Is equivalent to (not (position-if (complement predicate) sequence))."
  ;; also (apply #'andf (mapcar predicate sequence)), but is slower
  (not (position-if (complement predicate) sequence
		    :from-end from-end :start start :end end :key key)))

(defmacro let-array-dims (m &body body)
  "Define the symbols cols, rows, maxcol, maxrow as the properties of M in
BODY."
  (let ((flatbody (flatten body)))
    (labels ((find-in-body (symbol)
	       (find symbol flatbody)))
      ;;(format t "package:~A~%" *package*)
      (let ((cols (aif (find-symbol "COLS" *package*) (find-in-body it)))
	    (rows (aif (find-symbol "ROWS" *package*) (find-in-body it)))
	    (maxcol (aif (find-symbol "MAXCOL" *package*) (find-in-body it)))
	    (maxrow (aif (find-symbol "MAXROW" *package*) (find-in-body it)))
	    (arrdim (gensym)))
	(if (and maxcol (not cols))
	    (setf cols (gensym)))
	(if (and maxrow (not rows))
	    (setf rows (gensym)))
	;;(format t "cols:~A rows:~A maxcol:~A maxrow:~A~&body:~A"
	;;	cols rows maxcol maxrow body)
	`(let* ,(append
		 `((,arrdim (array-dimensions ,m)))
		 (if cols
		     `((,cols (cadr ,arrdim))))
		 (if rows
		     `((,rows (car ,arrdim))))
		 (if maxcol
		     `((,maxcol (1- ,cols))))
		 (if maxrow
		     `((,maxrow (1- ,rows)))))
	   ,@body)))))

(defun vector->list (vector)
  "Return a list made of elements in vector."
  (assert (typep vector 'vector))
  (let (r)
    (mapc-array (lambda (x) (push x r)) vector)
    (nreverse r)))

(defun head (list &optional (n 1))
  "Return the first n elements from list."
  (subseq list 0 (min n (length list))))

;; write another macro that looks like a defun, but emits multiple versions of
;; the
;; function, each with a specified set of arguments set to specified constants
;; (using let).
;; Use it to optimize functions, e.g.
;; (defun branch (b x y) (if b x y)) should become
;;     (defun branch-a (x y) (let ((b t)) (if b x y)))
;;     (defun branch-b (x y) (let ((b nil)) (if b x y)))
;; and (defun branch (b x y) (case b ((t (branch-a x y)) (nil (branch-b x y)))))
;; The names of the defined functions should be implicit, i.e. not using defun.
;; The parameter bindings have to be specified as macro arguments.
;; ... hmm maybe also emit compiler-macros? (maybe not, because they would only
;; branch on compile-time-known constant data).
(defmacro defun-specialize (((name ((parameter value) &rest plist))
			     &rest names-and-parameters)
			    lambda-list
			    &body body)
  "Define several functions specified by NAMES-AND-PARAMETERS with body BODY.
For each function named NAME, the PARAMETERs are symbol-macrolet to the
respective VALUEs in BODY.
Ex: (defun-specialize
        ((hl-head ((ht t)))
         (hl-last ((ht nil))))
        (list &optional n)
      (if ht
          (head list n)
          (last list n)))"
  (push (list name (cons (list parameter value) plist)) names-and-parameters)
  (format t "names-and-parameters:~A~%" names-and-parameters)
  `(progn 
     ,@(loop for name-and-plist in names-and-parameters collect
	    (destructuring-bind (name plist) name-and-plist
	      `(defun ,name ,lambda-list
		 (symbol-macrolet (,@plist)
		   ,@body))))))

(defun mappair* (function list)
  (let (result)
    (if (null list)
	nil
	(labels ((rec (item rest)
		   (if (null rest)
		       (progn (push (funcall function item (car list)) result)
			      (nreverse result))
		       (progn (push (funcall function item (car rest)) result)
			      (rec (car rest) (cdr rest))))))
	  (rec (car list) (cdr list))))))

(defun mapl-maxn (n function list &rest more-lists)
  "Call (apply #'mapl function list more-lists), but make at most n calls
to function."
  (apply #'mapl
	 (lambda (&rest rest)
	   (if (= n 0)
	       (return-from mapl-maxn))
	   (setf n (1- n))
	   (apply function rest))
	 list
	 more-lists))

(defun mapl* (n function list)
  "Consider list as a circular list, and call function with n successive
elements, so that each element has been used as 1st parameter. list must have a
length of at least n/2."
  (let* ((c (append list (head list (1- n)))))
    (mapl-maxn (length list)
	       (lambda (l) (apply function (head l n)))
	       c)))

(defmacro progn-repeat (form repeat &key (unroll nil))
  `(do-unrollable ,unroll ((i 0 (1+ i))) ((>= i ,repeat)) ,form))

(defmacro prind (&rest args)
  "Print args"
  `(progn
     ,@(loop for a in args collect
	    (if (eq a T)
		`(format t "~%")
		`(format t "~A:~A " ,(format nil "~A" a) ,a)))
     (format t "~%")))

(defun sgn (x)
  "Return -1, 0, or 1, if X is less, equal, or greater than 0, respectively."
  (declare (type number x))
  (cmp x 0))

;;(defun sequence-assemble (sequences starts ends)
;;  "creates a sequence of type "


;(defun foldl  == reduce
; unfold p f g seed == (loop for x = seed then (g x) until (p x) collect (f x))


;; implement tests using eval-when :compile-toplevel?

;; check out cl-series (functional!!! replacement for LOOP)
