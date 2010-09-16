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

(defun do-list-iterations (varlist endlist)
  (let* ((vars (variables-in-varlist varlist))
	 (mapping `(mapcar #'list ',vars (list ,@vars))))
    (with-gensyms (iters)
      (eval
       `(let (,iters)
	  (values 
	   (do ,varlist (,(car endlist) ,mapping)
	     (push ,mapping ,iters))
	   (nreverse ,iters)))))))

(defmacro unroll-do (varlist endlist &body body)
  "Almost like do, but unrolls the loop. Difference is, all variables in varlist
are bound in the unrolled bodies to their respective values for each step.
Therefore modifying a variable in the body does not influence the next loop
step.
varlist: ({var|(var [init-form [step-form]])}*)
endlist: (end-test-form result-form*)"
  ;; maybe implement:
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
  (let ((vars (variables-in-varlist varlist)))
    (multiple-value-bind (result iters)
	(do-list-iterations varlist endlist)
      `(progn
	 ,@(loop for i in iters collect
		`(let ,i
		   (declare (ignorable ,@vars))
		   ,@body))
	 (let ,result
	   (declare (ignorable ,@vars))
	   ,@(cdr endlist))))))

(defmacro do-unrollable (unroll varlist endlist &body body)
  "Like do, but unrolls loop if UNROLL is T. "
  (if unroll
      `(unroll-do ,varlist ,endlist ,@body)
      `(do ,varlist ,endlist ,@body)))

(defmacro timeit ((repeat &key stats unroll disable-gc) &body body)
  "Return the total time of running BODY (number REPEAT) times. If STATS is T,
measure (with possible overhead) and additionally return statistics (min, mean,
max) of each execution duration.
If UNROLL is T, unroll the repetitions."
  (with-gensyms (start stop times i)
    `(progn
       (let (,start ,stop ,@(if stats `((,times (make-array ,repeat)))))
	 ,(if disable-gc #+sbcl `(progn
				   (sb-ext:gc)
				   (sb-ext:gc-off)))
	 (setq ,start (get-internal-real-time))
	 (do-unrollable ,unroll
	     ((,i 0 (1+ ,i))) ((>= ,i ,repeat))
	   ,@(append (if stats
			 `((setf (elt ,times ,i)
				 (get-internal-real-time))))
		     body))
	 (setq ,stop (get-internal-real-time))
	 ,(if disable-gc #+sbcl `(sb-ext:gc-on))
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

(defmacro within-p (min max &rest values)
  "Return T if for all v in VALUES: (<= MIN v MAX), NIL otherwise"
  ;; undeprecate after changing semantics
  ;;(warn "deprecated #'within-p: use #'<= instead")
  `(and ,@(loop for v in values collect `(<= ,min ,v ,max))))

(defun within (val min max)
  (declare (type real val min max))
  (at-most (at-least val min) max))

(defun binary-search (obj sorted-sequence &key (predicate #'cmp) (exact t)
		      (key (lambda (x) x)) (fail (constantly nil)))
  (declare (type sequence sorted-sequence)
	   (type (function (t t) (integer -1 1)) predicate)
	   (type boolean exact)
	   (type (function (t) t) key)
	   (type (function (t t) t) fail))
  ;;(declare (optimize (speed 3)))
  "Search for OBJ in the SORTED-SEQUENCE according to the PREDICATE.
If KEY is given, it is called with item in SORTED-SEQUENCE and returns an ITEM.
PREDICATE must accept two arguments, obj and an ITEM of SORTED-SEQUENCE, and
return -1, 0, 1 depending on obj being <, =, > than the ITEM, respectively.
If EXACT is t, and OBJ is not found, return the result of calling FAIL with the
two nearest elements or NIL if FAIL is not given, the closest element in the
list otherwise."
  (if (null key)
      (setf key (lambda (x) x)))
  (if (null fail)
      (setf fail (constantly nil)))
  (let ((len (length sorted-sequence)))
    (labels ((rec (a b c)
	       (declare (type fixnum a b c))
	       ;;(format t "a:~A b:~A c:~A~%" a b c)
	       (let* ((almost-done (<= 0 (- c a) 1))
		      (done (and almost-done (= b c)))
		      (b-elt (elt sorted-sequence b))
		      (b-item (funcall key b-elt)))
		 (if done
		     (if (or (not exact)
			     (= 0 (funcall predicate obj b-item)))
			 b-elt
			 (funcall fail
				  (elt sorted-sequence a)
				  (elt sorted-sequence c)))
		     (ecase (funcall predicate obj b-item)
		       (-1 (rec a (floor (lerp .5 a b)) b))
		       (0 b-elt)
		       (1 (let ((m (if almost-done
				       c
				       (floor (lerp .5 b c)))))
			    (rec b m c))))))))
      (if (null sorted-sequence)
	  nil
	  (rec 0 (floor len 2) (1- len))))))

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
				   fail-emitter
				   &key
				   (body-emitter nil)
				   (exact t))
  "Emit a binary search routine by calling code emitter functions whose combined
results perform a binary search on SORTED-SEQUENCE.
PREDICATE-EMITTER must accept 4 parameters (A LOWER-EMIT A-WIN HIGHER-EMIT) and
must evaluate either LOWER-EMIT, A-WIN, or HIGHER-EMIT if the argument of the
binary search is lower, equal, or higher to A, respectively. A is an element of
SORTED-SEQUENCE.
WIN-EMITTER must accept 1 parameter (WIN-ELT) and terminate the search
successfully returning WIN-ELT, an element of SORTED-SEQUENCE.
EMITTED-FAIL must accept 2 parameters (LOWER-ELT HIGHER-ELT) and abort the
search, optionally returning LOWER-ELT and HIGHER-ELT as the nearest elements.
If BODY-EMITTER is given, it must accept and evaluate 1 parameter.
If EXACT is T and no result is found, EMITTED-FAIL is evaluated, otherwise
WIN-EMITTER is evaluated with the closest element in SORTED-SEQUENCE."
  (assert (not (null sorted-sequence)))
  (let ((len (length sorted-sequence)))
    (labels ((rec (a b c)
	       (declare (type fixnum a b c))
	       ;;(prind a b c)
	       (let* ((a-elt (elt sorted-sequence a))
		      (b-elt (elt sorted-sequence b))
		      (c-elt (elt sorted-sequence c))
		      (b-elt-win (funcall win-emitter b-elt))
		      (almost-done (<= 0 (- c a) 1))
		      (done (and almost-done (= b c)))
		      (lower-code (if almost-done
				      (if (not exact)
					  b-elt-win
					  (funcall fail-emitter a-elt b-elt))
				      (rec a (floor (lerp .5 a b)) b)))
		      (higher-code (if done
				       (if (not exact)
					   b-elt-win
					   (funcall fail-emitter b-elt c-elt))
				       (if almost-done
					   (rec b c c)
					   (rec b (floor (lerp .5 b c)) c)))))
		 (funcall predicate-emitter
			  b-elt lower-code b-elt-win higher-code))))
      (let ((bsearch (rec 0 (floor len 2) (1- len))))
	(if body-emitter
	    (funcall body-emitter bsearch)
	    bsearch)))))

(defun binary-search-emitters (name &key (fail-result nil))
  (flet ((predicate (a lower-emit a-win higher-emit)
	   `(if (< search-elt ,A)
		,lower-emit
		(if (> search-elt ,A)
		    ,higher-emit
		    ,a-win)))
	 (win (win-elt)
	   `(values ,win-elt t))
	 (fail (a-elt b-elt)
	   (if fail-result
	       `(values (cons ,a-elt ,b-elt) nil)
	       `(values nil nil)))
	 (body (body)
	   `(defun ,name (search-elt)
	      ,body)))
    (values #'predicate #'win #'fail #'body)))

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
  ;; :a to :z instead of 'a to 'z wont work, they might be normal arguments
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
		   (append (delete 'rest s) (list '&rest 'rest))
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

(defun andf (&rest list)
  "Like AND, but as function."
  ;; If you use andf, consider using the built-in function every
  (labels ((rec (l last)
	     (if (null l)
		 last
		 (if (eq nil (car l))
		     nil
		     (rec (cdr l) (car l))))))
    (rec list t)))

(defun orf (&rest list)
  "Like OR, but as function."
  ;; If you use orf, consider using the built-in function some
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

(defun function-specializer (names-and-parameters lambda-list body &key rec)
  ;; Ex: (function-specializer '((test1 (a 1)) (test2 (a 2))) '() 'a)
  ;;(format t "names-and-parameters:~A~%" names-and-parameters)
  (loop for name-and-plist in names-and-parameters collect
       (destructuring-bind (name plist) name-and-plist
	 `(,name ,lambda-list
		 (symbol-macrolet (,@plist)
		   (locally
		       #+sbcl (declare (sb-ext:muffle-conditions
					sb-ext:code-deletion-note))
		       ,@(if rec
			     (subst name 'rec body)
			     body)))))))

(defmacro specializing-flet (((name bindings)
			      &rest name-defs)
			     (lambda-list &body function-body)
			     &body body)
  "Define several local functions specified by NAME-DEFS with FUNCTION-BODY.
For each function named NAME, the BINDINGSs are symbol-macrolet in BODY.
Ex: (specializing-flet ((hl-head ((ht t)))
                        (hl-last ((ht nil))))
        ((list &optional (n 1))
         (if ht
             (head list n)
             (last list n)))
      (hl-last '(1 2 3)))"
  (push (list name bindings) name-defs)
  `(flet ,(function-specializer name-defs lambda-list function-body)
     ,@body))

(defmacro specializing-labels (((name bindings)
				&rest name-defs)
			       (lambda-list &body function-body)
			       &body body)
  "Define several local functions specified by NAME-DEFS with FUNCTION-BODY.
For each function named NAME, the BINDINGSs are symbol-macrolet in BODY.
In each FUNCTION-BODY, the symbol REC is replaced with the respective NAME.
Ex: (specializing-labels ((hl-head ((ht t)))
                          (hl-last ((ht nil))))
        ((list &optional (n 1))
         (if ht
             (head list n)
             (last list n)))
      (hl-last '(1 2 3)))"
  (push (list name bindings) name-defs)
  `(labels ,(function-specializer name-defs lambda-list function-body
				  :rec t)
     ,@body))

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
(defmacro specializing-defun (((name bindings) &rest name-defs)
			      lambda-list &body body)
  "Define several local functions specified by NAME-DEFS with FUNCTION-BODY.
For each function named NAME, the BINDINGSs are symbol-macrolet in BODY.
In each FUNCTION-BODY, the symbol REC is replaced with the respective NAME.
Ex: (specializing-defun
        ((hl-head ((ht t)))
         (hl-last ((ht nil))))
        (list &optional (n 1))
      (if ht
          (head list n)
          (last list n)))"
  (push (list name bindings) name-defs)
  `(progn
     ,@(loop for s in (function-specializer name-defs lambda-list body :rec t)
	  collect `(defun ,@s))))

(labels ((rec (function result item rest wrap)
	   (if (null rest)
	       (progn (if wrap
			  (push (funcall function item wrap) result))
		      (nreverse result))
	       (progn (rec function
			   (cons (funcall function item (car rest)) result)
			   (car rest)
			   (cdr rest)
			   wrap)))))
  (defun mappair (function list)
    (if (null list)
	nil
	(rec function nil (car list) (cdr list) nil)))
  (defun mappair* (function list)
    (if (null list)
	nil
	(rec function nil (car list) (cdr list) (car list)))))

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

(defmacro progn-repeat ((repeat &key unroll) &body body)
  "Repeat BODY (times REPEAT), at least once. If UNROLL is T, unroll the loop
and REPEAT must be an integer greater 0."
  (assert (or (and (not unroll) (or (symbolp repeat) (integerp repeat)))
	      (and (integerp repeat) (> repeat 0))))
  (with-gensyms (i)
    `(progn
       (do-unrollable ,unroll ((,i 0 (1+ ,i))) ((>= ,i (1- ,repeat)))
	 ;; progn is necessary
	 ;; (e.g. body=(t t) is an invalid tagbody (due to do))
	 (progn ,@body))
       ,@body)))

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
  (warn "deprecated sgn: use signum instead")
  (cmp x 0))

(defmacro 2nd-value (&body body)
  "Returns the 2nd value of the multiple values returned by BODY."
  (with-gensyms (a b)
    `(multiple-value-bind (,a ,b)
	 ,@body
       (declare (ignore ,a))
       ,b)))

(defun let+-bindings (bindings body)
  ;; type declarations are not handled, is it neccessary to declare immediately?
  (if (null bindings)
      body
      (let ((binding (car bindings))
	    (rest (let+-bindings (cdr bindings) body)))
	(if (symbolp binding)
	    `((let (,binding) ,@rest))
	    (case (length binding)
	      ((0 1) (error "binding must have name and value"))
	      (2 `((let (,binding) ,@rest)))
	      (otherwise `((multiple-value-bind (,@(butlast binding))
			       ,@(last binding)
			     ,@rest))))))))

(defmacro let+ (bindings &body body)
  (car (let+-bindings bindings body)))

(defmacro d-b-lambda (lambda-list &body body)
  `(lambda (&rest arguments)
     (destructuring-bind ,lambda-list arguments
       ,@body)))

(defun log2 (value)
  "Return the log to the base 2 of VALUE."
  (/ (log value) (log 2)))

(defun pow2 (x)
  "Return x*x."
  (* x x))

(defmacro filter (&whole whole
		   item sequence &key from-end test start end count key)
  "Like remove, but return all the matching items instead."
  (declare (ignore item sequence from-end test start end count key))
  (aif (member :test whole)
       (setf (cadr it) `(complement ,(cadr it)))
       (asetf whole (append it '(:test (complement #'eql)))))
  `(remove ,@(cdr whole)))

(defmacro filter-if (&whole whole
		     predicate sequence &key from-end start end count key)
  "Like remove-if, but return all the matching items instead."
  (declare (ignore predicate sequence from-end start end count key))
  (let ((nwhole (copy-list whole)))
    (asetf (cadr nwhole) `(complement ,it))
    `(remove-if ,@(cdr nwhole))))

(defun join (list &optional (separator " "))
  "Join the strings in LIST, separated by SEPARATOR each."
  (if (null list)
      ""
      (concatenate 'string (car list)
		   (apply #'concatenate 'string 
			  (mapcar (lambda (x)
				    (concatenate 'string separator x))
				  (cdr list))))))

(defun exp-until-predicate (x function predicate)
  "Call FUNCTION with X and call PREDICATE with these results. If its result is
nil, grow ITERS expontentially and repeat, otherwise return X and last results."
  (let ((results (multiple-value-list (funcall function x))))
    (if (apply predicate results)
	(apply #'values x results)
	(exp-until-predicate (* 2 x) function predicate))))

(defmacro timecps ((repeat &key stats unroll disable-gc (time 0.1)) &body body)
  "Like timeit, but return calls per second. TIME is the approximate measuring
time."
  (with-gensyms (run iters timeit-call measure-iters)
    `(macrolet ((,timeit-call (,iters)
		  `(timeit (,,repeat :stats ,,stats
				     :unroll ,,unroll
				     :disable-gc ,,disable-gc)
		     (progn-repeat (,,iters)
		       ,',@body))))
       (labels ((,measure-iters (,iters)
		  (multiple-value-bind (,iters ,run)
		      (exp-until-predicate ,iters
					   (lambda (x) (,timeit-call x))
					   (lambda (x &rest r)
					     (declare (ignore r))
					     (>= x 0.005)))
		    (floor (/ (* ,time ,iters) ,run)))))
	 (let* ((,iters (,measure-iters 1)))
	   ;;(prind ,iters)
	   (let ((,run (multiple-value-list (,timeit-call ,iters))))
	     ;;(prind ,run)
	     (apply #'values (mapcar (lambda (,run)
				       (if (> ,run 0)
					   (/ ,iters ,run)
					   nil))
				     ,run))))))))

(defmacro specializing-case (symbol cases &body body)
  "Like case, but only one value allowed per CASE, and execute the same BODY in
all cases. Before each BODY, the SYMBOL is symbol-macrolet to each CASE.
The default (otherwise) case is automatically defined.
Ex: (specializing-case x (1 2 nil t #'identity) (princ x))"
  (declare (type symbol symbol))
  `(cond
     ,@(loop for c in cases 
	  collect
	  `((eql ,symbol ,c)
	    (symbol-macrolet ((,symbol ,c))
	      (locally
		  #+sbcl (declare (sb-ext:muffle-conditions
				   sb-ext:code-deletion-note))
		  ,@body))))
     (t
      (locally
	  #+sbcl (declare (sb-ext:muffle-conditions
			   sb-ext:code-deletion-note))
	  ,@body))))

(defmacro specializing-cond-let ((test bindings &rest tests-and-bindings)
				 &body body)
  "A combination of cond and symbol-macrolet.
Ex: (specializing-cond-let
        (((= a 1) ((a 1)))
         (t ((a nil) (b nil))))
      (prind a b))"
  (let ((tests-and-bindings (append (list test bindings) tests-and-bindings)))
    `(cond ,@(loop
		for test-and-bindings in tests-and-bindings
		for test = (car test-and-bindings)
		for bindings = (cadr test-and-bindings)
		collect `(,test (symbol-macrolet ,bindings ,@body))))))

(defun unique-sfast (sequence &key (test #'eql))
  (let ((ht (make-hash-table :test test))
	(result))
    (map nil
	 (lambda (x) (setf (gethash x ht) 1))
	 sequence)
    (maphash (lambda (k v)
	       (declare (ignore v)) (setf result (cons k result)))
	     ht)
    result))

(defun unique (sequence &key only not countp (test #'eql) (key #'identity))
  "Return a list with all but one duplicate elements in SEQUENCE removed. If KEY
is non-NIL the result of calling KEY with an element is used in the comparison.
Equality is tested with TEST and must be one of EQ, EQL, EQUAL, or EQUALP. The
order of the returned elements is not specified.
If ONLY is T, return only the unique elements.
If NOT is T, return only the elements that have at least one duplicate.
If COUNTP is non-NIL, return only the elements whose count satisfies COUNTP.
Only one of ONLY, NOT, or COUNTP may be non-NIL."
  (assert (>= 1 (count-if (complement #'null) (list only not countp))))
  (specializing-cond-let
      ((only ((countp (lambda (x) (= x 1)))))
       (not ((countp (lambda (x) (> x 1)))))
       ((not countp) ((countp (constantly t)) (only 'default)))
       (t ()))
    (specializing-case key (#'identity)
      (labels ((update-f (x ht)
		 (let ((hx (if (eq key #'identity) x (funcall key x))))
		   (macrolet ((updatef (place)
				`(if (eq only 'default)
				     (setf ,place 1)
				     (incf ,place))))
		     (if (eq key #'identity)
			 (locally (declare (sb-ext:muffle-conditions
					    style-warning))
			   (updatef (gethash hx ht 0)))
			 (multiple-value-bind (hval h-p)
			     (gethash hx ht)
			   (if h-p
			       (updatef (car hval))
			       (setf (gethash hx ht) (cons 1 x))))))))
	       (retrieve-f (k v)
		 (let ((count (if (eq key #'identity) v (car v)))
		       (value (if (eq key #'identity) k (cdr v))))
		   (declare (type fixnum count))
		   (values (funcall countp count) value)))
	       (uniq (sequence test)
		 (let ((ht (make-hash-table :test test))
		       (result))
		   (map nil
			(lambda (x) (update-f x ht))
			sequence)
		   ;; loop is faster for lists
		   ;;(loop for s in sequence do (update-f s ht))
		   (maphash (lambda (k v)
			      (multiple-value-bind (include-p value)
				  (retrieve-f k v)
				(if include-p
				    (setf result (cons value result)))))
			    ht)
		   result)))
	(declare (inline uniq update-f retrieve-f))
	(uniq sequence test)))))

(defun dump-to-file (file open-options item)
  (if (= 0 (getf open-options :if-exists 0))
      (setf (getf open-options :if-exists) :overwrite))
  (if (= 0 (getf open-options :if-does-not-exist 0))
      (setf (getf open-options :if-does-not-exist) :create))
  (let ((stream (apply #'open file :direction :output open-options)))
    (pprint stream)
    (pprint item)
    (when (not (null stream))
	(pprint item stream)
	(terpri stream)
	(close stream))))

(defmacro no-error (((error-type default-value) &rest error-values)
		    &body body)
  (let ((error-values (append (list (list error-type default-value))
			      error-values)))
    (with-gensyms (tag)
      `(block ,tag
	 (handler-bind (,@(loop for ev in error-values
			     for e = (first ev) for v = (second ev)
			     collect `(,e (lambda (c)
					    (declare (ignore c))
					    (return-from ,tag ,v)))))
	   ,@body)))))

(defun measure-timediff (time-body1 time-body2
			 &key significance maxtime showtimes)
  (declare (type (real 0 1) significance))
  (labels ((measure (iters times1 times2)
	     (push (funcall time-body1 iters) times1)
	     (push (funcall time-body2 iters) times2)
	     (let* ((p (no-error ((arithmetic-error 1))
			 (statistics:t-test-two-sample-on-sequences times1
								    times2)))
		    (totaltime (+ (apply #'+ times1) (apply #'+ times2)))
		    (signif (<= p significance)))
	       (if (or signif (>= totaltime maxtime))
		   (let* ((mean-sd-n1 (multiple-value-list
				       (statistics:mean-sd-n times1)))
			  (mean-sd-n2 (multiple-value-list
				       (statistics:mean-sd-n times2)))
			  (mean-diff (- (first mean-sd-n1) (first mean-sd-n2))))
		     (when showtimes
		       (prind times1) (prind times2)
		       (if signif
			   (format t "Body1 is ~F ms ~A per call than Body2~%"
				   (/ (abs mean-diff) iters .001)
				   (if (< mean-diff 0) "faster" "slower"))))
		     (values (list p signif)
			     (list mean-diff (/ mean-diff iters))
			     mean-sd-n1 mean-sd-n2
			     iters))
		   (measure iters times1 times2)))))
    (let ((iters (exp-until-predicate 1
				      (lambda (i)
					(+ (funcall time-body1 i)
					   (funcall time-body2 i)))
				      (lambda (x) (>= x .05)))))
      (measure iters
	       (list (funcall time-body1 iters))
	       (list (funcall time-body2 iters))))))

(defmacro timediff (body1 body2
		    &key (significance .01) (maxtime .5) showtimes disable-gc)
  (with-gensyms (iter body)
    `(macrolet ((measure (,iter ,body)
		  `(timeit (,,iter :disable-gc ,',disable-gc) ,@,body)))
       (labels ((run-body1 (,iter)
		  (measure ,iter ,body1))
		(run-body2 (,iter)
		  (measure ,iter ,body2)))
	 (measure-timediff #'run-body1 #'run-body2
			   :significance ,significance
			   :maxtime ,maxtime
			   :showtimes ,showtimes)))))



;;(defun sequence-assemble (sequences starts ends)
;;  "creates a sequence of type "


;(defun foldl  == reduce
; unfold p f g seed == (loop for x = seed then (g x) until (p x) collect (f x))


;; implement tests using eval-when :compile-toplevel?

;; check out cl-series (functional!!! replacement for LOOP)
