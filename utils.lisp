;; this in-package is neccessary so that symbols :use-d in defpackage
;; (see package.lisp) will be visible in this file
(in-package :utils)
(use-package :alexandria)

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

(defun nflatten (l)
  (declare (type list l))
  "Return a flattened list (i.e. no tree structure in the list)"
  (labels ((rec (l acc)
	     (cond ((null l) acc)
		   ((consp (car l)) (rec (cdr l) (nconc (rec (car l) nil) acc)))
		   (t (rec (cdr l) (nconc (list (car l)) acc))))))
    (nreverse (rec l nil))))

(defun flatten-1 (l)
  "Return a tree with the outermost level of tree structure removed, only
the modified structure is copied.
e.g. (((1)) 2 (3 4)) -> ((1) 2 3 4), the (1) in both is eq.
"
  (labels ((rec (l acc)
	     (cond
	       ((null l) (nreverse acc))
	       (t (rec (cdr l) (if (consp (car l))
				   (nconc (reverse (car l)) acc)
				   (cons (car l) acc)))))))
    (rec l nil)))

(defun last1 (list)
  "Return the last element of LIST."
  (car (last list)))

(defmacro pushend (list1 &rest lists)
  "Concatenate LIST1 and all LISTS, using #'NCONC, and set LIST1 to the resulting list."
  `(setf ,list1 (apply #'nconc ,list1 ,@(butlast lists) (list ,(last1 lists)))))

(defmacro pushfront (list-last &rest lists)
  "Concatenate all LISTS and LIST-LAST, using #'NCONC, and set LIST-LAST to the resulting list."
  `(setf ,list-last (apply #'nconc ,@lists (list ,list-last))))

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

;; is defined by alexandria, but we want it independently of that
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
			&optional orig-form
			&key (it-parameter 1) ana-documentation)
  "(Re)Define the macro ANA-NAME, which first binds the parameter number
IT-PARAMETER (starting at 1) to the symbol it, and then evaluates and uses all
other parameters in FORM, which gets documentation ANA-DOCUMENTATION."
  (let* ((form (if (null orig-form)
		   (read-from-string (subseq (string ana-name) 1))
		   orig-form))
	 (ana-doc
	  (if (null ana-documentation)
	      (let ((old-doc (or
			      (documentation form 'function)
			      (format nil "~A has no documentation." form))))
		(format nil
			"Like ~A, but the ~:R parameter is evaluated before all others are~%evaluated, and its value is available as symbol it in all other parameter forms.~%~%Documentation for ~A:~%~A"
			form it-parameter form old-doc)))))
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
       ,ana-doc
       (let ((rest-head (subseq rest 0 ,(1- it-parameter)))
	     (rest-tail (subseq rest ,it-parameter)))
	 `(let ((it (,@(nth ,(1- it-parameter) rest))))
	    (,',form ,@rest-head it ,@rest-tail))))))

(defanaphoric arplacd)
(defanaphoric avalues)

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

(defmacro timeit ((repeat &key stats unroll print) &body body)
  "Return the total time of running BODY (number REPEAT) times. If STATS is T,
measure (with possible overhead) and additionally return statistics (min, mean,
max) of each execution duration.
If UNROLL is T, unroll the repetitions."
  (with-gensyms (start stop times i)
    `(progn
       (let (,start ,stop ,@(if stats `((,times (make-array ,repeat)))))
	 (setq ,start (get-internal-real-time))
	 (do-unrollable ,unroll
	     ((,i 0 (1+ ,i))) ((>= ,i ,repeat))
	   ,@(append (if stats
			 `((setf (elt ,times ,i)
				 (get-internal-real-time))))
		     body))
	 (setq ,stop (get-internal-real-time))
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
		 ,@(when print `((format t "~A~%~A~%~A~%~A~%"
					 ,start
					 (reduce #'min ,times)
					 (/ (reduce #'+ ,times)
					    ,repeat)
					 (reduce #'max ,times))))
		 (values ,start
			 (reduce #'min ,times)
			 (/ (reduce #'+ ,times)
			    ,repeat)
			 (reduce #'max ,times)))
	      `(progn 
		 ,@(when print `((format t "~A~%" ,start)))
		 ,start))))))

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
  (warn "deprecated within: use alexandria::clamp instead")
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
	     (if (consp list)
		 (rec (nthcdr n list) (cons (car list) acc))
		 (nreverse acc))))
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

(defun mapc-arrays-major (function &rest arrays)
  "Consider each array as a vector by viewing its elements in row-major order,
and call function with all indices of the vector. Return arrays."
  (let ((size (apply 'min (mapcar (lambda (x) (array-total-size x)) arrays))))
    (loop for i below size do
	 (funcall function i))
    arrays))

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

(defun extract-keywords (list)
  "Return two values: The first represents keyword bindings occurring in LIST as an alist, the second the LIST without the keyword bindings.
Ex: (extract-keywords '((1 2) :a b (3 4) :c d))
    == (values '((:c . d) (:a . b)) '((1 2) (3 4)))"
  (let ((keyword-bindings nil)
	(new-list nil))
    (labels ((rec (list)
	       (when (not (null list))
		 (if (keywordp (car list))
		     (progn
		       (if (or (null (cdr list)) (not (symbolp (cadr list))))
			   (error "Symbol expected after keyword ~A." (car list))
			   (push (cons (car list) (cadr list)) keyword-bindings))
		       (rec (cddr list)))
		     (progn
		       (push (car list) new-list)
		       (rec (cdr list)))))))
      (rec list)
      (values keyword-bindings (nreverse new-list)))))

(defun function-specializer (names-and-parameters lambda-list body &key recurse-symbol)
  "Return a function form suitable for flet or labels.
RECURSE-SYMBOL must be a symbol. If it is non-NIL, its occurrences in BODY are replaced by the respective defined function name.
Example: (function-specializer '((test1 (a 1)) (test2 (a 2))) '() 'a)"
  (declare (type symbol recurse-symbol))
  ;;(format t "names-and-parameters:~A~%" names-and-parameters)
  (loop for name-and-plist in names-and-parameters collect
       (destructuring-bind (name plist) name-and-plist
	 `(,name ,lambda-list
		 (symbol-macrolet (,@plist)
		   (locally
		       #+sbcl (declare (sb-ext:muffle-conditions
					sb-ext:code-deletion-note))
		       ,@(if recurse-symbol
			     (subst name recurse-symbol body)
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
      (hl-last '(1 2 3)))
Note that out of specializing-flet, specializing-labels and specializing-defun,
only specializing-flet doesn't accept a recursion-symbol (because functions
defined via flet cannot recurse)."
  (push (list name bindings) name-defs)
  `(flet ,(function-specializer name-defs lambda-list function-body)
     ,@body))

(defmacro specializing-labels (((name bindings)
				&rest name-defs)
			       (recurse-symbol lambda-list &body function-body)
			       &body body)
  "Define several local functions specified by NAME-DEFS with FUNCTION-BODY.
For each function named NAME, the BINDINGSs are symbol-macrolet in BODY.
If RECURSE-SYMBOL is non-NIL, then its occurrences in each FUNCTION-BODY are replaced with the respective NAME.
Ex: (specializing-labels ((add ((op :add)))
                          (sub ((op :sub))))
        (rec (list result)
          (if (null list)
              result
              (rec (cdr list) (ecase op
                                (:add (+ result (car list)))
	                        (:sub (- result (car list)))))))
      (add '(1 2 3) 0))"
  (push (list name bindings) name-defs)
  `(labels ,(function-specializer name-defs lambda-list function-body
				  :recurse-symbol recurse-symbol)
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
			      recurse-symbol lambda-list &body body)
  "Define several local functions specified by NAME-DEFS with FUNCTION-BODY.
For each function named NAME, the BINDINGSs are symbol-macrolet in BODY.
In each FUNCTION-BODY, if non-NIL, the symbol RECURSE-SYMBOL is replaced with the respective NAME.
Ex: (specializing-defun
        ((hl-head ((ht t)))
         (hl-last ((ht nil))))
        (list &optional (n 1))
      (if ht
          (head list n)
          (last list n)))"
  (push (list name bindings) name-defs)
  `(progn
     ,@(loop for s in (function-specializer name-defs lambda-list body :recurse-symbol recurse-symbol)
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
  ;; TODO: modify the pretty print dispatch table so that it prints representations readable by #'READ. (especially modify the table so that printing a float respects *print-base*.)
  (let ((i (gensym "I")))
    `(let ((*print-pretty* t)
	   (*print-right-margin* most-positive-fixnum))
       ,@(loop for a in args collect
	      (if (eq a T)
		  `(format t "~%")
		  `(progn
		     (format t "~A:" ,(format nil "~A" a))
		     (dolist (,i (handler-case (multiple-value-list ,a)
				   (error (e) (list e))))
		       (prin1 ,i)
		       (princ " ")))))
       (format t "~%"))))

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
  (with-gensyms (arguments)
    `(lambda (&rest ,arguments)
       (destructuring-bind ,lambda-list ,arguments
	 ,@body))))

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

(defmacro timecps ((repeat &key stats unroll (time 0.1)) &body body)
  "Like TIMEIT, but return calls per second. TIME is the approximate measuring
time.
Note: When :STATS is T, TIMEIT returns total, min, mean, and max as values."
  (with-gensyms (run iters timeit-call measure-iters)
    `(macrolet ((,timeit-call (,iters)
		  `(timeit (,,repeat :stats ,,stats
				     :unroll ,,unroll)
		     (progn-repeat (,,iters)
		       ,',@body))))
       (labels ((,measure-iters (,iters)
		  (multiple-value-bind (,iters ,run)
		      (exp-until-predicate ,iters
					   (lambda (x) (,timeit-call x))
					   (lambda (x &rest r)
					     (declare (ignore r))
					     (>= x 0.005)))
		    ;; floor here instead of ceiling was a bug: 0 iterations were possible
		    (ceiling (/ (* ,time ,iters) ,run)))))
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

;; remove-duplicates is a cl function, but has different options
(defun unique (sequence &key only not countp (test #'eql) (key #'identity) (ordered nil))
  "Return a list with all but one duplicate elements in SEQUENCE removed. This function has similar functionality to the UNIX tool uniq.
If KEY is non-NIL the result of calling KEY with an element is used in the comparison.
Equality is tested with TEST.
Only one of ONLY, NOT, or COUNTP may be non-NIL. If ONLY is T, return only the unique elements. If NOT is T, return only the elements that have at least one duplicate. If COUNTP is non-NIL, return only the elements whose count satisfies COUNTP.
If ORDERED is NIL, the order of the returned elements is not specified. Otherwise, the order of the elements in SEQUENCE is preserved.
The run-time of this function is linear in the number of elements of SEQUENCE."
  (assert (>= 1 (count-if (complement #'null) (list only not countp))))
  (specializing-cond-let
      ((only ((countp (lambda (x) (= x 1)))))
       (not ((countp (lambda (x) (> x 1)))))
       ((not countp) ((countp (constantly t)) (only 'default)))
       (t ()))
    (specializing-case key (#'identity)
      (labels ((update (x ht)
		 (let ((hx (if (eq key #'identity) x (funcall key x))))
		   (macrolet ((updatef (place)
				`(if (eq only 'default)
				     (setf ,place 1)
				     (incf ,place))))
		     (if (eq key #'identity)
			 #+sbcl (locally (declare (sb-ext:muffle-conditions
						   style-warning))
				  (updatef (gethash hx ht 0)))
			 #-sbcl (updatef (gethash hx ht 0))
			 (multiple-value-bind (hval h-p)
			     (gethash hx ht)
			   (if h-p
			       (updatef (car hval))
			       (setf (gethash hx ht) (cons 1 x))))))))
	       (retrieve (k v)
		 (let ((count (if (eq key #'identity) v (car v)))
		       (value (if (eq key #'identity) k (cdr v))))
		   (declare (type fixnum count))
		   (values (funcall countp count) value)))
	       (uniq (sequence test)
		 (let* ((ht (make-hash-table :test test))
			(result (if ordered (cons nil nil) nil))
			(tail result))
		   (if (typep sequence 'list)
		       (loop for s in sequence do (update s ht))
		       (loop for s across sequence do (update s ht)))
		   (labels ((unordered-check (k v)
			      (multiple-value-bind (include-p value)
				  (retrieve k v)
				(when include-p
				  (setf result (cons value result)))))
			    (unordered ()
			      (loop for k being the hash-key of ht
				 using (hash-value v) do
				   (unordered-check k v))
			      result)
			    (ordered-check (k v)
			      (multiple-value-bind (include-p value)
				  (retrieve k v)
				(when include-p
				  (setf (cdr tail) (cons value nil) tail (cdr tail)))))
			    (ordered ()
			      (if (typep sequence 'list)
				  (loop for k in sequence do
				       (ordered-check k (gethash k ht)))
				  (loop for k across sequence do
				       (ordered-check k (gethash k ht))))
			      (cdr result)))
		     (declare (inline unordered-check unordered ordered-check ordered))
		     (if ordered
			 (ordered)
			 (unordered))))))
	(declare (inline uniq update retrieve))
	(uniq sequence test)))))

;; (The following comment is an artifact due to #'SXHASH, which seems to compute ordered hash-values for ordered integers: CL-USER> (LOOP FOR I BELOW 10 COLLECT (SXHASH I)): (361475658 361475674 361475691 361475707 361475592 361475608 361475625 361475641 361475790 361475806).) This function has a subset of the functionality of #'UNIQUE. I leave this function here, because for some reason, this function always preserves the order of elements, irrespective of the value of ORDERED. #'UNIQUE (above) doesn't do that. Demonstration: (LET ((LIST '(1 2 3 4 1 3 0))) (PRINT (UTILS:UNIQUE LIST :ONLY T)) (PRINT (UTILS:UNIQUE-HAE? LIST :ORDERED T)) (UTILS:UNIQUE-HAE? LIST :ORDERED NIL))
(defun unique-hae? (list &key (test #'eql) (ordered nil))
  "Return the list of elements that occur only once in LIST. The comparison of the elements is done using TEST.
It computes the unique elements in linear time.
If ORDERED is non-NIL, it preserves the order of the elements."
  (let* ((ht (make-hash-table :test test))
	 (result (cons nil nil))
	 (tail result))
    (flet ((insert-list (list)
	     (loop for element in list do
		  (incf (gethash element ht 0))))
	   (build-result-ordered ()
	     (loop for element in list do
		  (when (= 1 (gethash element ht))
		    (setf (cdr tail) (cons element nil)
			  tail (cdr tail)))))
	   (build-result-unordered ()
	     (loop for element being the hash-key of ht do
		  (when (= 1 (gethash element ht))
		    (setf (cdr tail) (cons element nil)
			  tail (cdr tail))))))
      (insert-list list)
      (if ordered
	  (build-result-ordered)
	  (build-result-unordered))
      (cdr result))))

(defun dump-to-file (file open-options item)
  (if (eq 0 (getf open-options :if-exists 0))
      (setf (getf open-options :if-exists) :supersede))
  (if (eq 0 (getf open-options :if-does-not-exist 0))
      (setf (getf open-options :if-does-not-exist) :create))
  (let ((stream (apply #'open file :direction :output open-options)))
    (when (not (null stream))
      (pprint item stream)
      ;;(terpri stream)
      (close stream))))

(defun dump-list (list filename &key (control-string "~A ") (open-options))
  "Write list LIST to file FILENAME (overwriting if it exists) using the control string CONTROL-STRING for each item in the list."
  (with-open-file (stream filename :direction :output :if-exists (getf open-options :if-exists :supersede) :if-does-not-exist (getf open-options :if-does-not-exist :create))
    (loop for i in list do
	 (format stream control-string i))))

(defmacro no-error (((error-type default-value) &rest error-values)
		    &body body)
  (warn "deprecated no-error, use handler-case instead")
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

(defparameter *measurable-time* 0.05)

(defun measure-timediff (time-function1 time-function2
			 &key significance maxtime showtimes)
  (declare (type (real 0 1) significance))
  (labels ((measure (iters times1 times2)
	     (push (funcall time-function1 iters) times1)
	     (push (funcall time-function2 iters) times2)
	     (let* ((p (handler-case
			   (statistics:t-test-two-sample-on-sequences times1
								      times2)
			 (simple-error () 1) ;this occurs when one of the SD = 0.0.
			 (arithmetic-error () 1)))
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
		       (format t "Body1 mean ~F ms. Body2 mean ~F ms.~%"
			       (/ (first mean-sd-n1) iters .001)
			       (/ (first mean-sd-n2) iters .001))
		       (when signif
			 (format t "Body1 is ~F ms ~A (~3F speedup) per call than Body2.~%"
				 (/ (abs mean-diff) iters .001)
				 (if (< mean-diff 0) "faster" "slower")
				 (/ (first mean-sd-n2) (first mean-sd-n1)))))
		     (values (list p signif)
			     (list mean-diff (/ mean-diff iters))
			     mean-sd-n1 mean-sd-n2
			     iters))
		   (measure iters times1 times2)))))
    (let ((iters (exp-until-predicate 1
				      (lambda (i)
					(+ (funcall time-function1 i)
					   (funcall time-function2 i)))
				      (lambda (x) (>= x *measurable-time*)))))
      (measure iters
	       (list (funcall time-function1 iters))
	       (list (funcall time-function2 iters))))))

(defmacro timediff (form1 form2
		    &key (significance .01) (maxtime .5) showtimes)
  "Measure the run times of FORM1 and FORM2 multiple times until one of them is significantly faster than the other, or until MAXTIME is exhausted.
Returns many values, in this format:
  (p-value significant-p)
  (mean-diff mean-diff-of-one-iteration)
  (mean1 standard-deviation1 iterations)
  (mean2 standard-deviation2 iterations)
  number-of-form1-and-form2-loops-to-obtain-measurable-runtime"
  (with-gensyms (iter body)
    `(macrolet ((measure (,iter ,body)
		  `(timeit (,,iter) ,,body)))
       (labels ((run-body1 (,iter)
		  (measure ,iter ,form1))
		(run-body2 (,iter)
		  (measure ,iter ,form2)))
	 (measure-timediff #'run-body1 #'run-body2
			   :significance ,significance
			   :maxtime ,maxtime
			   :showtimes ,showtimes)))))

(defun length>= (l minl)
  "Return T if L has a length of at least MINL, NIL otherwise."
  ;; this is faster than (>= (length l) minl)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum minl))
  (if (eq minl 0)
      t
      (if (null l)
	  nil
	  (length>= (cdr l) (1- minl)))))

(defun choice (sequence)
  "Return a random element of SEQUENCE"
  (let ((len (length sequence)))
    (if (<= len 1)
	(elt sequence 0)
	(let ((i (random len)))
	  (elt sequence i)))))

(defmacro acond (&rest clauses)
  "Like COND, but the test value of CLAUSES is accessible as symbol IT in the
other clauses."
  (labels ((gen-if (cs)
	     (if (null cs)
		 nil
		 (let ((c (car cs)))
		   `(let ((it ,(car c)))
		      (if it
			  ,@(cdr c)
			  ,(gen-if (cdr cs))))))))
    (gen-if clauses)))

(defun flatten-n (n l)
  "flatten-1 the list L N times."
  (if (= 0 n)
      l
      (flatten-n (1- n) (flatten-1 l))))

(defun reduce-binary-index (function sequence &key (key #'identity))
  "Return the index of an element of SEQUENCE. Starting with A as the first
element, and the next element E, the predicate (FUNCTION E A) is called.
If it returns T, A becomes E. E becomes the next element.
FUNCTION should be transitive, i.e.
if (FUNCTION a b) and (FUNCTION b c), then (FUNCTION a c).
If KEY is given, it is used to extract the values of elements."
  (declare (type (function (t t) boolean) function)
	   (type (function (t) t) key)
	   (type sequence sequence))
  (specializing-case key (#'identity)
    (let ((index 0)
	  (val (funcall key (elt sequence 0)))
	  (len (length sequence)))
      (do ((j 1 (1+ j)))
	  ((>= j len) index)
	(let ((jval (funcall key (elt sequence j))))
	  (when (funcall function jval val)
	    (setf index j)
	    (setf val jval)))))))

(defun min-index (sequence &key (key #'identity))
  (reduce-binary-index #'<= sequence :key key))

(defun max-index (sequence &key (key #'identity))
  (reduce-binary-index #'>= sequence :key key))

(defun split (sequence sep)
  "Return a list of subsequences of SEQUENCE, split by the element SEP"
  (labels ((rec (rest list)
	     (let ((next (position sep rest)))
	       (if (null next)
		   (nreverse (cons rest list))
		   (rec (subseq rest (1+ next))
			(cons (subseq rest 0 next) list))))))
    (rec sequence nil)))
    
(defun ith (i sequence)
  (elt sequence (if (< i 0) (+ (length sequence) i) i)))

(defun set-ith (i sequence v)
  (setf (elt sequence (if (< i 0) (+ (length sequence) i) i)) v))

(defsetf ith set-ith)

(defun sigmoid (x)
  "sigmoid of x. Fails for x > 88.7 or x>709.7d0"
  (/ 1.0 (1+ (exp x))))

(defun all-eq-lengths (&rest sequences)
  (apply #'= (mapcar #'length sequences)))

(defun read-lines (filename)
  "read all lines from FILENAME and return them as list of lines."
  (with-open-file (stream filename)
    (labels ((rec (words)
	       (multiple-value-bind (line missing-newline-p)
		   (read-line stream)
		 (if missing-newline-p
		     (nreverse (cons line words))
		     (rec (cons line words))))))
      (rec nil))))
      
(defun extend-decision-tree (tree new-leaf new-path)
  "Given a decision tree TREE, follow a path NEW-PATH to reach leaf NEW-LEAF.
If NEW-LEAF or NEW-PATH don't yet exist, add them to the decision tree.
Returns the modified decision tree."
  (if (null new-path)
      (let ((leaves (assoc :leaves tree))) ;following types has led to tree: extend it with op
	(if (null leaves)
	    (acons :leaves (list new-leaf) tree) ;create new alist with :leaves containing new-leaf
	    (progn
	      (setf (cdr leaves) (cons new-leaf (cdr leaves)))
	      tree
	      ))) ;extend :leaves leaf with new-leaf
      (let* ((top (car new-path)) ;follow tree using top
	     (subtree (assoc top tree))
	     (rem-path (cdr new-path)))
	(if (null subtree)
	    (let ((new-tree (extend-decision-tree nil new-leaf rem-path))) ;type didn't exist: create it
	      (acons top new-tree tree))
	    (progn
	      (setf (cdr subtree) (extend-decision-tree (cdr subtree) new-leaf rem-path)) ;follow type and save potentially new returned tree
	      tree)))))

;; (defun product-cases ...) was here, use alexandria::map-product instead

(defun min-keep-type (&rest numbers)
  "Same as min, but keeps the original type of the return value."
  (let ((m (car numbers)))
    (dolist (number (cdr numbers))
      (when (< number m)
	(setf m number)))
    m))

(defun max-keep-type (&rest numbers)
  "Same as max, but keeps the original type of the return value."
  (let ((m (car numbers)))
    (dolist (number (cdr numbers))
      (when (> number m)
	(setf m number)))
    m))

(defvar *timeitf-compile-batch* 128 "timeitf's number of calls without a loop which are compiled.")

(defun timeitf (function repeats &optional (compile-batch *timeitf-compile-batch*))
  "Return the number of seconds needed to execute FUNCTION REPEATS times.
Do this by compiling a function which calls up to COMPILE-BATCH times in a row without a loop."
  (declare (optimize (debug 3)))
  (let* ((p (if (< repeats compile-batch)
		`(lambda () ,@(loop for i below repeats collect `(funcall ,function)))
		(let* ((p-batch (loop for i below compile-batch collect `(funcall ,function)))
		       (calls-batch (floor repeats compile-batch))
		       (calls-remain (- repeats (* calls-batch compile-batch)))
		       (p-remain (loop for i below calls-remain collect `(funcall ,function))))
		  `(lambda ()
		     (loop for i below ,calls-batch do ,@p-batch)
		     ,@p-remain)))))
    (multiple-value-bind (compiled-time-function warnings-p failure-p) (compile nil p)
      (declare (ignore warnings-p))
      (when failure-p
	(error "compiling p failed"))
      (let ((start (get-internal-real-time)))
	(funcall compiled-time-function)
	(let ((stop (get-internal-real-time)))
	  (/ (- stop start) internal-time-units-per-second))))))

(defun time-min-repeats-measurable (function min-seconds number-measurable &optional (compile-batch *timeitf-compile-batch*))
  "Exponentially increase number of repeats until calling FUNCTION this often takes at least MIN-SECONDS seconds in all of NUMBER-MEASURABLE tries."
  (do* ((rep 1 (* rep 2))) (nil nil)
    (when (loop for j below number-measurable always (let ((sec (timeitf function rep compile-batch)))
						       (>= sec min-seconds)))
      (return-from time-min-repeats-measurable rep))))

(defun timesec (function &key (min-out-of 18) (measurable-seconds 0.05) (measurable-repeats 5) (compile-batch *timeitf-compile-batch*))
  ;; min-out-of is 18, so that a measurement with measurable-seconds=0.05 takes about 2 seconds. This is because function min-repeats-measurable takes about 0.05sec * 2.
  ;; TODO: maybe use SBCL's SB-VM::with-cycle-counter to measure the number of elapsed cpu cycles. Example: (sb-vm::with-cycle-counter (+ 1 100)). The first value is the first value of the body, the second value the number of elapsed cpu cycles when evaluating the body.
  "Measure how long a function takes at least to execute, out of MIN-OUT-OF times.
Return the seconds per repeat, the same number as a long-float, and the number of repeats which was needed to get a running time of at least MEASURABLE-SECONDS for all of MEASURABLE-REPEATS tries.
COMPILE-BATCH is the number of compiled calls to FUNCTION which are made not by a loop."
  (let* ((rep (time-min-repeats-measurable function measurable-seconds measurable-repeats compile-batch))
	 (times (loop for i below min-out-of collect (timeitf function rep compile-batch)))
	 (min (/ (apply #'min-keep-type times) rep)))
    (values min (coerce min 'long-float) rep)))

(defun time-distribution (function &key (min-time 2.0) (min-measures 20))
  (labels ((timeit (function)
	     ;;(cl-user::gc) very slow
	     #+SBCL (nth-value 1 (sb-vm::with-cycle-counter (funcall function)))
	     #-SBCL (timeitf function 1)
	     ))
    (let ((times nil))
      (do* ((i 0 (1+ i))
	    (start (get-internal-real-time))
	    (now (get-internal-real-time) (get-internal-real-time))
	    (elapsed (/ (- now start) internal-time-units-per-second) (/ (- now start) internal-time-units-per-second)))
	   ((and (> elapsed min-time) (>= i min-measures)))
	(push (timeit function) times))
      times)))
  
;; this function seems to take about 0.61 the time of PROPER-LIST-P from ALEXANDRIA, but doesn't handle circular lists like ALEXANDRIA's does.
(defun unsafe-proper-list-p (object)
  "OBJECT must not be a circular list."
  (if (null object)
      t
      (and (listp object) (null (cdr (last object))))))

(defun any-hash-table-entry (ht &optional default-key default-value)
  "Return any key and value stored in hash-table HT, or the values DEFAULT-KEY and DEFAULT-VALUE."
  (with-hash-table-iterator (next ht)
    (multiple-value-bind (present-p key value) (next)
    (if present-p
	(values key value)
	(values default-key default-value)))))

(defun any-hash-table-key (ht &optional default-key)
  "Return any key stored in hash-table HT, or DEFAULT-KEY."
  (with-hash-table-iterator (next ht)
    (multiple-value-bind (present-p key value) (next)
      (declare (ignore value))
      (if present-p
	  key
	  default-key))))

(defun any-hash-table-value (ht &optional default-value)
  "Return any value stored in hash-table HT, or DEFAULT-VALUE."
  (with-hash-table-iterator (next ht)
    (multiple-value-bind (present-p key value) (next)
      (declare (ignore key))
      (if present-p
	  value
	  default-value))))

;;(defun sequence-assemble (sequences starts ends)
;;  "creates a sequence of type "


;(defun foldl  == reduce
; unfold p f g seed == (loop for x = seed then (g x) until (p x) collect (f x))

; swap == rotatef

;; implement tests using eval-when :compile-toplevel?

;; check out cl-series (functional!!! replacement for LOOP)
