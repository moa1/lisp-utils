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

(defmacro with-gensyms (symbols &body body)
  ;;(declare (type unique-list symbols))
  `(let ,(loop for symbol in symbols collect `(,symbol (gensym)))
     ,@body))

(defmacro timeit (repeat &body body)
  (declare (type integer repeat))
  (with-gensyms (start stop)
    `(progn
       (let ((,start (get-internal-real-time)))
	 ,@(flatten-1 (append (repeat body repeat))) ; how to force evaluation?
	 (let ((,stop (get-internal-real-time)))
	   (float (/ (- ,stop ,start) internal-time-units-per-second)))))))

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
  (with-gensyms (i)
    (let* ((up (if incl 'upto 'below))
	   (down (if incl 'downto 'above)))
      (cond ((and (numberp start) (numberp stop) (numberp step))
	     `(quote ,(range start :stop stop :step step :incl incl)))
	    ((not (numberp step))
	     form)
	    ((> step 0)
	     `(loop for ,i from ,start ,up ,stop by ,step collect ,i))
	    ((< step 0)
	     `(loop for ,i from ,start ,down ,stop by (- ,step) collect ,i))
	    (t form)))))

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
  (warn "deprecated #'const-fun: use #'constantly instead")
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


(defmacro stupid-check (check &key (msg "stupid-check-error:"))
  ;; this macro is full of brain-deadness
  (with-gensyms (result)
    (let ((msg-r msg))
      `(let ((,result ,check))
	 (if ,result
	     t
	     (error ,msg-r))))))

(defun sreplace-test ()
  ;; this function is full of brain-deadness
  (dolist (foo
	    '(((sreplace "abc123" "xxx") "xxx")
	      ((sreplace "abc123" "xxx" :start1 0 :end1 3) "xxx123")
	      ((sreplace "abc123" "xxx" :start1 0 :end1 3 :start2 1) "xx123")
	      ((sreplace "abc123" "xxx" :start1 3 :end1 3) "abcxxx123")
	      ((sreplace "abc123" "xyz" :start1 3 :end1 3 :start2 1 :end2 2)
	       "abcy123")
	      ((sreplace "abc123" "xxx" :start1 3 :end1 3) "abcxxx123")))
    (stupid-check (equal (eval (car foo)) (eval (cadr foo)))
		  :msg (format nil "~A" foo))))

(sreplace-test)

;; (sreplace "abc123" "xxx") "xxx"
;; (sreplace "abc123" "xxx" :start1 0 :end1 3) "xxx123"
;; (sreplace "abc123" "xxx" :start1 0 :end1 3 :start2 1) "xx123"
;; (sreplace "abc123" "xxx" :start1 1 :end1 1) "abcxxx123"
;; (sreplace "abc123" "xyz" :start1 3 :end1 3 :start2 1 :end2 2) "abcy123"
;; (sreplace "abc123" "xxx" :start1 3 :end1 3) "abcxxx123"

;;(defun sequence-assemble (sequences starts ends)
;;  "creates a sequence of type "


;(defun foldl  == reduce
; unfold p f g seed == (loop for x = seed then (g x) until (p x) collect (f x))


;; implement tests using eval-when :compile-toplevel?

;; check out cl-series (functional!!! replacement for LOOP)
