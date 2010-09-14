(in-package :utils)

(defun find-cos (sin2 radius2)
  (declare (type integer sin2)
	   (type integer radius2))
  (let+ ((q (- radius2 sin2))
	 (r (sqrt q))
	 (fr rr (floor r)))
	(declare (type float r)
		 (type fixnum q))
	(if (and (= rr 0.0) (= radius2 (+ (pow2 fr) sin2)))
	    fr
	    nil)))

(defun find-all-rational-angles (radius)
  (declare ;;(optimize (speed 3))
	   (type fixnum radius)
	   (inline find-cos))
  ;;(prind radius)
  (let ((radius2 (pow2 radius))
	table)
    (loop
       for sin fixnum from 0 upto radius
       for sin2 integer = (pow2 sin) do
	 (let ((cos (find-cos sin2 radius2)))
	   (if (not (null cos))
	       (let* ((sin-v (/ sin radius))
		      (cos-v (/ cos radius))
		      (angle (asin sin-v))
		      ;;(angle360 (* 180 (/ angle 3.14159265358)))
		      )
		 (declare (type float angle))
		 (push (list sin-v cos-v angle) table)))))
    table))

(defun find-all-rational-unit-vectors (radius)
  (let ((radius2 (pow2 radius))
	table)
    (loop
       for x upto radius
       for x2 = (pow2 x) do
	 (loop
	    for y upto (floor (sqrt (- radius2 x2)))
	    for y2 = (pow2 y) do
	      (let ((cos (find-cos (+ x2 y2) radius2)))
		(if (not (null cos))
		    (push (list (/ x radius) (/ y radius) (/ cos radius))
			  table)))))
    (prind radius (length table))
    table))

(defun max-unit-vector-angle-diff (radius)
  (flet ((scalar-product (a b)
	   (reduce #'+ (mapcar (lambda (a b)
				 (declare (type rational a b))
				 (* a b)) a b))))
    (let* ((table (find-all-rational-unit-vectors radius))
	   (l-table (length table))
	   (scalar (loop for i fixnum below (1- l-table)
		      for minj = (loop
				    for j from (1+ i) below l-table
				    for scalar = (scalar-product (elt table i)
								 (elt table j))
				    maximize scalar)
		      minimize minj)))
      (acos scalar))))

(defun find-good-unit-vector-radius (&optional (start 1) (stop 256))
  (let (minv)
    (loop for radius from start upto stop do
	 (let ((angle (max-unit-vector-angle-diff radius)))
	   (if (null minv)
	       (setf minv (list radius angle)))
	   (if (< angle (cadr minv))
	       (setf minv (list radius angle)))
	   (prind radius angle)))
    minv))

(define-constant +some-unit-vector-radii+
    '((425 0.15354379)
      (845 0.14608113)
      (1105 0.14608113))
  :test #'equal)

(defun max-angle-diff (radius)
  (apply #'max
	 (mappair #'- (mapcar #'caddr (find-all-rational-angles radius)))))

(defun find-good-radius (&optional (start 1) (stop 65536))
  (let ((min 1000)
	(min-radius nil)
	allmin)
    (loop for radius from start upto stop do
	 (let ((mad (max-angle-diff radius)))
	   (push (cons radius mad) allmin)
	   (if (< mad min)
	       (progn
		 (setf min mad)
		 (setf min-radius radius)
		 (prind min-radius min)))))
    (values min-radius
	    min
	    (head (sort allmin #'< :key #'cdr) 100))))

(define-constant +some-sine-radii+
    ;; root max-angle-diff
    '((9425 0.08242762)
      (10625 0.077631)
      (19825 0.07884544)
      (22525 0.066642046)
      (27625 0.052619435)
      (32045 0.052619338)
      (40885 0.046503305)
      (45305 0.037255764)
      (52925 0.043471456)
      (55250 0.052619338)
      (64090 0.0463953)) :test #'equal)

(defun generate-rational-sine-cosine-table (radius)
  (nreverse (find-all-rational-angles radius)))

(defun sine-cosine-wrapper (sine-p lookup)
  (lambda (angle)
    (let ((norm (mod angle (* 2 pi))))
      (cond
	((<= 0.0 norm (/ pi 2)) (funcall lookup norm))
	((<= (/ pi 2) norm pi) (* (funcall lookup (- pi norm))
				  (if sine-p 1 -1)))
	((<= pi norm (* pi 3/2)) (* (funcall lookup (- norm pi))
				    (if sine-p -1 -1)))
	((<= (* pi 3/2) norm) (* (funcall lookup (- (* pi 2) norm))
				 (if sine-p -1 1)))))))

(defun rational-sine-cosine-table-lookupper (root)
  (let* ((table (generate-rational-sine-cosine-table root))
 	 (min-angle (apply (compose #'- #'max)
			   (mappair #'- (mapcar #'caddr table))))
	 (len (ceiling (/ (/ pi 2) min-angle)))
	 (angle-array (make-array (list len))))
    (prind len)
    (loop
       for i below len
       for angle = (* (/ i (1- len)) (/ pi 2)) do
	 (setf (elt angle-array i)
	       (binary-search angle table :key #'caddr
			      :fail (lambda (a b)
				      (if (< (abs (- (caddr a) angle))
					     (abs (- (caddr b) angle)))
					  a b)))))
;;    (prind table len min-angle T angle-array)
    (assert (= (length (unique angle-array :test #'equal)) (length table)))
    (flet ((gen-lookup (sine-p)
	     (flet ((lookup (x)
		      (let ((e-num (floor (* (/ x pi 1/2) (1- len)))))
			(elt (map 'vector (if sine-p #'car #'cadr)
				  angle-array)
			     e-num))))
	       (sine-cosine-wrapper sine-p #'lookup))))
      (values (gen-lookup t) (gen-lookup nil)))))

(multiple-value-bind (sine cosine)
    (rational-sine-cosine-table-lookupper 45305)
  (defun sin-rational-* (angle)
    (funcall sine angle))
  (defun cos-rational-* (angle)
    (funcall cosine angle)))

(defun rational-sine-cosine-binary-searcher (root)
  (let ((table (generate-rational-sine-cosine-table root)))
    (flet ((gen-search (sine-p)
	     (flet ((lookup (x)
		      (funcall (if sine-p #'car #'cadr)
			       (binary-search
				x table :key #'caddr
				:fail (lambda (a b)
					(if (< (- x (caddr a))
					       (- (caddr a) x))
					    a b))))))
	       (sine-cosine-wrapper sine-p #'lookup))))
      (values (gen-search t) (gen-search nil)))))

(multiple-value-bind (sine cosine)
    (rational-sine-cosine-binary-searcher 45305)
  (defun sin-rational (angle)
    (funcall sine angle))
  (defun cos-rational (angle)
    (funcall cosine angle)))

(defun max-sine-error (best-sine-function sine-function)
  (apply #'max (let ((tests (loop for i from -10 upto 1010 collect
				 (* (/ i 1000) pi 2))))
		 (mapcar (lambda (x) (abs (- (funcall best-sine-function x)
					     (funcall sine-function x))))
			 tests))))

(defun test-sine-cosine (sine cosine)
  (and (< (max-sine-error #'cos cosine) .1)
       (< (max-sine-error #'sin sine) .1)
       (loop for i from -10 upto 1010
	  for angle = (/ i 1000)
	  always (= 1 (+ (pow2 (funcall sine angle))
			 (pow2 (funcall cosine angle)))))))

(assert (test-sine-cosine #'sin-rational #'cos-rational))

;; stuff for creating a binary search of the sine-cosine-table
;; takes a long time to compile though

(defmacro define-exact-sine-cosine-functions-binary-search (sine-name cosine-name root)
  ;; this does not yet look up values outside [0;pi/2]
  (declare (optimize (debug 3)))
  (multiple-value-bind (predicate win fail body)
      (binary-search-emitters nil :fail-result t)
    (declare (ignore win fail body))
    (let ((table (generate-rational-sine-cosine-table root)))
      (flet ((gen-table (function-name sine-p)
	       (labels ((table-get (table-angle)
			  (let ((p (find table-angle table
					 :test (lambda (angle elt)
						 (= (caddr elt)
						    angle)))))
			    (if sine-p (car p) (cadr p))))
			(win* (win-elt)
			  (table-get win-elt))

			(fail* (a-elt b-elt)
			  (let ((lower (table-get a-elt))
				(higher (table-get b-elt)))
			    `(if (> (- search-elt ,lower)
				    (- ,higher search-elt))
				 ,higher
				 ,lower)))
			(body* (body)
			  `(defun ,function-name (search-elt)
			     ,body)))
		 (emit-compile-binary-search (mapcar #'caddr table)
					     predicate #'win* #'fail*
					     :body-emitter #'body*))))
	`(progn
	   ,(gen-table sine-name t)
	   ,(gen-table cosine-name nil))))))
