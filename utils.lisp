(defun repeat (args)
  (declare (optimize (debug 3)))
  (declare (type list args))
  (labels ((repeat-help (l)
	     (if (null args)
		 l
		 (let* ((o (pop args))
			(n (pop args))
			(on (loop for i from 1 to n collect o)))
		   (repeat-help (append l on))))))
    (repeat-help nil)))

(defun flatten (l &optional (acc nil))
  (declare (type list l acc))
  "Return the flattened list (i.e. no more lists in the list)"
  (cond ((null l) acc)
	((consp (car l)) (flatten (cdr l) (nconc acc (flatten (car l)))))
	(t (flatten (cdr l) (nconc acc (list (car l)))))))

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

(defmacro timeit (&body body)
  (with-gensyms (start stop)
    `(progn
       (let ((,start (get-internal-run-time)))
	 ,@body
	 (let ((,stop (get-internal-run-time)))
	   (- ,stop ,start))))))

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
