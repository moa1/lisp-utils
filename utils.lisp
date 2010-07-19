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

(defmacro timeit (&body body)
  (let* ((start (gensym))
	 (stop (gensym)))
    `(progn
       (let ((,start (get-internal-run-time)))
	 ,@body
	 (let ((,stop (get-internal-run-time)))
	   (- ,stop ,start))))))
