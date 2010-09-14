(in-package :utils)

(defun member-tree-slow (item tree &key key (test #'eql) breadthfirst)
  (labels ((rec (list)
	     (cond ((funcall test
			     item
			     (if key (funcall key (car list)) (car list)))
		    list)
		   ((consp (car list))
		    (if breadthfirst
			(or (rec (cdr list)) (rec (car list)))
			(or (rec (car list)) (rec (cdr list)))))
		   (t (if (consp (cdr list))
			  (rec (cdr list)))))))
    (rec tree)))

(defun member-tree (item tree &key key (test #'eql) breadthfirst)
  (specializing-labels ((helper+key+test ((akey key) (atest test)))
			(helper-key+test ((akey nil) (atest test)))
			(helper+key-test ((akey key) (atest nil)))
			(helper-key-test ((akey nil) (atest nil))))
      ((list)
       (cond ((if atest
		  (funcall test
			   item
			   (if akey (funcall akey (car list)) (car list)))
		  (eql item (if akey (funcall akey (car list)) (car list))))
	      list)
	     ((consp (car list))
	      (if breadthfirst
		  (or (rec (cdr list)) (rec (car list)))
		  (or (rec (car list)) (rec (cdr list)))))
	     ((consp (cdr list))
	      (rec (cdr list)))
	     (t nil)))
    (if (null tree)
	nil
	(if key
	    (if (eq test #'eql)
		(helper+key-test tree)
		(helper+key+test tree))
	    (if (eq test #'eql)
		(helper-key-test tree)
		(helper-key+test tree))))))

(defun test-unroll-do ()
  (declare (optimize (debug 3)))
  (let* ((riters nil)
	 (res (unroll-do
		  ((a 1 (1+ a)) (b 0) c)
		  ((>= a 4) (incf a) (list a b c))
		(push (list a b c) riters)
		nil))
	 (iters (nreverse riters)))
    (assert (and (equal iters '((1 0 nil) (2 0 nil) (3 0 nil)))
		 (equal res '(5 0 nil))))))
(test-unroll-do)
