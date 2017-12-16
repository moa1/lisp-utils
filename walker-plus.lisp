(defpackage :walker-plus
  (:documentation "Parsers and deparsers for some more forms.")
  (:use :cl)
  (:export
   ;; for classes: export the class and _all_ accessors on one line so that deleting a class doesn't have to consider all exports of other classes
   ;; FORMS
   :multiple-value-bind-form :vars :values :declspecs
   :values-form
   :nth-value-form :value :values
   :defun-form
   :declaim-form :declspecs
   :funcall-form :var :arguments
   :assert-form :test
   ;; END OF FORMs
   :parse-p
   :parse
   ;; DEPARSER
   :deparse-multiple-value-bind-form
   :deparse-values-form
   :deparse-nth-value-form
   :deparse-defun-form
   :deparse-declaim-form
   :deparse-funcall-form
   :deparse-assert-form
   :deparse-p
   ;; DEAD CODE ANALYSIS
   :remove-dead-code!
   ;; FREE VARIABLE ANALYSIS
   :nso-free-in-ast?
   :nsos-accessed
   :free-nsos-accessed
   ))

(in-package :walker-plus)

;;;; FORMS

(defclass multiple-value-bind-form (walker:form walker:body-form)
  ((vars :initarg :vars :accessor walker:form-vars :type list :documentation "list of VARs")
   (values :initarg :values :accessor walker:form-values :type generalform)
   (declspecs :initarg :declspecs :accessor walker:form-declspecs :type list)))
(defclass values-form (walker:form walker:body-form)
  ())
(defclass nth-value-form (walker:form) ;TODO: implement NTH-VALUE-form, but I think this will not be easy since there is no way in LISP to specify multiple value types for a form. Probably implementing this will need an automatic type inferencer, like NIMBLE.
  ((value :initarg :value :accessor walker:form-value :type generalform)
   (values :initarg :values :accessor walker:form-values :type generalform)))
(defclass defun-form (walker:fun-binding)
  ())
(defclass declaim-form (walker:form)
  ((declspecs :initarg :declspecs :accessor walker:form-declspecs :type list)))
(defclass funcall-form (walker:form)
  ((var :initarg :sym :accessor walker:form-var :type walker:var)
   (arguments :initarg :arguments :accessor walker:form-arguments :type list :documentation "list of GENERALFORMs")))
(defclass assert-form (walker:form)
  ((test :initarg :test :accessor walker:form-test :type walker:generalform)))

;;;; END OF FORMS

(defmethod print-object ((object multiple-value-bind-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S ~A" (walker:form-vars object) (walker:form-values object) (walker:format-body object t nil))))
(defmethod print-object ((object values-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (walker:format-body object nil nil))))
(defmethod print-object ((object nth-value-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A" (walker:form-value object) (walker:form-values object))))
(defmethod print-object ((object defun-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S ~A" (walker:form-sym object) (walker:form-llist object) (walker:format-body object t t))))
(defmethod print-object ((object declaim-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (walker:form-declspecs object))))
(defmethod print-object ((object funcall-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (walker:form-var object))
    (loop for arg in (walker:form-arguments object) do
	 (format stream " ~S" arg))))
(defmethod print-object ((object assert-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (walker:form-test object))))

(defun parse-p (form lexical-namespace free-namespace parent)
  (declare (ignore lexical-namespace free-namespace parent))
  (when (and (listp form)
	     (let ((head (car form)))
	       (find head '(multiple-value-bind values nth-value defun declaim funcall assert))))
    #'parse))

(defun parse (form lexical-namespace free-namespace parent &key parser declspec-parser &allow-other-keys)
  (declare (optimize (debug 3)))
  (labels ((parser (form parent &key (lexical-namespace lexical-namespace))
	     (funcall parser form lexical-namespace free-namespace parent :parser parser :declspec-parser declspec-parser))
	   (parse-body (body current &key (lexical-namespace lexical-namespace))
	     (assert (walker:proper-list-p body) () "Body is not a proper list: ~S" body)
	     (loop for form in body collect (parser form current :lexical-namespace lexical-namespace))))
    (let ((head (car form))
	  (rest (cdr form)))
      (cond
	((eq head 'multiple-value-bind)
	 (assert (and (consp rest) (listp (car rest))) () "Cannot parse MULTIPLE-VALUE-BIND-form ~S" form)
	 (let* ((vars-form (let ((vars-form (car rest))) (loop for var in vars-form do (assert (symbolp var) () "VARs in MULTIPLE-VALUE-BIND-form must be symbols, not ~S" var)) vars-form))
		(values-form (cadr rest))
		(body (cddr rest))
		(current (make-instance 'multiple-value-bind-form :parent parent))
		(parsed-vars (loop for var-form in vars-form collect (parser var-form current)))
		(parsed-values (parser values-form current)))
	   (multiple-value-bind (body parsed-declspecs)
	       (walker:parse-declaration-in-body body lexical-namespace free-namespace current :declspec-parser declspec-parser)
	     (setf (walker:form-vars current) parsed-vars)
	     (setf (walker:form-values current) parsed-values)
	     (setf (walker:form-declspecs current) parsed-declspecs)
	     (setf (walker:form-body current) (parse-body body current)))
	   current))
	((eq head 'values)
	 (let* ((objects-form rest)
		(current (make-instance 'values-form :parent parent))
		(parsed-objects (loop for object-form in objects-form collect (parser object-form current))))
	   (setf (walker:form-body current) parsed-objects)
	   current))
	((eq head 'nth-value)
	 (assert (and (consp rest) (consp (cdr rest)) (null (cddr rest))) () "Cannot parse NTH-VALUE-form ~S" form)
	 (let* ((value-form (car rest))
		(values-form (cadr rest))
		(current (make-instance 'nth-value-form :parent parent))
		(parsed-value (parser value-form current))
		(parsed-values (parser values-form current)))
	   (setf (walker:form-value current) parsed-value)
	   (setf (walker:form-values current) parsed-values)
	   current))
	((eq head 'defun)
	 (multiple-value-bind (fun-type name) (walker:valid-function-name-p (car rest))
	   (let* ((lambda-list-and-body (cdr rest))
		  (block-name (ecase fun-type ((walker:fun) name) ((walker:setf-fun) (cadr name)))) ;CLHS Glossary "function block name" defines "If the function name is a list whose car is setf and whose cadr is a symbol, its function block name is the symbol that is the cadr of the function name."
		  (blo (make-instance 'walker:blo :name block-name :freep nil :sites nil))
		  (current (make-instance 'defun-form :parent parent :blo blo))
		  (sym (walker:namespace-lookup/create 'walker:fun name lexical-namespace free-namespace)))
	     (walker:parse-and-set-functiondef lambda-list-and-body #'walker:parse-ordinary-lambda-list (walker:augment-lexical-namespace blo lexical-namespace) free-namespace current :parser parser :declspec-parser declspec-parser)
	     (setf (walker:nso-definition blo) current)
	     (setf (walker:form-sym current) sym)
	     current)))
	((eq head 'declaim)
	 (let* ((declspecs rest)
		(current (make-instance 'declaim-form :parent parent))
		(parsed-declspecs (walker:parse-declspecs declspecs lexical-namespace free-namespace current :declspec-parser declspec-parser)))
	   (setf (walker:form-declspecs current) parsed-declspecs)
	   current))
	((eq head 'funcall)
	 (assert (symbolp (car rest)) () "Cannot parse FUNCALL-form ~S" form)
	 (let* ((fun-sym (car rest))
		(arg-forms (cdr rest))
		(sym (walker:namespace-lookup/create 'walker:var fun-sym lexical-namespace free-namespace))
		(current (make-instance 'funcall-form :parent parent :sym sym))
		(parsed-arguments nil))
	   (loop do
		(when (null arg-forms) (return))
		(assert (and (consp arg-forms) (listp (cdr arg-forms))) () "Invalid argument rest ~S in function or macro application" arg-forms)
		(push (parser (car arg-forms) current) parsed-arguments)
		(setf arg-forms (cdr arg-forms)))
	   (setf (walker:form-arguments current) (nreverse parsed-arguments))
	   current))
	((eq head 'assert)
	 (assert (consp rest) () "Cannot parse ASSERT-form ~S" form)
	 (let* ((current (make-instance 'assert-form :parent parent))
		(parsed-test (parser (car rest) current)))
	   (setf (walker:form-test current) parsed-test)
	   current))
	))))

;;;; DEPARSER

(defun deparse-multiple-value-bind-form (ast deparser)
  (list* 'multiple-value-bind
	 (mapcar (lambda (var) (funcall deparser var deparser)) (walker:form-vars ast))
	 (funcall deparser (walker:form-values ast) deparser)
	 (walker:deparse-body ast deparser t nil)))
(defun deparse-values-form (ast deparser)
  (list* 'values (walker:deparse-body ast deparser nil nil)))
(defun deparse-nth-value-form (ast deparser)
  (list* 'nth-value
	 (funcall deparser (walker:form-value ast) deparser)
	 (funcall deparser (walker:form-values ast) deparser)))
(defun deparse-defun-form (ast deparser)
  (list* 'defun
	 (funcall deparser (walker:form-sym ast) deparser)
	 (walker:deparse-body ast deparser t t)))
(defun deparse-declaim-form (ast deparser)
  (list* 'declaim
	 (funcall deparser (walker:form-declspecs deparser) ast)))
(defun deparse-funcall-form (ast deparser)
  (list* 'funcall
	 (funcall deparser (walker:form-var ast) deparser)
	 (mapcar (lambda (arg) (funcall deparser arg deparser)) (walker:form-arguments ast))))
(defun deparse-assert-form (ast deparser)
  (list 'assert
	(funcall deparser (walker:form-test ast) deparser)))

(defun deparse-p (ast)
  (typecase ast
    (multiple-value-bind-form #'deparse-multiple-value-bind-form)
    (values-form #'deparse-values-form)
    (nth-value-form #'deparse-nth-value-form)
    (defun-form #'deparse-defun-form)
    (declaim-form #'deparse-declaim-form)
    (funcall-form #'deparse-funcall-form)
    (assert-form #'deparse-assert-form)))

;;;; DEAD CODE ANALYSIS

(defun remove-dead-code! (ast &key (live-tags nil) (live-functions nil))
  "Modifies AST by removing dead code. Return non-NIL if AST does not return, NIL if AST returns."
  (declare (optimize (debug 3)))
  (labels ((recurse! (ast)
	     (remove-dead-code! ast :live-tags live-tags :live-functions live-functions))
	   (remove-dead-body! (dead)
	     (let ((body nil))
	       (loop for form in (walker:form-body ast) do
		    (unless (prog1 dead ;if DEAD is NIL, do not remove code yet
			      (or dead (setf dead (recurse! form))))
		      (push form body)))
	       (setf (walker:form-body ast) (nreverse body))
	       dead))
	   (no-code ()
	     (make-instance 'walker:selfevalobject :object nil))
	   (remove-dead-let! ()
	     (let ((dead nil)
		   (bindings nil))
	       (loop for binding in (walker:form-bindings ast) do
		    (if (prog1 dead ;if DEAD is NIL, do not remove code yet
			  (or dead (setf dead (recurse! (walker:form-value binding)))))
			(return)
			(push binding bindings)))
	       (setf (walker:form-bindings ast) (nreverse bindings))
	       (remove-dead-body! dead)))
	   (remove-dead-flet! ()
	     (loop for binding in (walker:form-bindings ast) do
		  (let ((dead (recurse! binding)))
		    (setf live-functions (acons (walker:form-sym binding) dead live-functions))))
	     (remove-dead-body! nil))
	   (find-innermost-form (dead1 dead2)
	     (let ((ast ast))
	       (loop do
		    (setf ast (walker:form-parent ast))
		    (when (eq ast dead1)
		      (return dead1))
		    (when (eq ast dead2)
		      (return dead2))
		    (assert (not (null ast))))))
	   (find-abort-form (dead1 dead2 and-or)
	     (cond
	       ((and (or (typep dead1 'walker:block-naming-form) (typep dead1 'walker:tagbody-form))
		     (or (typep dead2 'walker:block-naming-form) (typep dead2 'walker:tagbody-form)))
		;; find the innermost form and return it.
		(find-innermost-form dead1 dead2))
	       ((or (typep dead1 'walker:block-naming-form) (typep dead1 'walker:tagbody-form))
		(ecase and-or
		  (:and (and dead2 dead1)) ;the order is important
		  (:or (or dead1 dead2)))) ;the order is important
	       (t
		(ecase and-or
		  (:and (and dead1 dead2)) ;the order is important
		  (:or (or dead2 dead1)))))) ;the order is importantcc
	   (find-abort-form-and (dead1 dead2)
	     "Return the innermost abort-form if DEAD1 or DEAD2 are abort forms, or T if both forms are T, NIL otherwise"
	     (find-abort-form dead1 dead2 :and))
	   (find-abort-form-or (dead1 dead2)
	     "Return the innermost abort-form if DEAD1 or DEAD2 are abort forms, or T if one form is T, NIL otherwise"
	     (find-abort-form dead1 dead2 :or)))
    (etypecase ast
      (walker:selfevalobject nil)
      (walker:var nil)
      (walker:fun nil)
      (walker:progn-form
       (remove-dead-body! nil))
      (walker:var-binding nil)
      (walker:let-form
       (remove-dead-let!))
      (walker:let*-form
       (remove-dead-let!))
      (walker:application-form
       (let ((dead nil)
	     (args nil))
	 (loop for arg in (walker:form-arguments ast) do
	      (when (prog1 dead ;if DEAD is NIL, do not remove code yet
		      (or dead (setf dead (recurse! arg))))
		(setf arg (no-code)))
	      (push arg args))
	 (setf (walker:form-arguments ast) (nreverse args))
	 (let ((acons (assoc (walker:form-fun ast) live-functions)))
	   (unless (null acons)
	     (let ((dead-args dead)
		   (dead-fun (cdr acons)))
	       ;;(format t "dead-args:~S dead-fun:~S innermost:~S~%" dead-args dead-fun (find-abort-form-or dead-args dead-fun))
	       (find-abort-form-or dead-args dead-fun))))))
      (walker:setq-form
       (let ((dead nil)
	     (values nil))
	 (loop for value in (walker:form-values ast) do
	      (when (prog1 dead ;if DEAD is NIL, do not remove code yet
		      (or dead (setf dead (recurse! value))))
		(return))
	      (push value values))
	 (setf (walker:form-vars ast) (subseq (walker:form-vars ast) 0 (length values)))
	 (setf (walker:form-values ast) (nreverse values))
	 dead))
      (walker:if-form
       (let ((test-dead (recurse! (walker:form-test ast))))
	 (if test-dead
	     ;; remove the IF-FORM, and replace it with (WALKER:FORM-TEST AST).
	     (progn
	       (cond
		 ((typep (walker:form-test ast) 'walker:progn-form)
		  (let* ((body (walker:form-body (walker:form-test ast))))
		    (change-class ast 'walker:progn-form :body body :parent (walker:form-parent ast))))
		 (t
		  (change-class ast 'walker:progn-form :body (list (walker:form-test ast)) :parent (walker:form-parent ast))))
	       test-dead)
	     (let ((then-dead (recurse! (walker:form-then ast)))
		   (else-dead (unless (null (walker:form-else ast)) (recurse! (walker:form-else ast)))))
	       (find-abort-form-and then-dead else-dead)))))
      (walker:tagbody-form
       (let ((ht (make-hash-table)))
	 (setf live-tags (acons ast ht live-tags))
	 ;; repeat until no further live tags are found
	 (let ((dead
		(block liveness
		  (let ((old-count -1))
		    (loop until (= old-count (gethash :count ht 0)) do
			 (setf old-count (gethash :count ht 0))
			 (setf (gethash :count ht) 0)
			 ;;(let ((l nil)) (loop for tag being the hash-key of ht do (push tag l)) (prind "live" l old-count))
			 (let ((live t))
			   ;; mark all tags after the first non-returning form as dead
			   (loop for form in (walker:form-body ast) do
				;;(prind form live)
				(if live
				    (let ((dead (recurse! form))) ;this only visits live forms
				      (cond
					((null dead)
					 (setf live t))
					((eq dead t)
					 (setf live nil))
					((eq dead ast)
					 (setf live nil))
					(t
					 ;;(prind "aborting" (and dead t))
					 (return-from liveness dead))))
				    (when (typep form 'walker:tag) ;continue at live tags
				      (setf live (gethash form ht nil))))
				;;(prind live)
				))
			 ;;(let ((l nil)) (loop for tag being the hash-key of ht do (push tag l)) (prind "live2" l old-count))
			 ))
		  nil)))
	   ;; collect a live tag up to non-live tag
	   ;;(let ((l nil)) (loop for tag being the hash-key of ht do (push tag l)) (prind "after" l))
	   (let ((live t))
	     (let ((body nil))
	       (loop for form in (walker:form-body ast) do
		    (when (typep form 'walker:tag)
		      (setf (walker:nso-sites form) ;remove dead GO-FORMs
			    (remove-if (lambda (x) (gethash x ht t))
				       (walker:nso-sites form)))
		      (setf live (gethash form ht nil)))
		    ;;(prind (walker:deparse form) live)
		    (when live
		      (push form body)
		      (setf live (not (recurse! form)))))
	       (setf (walker:form-body ast) (nreverse body))
	       ))
	   dead)))
      (walker:tag
       (let* ((tag ast)
	      (tagbody-form (walker:nso-definition tag))
	      (ht (cdr (assoc tagbody-form live-tags))))
	 (assert (not (null ht)))
	 (setf (gethash tag ht) t) ;mark TAG as live
	 (incf (gethash :count ht))
	 nil))
      (walker:go-form
       (let* ((tag (walker:form-tag ast))
	      (tagbody-form (walker:nso-definition tag))
	      (ht (cdr (assoc tagbody-form live-tags))))
	 (assert (not (null ht)))
	 (setf (gethash ast ht) nil) ;mark GO-FORM as live
	 (setf (gethash tag ht) t) ;mark TAG as live
	 (incf (gethash :count ht))
	 tagbody-form))
      (walker:block-form
       (let ((dead (remove-dead-body! nil)))
	 (cond
	   ((typep dead 'walker:block-form)
	    (unless (eq dead ast)
	      dead))
	   (t
	    dead))))
      (walker:return-from-form
       (walker:nso-definition (walker:form-blo ast)))
      (walker-plus:values-form
       (remove-dead-body! nil))
      (walker-plus:multiple-value-bind-form
       (let ((dead (recurse! (walker:form-values ast))))
	 (remove-dead-body! dead)))
      (walker:quote-form nil)
      (walker-plus:assert-form
       (recurse! (walker:form-test ast)))
      (walker:fun-binding
       (let* ((dead nil)
	      (llist (walker:form-llist ast))
	      (init-args (append (walker:llist-optional llist) (walker:llist-key llist) (walker:llist-aux llist))))
	 (loop for arg in init-args do
	      (when (prog1 dead ;if DEAD is NIL, do not remove code yet
		      (or dead (setf dead (recurse! (walker:argument-init arg)))))
		(setf (walker:argument-init arg) (no-code))))
	 (let ((dead (remove-dead-body! dead)))
	   ;; after the LLIST, one can (RETURN-FROM ,(WALKER:FORM-SYM AST))
	   (cond
	     ((typep dead 'walker:fun-binding)
	      (unless (eq dead ast)
		dead))
	     (t
	      dead)))))
      (walker:flet-form
       (remove-dead-flet!)) ;TODO: remove unused function bindings
      (walker:labels-form
       (remove-dead-flet!)) ;TODO: remove unused function bindings
      )))

(defun test-remove-dead-code ()
  (flet ((assert-result (form expected)
	   (let ((ast (walker:parse-with-namespace form :parser (walker:make-parser (list #'walker-plus:parse-p #'walker:parse-p)))))
	     (when (remove-dead-code! ast)
	       (setf ast (make-instance 'walker:selfevalobject :object nil)))
	     (let ((actual (walker:deparse ast :deparser (walker:make-deparser (list #'walker-plus:deparse-p #'walker:deparse-p)))))
	       (assert (equal actual expected) () "Remove dead code in ~S~%gave ~S,~%but should have been ~S" form actual expected)))))
    (assert-result '(block nil (+ 1 (return-from nil) 2)) '(block nil (+ 1 (return-from nil) nil)))
    (assert-result '(block nil (progn 1 (return-from nil) 2) 3) '(block nil (progn 1 (return-from nil))))
    (assert-result '(block nil (let ((a nil) (b (return-from nil)) c) 1)) '(block nil (let ((a nil) (b (return-from nil))))))
    (assert-result '(block nil (setq a 1 x (return-from nil) b 1)) '(block nil (setq a 1 x (return-from nil))))
    (assert-result '(let ((a nil) (b nil)) (block nil (setq a 0 x (return-from nil) b 1)) (values a b)) '(let ((a nil) (b nil)) (block nil (setq a 0 x (return-from nil))) (values a b)))
    (assert-result '(let ((a t)) (block nil (let ((a (setq a 0)) (x (return-from nil)) (b (incf a))) 2)) a) '(let ((a t)) (block nil (let ((a (setq a 0)) (x (return-from nil))))) a))
    (assert-result '(let ((a 1.0)) (block nil (if 1 (return) (setq a 1))) a) '(let ((a 1.0)) (block nil (if 1 (return) (setq a 1))) a))
    (assert-result '(let ((a 1.0)) (block nil (if 1 (setq a 1) (return))) a) '(let ((a 1.0)) (block nil (if 1 (setq a 1) (return))) a))
    (assert-result '(tagbody (go l) s (go e) a b (go d) l (go s) d (go a) e) '(tagbody (go l) s (go e) l (go s) e))
    (assert-result '(tagbody e (go t) s (go u) t (tagbody (go l) (go e) a b (go d) l (go s) d (go a) e) u) '(tagbody e (go t) s (go u) t (tagbody (go l) l (go s)) u))
    (assert-result '(tagbody e (go f) g (tagbody (go l) (go e) a b (go d) l (go s) d (go a) e) f (go g) s) '(tagbody e (go f) g (tagbody (go l) l (go s)) f (go g) s))
    (assert-result '(tagbody s (block nil (if 1 (return-from nil) (go e)) 2) (go s) e) '(tagbody s (block nil (if 1 (return-from nil) (go e))) (go s) e))
    (assert-result '(let ((a nil)) (block nil (if (progn (setq a t) (return-from nil) (setq a 1)) (setq a 1) (setq a 1)) (setq a 1)) a) '(let ((a nil)) (block nil (progn (setq a t) (return-from nil))) a))
    (assert-result '(let ((a nil)) (block nil (if (let () (return-from nil) (setq a 1)) (setq a 1) (setq a 1)) (setq a 1)) a) '(let ((a nil)) (block nil (progn (let () (return-from nil)))) a))
    (assert-result '(block nil (multiple-value-bind (a b) (return-from nil) 1)) '(block nil (multiple-value-bind (a b) (return-from nil))))
    (assert-result '(block nil (multiple-value-bind (a b) (values 1 (return-from nil)) 1)) '(block nil (multiple-value-bind (a b) (values 1 (return-from nil)))))
    (assert-result '(block nil (multiple-value-bind (a b) (values 1 2) (return-from nil) 3)) '(block nil (multiple-value-bind (a b) (values 1 2) (return-from nil))))
    (assert-result '(let ((a nil)) (block b (flet ((f1 (&optional (x (return-from b)) (y (setq a 0))) 1)) (f1) (setq a 0))) a) '(let ((a nil)) (block b (flet ((f1 (&optional (x (return-from b)) (y nil)))) (f1))) a))
    ))
(test-remove-dead-code)

;;;; FREE VARIABLE ANALYSIS

(defun nso-free-in-ast? (nso ast)
  "Return T if the name-space-object NSO is not defined within AST, NIL otherwise."
  (declare (type (or walker:sym walker:blo walker:tag) nso)
	   (type walker:generalform ast))
  (or (walker:nso-freep nso)
      (let ((definition (walker:nso-definition nso))) ;the AST that the NSO is defined in
	(block search-definition
	  (walker:map-ast (lambda (ast)
			    (when (eq definition ast)
			      (return-from search-definition nil)))
			  ast)
	  t))))

(defun nsos-accessed (ast &key (deparser (walker:make-deparser (list #'walker-plus:deparse-p #'walker:deparse-p))))
  "Return two values: the list of NSOs that are read within AST and the list of NSOs that are written within AST."
  (let ((accessed (make-hash-table))
	(written (make-hash-table))
	(defined (make-hash-table)))
    (walker:map-ast (lambda (ast)
		      (cond
			((typep ast 'walker:setq-form)
			 (loop for var in (walker:form-vars ast) do
			      (incf (gethash var written 0))))
			((typep ast 'walker:nso)
			 (incf (gethash ast accessed 0)))
			((or (typep ast 'walker:let-form) (typep ast 'walker:let*-form) (typep ast 'walker:flet-form) (typep ast 'walker:labels-form))
			 (loop for binding in (walker:form-bindings ast) do
			      (incf (gethash (walker:form-sym binding) defined 0))))
			((typep ast 'walker:argument)
			 (incf (gethash (walker:argument-var ast) defined 0)))))
		    ast
		    :deparser deparser)
    (let ((nsos-read nil)
	  (nsos-written nil))
      (loop for nso being the hash-key of accessed using (hash-value naccessed) do
	   (let ((nwritten (gethash nso written 0))
		 (ndefined (gethash nso defined 0)))
	     (when (> (- naccessed nwritten ndefined) 0)
	       (push nso nsos-read))))
      (loop for nso being the hash-key of written do
	   (push nso nsos-written))
      (values nsos-read nsos-written))))

(defun free-nsos-accessed (ast &key (deparser (walker:make-deparser (list #'walker-plus:deparse-p #'walker:deparse-p))))
  "Return two values: the list of NSOs that are read within AST, and free in AST, and the list of NSOs that are written within AST, and free in AST."
  (multiple-value-bind (nsos-read nsos-written) (nsos-accessed ast :deparser deparser)
    (values (remove-if (lambda (nso) (not (nso-free-in-ast? nso ast))) nsos-read)
	    (remove-if (lambda (nso) (not (nso-free-in-ast? nso ast))) nsos-written))))

(let* ((form '(let ((y 0) (z 0))
	       (flet ((+ (&rest numbers) nil))
		 (let ((x 1)) (setq y (+ x z) x z)))))
       (ast (walker:parse-with-namespace form))
       (ast1 (car (walker:form-body ast)))
       (ast2 (car (walker:form-body ast1)))
       (nso-y (walker:form-sym (car (walker:form-bindings ast))))
       (nso-z (walker:form-sym (cadr (walker:form-bindings ast))))
       (nso-numbers (walker:argument-var (walker:llist-rest (walker:form-llist (car (walker:form-bindings ast1))))))
       (nso-+ (walker:form-sym (car (walker:form-bindings ast1))))
       (nso-x (walker:form-sym (car (walker:form-bindings ast2)))))
  (declare (ignore nso-numbers))
  (assert (not (nso-free-in-ast? nso-y ast)))
  (assert (not (nso-free-in-ast? nso-z ast)))
  (assert (not (nso-free-in-ast? nso-+ ast)))
  (assert (not (nso-free-in-ast? nso-x ast)))
  (assert (nso-free-in-ast? nso-y ast1))
  (assert (nso-free-in-ast? nso-z ast1))
  (assert (not (nso-free-in-ast? nso-+ ast1)))
  (assert (not (nso-free-in-ast? nso-x ast1)))
  (assert (nso-free-in-ast? nso-y ast2))
  (assert (nso-free-in-ast? nso-z ast2))
  (assert (nso-free-in-ast? nso-+ ast2))
  (assert (not (nso-free-in-ast? nso-x ast2)))
  (flet ((set-equal (list1 list2)
	   (and (null (set-difference list1 list2))
		(null (set-difference list2 list1)))))
    (multiple-value-bind (nsosr nsosw) (nsos-accessed ast)
      (assert (set-equal nsosr (list nso-+ nso-x nso-z)))
      (assert (set-equal nsosw (list nso-x nso-y))))
    (multiple-value-bind (fnsosr fnsosw) (free-nsos-accessed ast)
      (assert (set-equal fnsosr nil))
      (assert (set-equal fnsosw nil)))
    (multiple-value-bind (nsosr nsosw) (nsos-accessed ast1)
      (assert (set-equal nsosr (list nso-+ nso-x nso-z)))
      (assert (set-equal nsosw (list nso-x nso-y))))
    (multiple-value-bind (fnsosr fnsosw) (free-nsos-accessed ast1)
      (assert (set-equal fnsosr (list nso-z)))
      (assert (set-equal fnsosw (list nso-y))))
    (multiple-value-bind (nsosr nsosw) (nsos-accessed ast2)
      (assert (set-equal nsosr (list nso-+ nso-x nso-z)))
      (assert (set-equal nsosw (list nso-x nso-y))))
    (multiple-value-bind (fnsosr fnsosw) (free-nsos-accessed ast2)
      (assert (set-equal fnsosr (list nso-z nso-+)))
      (assert (set-equal fnsosw (list nso-y))))))
;;Test that the list of variables that are read within the function definition #'BLA, and which are not defined within #'BLA, but outside of #'BLA, are computed correctly.
(let* ((form '(let ((y 0) (z 0))
	       (flet ((bla (&rest numbers) (+ numbers y z))))))
       (ast (walker:parse-with-namespace form))
       (ast1 (car (walker:form-body ast)))
       (ast2 (car (walker:form-body (car (walker:form-bindings ast1)))))
       (nso-y (walker:form-sym (car (walker:form-bindings ast))))
       (nso-z (walker:form-sym (cadr (walker:form-bindings ast))))
       (nso-numbers (walker:argument-var (walker:llist-rest (walker:form-llist (car (walker:form-bindings ast1))))))
       (nso-+ (walker:form-fun ast2)))
  (flet ((set-equal (list1 list2)
	   (and (null (set-difference list1 list2))
		(null (set-difference list2 list1)))))
    (multiple-value-bind (nsosr nsosw) (free-nsos-accessed ast1)
      (assert (set-equal nsosr (list nso-+ nso-y nso-z)))
      (assert (set-equal nsosw nil)))
    (multiple-value-bind (nsosr nsosw) (free-nsos-accessed ast2)
      (assert (set-equal nsosr (list nso-+ nso-numbers nso-y nso-z)))
      (assert (set-equal nsosw nil)))
    (multiple-value-bind (nsosr nsosw) (nsos-accessed ast1)
      (assert (set-equal nsosr (list nso-+ nso-numbers nso-y nso-z)))
      (assert (set-equal nsosw nil)))
    (multiple-value-bind (nsosr nsosw) (nsos-accessed ast2)
      (assert (set-equal nsosr (list nso-+ nso-numbers nso-y nso-z)))
      (assert (set-equal nsosw nil)))))
