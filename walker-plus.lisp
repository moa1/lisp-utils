 (defpackage :walker-plus
  (:documentation "Parsers and deparsers for some more forms.")
  (:use :cl)
  (:export
   ;; for classes: export the class and _all_ accessors on one line so that deleting a class doesn't have to consider all exports of other classes
   ;; FORMS
   :multiple-value-bind-form :vars :values :declspecs
   :values-form :values
   :nth-value-form :value :values
   :defun-form
   :declaim-form :declspecs
   :funcall-form :var :arguments
   :assert-form :test
   ;; END OF FORMs
   :parser-plus
   ;;:parse is exported from package WALKER
   ;; DEPARSER
   ;;:deparse is exported from package WALKER
   ;;:eval-order is exported from package WALKER
   ;; ARGUMENTS AND LAMBDA LISTS
   :arguments-assign-to-lambda-list
   ;; DEAD CODE ANALYSIS
   :remove-dead-code!
   ;; FREE VARIABLE ANALYSIS
   :nso-free-in-ast?
   :nsos-accessed
   :free-nsos-accessed
   ))

(in-package :walker-plus)

;;;; FORMS

(defclass multiple-value-bind-form (walker:form walker:body-mixin)
  ((vars :initarg :vars :accessor walker:form-vars :type list :documentation "list of VARs")
   (values :initarg :values :accessor walker:form-values :type form)
   (declspecs :initarg :declspecs :accessor walker:form-declspecs :type list)))
(defclass values-form (walker:form)
  ((values :initarg :values :accessor walker:form-values :type form)))
(defclass nth-value-form (walker:form) ;TODO: implement NTH-VALUE-form, but I think this will not be easy since there is no way in LISP to specify multiple value types for a form. Probably implementing this will need an automatic type inferencer, like NIMBLE.
  ((value :initarg :value :accessor walker:form-value :type form)
   (values :initarg :values :accessor walker:form-values :type form)))
(defclass defun-form (walker:fun-binding)
  ())
(defclass declaim-form (walker:form)
  ((declspecs :initarg :declspecs :accessor walker:form-declspecs :type list)))
(defclass funcall-form (walker:form)
  ((var :initarg :var :accessor walker:form-var :type walker:var)
   (arguments :initarg :arguments :accessor walker:form-arguments :type list :documentation "list of FORMs")))
(defclass assert-form (walker:form)
  ;; TODO FIXME: this should be extended to allow the full ANSI Common Lisp ASSERT.
  ((test :initarg :test :accessor walker:form-test :type walker:form)))

;;;; END OF FORMS

(defmethod print-object ((object multiple-value-bind-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S ~A" (walker:form-vars object) (walker:form-values object) (walker:format-body object t nil))))
(defmethod print-object ((object values-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (car (walker:form-values object)))
    (loop for value in (cdr (walker:form-values object)) do
	 (format stream " ~A" value))))
(defmethod print-object ((object nth-value-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A" (walker:form-value object) (walker:form-values object))))
(defmethod print-object ((object defun-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S ~A" (walker:form-fun object) (walker:form-llist object) (walker:format-body object t t))))
(defmethod print-object ((object declaim-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (walker:form-declspecs object))))
(defmethod print-object ((object funcall-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (walker:form-var object))
    (loop for arg in (walker:form-arguments object) do
	 (format stream " ~S" arg))))
(defmethod print-object ((object assert-form) stream)
  ;; TODO FIXME: this should be extended to allow the full ANSI Common Lisp ASSERT.
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (walker:form-test object))))

(defclass parser-plus (walker:parser)
  ()
  (:documentation "Subclass of WALKER:PARSER used for parsing the additional forms defined in walker-plus.lisp."))

(defmethod walker:copy-parser ((parser parser-plus))
  (make-instance (type-of parser)
		 :lexical-namespace (walker:parser-lexical-namespace parser)
		 :free-namespace (walker:parser-free-namespace parser)))

;;;; PARSE-FORM

(defmethod walker:parse-form ((parser parser-plus) (head (eql 'multiple-value-bind)) rest parent source)
  (assert (and (consp rest) (listp (car rest))) () "Cannot parse MULTIPLE-VALUE-BIND-form ~S" source)
  (let* ((vars-form (let ((vars-form (car rest))) (loop for var in vars-form do (assert (symbolp var) () "VARs in MULTIPLE-VALUE-BIND-form must be symbols, not ~S" var)) vars-form))
	 (values-form (cadr rest))
	 (body (cddr rest))
	 (current (walker:make-ast parser 'multiple-value-bind-form :parent parent :source source))
	 (parsed-vars (loop for var-form in vars-form collect (walker:parse parser var-form current)))
	 (parsed-values (walker:parse parser values-form current)))
    (multiple-value-bind (body parsed-declspecs)
	(walker:parse-declaration-in-body parser body current)
      (setf (walker:form-vars current) parsed-vars)
      (setf (walker:form-values current) parsed-values)
      (setf (walker:form-declspecs current) parsed-declspecs)
      (setf (walker:form-body current) (walker:parse-body parser body current)))
    current))

(defmethod walker:parse-form ((parser parser-plus) (head (eql 'values)) rest parent source)
  (let* ((objects-form rest)
	 (current (walker:make-ast parser 'values-form :parent parent :source source))
	 (parsed-objects (loop for object-form in objects-form collect (walker:parse parser object-form current))))
    (setf (walker:form-values current) parsed-objects)
    current))

(defmethod walker:parse-form ((parser parser-plus) (head (eql 'nth-value)) rest parent source)
  (assert (and (consp rest) (consp (cdr rest)) (null (cddr rest))) () "Cannot parse NTH-VALUE-form ~S" (cons head rest))
  (let* ((value-form (car rest))
	 (values-form (cadr rest))
	 (current (walker:make-ast parser 'nth-value-form :parent parent :source source))
	 (parsed-value (walker:parse parser value-form current))
	 (parsed-values (walker:parse parser values-form current)))
    (setf (walker:form-value current) parsed-value)
    (setf (walker:form-values current) parsed-values)
    current))

(defmethod walker:parse-form ((parser parser-plus) (head (eql 'defun)) rest parent source)
  (multiple-value-bind (fun-type name) (walker:valid-function-name-p (car rest))
    (let* ((lambda-list-and-body (cdr rest))
	   (block-name (ecase fun-type ((walker:fun) name) ((walker:setf-fun) (cadr name)))) ;CLHS Glossary "function block name" defines "If the function name is a list whose car is setf and whose cadr is a symbol, its function block name is the symbol that is the cadr of the function name."
	   (blo (walker:make-ast parser 'walker:blo :name block-name :freep nil :sites nil :source block-name))
	   (current (walker:make-ast parser 'defun-form :parent parent :source source :blo blo))
	   (fun (walker:namespace-lookup/create 'walker:fun name parser)))
      (walker:parse-and-set-functiondef (walker:augment-lexical-namespace blo parser) lambda-list-and-body #'walker:parse-ordinary-lambda-list current)
      (setf (walker:nso-definition blo) current)
      (setf (walker:form-fun current) fun)
      current)))

(defmethod walker:parse-form ((parser parser-plus) (head (eql 'declaim)) rest parent source)
  (let* ((declspecs rest)
	 (current (walker:make-ast parser 'declaim-form :parent parent :source source))
	 (parsed-declspecs (walker:parse-declspecs parser declspecs current)))
    (setf (walker:form-declspecs current) parsed-declspecs)
    current))

(defmethod walker:parse-form ((parser parser-plus) (head (eql 'funcall)) rest parent source)
  (assert (consp rest) () "Cannot parse FUNCALL-form ~S" (cons head rest))
  (let* ((fun-form (car rest))
	 (arg-forms (cdr rest))
	 (fun (walker:parse parser fun-form parent)))
    (walker:parse-macro-or-function-application parser nil fun arg-forms parent source)))

(defmethod walker:parse-form ((parser parser-plus) (head (eql 'assert)) rest parent source)
  ;; TODO FIXME: this should be extended to allow the full ANSI Common Lisp ASSERT.
  (assert (consp rest) () "Cannot parse ASSERT-form ~S" (cons head rest))
  (let* ((current (walker:make-ast parser 'assert-form :parent parent :source source))
	 (parsed-test (walker:parse parser (car rest) current)))
    (setf (walker:form-test current) parsed-test)
    current))

;;;; DEPARSER

(defmethod walker:deparse ((deparser walker:deparser) (ast multiple-value-bind-form) path)
  (list* 'multiple-value-bind
	 (walker:deparse-path-list deparser (walker:form-vars ast) path)
	 (walker:deparse-path deparser (walker:form-values ast) path)
	 (walker:deparse-body deparser ast path t nil)))
(defmethod walker:eval-order ((orderer walker:orderer) (ast multiple-value-bind-form))
  `(,#'walker:form-values ,#'walker:form-body))

(defmethod walker:deparse ((deparser walker:deparser) (ast values-form) path)
  (list* 'values
	 (walker:deparse-path-list deparser (walker:form-values ast) path)))
(defmethod walker:eval-order ((orderer walker:orderer) (ast values-form))
  `(,#'walker:form-values))

(defmethod walker:deparse ((deparser walker:deparser) (ast nth-value-form) path)
  (list* 'nth-value
	 (walker:deparse-path deparser (walker:form-value ast) path)
	 (walker:deparse-path deparser (walker:form-values ast) path)))
(defmethod walker:eval-order ((orderer walker:orderer) (ast nth-value-form))
  `(,#'walker:form-value ,#'walker:form-values))

(defmethod walker:deparse ((deparser walker:deparser) (ast defun-form) path)
  (list* 'defun
	 (walker:deparse-path deparser (walker:form-fun ast) path)
	 (walker:deparse-path deparser (walker:form-llist ast) path)
	 (walker:deparse-body deparser ast path t t)))
(defmethod walker:eval-order ((orderer walker:orderer) (ast defun-form))
  `(,#'walker:form-llist ,#'walker:form-body))

(defmethod walker:deparse ((deparser walker:deparser) (ast declaim-form) path)
  (list* 'declaim
	 (walker:deparse-path deparser (walker:form-declspecs ast) path)))
(defmethod walker:eval-order ((orderer walker:orderer) (ast declaim-form))
  nil)

(defmethod walker:deparse ((deparser walker:deparser) (ast funcall-form) path)
  (list* 'funcall
	 (walker:deparse-path deparser (walker:form-var ast) path)
	 (walker:deparse-path-list deparser (walker:form-arguments ast) path)))
(defmethod walker:eval-order ((orderer walker:orderer) (ast funcall-form))
  `(,#'walker:form-var ,#'walker:form-arguments))

(defmethod walker:deparse ((deparser walker:deparser) (ast assert-form) path)
  ;; TODO FIXME: this should be extended to allow the full ANSI Common Lisp ASSERT.
  (list 'assert
	(walker:deparse-path deparser (walker:form-test ast) path)))
(defmethod walker:eval-order ((orderer walker:orderer) (ast assert-form))
  ;; TODO FIXME: this should be extended to allow the full ANSI Common Lisp ASSERT.
  `(,#'walker:form-test))

;;;; ARGUMENTS AND LAMBDA LISTS

(defmethod arguments-assign-to-lambda-list ((parser walker:parser) (llist walker:ordinary-llist) arguments)
  "LLIST is a parsed ordinary (TODO: or macro) lambda list. ARGUMENTS is the parsed list of arguments, i.e. (FORM-ARGUMENTS APPLICATION-FORM). PARSER is needed to allow subclassing multiple PARSERs and #'ARGUMENTS-ASSIGN-TO-LAMBDA-LIST for different purposes.
Returns an alist, with VARs (from the LLIST) as keys and FORMs (from ARGUMENTS) as values."
  ;; TODO: FIXME: currently, #'ARGUMENTS-ASSIGN-TO-LAMBDA-LIST needs ARGUMENTS whose values are parsed by #'WALKER:PARSE. This means that if keyword name arguments (for example :TEST in '(FIND 1 '(2 1 3) :TEST #'EQ)) are in package KEYWORD, they should be passed as WALKER:OBJECT-FORMs, not as WALKER:VAR-READINGs. Currently, #'ARGUMENTS-ASSIGN-TO-LAMBDA-LIST assumes that keyword name arguments are WALKER:VAR-READINGs.
  ;; take the LLIST as scaffold and assign ARGUMENTS to it.
  (let ((result nil)
	(original-arguments arguments))
    (flet ((keyword-name (arg)
	     (if (walker:parameter-keywordp arg)
		 (walker:parameter-keyword arg)
		 (intern (string (walker:nso-name (walker:parameter-var arg))) 'keyword)))
	   (init-form (arg)
	     (if (walker:parameter-init arg)
		 (walker:parameter-init arg)
		 (walker:make-nil parser arg)))
	   (set-result! (key datum)
	     (setf result (acons key datum result))))
      (loop for arg in (walker:llist-required llist) do
	   (assert (not (null arguments)) () "Missing required argument ~W~%in lambda list ~W~%for argument list ~W" (walker:nso-name (walker:parameter-var arg)) llist original-arguments)
	   (set-result! (walker:parameter-var arg) (pop arguments)))
      (loop for arg in (walker:llist-optional llist) do
	   (cond
	     ((null arguments)
	      (set-result! (walker:parameter-var arg) (init-form arg))
	      (when (walker:parameter-suppliedp arg)
		(set-result! (walker:parameter-suppliedp arg) (walker:make-nil parser arg))))
	     (t
	      (set-result! (walker:parameter-var arg) (pop arguments))
	      (when (walker:parameter-suppliedp arg)
		(set-result! (walker:parameter-suppliedp arg) (walker:make-object parser t arg))))))
      (unless (or (walker:llist-rest llist) (walker:llist-key llist))
	(assert (null arguments) () "Superfluous arguments: ~W" arguments))
      (when (walker:llist-rest llist)
	(set-result! (walker:parameter-var (walker:llist-rest llist)) arguments))
      (when (walker:llist-key llist)
	(assert (evenp (length arguments)) () "Odd number of arguments to &KEY: ~W" arguments))
      (labels ((find-name (name arguments)
		 (if (null arguments)
		     nil
		     (let ((arg (car arguments)))
		       (if (and (typep arg 'walker:var-reading)
				(eql (walker:nso-name (walker:form-var arg)) name))
			 arguments
			 (find-name name (cddr arguments)))))))
	(loop for arg in (walker:llist-key llist) do
	     (let* ((name (keyword-name arg))
		    (cdr (find-name name arguments)))
	       (cond
		 ((null cdr)
		  (set-result! (walker:parameter-var arg) (init-form arg))
		  (when (walker:parameter-suppliedp arg)
		    (set-result! (walker:parameter-suppliedp arg) (walker:make-nil parser arg))))
		 (t
		  (set-result! (walker:parameter-var arg) (cadr cdr))
		  (when (walker:parameter-suppliedp arg)
		    (set-result! (walker:parameter-suppliedp arg) (walker:make-nil parser arg)))))))
	;; keyword argument checking is suppressed if &ALLOW-OTHER-KEYS is present, see CLHS "3.4.1.4 Specifiers for keyword parameters"
	(unless (or (walker:llist-allow-other-keys llist)
		    (cadr (find-name :allow-other-keys arguments)))
	  (labels ((check (rest)
		     (if (null rest)
			 t
			 (let ((arg (car rest)))
			   (and (walker:llist-key llist)
				(or (position (walker:nso-name (walker:form-var arg)) (walker:llist-key llist) :key #'keyword-name :test #'eql)
				    (error "Unknown keyword argument name ~S~%in keyword list ~S" (walker:nso-name arg) (mapcar #'keyword-name (walker:llist-key llist)))))
			   (check (cddr rest))))))
	    (check arguments))))
      (loop for arg in (walker:llist-aux llist) do
	   (set-result! (walker:parameter-var arg) (init-form arg)))
      (nreverse result))))

(defun test-arguments-assign-to-lambda-list ()
  (flet ((assert-error (llist-list argument-list)
	   (let* ((form `(labels ((f ,llist-list nil)) (f ,@argument-list)))
		  (ast (walker:parse-with-namespace form))
		  (parser (walker:make-parser :variables nil :functions nil :macros nil))
		  (llist (walker:form-llist (walker:form-binding-1 ast)))
		  (arguments (walker:form-arguments (walker:form-body-1 ast)))
		  (result nil))
	     (handler-case (setf result (arguments-assign-to-lambda-list parser llist arguments))
	       (error ()
		 t)
	       (:no-error (x)
		 (declare (ignore x))
		 (error "(ARGUMENTS-ASSIGN-TO-LAMBDA-LIST ~S ~S)~%should have given an error,~%but gave ~W"
			llist-list arguments result)))))
	 (assert-result (llist-list argument-list desired-result-alist)
	   (let* ((form `(labels ((f ,llist-list nil)) (f ,@argument-list)))
		  (ast (walker:parse-with-namespace form))
		  (parser (walker:make-parser :variables nil :functions nil :macros nil))
		  (llist (walker:form-llist (walker:form-binding-1 ast)))
		  (arguments (walker:form-arguments (walker:form-body-1 ast)))
		  (result (arguments-assign-to-lambda-list parser llist arguments)))
	     (labels ((value-of (x)
			(etypecase x
			  (walker:object-form (walker:form-object x))
			  (walker:var-reading (walker:nso-name (walker:form-var x)))
			  (null nil)
			  (cons (loop for y in x collect (value-of y)))))
		      (result-to-alist (cons)
			(declare (optimize (debug 3)))
			(list (walker:nso-name (car cons))
			      (type-of (cdr cons))
			      (value-of (cdr cons)))))
	       (let ((obtained-result-alist (mapcar #'result-to-alist result)))
		 (assert (equal desired-result-alist obtained-result-alist) () "(ARGUMENTS-ASSIGN-TO-LAMBDA-LIST ~S ~S)~%wanted ~S~%but gave ~S" llist-list argument-list desired-result-alist obtained-result-alist))))))
    (assert-result '(a b c) '(1 2 3) '((a walker:object-form 1) (b walker:object-form 2) (c walker:object-form 3)))
    (assert-error '(a) '(1 2))
    (assert-error '(a b) '(1))
    (assert-error '(a &optional b (c t)) '())
    (assert-result '(a &optional b (c t)) '(1 2) '((a walker:object-form 1) (b walker:object-form 2) (c walker:object-form t)))
    (assert-result '(a &optional b (c t)) '(1) '((a walker:object-form 1) (b walker:object-form nil) (c walker:object-form t)))
    (assert-result '(a &optional b (c t cp)) '(1 2) '((a walker:object-form 1) (b walker:object-form 2) (c walker:object-form t) (cp walker:object-form nil)))
    (assert-result '(a &optional b (c t cp)) '(1 2 3) '((a walker:object-form 1) (b walker:object-form 2) (c walker:object-form 3) (cp walker:object-form t)))
    ;;works correctly, but testing for (C (WALKER:VAR b)) is not implemented: (assert-result '(a &optional b (c b)) '(1 2) '((a walker:object-form 1) (b walker:object-form 2) (c walker:var b)))
    (assert-error '(a &optional b) '(1 2 3))
    (assert-result '(&rest r) '(1 2) '((r cons (1 2))))
    (assert-result '(a &optional b &rest r) '(1 2 3) '((a walker:object-form 1) (b walker:object-form 2) (r cons (3))))
    (assert-error '(a &rest r &key b (c t)) '())
    (assert-result '(a &rest r &key b (c t)) '(1) '((a walker:object-form 1) (r null nil) (b walker:object-form nil) (c walker:object-form t)))
    (assert-error '(a &rest r &key b (c t)) '(1 2))
    (assert-result '(a &rest r &key b (c t)) '(1 :b 2) '((a walker:object-form 1) (r cons (:b 2)) (b walker:object-form 2) (c walker:object-form t)))
    (assert-result '(a &rest r &key b ((hello c) t)) '(1 :b 2) '((a walker:object-form 1) (r cons (:b 2)) (b walker:object-form 2) (c walker:object-form t)))
    (assert-result '(a &rest r &key b ((hello c) t)) '(1 :b 2 hello 3) '((a walker:object-form 1) (r cons (:b 2 hello 3)) (b walker:object-form 2) (c walker:object-form 3)))
    (assert-error '(a &rest r &key b (c t)) '(1 :bla 2))
    (assert-result '(a &rest r &key b (c t) &allow-other-keys) '(1 :bla 2) '((a walker:object-form 1) (r cons (:bla 2)) (b walker:object-form nil) (c walker:object-form t)))
    (assert-error '(a &rest r &key b (c t) &allow-other-keys) '(1 :bla 2 :allow-other-keys)) ;odd keywords
    (assert-result '(a &rest r &key b (c t) &allow-other-keys) '(1 :bla 2 :allow-other-keys nil) '((a walker:object-form 1) (r cons (:bla 2 :allow-other-keys nil)) (b walker:object-form nil) (c walker:object-form t))) ;if either &ALLOW-OTHER-KEYS is present or :ALLOW-OTHER-KEYS==T, suppress keyword checking
    (assert-result '(a &rest r &key b (c t)) '(1 :bla 2 :allow-other-keys t) '((a walker:object-form 1) (r cons (:bla 2 :allow-other-keys t)) (b walker:object-form nil) (c walker:object-form t)))
    (assert-result '(a &rest r &key b (c t) &allow-other-keys) '(1 :bla 2 :allow-other-keys t) '((a walker:object-form 1) (r cons (:bla 2 :allow-other-keys t)) (b walker:object-form nil) (c walker:object-form t)))
    ))
(test-arguments-assign-to-lambda-list)

;;;; DEAD CODE ANALYSIS

(defun remove-dead-code! (parser ast &key (live-tags nil) (live-functions nil))
  "Modifies AST by removing dead code. Return non-NIL if AST does not return, NIL if AST returns."
  (declare (optimize (debug 3)))
  (labels ((recurse! (ast)
	     (remove-dead-code! parser ast :live-tags live-tags :live-functions live-functions))
	   (remove-dead-body! (dead)
	     (let ((body nil))
	       (loop for form in (walker:form-body ast) do
		    (unless (prog1 dead ;if DEAD is NIL, do not remove code yet
			      (or dead (setf dead (recurse! form))))
		      (push form body)))
	       (setf (walker:form-body ast) (nreverse body))
	       dead))
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
		    (setf live-functions (acons (walker:form-fun binding) dead live-functions))))
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
	       ((and (or (typep dead1 'walker:block-naming-mixin) (typep dead1 'walker:tagbody-form))
		     (or (typep dead2 'walker:block-naming-mixin) (typep dead2 'walker:tagbody-form)))
		;; find the innermost form and return it.
		(find-innermost-form dead1 dead2))
	       ((or (typep dead1 'walker:block-naming-mixin) (typep dead1 'walker:tagbody-form))
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
      (walker:object-form nil)
      (walker:var-reading nil)
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
		(setf arg (walker:make-nil parser ast)))
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
	     (vars nil))
	 (loop for var in (walker:form-vars ast) do
	      (when (prog1 dead ;if DEAD is NIL, do not remove code yet
		      (or dead (setf dead (recurse! (walker:form-value var)))))
		(return))
	      (push var vars))
	 (setf (walker:form-vars ast) (subseq (walker:form-vars ast) 0 (length vars)))
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
				    (when (typep form 'walker:tagpoint) ;continue at live tags
				      (setf live (gethash (walker:form-tag form) ht nil))))
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
		    (when (typep form 'walker:tagpoint)
		      (let ((tag (walker:form-tag form)))
			(setf (walker:nso-sites tag) ;remove dead GO-FORMs
			      (remove-if (lambda (x) (gethash x ht t))
					 (walker:nso-sites tag)))
			(setf live (gethash tag ht nil))))
		    ;;(prind (walker:deparse form) live)
		    (when live
		      (push form body)
		      (setf live (not (recurse! form)))))
	       (setf (walker:form-body ast) (nreverse body))
	       ))
	   dead)))
      (walker:tagpoint
       (let* ((tag (walker:form-tag ast))
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
       (let ((dead nil) ;copied from #'REMOVE-DEAD-BODY
	     (values nil))
	 (loop for form in (walker:form-values ast) do
	      (unless (prog1 dead ;if DEAD is NIL, do not remove code yet
			(or dead (setf dead (recurse! form))))
		(push form values)))
	 (setf (walker:form-values ast) (nreverse values))
	 dead))
      (walker-plus:multiple-value-bind-form
       (let ((dead (recurse! (walker:form-values ast))))
	 (remove-dead-body! dead)))
      (walker:quote-form nil)
      (walker-plus:assert-form
       ;; TODO FIXME: this should be extended to allow the full ANSI Common Lisp ASSERT.
       (recurse! (walker:form-test ast)))
      (walker:fun-binding
       (let* ((dead nil)
	      (llist (walker:form-llist ast))
	      (init-args (append (walker:llist-optional llist) (walker:llist-key llist) (walker:llist-aux llist))))
	 (loop for arg in init-args do
	      (when (prog1 dead ;if DEAD is NIL, do not remove code yet
		      (or dead (setf dead (recurse! (walker:parameter-init arg)))))
		(setf (walker:parameter-init arg) (walker:make-nil parser arg))))
	 (let ((dead (remove-dead-body! dead)))
	   ;; after the LLIST, one can (RETURN-FROM ,(WALKER:FORM-FUN AST))
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
	   (let ((ast (walker:parse-with-namespace form :parser (walker:make-parser :type 'parser-plus)))
		 (parser (walker:make-parser)))
	     (when (remove-dead-code! parser ast)
	       (setf ast (walker:make-nil nil)))
	     (let ((actual (walker:deparse (make-instance 'walker:deparser) ast nil)))
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
	   (type walker:form ast))
  (or (walker:nso-freep nso)
      (let ((definition (walker:nso-definition nso))) ;the AST that the NSO is defined in
	(block search-definition
	  (walker:map-ast (lambda (ast path)
			    (declare (ignore path))
			    (when (eq definition ast)
			      (return-from search-definition nil)))
			  ast)
	  t))))

(defun nsos-accessed (ast)
  "Return two values: the list of NSOs that are read within AST and the list of NSOs that are written within AST."
  (let ((accessed (make-hash-table))
	(written (make-hash-table))
	(defined (make-hash-table)))
    (walker:map-ast (lambda (ast path)
		      (declare (ignore path))
		      (cond
			((typep ast 'walker:var-reading)
			 (incf (gethash (walker:form-var ast) accessed 0)))
			((typep ast 'walker:var-writing)
			 (incf (gethash (walker:form-var ast) written 0)))
			((typep ast 'walker:nso)
			 (incf (gethash ast accessed 0)))
			((or (typep ast 'walker:let-form) (typep ast 'walker:let*-form) (typep ast 'walker:flet-form) (typep ast 'walker:labels-form))
			 (loop for binding in (walker:form-bindings ast) do
			      (incf (gethash (walker:form-sym binding) defined 0))))
			((typep ast 'walker:parameter)
			 (incf (gethash (walker:parameter-var ast) defined 0)))))
		    ast)
    (let ((nsos-read nil)
	  (nsos-written nil))
      (loop for nso being the hash-key of accessed using (hash-value naccessed) do
	   (let ((nwritten (gethash nso written 0))
		 (ndefined (gethash nso defined 0)))
	     ;;(format t "nso:~S naccessed:~S nwritten:~S ndefined:~S~%" nso naccessed nwritten ndefined)
	     (when (> (- naccessed nwritten ndefined) 0)
	       (push nso nsos-read))))
      (loop for nso being the hash-key of written do
	   (push nso nsos-written))
      (values nsos-read nsos-written))))

(defun free-nsos-accessed (ast)
  "Return two values: the list of NSOs that are read within AST, and free in AST, and the list of NSOs that are written within AST, and free in AST."
  (multiple-value-bind (nsos-read nsos-written) (nsos-accessed ast)
    (values (remove-if (lambda (nso) (not (nso-free-in-ast? nso ast))) nsos-read)
	    (remove-if (lambda (nso) (not (nso-free-in-ast? nso ast))) nsos-written))))

(let* ((form '(let ((y 0) (z 0))
	       (flet ((+ (&rest numbers) nil))
		 (let ((x 1)) (setq y (+ x z) x z)))))
       (ast (walker:parse-with-namespace form))
       (ast1 (walker:form-body-1 ast))
       (ast2 (walker:form-body-1 ast1))
       (nso-y (walker:form-var (walker:form-binding-1 ast)))
       (nso-z (walker:form-var (walker:form-binding-2 ast)))
       (nso-numbers (walker:parameter-var (walker:llist-rest (walker:form-llist (walker:form-binding-1 ast1)))))
       (nso-+ (walker:form-fun (walker:form-binding-1 ast1)))
       (nso-x (walker:form-var (walker:form-binding-1 ast2))))
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
       (ast1 (walker:form-body-1 ast))
       (ast2 (walker:form-body-1 (walker:form-binding-1 ast1)))
       (nso-y (walker:form-var (walker:form-binding-1 ast)))
       (nso-z (walker:form-var (walker:form-binding-2 ast)))
       (nso-numbers (walker:parameter-var (walker:llist-rest (walker:form-llist (walker:form-binding-1 ast1)))))
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
