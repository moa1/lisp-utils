(load "/home/toni/quicklisp/setup.lisp")

#|
(ql:quickload :hu.dwim.walker)
(use-package :hu.dwim.walker)

(let* ((form '(defun foo (x)
			(declare (type fixnum x))
			(labels ((bar ()
				   (let ((y (+ x pi)))
				     (values x y))))
			  (declare (ftype (function () (values fixnum double-float)) bar))
			  (let ((bar 5.0))
			    (declare (type single-float bar))
			    (values bar (type-of bar))))))
		(ast (walk-form form))
		(var (car (arguments-of (car (body-of (car (body-of (car (body-of ast))))))))))
	   (declared-type-of var)) ;returns NIL, but should be SINGLE-FLOAT.
|#

;; How to define DEFUN from special forms:
;;CL-USER> (macroexpand '(setf (symbol-function 'test) (lambda () 5)))
;;(LET* ((#:NEW596 (LAMBDA () 5)))
;;  (FUNCALL #'(SETF SYMBOL-FUNCTION) #:NEW596 'TEST))
;;T

;; CLHS 3.1.2.1.2.1 Special Forms
;; block      let*                  return-from      
;; catch      load-time-value       setq             
;; eval-when  locally               symbol-macrolet  
;; flet       macrolet              tagbody          
;; function   multiple-value-call   the              
;; go         multiple-value-prog1  throw            
;; if         progn                 unwind-protect   
;; labels     progv                                  
;; let        quote

;; CLHS 3.8 Symbol DECLARE
;; dynamic-extent  ignore     optimize  
;; ftype           inline     special   
;; ignorable       notinline  type      

(ql:quickload :alexandria)

(defpackage :walker
  (:use :cl :alexandria)
  (:export))

(in-package :walker)

(defstruct (lexenv
	     (:constructor make-lexenv-kw))
  "A lexical environment."
  variables
  functions
  ;; blocks
  ;; tags
  ;; types ;i.e. class- or structure-names
  )

;;;; NAMESPACES

(defclass sym ()
  ((name :initarg :name :accessor sym-name :type symbol
	 :documentation "NIL if not known")
   (freep :initarg :freep :accessor sym-freep :type boolean
	  :documentation "T if it is a free variable/function, or NIL if bound. Note that this is specific to a namespace.")
   (definition :initarg :definition :accessor sym-definition ;;TODO uncomment when llist is implemented :typep (or binding llist)
	       :documentation "the parsed object (of type (OR BINDING LLIST)) it is defined in, NIL if not known")
   (declspecs :initarg :declspecs :accessor sym-declspecs :type list
	      :documentation "a list of DECLSPECs that apply to this symbol")
   )
  (:documentation "A symbol referring to a variable or function.
Note that symbols are always parsed in a lexical manner, regardless of whether the actual variable this symbol is referring to may be a lexical or special variable. For example, the symbols *A* in the form (LET ((*A* 1)) *A*) may refer to a lexical or special variable."))
(defclass var (sym)
  ())
(defclass fun (sym)
  ())

(defmethod print-object ((object sym) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "NAME:~A" (sym-name object))))

(defun make-namespace ()
  nil)

(defun augment-namespace (symbol object namespace)
  "Create a copy of NAMESPACE where the symbol SYMBOL is bound to object OBJECT.
Example: (augment-namespace 'var-a (make-instance 'var :name 'var-a) (make-namespace))"
  (acons symbol object namespace))

(defun augment-namespace-with-sym (sym namespace)
  (augment-namespace (sym-name sym) sym namespace))

(defun augment-namespace-with-syms (syms namespace)
  (loop for sym in syms do
       (setf namespace (augment-namespace (sym-name sym) sym namespace)))
  namespace)

(defun namespace-boundp (symbol namespace)
  "Return T if the symbol SYMBOL is bound in NAMESPACE."
  (let ((cell (assoc symbol namespace)))
    (not (null cell))))

(defun namespace-lookup (symbol namespace)
  "Return the object bound to symbol SYMBOL in NAMESPACE."
  (let ((cell (assoc symbol namespace)))
    (if (null cell)
	(error "unknown symbol ~A in namespace." symbol)
	(cdr cell))))

(defun print-namespace (namespace &optional (stream t))
  (loop for cell in (reverse namespace) do ;print from oldest to newest
       (flet ((format-sym (sym)
		(let ((str (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
		  (with-output-to-string (output str)
		    (print-unreadable-object (sym output :type t :identity t)
		      (format output "NAME:~A FREEP:~A DEFINITION:~A DECLSPECS:~A" (sym-name sym) (sym-freep sym) (sym-definition sym) (sym-declspecs sym))))
		  str)))
	 (format stream "~A: ~A~%" (car cell) (format-sym (cdr cell))))))

;;;; DECLARATIONS

(defclass declspec ()
  ((parent :initarg :parent :accessor declspec-parent)))
(defclass declspec-type (declspec)
  ((type :initarg :type :accessor declspec-type)
   (vars :initarg :vars :accessor declspec-vars :type list)))

(defun parse-declaration-in-body (body variables functions parent &key customparsep-function customparse-function)
  "Parses declarations in the beginning of BODY.
Returns four values: the rest of the BODY that does not start with a DECLARE-expression, a list of DECLSPEC-objects, the updated VARIABLES and FUNCTIONS.
Side-effects: Adds references of the created DECLSPEC-objects to the DECLSPEC-slots of VARIABLES and FUNCTIONS."
  (declare (optimize (debug 3)))
  (assert (listp body) () "Malformed BODY:~%~A" body)
  (labels ((lexboundp (symbol)
	     (namespace-boundp symbol variables))
	   (lexfboundp (symbol)
	     (namespace-boundp symbol functions))
	   (varlookup/create (symbol)
	     (if (lexboundp symbol)
		 (namespace-lookup symbol variables)
		 (let ((new-sym (make-instance 'var :name symbol :freep t :declspecs nil)))
		   (setf variables (augment-namespace-with-sym new-sym variables)) ;do not bind :DEFINITION
		   new-sym)))
	   (funlookup/create (symbol)
	     (if (lexfboundp symbol)
		 (namespace-lookup symbol functions)
		 (let ((new-sym (make-instance 'fun :name symbol :freep t :declspecs nil)))
		   (setf functions (augment-namespace-with-sym new-sym functions)) ;do not bind :DEFINITION
		   new-sym)))
	   (parse-declspec-type (expr)
	     "parse a single declaration-specifier.
Example: (parse-declare '(type fixnum a b c) nil nil nil)"
	     (assert (and (listp expr) (symbolp (car expr)) (listp (cdr expr))) () "Cannot parse declaration-specifier: ~A" expr)
	     (let ((head (car expr))
		   (rest (cdr expr)))
	       (cond
		 ((eq head 'type)
		  (assert (and (listp rest)))
		  ;;(let ((typespec 
		  ))))
	   (parse-declspecs (declspecs collected-declspecs)
	     "Example: (parse-declspecs '(type fixnum a b c) nil)"
	     (cond
	       ((null declspecs)
		(nreverse collected-declspecs))
	       (t
		(let* ((expr (car declspecs))
		       (identifier (car expr))
		       (body (cdr expr)))
		  (assert (symbolp identifier) () "Malformed declaration identifier ~A" identifier)
		  (let ((parsed-declspec
			 (cond
			   ((and (not (null customparsep-function)) (funcall customparsep-function identifier body variables functions parent))
			    (funcall customparse-function identifier body variables functions parent))
			   ((eq identifier 'type)
			    (assert (and (listp body) (listp (cdr body))) () "Malformed TYPE declaration: ~A" expr)
			    (let* ((typespec (car body))
				   (vars (cdr body)))
			      ;; will not check TYPESPEC, has to be done in user code.
			      (let* ((parsed-vars (loop for var in vars collect (varlookup/create var)))
				     (parsed-declspec (make-instance 'declspec-type :parent parent :type typespec :vars parsed-vars)))
				(loop for var in parsed-vars do
				     (push parsed-declspec (sym-declspecs var)))
				parsed-declspec)))
			   (t (error "Unknown declaration specifier ~A" expr)))))
		    (parse-declspecs (cdr declspecs) (cons parsed-declspec collected-declspecs)))))))
	   (parse-declare (body collected-declspecs)
	     (let ((head (car body))
		   (rest (cdr body)))
	       (cond
		 ((and (listp head) (eq (car head) 'declare))
		  (let* ((declspecs (cdr head)))
		    (parse-declare rest (parse-declspecs declspecs collected-declspecs))))
		 (t (values body (nreverse collected-declspecs) variables functions))))))
    (parse-declare body nil)))

(multiple-value-bind (body declspecs) (parse-declaration-in-body '(declare (type fixnum)) nil nil nil)
  (assert (and (equal body '(declare (type fixnum))) (null declspecs))))
;;TODO: test that (parse-declaration-in-body '((declare ()) 5) nil nil nil) throws an error.
(multiple-value-bind (body declspecs) (parse-declaration-in-body '((declare (type fixnum a)) 5) nil nil nil)
  (assert (and (equal body '(5)) (typep (car declspecs) 'declspec-type))))
(assert (sym-freep (car (declspec-vars (car (nth-value 1 (parse-declaration-in-body '((declare (type fixnum a)) 5) nil nil nil)))))))

;;;; FORMS

(defclass form ()
  ((parent :initarg :parent :accessor form-parent)))
(defclass constant-form (form)
  ((value :initarg :value :accessor constant-value)))
(defclass body-form ()
  ((body :initarg :body :accessor form-body)))
(defclass special-form (form)
  ())
(defclass progn-form (special-form body-form)
  ())
(defclass binding ()
  ((parent :initarg :parent :accessor binding-parent)
   (sym :initarg :sym :accessor binding-sym :type sym)
   (definition :initarg :definition :accessor binding-definition)))
(defclass var-binding (binding)
  ())
(defclass let-form (special-form body-form)
  ((bindings :initarg :bindings :accessor form-bindings :type list) ;list of VAR-BINDINGs
   (declspecs :initarg :declspecs :accessor form-declspecs :type list))) ;list of DECLSPECs
(defclass let*-form (special-form body-form)
  ((bindings :initarg :bindings :accessor form-bindings :type list) ;list of VAR-BINDINGs
   (declspecs :initarg :declspecs :accessor form-declspecs :type list))) ;list of DECLSPECs
;; (defclass fun-binding (binding)
;;   ())
;; (defclass function-form (body-form declare-form)
;;   ((llist :initarg :llist :accessor form-llist :type llist))) 
;; (defclass flet-form (special-form body-form declare-form)
;;   ((bindings :initarg :bindings :accessor form-bindings :type list) ;list of FUN-BINDINGs, with (type-of (binding-definition (car LIST-of-FUN-BINDINGs)))==FUNCTION-FORM
;;    (declspecs :initarg :declspecs :accessor form-declspecs :type list))) ;list of DECLSPECs
;; (defclass labels-form (special-form body-form declare-form)
;;   ((bindings :initarg :bindings :accessor form-bindings :type list) ;list of FUN-BINDINGs, with (type-of (binding-definition (car LIST-of-FUN-BINDINGs)))==FUNCTION-FORM
;;    (declspecs :initarg :declspecs :accessor form-declspecs :type list))) ;list of DECLSPECs

(defmethod print-object ((object constant-form) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (constant-value object))))
(defmethod print-object ((object body-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (form-body object))))
(defmethod print-object ((object declspec-type) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "TYPE:~A VARS:~A" (declspec-type object) (declspec-vars object))))

;; TODO: maybe rename to PARSE-FORM.
(defun parse (form variables functions parent &key customparsep-function customparse-function customparsedeclspecp-function customparsedeclspec-function)
  (declare (optimize (debug 3)))
  (labels ((lexboundp (symbol)
	     (namespace-boundp symbol variables))
	   (lexfboundp (symbol)
	     (namespace-boundp symbol functions))
	   (varlookup/create (symbol)
	     (if (lexboundp symbol)
		 (namespace-lookup symbol variables)
		 (make-instance 'var :name symbol :freep t :declspecs nil))) ;do not bind :DEFINITION
	   (funlookup/create (symbol)
	     (if (lexfboundp symbol)
		 (namespace-lookup symbol functions)
		 (make-instance 'fun :name symbol :freep t :declspecs nil))) ;do not bind :DEFINITION
	   (reparse (form parent &key (variables variables) (functions functions))
	     (parse form variables functions parent
		    :customparsep-function customparsep-function
		    :customparse-function customparse-function
		    :customparsedeclspecp-function customparsedeclspecp-function
		    :customparsedeclspec-function customparsedeclspec-function)))
    (cond
      ((and (not (null customparsep-function)) (funcall customparsep-function form variables functions parent))
       (funcall customparse-function form variables functions parent))
      ((or (eq form nil) (eq form t))
       (make-instance 'constant-form :value form :parent parent))
      ((symbolp form)
       (varlookup/create form))
      ((and (consp form) (eq (car form) 'function) (consp (cdr form)) (symbolp (cadr form)) (null (cddr form)))
       (funlookup/create form))
      ((atom form)
       (make-instance 'constant-form :value form :parent parent))
      (t
       (let ((head (car form))
	     (rest (cdr form)))
	 (cond
	   ((eq head 'progn)
	    (let* ((current (make-instance 'progn-form :parent parent :body nil))
		   (body rest)
		   (parsed-body (loop for form in body collect
				     (reparse form current))))
	      (setf (form-body current) parsed-body)
	      current))
	   ((or (eq head 'let) (eq head 'let*)) ;TODO: extend this case to LABELS and FLET.
	    (assert (and (consp rest) (listp (car rest))) () "cannot parse ~A-form:~%~A" head form)
	    (let* ((definitions (car rest))
		   (body (cdr rest))
		   (form-type (ecase head ((let) 'let-form) ((let*) 'let*-form)))
		   (current (make-instance form-type :parent parent :body nil))) ;(:BINDINGS :DECLSPECS) are defined below
	      (flet ((parse-var-binding (def variables)
		       (assert (and (consp def) (symbolp (car def)) (not (null (car def))) (not (null (cdr def))) (null (cddr def))) () "cannot parse definition in ~A-form:~%~A" head def)
		       (let* ((name (first def))
			      (value-form (second def))
			      (parsed-value (reparse value-form current :variables variables))
			      (sym (make-instance 'var :name name :freep nil :definition :undef :declspecs nil))
			      (binding (make-instance 'var-binding :parent parent :sym sym :definition parsed-value)))
			 (setf (sym-definition sym) binding)
			 binding)))
		(multiple-value-bind (parsed-bindings new-variables)
		    (cond
		      ((eq head 'let)
		       (let* ((parsed-bindings (loop for def in definitions collect (parse-var-binding def variables)))
			      (parsed-syms (loop for binding in parsed-bindings collect (binding-sym binding))))
			 (values parsed-bindings (augment-namespace-with-syms parsed-syms variables))))
		      ((eq head 'let*)
		       (let* ((new-variables variables)
			      (parsed-bindings (loop for def in definitions collect
						    (let* ((parsed-binding (parse-var-binding def new-variables))
							   (parsed-sym (binding-sym parsed-binding)))
						      (setf new-variables (augment-namespace-with-sym parsed-sym new-variables))
						      parsed-binding))))
			 (values parsed-bindings new-variables)))
		      (t (error "unknown HEAD")))
		  (multiple-value-bind (body parsed-declspecs new-variables new-functions)
		      (parse-declaration-in-body body new-variables functions parent :customparsep-function customparsedeclspecp-function :customparse-function customparsedeclspec-function)
		    (let ((parsed-body (loop for form in body collect
					    (reparse form current :variables new-variables :functions new-functions))))
		      (setf (form-bindings current) parsed-bindings)
		      (setf (form-declspecs current) parsed-declspecs)
		      (setf (form-body current) parsed-body)
		      current))))))
	   )))
      )))

;; tests for PARSE
(defun test-parse-symbol-reference ()
  (declare (optimize (debug 3)))
  (flet ((lexical-namespaces-at-here (form)
	   "Parse FORM and collect the lexical namespaces at the positions in FORM marked by the symbol -HERE-.
Returns two values: a list containing the lexical variable namespaces, and a list containing the lexical function namespaces."
	   (let* ((variables-here-list nil)
		  (functions-here-list nil))
	     (parse form nil nil nil
		    :customparsep-function (lambda (form vars funs parent)
					     (declare (ignore vars funs parent))
					     (eq form '-here-))
		    :customparse-function (lambda (form variables functions parent)
					    (declare (ignorable form parent))
					    (push variables variables-here-list)
					    (push functions functions-here-list)))
	     (values (nreverse variables-here-list) (nreverse functions-here-list)))))
    (assert (sym-freep (parse 'a nil nil nil)))
    (assert (sym-freep (parse '(function a) nil nil nil)))
    (assert (not (sym-freep (car (form-body (parse '(let ((a 1)) a) nil nil nil))))))
    ;;TODO uncomment when FLET is implemented: (assert (not (sym-freep (car (form-body (parse '(flet ((a 1)) #'a) nil nil nil))))))
    (assert (let* ((variables (car (lexical-namespaces-at-here '(let ((b 5) (c b)) -here-))))
		   (b-sym (namespace-lookup 'b variables))
		   (c-sym (namespace-lookup 'c variables)))
	      (not (eq b-sym (binding-definition (sym-definition c-sym))))))
    (assert (let* ((variables (car (lexical-namespaces-at-here '(let* ((b 5) (c b)) -here-))))
		   (b-sym (namespace-lookup 'b variables))
		   (c-sym (namespace-lookup 'c variables)))
	      (eq b-sym (binding-definition (sym-definition c-sym)))))
    (let* ((variables (lexical-namespaces-at-here '(let ((b 5)) -here- (let ((b b)) -here-))))
	   (variables-1 (car variables))
	   (variables-2 (cadr variables))
	   (b-1 (namespace-lookup 'b variables-1))
	   (b-2 (namespace-lookup 'b variables-2)))
      (assert (eq (constant-value (binding-definition (sym-definition b-1))) 5))
      (assert (eq b-1 (binding-definition (sym-definition b-2))))))
  (let* ((form '(let ((a 1)) (declare (type fixnum a)) a))
	 (ast (parse form nil nil nil))
	 (declspec-type (car (form-declspecs ast))))
    (assert (equal (car (declspec-vars declspec-type)) (car (form-body ast))))))
(test-parse-symbol-reference)
