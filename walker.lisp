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

(defclass sym ()
  ((name :initarg :name :accessor sym-name) ;NIL if not known
   (definition :initarg :definition :accessor sym-definition) ;NIL if not known
   (type :initarg :type :accessor sym-type) ;the declared type, NIL if not known
   ))

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
		      (format output "NAME:~A DEFINITION:~A TYPE:~A" (sym-name sym) (sym-definition sym) (sym-type sym))))
		  str)))
	 (format stream "~A: ~A~%" (car cell) (format-sym (cdr cell))))))

(defclass form ()
  ((parent :initarg :parent :accessor form-parent)))
(defclass constant-form (form)
  ((value :initarg :value :accessor constant-value)))
(defclass form-with-body ()
  ((body :initarg :body :accessor form-body)))
(defclass special-form (form)
  ())
(defclass progn-form (special-form form-with-body)
  ())
(defclass let-form (special-form form-with-body)
  ((definitions :initarg :definitions :accessor form-definitions)))
(defclass let*-form (special-form form-with-body)
  ((definitions :initarg :definitions :accessor form-definitions)))

(defmethod print-object ((object constant-form) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (constant-value object))))
(defmethod print-object ((object form-with-body) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (form-body object))))

(defun parse (form variables functions parent &key interruptp-function interrupt-function)
  (declare (optimize (debug 3)))
  (labels ((lexboundp (symbol)
	     (namespace-boundp symbol variables))
	   (lexfboundp (symbol)
	     (namespace-boundp symbol functions))
	   (varlookup (symbol)
	     (if (lexboundp symbol)
		 (namespace-lookup symbol variables)
		 (make-instance 'var :name symbol :definition nil)))
	   (funlookup (symbol)
	     (if (lexboundp symbol)
		 (namespace-lookup symbol variables)
		 (make-instance 'fun :name symbol :definition nil)))
	   (reparse (form parent &key (variables variables) (functions functions))
	     (parse form variables functions parent
		    :interruptp-function interruptp-function
		    :interrupt-function interrupt-function)))
    (cond
      ((and (not (null interruptp-function)) (funcall interruptp-function form variables functions parent))
       (funcall interrupt-function form variables functions parent))
      ((or (eq form nil) (eq form t))
       (make-instance 'constant-form :value form :parent parent))
      ((symbolp form)
       (varlookup form))
      ((and (consp form) (eq (car form) 'function) (consp (cdr form)) (symbolp (cadr form)) (null (cddr form)))
       (funlookup form))
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
	    (assert (and (consp rest) (listp (car rest))) () "cannot parse LET-form:~%~A" form)
	    (let* ((definitions (car rest))
		   (body (cdr rest))
		   (current (make-instance 'let-form :parent parent :definitions nil :body nil)))
	      (flet ((parse-definition (def variables)
		       (assert (and (consp def) (symbolp (car def)) (not (null (car def))) (not (null (cdr def))) (null (cddr def))) () "cannot parse definition in ~A-form:~%~A" head def)
		       (let* ((name (first def))
			      (value-form (second def))
			      (parsed-value (reparse value-form current :variables variables)))
			 (make-instance 'var :name name :definition parsed-value :type nil))))
		(multiple-value-bind (parsed-variables new-variables)
		    (if (eq head 'let)
			(let ((parsed-variables (loop for def in definitions collect (parse-definition def variables))))
			  (values parsed-variables (augment-namespace-with-syms parsed-variables variables)))
			(let* ((new-variables variables)
			       (parsed-variables (loop for def in definitions collect
						      (let ((sym (parse-definition def new-variables)))
							(setf new-variables (augment-namespace-with-sym sym new-variables))
							sym))))
			  (values parsed-variables new-variables)))
		  (let ((parsed-body (loop for form in body collect
				     (reparse form current :variables new-variables))))
		    (setf (form-definitions current) parsed-variables)
		    (setf (form-body current) parsed-body)
		    current)))))
	   )))
      )))

;; tests for PARSE
(flet ((lexical-namespaces-at-here (form)
	 "Parse FORM and collect the lexical namespaces at the positions in FORM marked by the symbol -HERE-.
Returns two values: a list containing the lexical variable namespaces, and a list containing the lexical function namespaces."
	 (let* ((variables-here-list nil)
		(functions-here-list nil))
	   (parse form nil nil nil
		  :interruptp-function (lambda (form vars funs parent)
					 (declare (ignore vars funs parent))
					 (eq form '-here-))
		  :interrupt-function (lambda (form variables functions parent)
					(declare (ignorable form parent))
					(push variables variables-here-list)
					(push functions functions-here-list)))
	   (values (nreverse variables-here-list) (nreverse functions-here-list)))))
  (assert (let* ((variables (car (lexical-namespaces-at-here '(let ((b 5) (c b)) -here-))))
		 (b-sym (namespace-lookup 'b variables))
		 (c-sym (namespace-lookup 'c variables)))
	    (not (eq b-sym (sym-definition c-sym)))))
  (assert (let* ((variables (car (lexical-namespaces-at-here '(let* ((b 5) (c b)) -here-))))
		 (b-sym (namespace-lookup 'b variables))
		 (c-sym (namespace-lookup 'c variables)))
	    (eq b-sym (sym-definition c-sym))))
  (let* ((variables (lexical-namespaces-at-here '(let ((b 5)) -here- (let ((b b)) -here-))))
	 (variables-1 (car variables))
	 (variables-2 (cadr variables))
	 (b-1 (namespace-lookup 'b variables-1))
	 (b-2 (namespace-lookup 'b variables-2)))
    (assert (eq (constant-value (sym-definition b-1)) 5))
    (assert (eq b-1 (sym-definition b-2)))))
