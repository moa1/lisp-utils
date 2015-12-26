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

;; Note that the classes VAR, FUN and so on should be named VARIABLE, FUNCTION and so on, but SBCL pollutes the CL package.

;; How to define DEFUN from special forms:
;;CL-USER> (macroexpand '(setf (symbol-function 'test) (lambda () 5)))
;;(LET* ((#:NEW596 (LAMBDA () 5)))
;;  (FUNCALL #'(SETF SYMBOL-FUNCTION) #:NEW596 'TEST))
;;T
;; is accessor #'FDEFINITION redundant to #'SYMBOL-FUNCTION?

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

;; https://stackoverflow.com/questions/4394942/which-standard-common-lisp-macros-special-forms-establish-implicit-blocks-named
;; Which standard Common Lisp macros/special forms establish implicit blocks named nil?
;; DO DO* PROG PROG* LOOP DOLIST DOTIMES DO-SYMBOLS DO-ALL-SYMBOLS DO-EXTERNAL-SYMBOLS

;; special forms defining named blocks: BLOCK FLET LABELS MACROLET

(ql:quickload :lambda-fiddle)

(defpackage :walker
  (:use :cl)
  (:export))

(in-package :walker)

(defstruct (lexenv
	     (:constructor make-lexenv-kw))
  "A lexical environment."
  variables
  functions
  blocks
  ;; tags
  ;; types ;i.e. class- or structure-names
  )

;;;; NAMESPACES

(defclass nso ()
  ((name :initarg :name :accessor nso-name :type symbol
	 :documentation "NIL if not known")
   (freep :initarg :freep :accessor nso-freep :type boolean
	  :documentation "T if it is a free variable/function, or NIL if bound. Note that this is specific to a namespace."))
  (:documentation "a namespace-object (NSO) containing a name and information whether it is free or bound"))
(defclass sym (nso)
  ((definition :initarg :definition :accessor nso-definition ;;TODO uncomment when llist is implemented :typep (or binding llist)
	       :documentation "the parsed object (of type (OR BINDING LLIST)) it is defined in, NIL if not known")
   (declspecs :initarg :declspecs :accessor nso-declspecs :type list
	      :documentation "a list of DECLSPECs that apply to this symbol")
   )
  (:documentation "A symbol referring to a variable or function.
Note that symbols are always parsed in a lexical manner, regardless of whether the actual variable this symbol is referring to may be a lexical or special variable. For example, the symbols *A* in the form (LET ((*A* 1)) *A*) may refer to a lexical or special variable."))
(defclass var (sym)
  ())
(defclass fun (sym)
  ())
(defclass blo (nso)
  ((definition :initarg :definition :accessor nso-definition
	       :documentation "the parsed object of type BLOCK-FORM that it is defined in, NIL if not known"))
  (:documentation "A named block."))

(defvar *print-detailed-walker-objects* nil "If T, print more details of objects in package WALKER.")

(defmethod print-object ((object sym) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if *print-detailed-walker-objects*
	(format stream "NAME:~A FREEP:~A" (nso-name object) (nso-freep object))
	(format stream "NAME:~A" (nso-name object)))))
(defmethod print-object ((object blo) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if *print-detailed-walker-objects*
	(format stream "NAME:~A FREEP:~A" (nso-name object) (nso-freep object))
	(format stream "NAME:~A" (nso-name object)))))

(defun make-namespace ()
  nil)

(defun augment-namespace (symbol object namespace)
  "Create a copy of NAMESPACE where the symbol SYMBOL is bound to object OBJECT.
Example: (augment-namespace 'var-a (make-instance 'var :name 'var-a) (make-namespace))"
  (acons symbol object namespace))

(defun augment-namespace-with-sym (sym namespace)
  (augment-namespace (nso-name sym) sym namespace))

(defun augment-namespace-with-syms (syms namespace)
  (loop for sym in syms do
       (setf namespace (augment-namespace (nso-name sym) sym namespace)))
  namespace)

(defun augment-namespace-with-blo (blo namespace)
  (augment-namespace (nso-name blo) blo namespace))

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
		      (format output "NAME:~A FREEP:~A DEFINITION:~A DECLSPECS:~A" (nso-name sym) (nso-freep sym) (nso-definition sym) (nso-declspecs sym))))
		  str)))
	 (format stream "~A: ~A~%" (car cell) (format-sym (cdr cell))))))

;;;; DECLARATIONS

(defclass declspec ()
  ((parent :initarg :parent :accessor declspec-parent)))
(defclass declspec-type (declspec)
  ((type :initarg :type :accessor declspec-type)
   (vars :initarg :vars :accessor declspec-vars :type list)))
(defclass declspec-ftype (declspec)
  ((type :initarg :type :accessor declspec-type)
   (vars :initarg :vars :accessor declspec-vars :type list))) ;TODO: maybe rename this to FUNS.

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
			   ((find identifier '(type ftype))
			    (assert (and (listp body) (listp (cdr body))) () "Malformed ~A declaration: ~A" identifier expr)
			    (let* ((typespec (car body))
				   (syms (cdr body)))
			      ;; will not check TYPESPEC, has to be done in user code.
			      (let* ((symlookup/create (ecase identifier ((type) #'varlookup/create) ((ftype) #'funlookup/create)))
				     (parsed-syms (loop for sym in syms collect (funcall symlookup/create sym)))
				     (object-type (ecase identifier ((type) 'declspec-type) ((ftype) 'declspec-ftype)))
				     (parsed-declspec (make-instance object-type :parent parent :type typespec :vars parsed-syms)))
				(loop for sym in parsed-syms do
				     (push parsed-declspec (nso-declspecs sym)))
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
(multiple-value-bind (body declspecs variables functions) (parse-declaration-in-body '((declare (type fixnum a)) 5) nil nil nil)
  (assert (and (equal body '(5)) (typep (car declspecs) 'declspec-type)))
  (assert (nso-freep (car (declspec-vars (car declspecs)))))
  (assert (eq (car (declspec-vars (car declspecs))) (namespace-lookup 'a variables)))
  (assert (not (namespace-boundp 'a functions))))
(multiple-value-bind (body declspecs variables functions) (parse-declaration-in-body '((declare (ftype (function () fixnum) a)) 5) nil nil nil)
  (assert (and (equal body '(5)) (typep (car declspecs) 'declspec-ftype)))
  (assert (nso-freep (car (declspec-vars (car declspecs)))))
  (assert (eq (car (declspec-vars (car declspecs))) (namespace-lookup 'a functions)))
  (assert (not (namespace-boundp 'a variables))))

(defmethod print-object ((object declspec-type) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~A" (cons 'type (cons (declspec-type object) (declspec-vars object))))))
(defmethod print-object ((object declspec-ftype) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~A" (cons 'ftype (cons (declspec-type object) (declspec-vars object))))))

;;;; FORMS

(defclass form ()
  ((parent :initarg :parent :accessor form-parent)))
(defclass constant-form (form)
  ((value :initarg :value :accessor form-value :type form)))
(defclass body-form () ;Note: objects of this type must never be created, only subtypes of this type.
  ((body :initarg :body :accessor form-body :type list))) ;list of FORMs
(defclass special-form (form)
  ())
(defclass progn-form (special-form body-form)
  ())
(defclass binding ()
  ((parent :initarg :parent :accessor binding-parent) ;e.g. the LET-form in which it is defined.
   (sym :initarg :sym :accessor binding-sym :type sym)))
(defclass var-binding (binding)
  ((value :initarg :value :accessor binding-value :documentation "The form initializing the variable." :type form)))
(defclass bindings-form ()  ;Note: objects of this type must never be created, only subtypes of this type.
  ((bindings :initarg :bindings :accessor form-bindings :type list) ;list of VAR-BINDINGs, or FUN-BINDINGs
   (declspecs :initarg :declspecs :accessor form-declspecs :type list))) ;list of DECLSPECs
(defclass let-form (special-form body-form bindings-form)
  ())
(defclass let*-form (special-form body-form bindings-form)
  ())
(defclass argument ()
  ((parent :initarg :parent :accessor argument-parent :type functiondef)
   (var :initarg :var :accessor argument-var :type var)))
(defclass simple-argument (argument)
  ())
(defclass optional-argument (argument) ;for &OPTIONAL and &KEY arguments
  ((init :initarg :init :accessor argument-init :type (or null form))
   (suppliedp :initarg :suppliedp :accessor argument-suppliedp :type (or null var))))
(defclass aux-argument (argument)
  ((init :initarg :init :accessor argument-init :type (or null form))))
(defclass llist ()
  ((parent :initarg :parent :accessor form-parent)
   (whole :initarg :whole :accessor llist-whole :type (or null argument))
   (environment :initarg :environment :accessor llist-environment :type (or null argument))
   (required :initarg :required :accessor llist-required :type list) ;list, with each element of type ARGUMENT, or LLIST (for macro-lambda-lists).
   (optional :initarg :optional :accessor llist-optional :type (or null list)) ;list, with each element of type OPTIONAL-ARGUMENT.
   (rest :initarg :rest :accessor llist-rest :type (or null argument))
   (body :initarg :body :accessor llist-body :type (or null argument))
   (key :initarg :key :accessor llist-key :type (or null list)) ;list, with each element of type OPTIONAL-ARGUMENT.
   (allow-other-keys-p :initarg :allow-other-keys-p :accessor llist-allow-other-keys-p :type boolean)
   (aux :initarg :aux :accessor llist-aux :type (or null list)))) ;list, with each element of type AUX-ARGUMENT.
(defclass functiondef (body-form)
  ((parent :initarg :parent :accessor form-parent)
   (llist :initarg :llist :accessor form-llist :type llist)
   (declspecs :initarg :declspecs :accessor form-declspecs :type list))
  (:documentation "Used to define a function without name."))
(defclass block-form (special-form body-form)
  ((blo :initarg :blo :accessor form-blo :type blo)))
(defclass fun-binding (binding functiondef block-form)
  ())
(defclass flet-form (special-form body-form bindings-form)
  ())
(defclass labels-form (special-form body-form bindings-form)
  ())
(defclass lambda-form (special-form functiondef)
  ())
(defclass return-from-form (special-form)
  ((blo :initarg :blo :accessor form-blo :type blo)
   (value :initarg :value :accessor form-value :type form)))
(defclass locally-form (special-form body-form)
  ((declspecs :initarg :declspecs :accessor form-declspecs :type list)))
(defclass the-form (special-form)
  ((type :initarg :type :accessor form-type :type (or symbol list))
   (value :initarg :value :accessor form-value :type form)))

(defmethod print-object ((object constant-form) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (form-value object))))
(defmethod print-object ((object progn-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (form-body object))))
(defmethod print-object ((object var-binding) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when *print-detailed-walker-objects*
      (format stream "~A ~A" (binding-sym object) (binding-value object)))))
(defun body-with-declspecs (object)
  (if (null (form-declspecs object))
      (if (= (length (form-body object)) 1)
	  (car (form-body object))
	  (form-body object))
      (cons (cons 'declare (form-declspecs object)) (form-body object))))
(defmethod print-object ((object bindings-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if *print-detailed-walker-objects*
	(format stream "BINDINGS:~A ~A" (form-bindings object) (body-with-declspecs object))
	(format stream "~A" (form-body object)))))
(defmethod print-object ((object simple-argument) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (argument-var object))))
(defmethod print-object ((object optional-argument) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cond
      ((and (null (argument-init object)) (null (argument-suppliedp object)))
       (format stream "~A" (argument-var object)))
      ((and (null (argument-suppliedp object)))
       (format stream "(~A ~A)" (argument-var object) (argument-init object)))
      (t
       (format stream "(~A ~A ~A)" (argument-var object) (argument-init object) (argument-suppliedp object))))))
(defmethod print-object ((object llist) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when *print-detailed-walker-objects*
      (format stream "~A" (append
			   (let ((it (llist-whole object))) (when it (list '&whole it)))
			   (let ((it (llist-environment object))) (when it (list '&whole it)))
			   (llist-required object)
			   (let ((it (llist-optional object))) (when it (cons '&optional it)))
			   (let ((it (llist-rest object))) (when it (list '&whole it)))
			   (let ((it (llist-body object))) (when it (list '&whole it)))
			   (let ((it (llist-key object))) (when it (cons '&optional it)))
			   (let ((it (llist-allow-other-keys-p object))) (when it (list '&allow-other-keys)))
			   (let ((it (llist-aux object))) (when it (cons '&optional it))))))))
(defmethod print-object ((object fun-binding) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when *print-detailed-walker-objects*
      (format stream "~A ~A ~A" (binding-sym object) (form-llist object) (body-with-declspecs object)))))
(defmethod print-object ((object lambda-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A" (form-llist object) (body-with-declspecs object))))
(defmethod print-object ((object block-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A" (form-blo object) (form-body object))))
(defmethod print-object ((object return-from-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A" (form-blo object) (form-value object))))
(defmethod print-object ((object locally-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (body-with-declspecs object))))
(defmethod print-object ((object the-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A" (form-type object) (form-value object))))

(defun split-lambda-list* (lambda-list)
  (let ((allow-other-keys-p nil))
    (when (find '&allow-other-keys lambda-list)
      (setf allow-other-keys-p t)
      (setf lambda-list (delete '&allow-other-keys lambda-list)))
    (destructuring-bind (required whole environment optional rest body key aux)
	(lambda-fiddle:split-lambda-list lambda-list)
      (assert (not (and rest body)) () "A lambda list may contain only either &REST or &BODY (see CLHS 3.4.4 Macro Lambda Lists: restvar::= [{&rest | &body} var],~% but REST:~A BODY:~A" rest body)
      (values whole environment required optional rest body key allow-other-keys-p aux))))

;; TODO: maybe rename to PARSE-FORM.
(defun parse (form variables functions blocks parent &key customparsep-function customparse-function customparsedeclspecp-function customparsedeclspec-function)
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
	   (blolookup/create (symbol)
	     (if (namespace-boundp symbol blocks)
		 (namespace-lookup symbol blocks)
		 (make-instance 'blo :name symbol :freep t))) ;do not bind :DEFINITION
	   (reparse (form parent &key (variables variables) (functions functions) (blocks blocks))
	     (parse form variables functions blocks parent
		    :customparsep-function customparsep-function
		    :customparse-function customparse-function
		    :customparsedeclspecp-function customparsedeclspecp-function
		    :customparsedeclspec-function customparsedeclspec-function))
	   (parse-lambda-list (lambda-list variables functions parent &key (allow-macro-lambda-list nil))
	     "Returns two values: an instance of type LLIST (representing the parsed LAMBDA-LIST) and the namespace VARIABLES augmented by the variables created by the arguments in LAMBDA-LIST.
Supported lambda list keywords:
CLHS Figure 3-12. Standardized Operators that use Ordinary Lambda Lists: An ordinary lambda list can contain the lambda list keywords shown in the next figure. &allow-other-keys &key &rest &aux &optional
CLHS Figure 3-18. Lambda List Keywords used by Macro Lambda Lists: A macro lambda list can contain the lambda list keywords shown in the next figure. &allow-other-keys &environment &rest &aux &key &whole &body &optional"
	     ;; TODO: add possibility to call custom parsing function.
	     (assert (typep parent 'functiondef))
	     ;; TODO: come up with a scheme for this function so that it is easily extensible with other types of lambda lists.
	     ;; "CLHS 3.4.1 Ordinary Lambda Lists" says: "An init-form can be any form. Whenever any init-form is evaluated for any parameter specifier, that form may refer to any parameter variable to the left of the specifier in which the init-form appears, including any supplied-p-parameter variables, and may rely on the fact that no other parameter variable has yet been bound (including its own parameter variable)." But luckily the order of allowed keywords is fixed for both normal lambda-lists and macro lambda-lists, and a normal lambda-list allows a subset of macro lambda-lists. Maybe I'll have to rewrite this for other lambda-lists.
	     (multiple-value-bind (whole environment required optional rest body key allow-other-keys-p aux)
		 (split-lambda-list* lambda-list)
	       (assert (if (not allow-macro-lambda-list) (and (null whole) (null environment) (null body)) t) () "Ordinary lambda list erroneously contains &WHOLE, &ENVIRONMENT, or &BODY: ~A" lambda-list)
	       (let ((new-llist (make-instance 'llist :parent parent))
		     (new-variables variables))
		 (labels ((add-simple-argument (varname)
			    (let* ((new-argument (make-instance 'simple-argument :parent new-llist))
				   (new-var (make-instance 'var :name varname :freep nil :definition new-argument :declspecs nil)))
			      (setf (argument-var new-argument) new-var)
			      (setf new-variables (augment-namespace-with-sym new-var new-variables))
			      new-argument))
			  (add-optional-argument (varname init-form supplied-varname)
			    "If INIT-FORM or SUPPLIED-VARNAME is not given for the optional argument, pass NIL for it. If SUPPLIED-VARNAME is given, INIT-FORM must be non-NIL as well."
			    (assert (if supplied-varname (not (null init-form)) t))
			    (let* ((new-argument (make-instance 'optional-argument :parent new-llist))
				   (new-var (make-instance 'var :name varname :freep nil :definition new-argument :declspecs nil)))
			      (setf (argument-var new-argument) new-var)
			      (setf new-variables (augment-namespace-with-sym new-var new-variables))
			      (let ((parsed-init-form (if (null init-form) nil (reparse init-form new-argument :variables new-variables))))
				(setf (argument-init new-argument) parsed-init-form)
				(let ((new-supplied-var (if (null supplied-varname) nil (make-instance 'var :name supplied-varname :freep nil :definition new-argument :declspecs nil))))
				  (setf (argument-suppliedp new-argument) new-supplied-var)
				  (when new-supplied-var (setf new-variables (augment-namespace-with-sym new-supplied-var new-variables)))
				  new-argument))))
			  (parse-simple-argument (varname)
			    (assert (not (or (null varname) (eq varname t))) () "Argument name must not be NIL or T, but is ~A" varname)
			    ;; TODO: add checking for "constant variable" in: CLHS 3.4.1 Ordinary Lambda Lists says: A var or supplied-p-parameter must be a symbol that is not the name of a constant variable.
			    varname)
			  (parse-optional-or-key-or-aux-argument (argform argument-type)
			    "Parse optional or keyword argument ARGFORM and return its parsed AST."
			    (assert (find argument-type '(&OPTIONAL &KEY &AUX)))
			    (flet ((parse-all ()
				     (cond
				       ((and (not (null argform)) (not (eq argform t)) (symbolp argform))
					(values argform nil nil))
				       (t
					(assert (consp argform) () "Invalid ~A argument ~A must be either a SYMBOL or of the form (SYMBOL FORM) or (SYMBOL FORM SYMBOL)" argument-type argform)
					(let ((varname (car argform)) (init-form nil) (supplied-varname nil)
					      (rest (cdr argform)))
					  (assert (listp rest) () "Invalid ~A argument seems to be of the form (SYMBOL FORM) or (SYMBOL FORM SYMBOL), but is ~A" argument-type argform)
					  (when (not (null rest))
					    (let ((init-form2 (car rest))
						  (rest (cdr rest)))
					      (setf init-form init-form2) ;nothing to check for INIT-FORM
					      (when (eq argument-type '&AUX)
						(assert (null rest) () "Invalid ~A argument seems to be of the form (SYMBOL FORM), but is ~A which is too long" argument-type argform))
					      (assert (listp rest) () "Invalid ~A argument seems to be of the form (SYMBOL FORM SYMBOL), but is ~A" argument-type argform)
					      (when (not (null rest))
						(setf supplied-varname (car rest))
						(assert (null (cdr rest)) () "Invalid ~A argument ~A seems to be of the form (SYMBOL FORM SYMBOL), but does not match" argument-type argform))))
					  (values varname init-form supplied-varname))))))
			      (multiple-value-bind (varname init-form supplied-varname) (parse-all)
				(when (eq argument-type '&KEY)
				  (cond
				    ((symbolp varname))
				    ((and (consp varname) (eq (car varname) 'keyword-name)) ;TODO: only allow VARNAME to be of the form (keyword-name SYMBOL) for &KEY arguments (and not &OPTIONAL).
				     (assert (and (consp (cdr varname)) (null (cddr varname))) () "If the optional argument variable name starts with the symbol KEYWORD-NAME, then it must be of the form (keyword-name SYMBOL), but is ~A" varname)
				     (setf varname (cadr varname)))
				    (t (error "Invalid ~A argument name ~A" argument-type varname))))
				(assert (not (or (null varname) (eq varname t))) () "Argument name must not be NIL or T, but is ~A" varname)
				(assert (or (null supplied-varname) (and (symbolp supplied-varname) (not (or (null supplied-varname) (eq supplied-varname t))))) () "Supplied-p-parameter (of optional argment ~A) must not be NIL or T, but is ~A" varname supplied-varname)
				(values varname init-form supplied-varname)))))
		   ;; evaluation is in this order: "wholevar envvar reqvars envvar optvars envvar restvar envvar keyvars envvar auxvars envvar" (from CLHS 3.4.4 Macro Lambda Lists).
		   (let* ((whole (unless (null whole) (add-simple-argument (parse-simple-argument whole))))
			  (environment (unless (null environment) (add-simple-argument (parse-simple-argument environment))))
			  (required (loop for varname in required collect
					 (cond
					   ((and (not (null varname)) (not (eq varname t)) (symbolp varname))
					    (add-simple-argument (parse-simple-argument varname)))
					   ((and allow-macro-lambda-list (listp varname))
					    (multiple-value-bind (parsed-llist new-variables2)
						(parse-lambda-list varname new-variables functions new-llist :allow-macro-lambda-list allow-macro-lambda-list)
					      (setf new-variables new-variables2)
					      parsed-llist))
					   (t (error "Invalid required argument ~A in lambda list ~A" varname lambda-list)))))
			  (optional (loop for argform in optional collect (multiple-value-bind (varname init-form supplied-varname) (parse-optional-or-key-or-aux-argument argform '&optional) (add-optional-argument varname init-form supplied-varname))))
			  (rest (unless (null rest) (add-simple-argument (parse-simple-argument rest))))
			  (body (unless (null rest) (add-simple-argument (parse-simple-argument rest))))
			  (key (loop for argform in key collect (multiple-value-bind (varname init-form supplied-varname) (parse-optional-or-key-or-aux-argument argform '&key) (add-optional-argument varname init-form supplied-varname))))
			  (aux (loop for argform in aux collect (multiple-value-bind (varname init-form supplied-varname) (parse-optional-or-key-or-aux-argument argform '&aux) (add-optional-argument varname init-form supplied-varname)))))
		     (setf (llist-whole new-llist) whole)
		     (setf (llist-environment new-llist) environment)
		     (setf (llist-required new-llist) required)
		     (setf (llist-optional new-llist) optional)
		     (setf (llist-rest new-llist) rest)
		     (setf (llist-body new-llist) body)
		     (setf (llist-key new-llist) key)
		     (setf (llist-allow-other-keys-p new-llist) allow-other-keys-p)
		     (setf (llist-aux new-llist) aux)))
		 (values new-llist new-variables))))
	   (parse-functiondef (form variables functions blocks parent)
	     "Parse FORM, which must be of the form (LAMBDA-LIST &BODY BODY) and create a corresponding FUNCTIONDEF-object."
	     (flet ((split-lambda-list-and-body (form)
		      (assert (and (consp form) (listp (car form))) () "Invalid lambda-list ~A" form)
		      (let ((lambda-list (car form))
			    (body (cdr form)))
			(assert (listp body) () "Invalid body ~A; must be a list" body)
			(values lambda-list body))))
	       (multiple-value-bind (lambda-list body) (split-lambda-list-and-body form)
		 (let ((new-functiondef (make-instance 'functiondef :parent parent)))
		   (multiple-value-bind (new-llist variables-in-functiondef)
		       (parse-lambda-list lambda-list variables functions new-functiondef :allow-macro-lambda-list nil)
		     (setf (form-llist new-functiondef) new-llist)
		     (multiple-value-bind (body parsed-declspecs variables-in-functiondef functions-in-functiondef)
			 (parse-declaration-in-body body variables-in-functiondef functions new-functiondef :customparsep-function customparsedeclspecp-function :customparse-function customparsedeclspec-function)
		       (setf (form-declspecs new-functiondef) parsed-declspecs)
		       (let ((parsed-body (loop for form in body collect (reparse form new-functiondef :variables variables-in-functiondef :functions functions-in-functiondef :blocks blocks))))
			 (setf (form-body new-functiondef) parsed-body))))
		   new-functiondef))))
	   (setf-binding-slots-to-functiondef-slots (binding parsed-functiondef)
	     (setf (form-parent binding) (form-parent parsed-functiondef) ;copy everything from PARSED-FUNCTIONDEF to BINDING
		   (form-llist binding) (form-llist parsed-functiondef)
		   (form-declspecs binding) (form-declspecs parsed-functiondef)
		   (form-body binding) (form-body parsed-functiondef))))
    (cond
      ((and (not (null customparsep-function)) (funcall customparsep-function form variables functions blocks parent))
       (funcall customparse-function form variables functions blocks parent))
      ((or (eq form nil) (eq form t))
       (make-instance 'constant-form :value form :parent parent))
      ((symbolp form)
       (varlookup/create form)) ;TODO: insert code of #'VARLOOKUP/CREATE here if it is called only once.
      ((and (consp form) (eq (car form) 'function) (consp (cdr form)) (symbolp (cadr form)) (null (cddr form)))
       (funlookup/create (cadr form))) ;TODO: insert code of #'FUNLOOKUP/CREATE here if it is called only once.
      ((atom form)
       (make-instance 'constant-form :value form :parent parent))
      (t
       (let ((head (car form))
	     (rest (cdr form)))
	 (cond
	   ((eq head 'progn)
	    (let* ((current (make-instance 'progn-form :parent parent :body nil))
		   (body rest)
		   (parsed-body (loop for form in body collect (reparse form current))))
	      (setf (form-body current) parsed-body)
	      current))
	   ((find head '(let let* flet labels))
	    (assert (and (consp rest) (listp (car rest))) () "cannot parse ~A-form:~%~A" head form)
	    (let* ((definitions (car rest))
		   (body (cdr rest))
		   (form-type (ecase head ((let) 'let-form) ((let*) 'let*-form) ((flet) 'flet-form) ((labels) 'labels-form)))
		   (current (make-instance form-type :parent parent :body nil))) ;:BINDINGS and :DECLSPECS are defined below
	      (labels ((make-var-binding (def variables)
			 (assert (and (consp def) (symbolp (car def)) (not (null (car def))) (not (null (cdr def))) (null (cddr def))) () "cannot parse definition in ~A-form:~%~A" head def)
			 (let* ((name (car def))
				(value-form (cadr def))
				(binding (make-instance 'var-binding :parent current))
				(parsed-value (reparse value-form binding :variables variables))
				(sym (make-instance 'var :name name :freep nil :definition binding :declspecs nil)))
			   (setf (binding-sym binding) sym) (setf (binding-value binding) parsed-value)
			   binding))
		       (make-fun-binding (def functions)
			 (assert (and (consp def) (symbolp (car def)) (not (null (car def))) (not (null (cdr def)))) () "cannot parse definition in ~A-form:~%~A" head def)
			 (let* ((name (car def))
				(body-form (cdr def))
				(blo (make-instance 'blo :name name :freep nil))
				(binding (make-instance 'fun-binding :parent current :blo blo))
				(parsed-functiondef (parse-functiondef body-form variables functions (augment-namespace-with-blo blo blocks) current))
				(sym (make-instance 'fun :name name :freep nil :definition binding :declspecs nil)))
			   (setf (binding-sym binding) sym)
			   (setf (nso-definition blo) binding)
			   (setf-binding-slots-to-functiondef-slots binding parsed-functiondef)
			   binding)))
		(multiple-value-bind (parsed-bindings new-variables new-functions)
		    (let ((parse-value-function (ecase head ((let let*) #'make-var-binding) ((flet labels) #'make-fun-binding)))
			  (namespace (ecase head ((let let*) variables) ((flet labels) functions))))
		      (cond
			((find head '(let flet))
			 (let* ((parsed-bindings (loop for def in definitions collect (funcall parse-value-function def namespace)))
				(parsed-syms (loop for binding in parsed-bindings collect (binding-sym binding))))
			   (ecase head
			     ((let) (values parsed-bindings (augment-namespace-with-syms parsed-syms variables) functions))
			     ((flet) (values parsed-bindings variables (augment-namespace-with-syms parsed-syms functions))))))
			((find head '(let* labels))
			 (let* ((new-namespace namespace)
				(parsed-bindings (loop for def in definitions collect
						      (let* ((parsed-binding (funcall parse-value-function def new-namespace))
							     (parsed-sym (binding-sym parsed-binding)))
							(setf new-namespace (augment-namespace-with-sym parsed-sym new-namespace))
							parsed-binding))))
			   (ecase head
			     ((let*) (values parsed-bindings new-namespace functions))
			     ((labels) (values parsed-bindings variables new-namespace)))))
			(t (error "unknown HEAD"))))
		  (multiple-value-bind (body parsed-declspecs new-variables new-functions)
		      (parse-declaration-in-body body new-variables new-functions current :customparsep-function customparsedeclspecp-function :customparse-function customparsedeclspec-function)
		    (setf (form-bindings current) parsed-bindings)
		    (setf (form-declspecs current) parsed-declspecs)
		    (let ((parsed-body (loop for form in body collect
					    (reparse form current :variables new-variables :functions new-functions))))
		      (setf (form-body current) parsed-body)
		      current))))))
	   ((eq head 'lambda)
	    (let* ((lambda-list-and-body (cdr form))
		   (current (make-instance 'lambda-form))
		   (parsed-functiondef (parse-functiondef lambda-list-and-body variables functions blocks parent)))
	      (setf-binding-slots-to-functiondef-slots current parsed-functiondef)
	      current))
	   ((eq head 'block)
	    (assert (and (consp rest) (symbolp (car rest)) (listp (cdr rest))) () "Cannot parse BLOCK-form ~A" form)
	    (let* ((name (car rest))
		   (body (cdr rest))
		   (blo (make-instance 'blo :name name :freep nil))
		   (current (make-instance 'block-form :parent parent :blo blo))
		   (parsed-body (loop for form in body collect
				     (reparse form current :blocks (augment-namespace-with-blo blo blocks)))))
	      (setf (nso-definition blo) current)
	      (setf (form-body current) parsed-body)
	      current))
	   ((eq head 'return-from)
	    (assert (and (consp rest) (symbolp (car rest)) (consp (cdr rest)) (null (cddr rest))) () "Cannot parse RETURN-FROM-form ~A" form)
	    (let* ((name (car rest))
		   (value-form (cadr rest))
		   (blo (blolookup/create name))
		   (current (make-instance 'return-from-form :parent parent :blo blo))
		   (parsed-value (reparse value-form current)))
	      (setf (form-value current) parsed-value)
	      current))
	   ((eq head 'locally)
	    (assert (and (consp rest) (consp (car rest)) (consp (cdr rest))) () "Cannot parse LOCALLY-form ~A" form)
	    (let ((body rest)
		  (current (make-instance 'locally-form :parent parent)))
	      (multiple-value-bind (body parsed-declspecs new-variables new-functions)
		  (parse-declaration-in-body body variables functions current :customparsep-function customparsedeclspecp-function :customparse-function customparsedeclspec-function)
		(setf (form-declspecs current) parsed-declspecs)
		(let ((parsed-body (loop for form in body collect
					(reparse form current :variables new-variables :functions new-functions))))
		  (setf (form-body current) parsed-body)
		  current))))
	   ((eq head 'the)
	    (assert (and (consp rest) (or (symbolp (car rest)) (consp (car rest))) (consp (cdr rest)) (null (cddr rest))) () "Cannot parse THE-form ~A" form)
	    (let* ((value-type-form (car rest))
		   (value-form (cadr rest))
		   (current (make-instance 'the-form :parent parent :type value-type-form))
		   (parsed-value (reparse value-form current)))
	      (setf (form-value current) parsed-value)
	      current))))))))

(defun parse-with-empty-namespaces (form &key customparsep-function customparse-function customparsedeclspecp-function customparsedeclspec-function)
  (parse form (make-namespace) (make-namespace) (make-namespace) (make-namespace)
	 :customparsep-function customparsep-function
	 :customparse-function customparse-function
	 :customparsedeclspecp-function customparsedeclspecp-function
	 :customparsedeclspec-function customparsedeclspec-function))

;; tests for PARSE
(defun lexical-namespaces-at (form heresymbol)
  "Parse FORM and collect the lexical namespaces at the positions in FORM marked by the symbol HERESYMBOL.
Returns two values: a list containing the lexical variable namespaces, and a list containing the lexical function namespaces."
  (let* ((variables-here-list nil)
	 (functions-here-list nil)
	 (blocks-here-list nil))
    (parse-with-empty-namespaces form
				 :customparsep-function (lambda (form variables functions blocks parent)
							  (declare (ignore variables functions blocks parent))
							  (eq form heresymbol))
				 :customparse-function (lambda (form variables functions blocks parent)
							 (declare (ignorable form parent))
							 (push variables variables-here-list)
							 (push functions functions-here-list)
							 (push blocks blocks-here-list)))
    (values (nreverse variables-here-list) (nreverse functions-here-list) (nreverse blocks-here-list))))

(defun test-parse-symbol-reference ()
  (declare (optimize (debug 3)))
  (flet ((lexical-namespaces-at (form)
	   (lexical-namespaces-at form '-here-)))
    (assert (nso-freep (parse-with-empty-namespaces 'a)))
    (assert (nso-freep (parse-with-empty-namespaces '(function a))))
    (assert (not (nso-freep (car (form-body (parse-with-empty-namespaces '(let ((a 1)) a)))))))
    (assert (not (nso-freep (car (form-body (parse-with-empty-namespaces '(let* ((a 1)) a)))))))
    (assert (not (nso-freep (car (form-body (parse-with-empty-namespaces '(flet ((a ())) #'a)))))))
    (assert (not (nso-freep (car (form-body (parse-with-empty-namespaces '(labels ((a ())) #'a)))))))
    (assert (let* ((variables (car (lexical-namespaces-at '(let ((b 5) (c b)) -here-))))
		   (b-sym (namespace-lookup 'b variables))
		   (c-sym (namespace-lookup 'c variables)))
	      (not (eq b-sym (binding-value (nso-definition c-sym))))))
    (assert (let* ((functions (car (nth-value 1 (lexical-namespaces-at '(flet ((b ()) (c () #'b)) -here-)))))
		   (b-sym (namespace-lookup 'b functions))
		   (c-sym (namespace-lookup 'c functions)))
	      (not (eq b-sym (car (form-body (nso-definition c-sym)))))))
    (assert (let* ((variables (car (lexical-namespaces-at '(let* ((b 5) (c b)) -here-))))
		   (b-sym (namespace-lookup 'b variables))
		   (c-sym (namespace-lookup 'c variables)))
	      (eq b-sym (binding-value (nso-definition c-sym)))))
    (assert (let* ((functions (car (nth-value 1 (lexical-namespaces-at '(labels ((b ()) (c () #'b)) -here-)))))
		   (b-sym (namespace-lookup 'b functions))
		   (c-sym (namespace-lookup 'c functions)))
	      (eq b-sym (car (form-body (nso-definition c-sym))))))
    (let* ((variables (lexical-namespaces-at '(let ((b 5)) -here- (let ((b b)) -here-))))
	   (variables-1 (car variables))
	   (variables-2 (cadr variables))
	   (b-1 (namespace-lookup 'b variables-1))
	   (b-2 (namespace-lookup 'b variables-2)))
      (assert (eq (form-value (binding-value (nso-definition b-1))) 5))
      (assert (eq b-1 (binding-value (nso-definition b-2))))
      (assert (not (eq b-1 b-2))))
    (let* ((variables (lexical-namespaces-at '(let* ((b 5)) -here- (let* ((b b)) -here-))))
	   (variables-1 (car variables))
	   (variables-2 (cadr variables))
	   (b-1 (namespace-lookup 'b variables-1))
	   (b-2 (namespace-lookup 'b variables-2)))
      (assert (eq (form-value (binding-value (nso-definition b-1))) 5))
      (assert (eq b-1 (binding-value (nso-definition b-2))))
      (assert (not (eq b-1 b-2))))
    (let* ((functions (nth-value 1 (lexical-namespaces-at '(flet ((b ())) -here- (flet ((b () #'b)) -here-)))))
	   (functions-1 (car functions))
	   (functions-2 (cadr functions))
	   (b-1 (namespace-lookup 'b functions-1))
	   (b-2 (namespace-lookup 'b functions-2)))
      (assert (not (eq b-1 b-2)))
      (assert (eq b-1 (car (form-body (nso-definition b-2))))))
    (let* ((functions (nth-value 1 (lexical-namespaces-at '(labels ((b ())) -here- (labels ((b () #'b)) -here-)))))
	   (functions-1 (car functions))
	   (functions-2 (cadr functions))
	   (b-1 (namespace-lookup 'b functions-1))
	   (b-2 (namespace-lookup 'b functions-2)))
      (assert (not (eq b-1 b-2)))
      (assert (eq b-1 (car (form-body (nso-definition b-2))))))))
(test-parse-symbol-reference)

(defun test-parse-declaration ()
  (flet ((all-equal (&rest rest)
	   (if (null rest)
	       t
	       (loop for o1 in (butlast rest 1) for o2 in (cdr rest) always (equal o1 o2)))))
    (let* ((form '(let ((a 1)) (declare (type fixnum a)) a))
	   (ast (parse-with-empty-namespaces form))
	   (declspec-type (car (form-declspecs ast))))
      (assert (all-equal (binding-sym (car (form-bindings ast))) (car (declspec-vars declspec-type)) (car (form-body ast)))))
    (let* ((form '(flet ((a ())) (declare (ftype fixnum a)) #'a))
	   (ast (parse-with-empty-namespaces form))
	   (declspec-ftype (car (form-declspecs ast))))
      (assert (all-equal (binding-sym (car (form-bindings ast))) (car (declspec-vars declspec-ftype)) (car (form-body ast)))))
    (let* ((ast (parse-with-empty-namespaces '(locally (declare (type fixnum a)) a)))
	   (declspec-type (car (form-declspecs ast)))
	   (declspec-type-a (car (declspec-vars declspec-type)))
	   (body-a (car (form-body ast))))
      (assert (eq declspec-type-a body-a))
      (assert (nso-freep declspec-type-a)))))
(test-parse-declaration)

(defun test-parse-lambda-list ()
  (declare (optimize (debug 3)))
  (flet ((lexical-namespaces-at (form)
	   (lexical-namespaces-at form '-here-)))
    (let* ((variables (lexical-namespaces-at '(let ((a 1)) -here- (flet ((test (a &optional (b (progn -here- a))) -here-)) -here-))))
	   (a1-sym (namespace-lookup 'a (car variables)))
	   (a2-sym (namespace-lookup 'a (cadr variables)))
	   (a3-sym (namespace-lookup 'a (caddr variables)))
	   (a4-sym (namespace-lookup 'a (cadddr variables))))
      (assert (equal a1-sym a4-sym))
      (assert (equal a2-sym a3-sym))
      (assert (not (equal a1-sym a2-sym)))))
  (let* ((ast (parse-with-empty-namespaces '(flet ((test (a &optional (b a)) b)) a b)))
	 (test (car (form-bindings ast)))
	 (llist (form-llist test))
	 (llist-a (argument-var (car (llist-required llist))))
	 (llist-b (argument-var (car (llist-optional llist))))
	 (llist-binit (argument-init (car (llist-optional llist))))
	 (body-a (first (form-body ast)))
	 (body-b (second (form-body ast))))
    (assert (equal llist-a llist-binit))
    (assert (not (equal llist-a body-a)))
    (assert (not (equal llist-b body-b))))
  ;; TODO maybe: In the AST returned by (PARSE-WITH-EMPTY-NAMESPACES '(MACROLET ((TEST (A (&OPTIONAL (C A))) -HERE-)))) at lexical position -HERE- the init form of ARGUMENT-C must be equal to (ARGUMENT-VAR A-ARGUMENT).
  )
(test-parse-lambda-list)

(defun test-parse-block-reference ()
  (declare (optimize (debug 3)))
  (assert (nso-freep (form-blo (parse-with-empty-namespaces '(return-from undef 1)))))
  (let* ((ast (parse-with-empty-namespaces '(block test (return-from test 1))))
	 (block-blo (form-blo ast))
	 (return-from-blo (form-blo (car (form-body ast)))))
    (assert (equal block-blo return-from-blo))
    (assert (eq (nso-name block-blo) 'test))
    (assert (not (nso-freep block-blo)))
    (assert (eq (nso-definition block-blo) ast)))
  (let* ((ast (parse-with-empty-namespaces '(flet ((test () (return-from test 1))) (return-from test 2))))
	 (test-fun (car (form-bindings ast)))
	 (test-fun-blo (form-blo test-fun))
	 (return-from1-blo (form-blo (car (form-body test-fun))))
	 (return-from2-blo (form-blo (car (form-body ast)))))
    (assert (equal test-fun-blo return-from1-blo))
    (assert (eq (nso-name test-fun-blo) 'test))
    (assert (not (nso-freep test-fun-blo)))
    (assert (eq (nso-definition test-fun-blo) test-fun))
    (assert (not (eq test-fun-blo return-from2-blo)))
    (assert (nso-freep return-from2-blo))))
(test-parse-block-reference)
