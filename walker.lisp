;;(load "/home/toni/quicklisp/setup.lisp")

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

(defpackage :walker
  (:documentation "A syntactic parser of Common Lisp.
Its scope is the syntactic handling of arbitrary Common Lisp code, i.e. parsing a list into an abstract syntax tree, and TODO: FIXME: converting an abstract syntax tree into a list.
It should have as little semantics in it as possible (while still being useful as a Common Lisp parser). In particular, there is no notion of constant variables (so NIL and T are ordinary variable names).
Type declarations are parsed, but the contained types are neither parsed nor interpreted.")
  (:use :cl)
  (:export
   ;; NAMESPACES
   :nso
   :nso-name
   :nso-freep
   :sym
   :nso-definition
   :nso-declspecs
   :var
   :fun
   :blo
   :nso-definition
   :*print-detailed-walker-objects*   
   :make-namespace
   :augment-namespace
   :augment-namespace-with-var
   :augment-namespace-with-fun
   :augment-namespace-with-blo
   :namespace-boundp
   :namespace-lookup
   :print-namespace
   :valid-function-name-p
   :varlookup/create
   :funlookup/create
   :blolookup/create
   ;; DECLARATIONS
   :declspec
   :declspec-parent
   :declspec-type
   :declspec-vars
   :declspec-ftype
   :declspec-type
   :declspec-funs
   :parse-declaration-in-body
   ;; LAMBDA LISTS
   :argument
   :argument-parent
   :argument-var
   :required-argument
   :optional-argument
   :argument-init
   :argument-suppliedp
   :argument-keywordp
   :argument-keyword
   :aux-argument
   :argument-init
   :llist
   :form-parent
   :ordinary-llist
   :llist-required
   :llist-optional
   :llist-rest
   :llist-key
   :llist-allow-other-keys
   :llist-aux
   :macro-llist
   :llist-whole
   :llist-environment
   :llist-required
   :llist-optional
   :llist-rest
   :llist-body
   :llist-key
   :llist-allow-other-keys
   :llist-aux
   :parse-required-argument
   :parse-optional-or-key-or-aux-argument
   ;;:parse-lambda-list ;do not export this as it should be split into several smaller functions.
   :parse-ordinary-lambda-list
   :parse-macro-lambda-list
   ;; FORMS
   :generalform
   :selfevalobject
   :selfevalobject-object
   :form
   :form-parent
   :body-form
   :form-body
   :special-form
   :progn-form
   :binding
   :binding-parent
   :binding-sym
   :var-binding
   :binding-value
   :bindings-form
   :form-bindings
   :form-declspecs
   :let-form
   :let*-form
   :functiondef
   :form-parent
   :form-llist
   :form-declspecs
   :block-form
   :form-blo
   :fun-binding
   :flet-form
   :labels-form
   :lambda-form
   :return-from-form
   :form-blo
   :form-value
   :locally-form
   :form-declspecs
   :the-form
   :form-type
   :form-value
   :if-form
   :form-test
   :form-then
   :form-else
   :setq-form
   :form-vars
   :form-values
   :catch-form
   :form-tag
   :throw-form
   :form-tag
   :form-value
   :eval-when-form
   :form-situations
   :load-time-value-form
   :form-value
   :form-readonly
   :quote-form
   :form-object
   :multiple-value-call-form
   :form-function
   :multiple-value-prog1-form
   :form-function
   :progv-form
   :form-symbols
   :form-values
   :unwind-protect-form
   :form-protected
   :application-form
   :form-fun
   :form-arguments
   :body-with-declspecs
   :parse
   :parse-with-empty-namespaces
   :lexical-namespaces-at
   ))

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
  ((name :initarg :name :accessor nso-name :type (or symbol list)) ;LIST is allowed for functions named (SETF NAME)
   (freep :initarg :freep :accessor nso-freep :type boolean
	  :documentation "T if it is a free variable/function, or NIL if bound. Note that this is specific to a namespace."))
  (:documentation "a namespace-object (NSO) containing a name and information whether it is free or bound"))
(defclass sym (nso)
  ((definition :initarg :definition :accessor nso-definition :type (or binding llist)
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

(defvar *print-detailed-walker-objects* t "If T, print more details of objects in package WALKER.")

(defmethod print-object ((object sym) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if *print-detailed-walker-objects*
	(format stream "NAME:~S FREEP:~S" (nso-name object) (nso-freep object))
	(format stream "NAME:~S" (nso-name object)))))
(defmethod print-object ((object blo) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if *print-detailed-walker-objects*
	(format stream "NAME:~S FREEP:~S" (nso-name object) (nso-freep object))
	(format stream "NAME:~S" (nso-name object)))))

(defun make-namespace ()
  nil)

(defun augment-namespace (symbol object namespace)
  "Create a copy of NAMESPACE where the symbol SYMBOL is bound to object OBJECT.
Example: (augment-namespace 'var-a (make-instance 'var :name 'var-a) (make-namespace))"
  (acons symbol object namespace))

(defun augment-namespace-with-var (sym namespace)
  (assert (typep sym 'var))
  (augment-namespace (nso-name sym) sym namespace))

(defun augment-namespace-with-fun (sym namespace)
  (assert (typep sym 'fun))
  (augment-namespace (nso-name sym) sym namespace))

(defun augment-namespace-with-blo (blo namespace)
  (assert (typep blo 'blo))
  (augment-namespace (nso-name blo) blo namespace))

(defun namespace-boundp (symbol namespace)
  "Return T if the symbol SYMBOL is bound in NAMESPACE."
  (let ((cell (assoc symbol namespace :test #'equal)))
    (not (null cell))))

(defun namespace-lookup (symbol namespace)
  "Return the object bound to symbol SYMBOL in NAMESPACE."
  (let ((cell (assoc symbol namespace :test #'equal)))
    (if (null cell)
	(error "unknown symbol ~S in namespace." symbol)
	(cdr cell))))

(defun print-namespace (namespace &optional (stream t))
  (loop for cell in (reverse namespace) do ;print from oldest to newest
       (flet ((format-sym (sym)
		(let ((str (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
		  (with-output-to-string (output str)
		    (print-unreadable-object (sym output :type t :identity t)
		      (format output "NAME:~S FREEP:~S DEFINITION:~S DECLSPECS:~S" (nso-name sym) (nso-freep sym) (nso-definition sym) (nso-declspecs sym))))
		  str)))
	 (format stream "~S: ~S~%" (car cell) (format-sym (cdr cell))))))

(defun valid-function-name-p (name)
  "Checks whether NAME is naming a function, i.e. is either of the form NAME or (SETF NAME).
In those cases, return as first value 'FUN or 'SETF-FUN, and as second value the NAME. Returns NIL if FORM is not a function form."
  (if (symbolp name)
      (values 'fun name)
      (when (and (consp name) (eq (car name) 'setf) (consp (cdr name)) (null (cddr name)))
	(let ((name (cadr name)))
	  (values 'setf-fun name)))))

(assert (equal (multiple-value-list (valid-function-name-p nil)) '(FUN NIL)))
(assert (equal (multiple-value-list (valid-function-name-p t)) '(FUN T)))
(assert (equal (multiple-value-list (valid-function-name-p 'a)) '(FUN A)))
(assert (equal (multiple-value-list (valid-function-name-p '(setf a))) '(SETF-FUN A)))
(assert (null (valid-function-name-p '(setf a b))))

(defun varlookup/create (symbol variables)
  (assert (symbolp symbol))
  (if (namespace-boundp symbol variables)
      (values (namespace-lookup symbol variables) t)
      (values (make-instance 'var :name symbol :freep t :declspecs nil) nil))) ;do not bind :DEFINITION

(defun funlookup/create (symbol functions)
  (multiple-value-bind (fun-type name) (valid-function-name-p symbol)
    (declare (ignore name))
    (assert (not (null fun-type)) () "Invalid function name ~S" symbol)
    (if (namespace-boundp symbol functions)
	(values (namespace-lookup symbol functions) t)
	(values (make-instance 'fun :name symbol :freep t :declspecs nil) nil)))) ;do not bind :DEFINITION

(defun blolookup/create (symbol blocks)
  (assert (symbolp symbol))
  (if (namespace-boundp symbol blocks)
      (values (namespace-lookup symbol blocks) t)
      (values (make-instance 'blo :name symbol :freep t) nil))) ;do not bind :DEFINITION

;;;; DECLARATIONS

(defclass declspec ()
  ((parent :initarg :parent :accessor declspec-parent)))
(defclass declspec-type (declspec)
  ((type :initarg :type :accessor declspec-type)
   (vars :initarg :vars :accessor declspec-vars :type list)))
(defclass declspec-ftype (declspec)
  ((type :initarg :type :accessor declspec-type)
   (funs :initarg :funs :accessor declspec-funs :type list)))

(defun parse-declaration-in-body (body variables functions parent &key customparsep-function customparse-function)
  "Parses declarations in the beginning of BODY.
Returns four values: the rest of the BODY that does not start with a DECLARE-expression, a list of DECLSPEC-objects, the updated VARIABLES and FUNCTIONS.
Side-effects: Adds references of the created DECLSPEC-objects to the DECLSPEC-slots of VARIABLES and FUNCTIONS."
  (declare (optimize (debug 3)))
  (assert (listp body) () "Malformed BODY:~%~S" body)
  (labels ((varlookup/create* (symbol)
	     (multiple-value-bind (var boundp) (varlookup/create symbol variables)
	       (when (not boundp)
		 (setf variables (augment-namespace-with-var var variables))) ;do not bind :DEFINITION
	       var))
	   (funlookup/create* (symbol)
	     (multiple-value-bind (fun boundp) (funlookup/create symbol functions)
	       (when (not boundp)
		 (setf functions (augment-namespace-with-fun fun functions))) ;do not bind :DEFINITION
	       fun))
	   (parse-declspecs (declspecs collected-declspecs)
	     "Example: (parse-declspecs '(type fixnum a b c) nil)"
	     (cond
	       ((null declspecs)
		(nreverse collected-declspecs))
	       (t
		(let* ((expr (car declspecs))
		       (identifier (car expr))
		       (body (cdr expr)))
		  (assert (symbolp identifier) () "Malformed declaration identifier ~S" identifier)
		  (let ((parsed-declspec
			 (cond
			   ((and (not (null customparsep-function)) (funcall customparsep-function identifier body variables functions parent))
			    (funcall customparse-function identifier body variables functions parent))
			   ((find identifier '(type ftype))
			    (assert (and (listp body) (listp (cdr body))) () "Malformed ~S declaration: ~S" identifier expr)
			    (let* ((typespec (car body))
				   (syms (cdr body)))
			      ;; will not check TYPESPEC, has to be done in user code.
			      (let* ((symlookup/create (ecase identifier ((type) #'varlookup/create*) ((ftype) #'funlookup/create*)))
				     (parsed-syms (loop for sym in syms collect (funcall symlookup/create sym)))
				     (parsed-declspec (ecase identifier
							((type) (make-instance 'declspec-type :parent parent :type typespec :vars parsed-syms))
							((ftype) (make-instance 'declspec-ftype :parent parent :type typespec :funs parsed-syms)))))
				(loop for sym in parsed-syms do
				     (push parsed-declspec (nso-declspecs sym)))
				parsed-declspec)))
			   ((find identifier '(dynamic-extent ignore optimize inline special ignorable notinline))
			    (error "declaration identifier ~A not implemented yet" identifier))
			   (t (error "Unknown declaration specifier ~S" expr)))))
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
  (assert (nso-freep (car (declspec-funs (car declspecs)))))
  (assert (eq (car (declspec-funs (car declspecs))) (namespace-lookup 'a functions)))
  (assert (not (namespace-boundp 'a variables))))
(multiple-value-bind (body declspecs variables functions) (parse-declaration-in-body '((declare (ftype (function () fixnum) (setf a))) 5) nil nil nil)
  (assert (and (equal body '(5)) (typep (car declspecs) 'declspec-ftype)))
  (assert (nso-freep (car (declspec-funs (car declspecs)))))
  (assert (eq (car (declspec-funs (car declspecs))) (namespace-lookup '(setf a) functions)))
  (assert (not (namespace-boundp '(setf a) variables))))

(defmethod print-object ((object declspec-type) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (cons 'type (cons (declspec-type object) (declspec-vars object))))))
(defmethod print-object ((object declspec-ftype) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (cons 'ftype (cons (declspec-type object) (declspec-vars object))))))

;;;; LAMBDA LISTS
;; see CLHS 3.4 Lambda Lists

(defclass argument ()
  ((parent :initarg :parent :accessor argument-parent :type functiondef)
   (var :initarg :var :accessor argument-var :type var)))
(defclass required-argument (argument)
  ())
(defclass optional-argument (argument)
  ((init :initarg :init :accessor argument-init :type (or null generalform)) ;NIL means it was not specified, and then the parser does not assign a default initial form. (e.g. DEFTYPE would have * by default instead of NIL, but it's not the parser's job to define semantics.)
   (suppliedp :initarg :suppliedp :accessor argument-suppliedp :type (or null var)))) ;NIL means not present
(defclass key-argument (optional-argument)
  ((keywordp :initarg :keywordp :accessor argument-keywordp :type boolean) ;NIL means not present
   (keyword :initarg :keyword :accessor argument-keyword :type symbol))) ;has no meaning if KEYWORDP==NIL
(defclass aux-argument (argument)
  ((init :initarg :init :accessor argument-init :type (or null generalform))))
(defclass llist ()
  ((parent :initarg :parent :accessor form-parent)))
(defclass ordinary-llist (llist)
  ((required :initarg :required :accessor llist-required :type list) ;list, with each element of type REQUIRED-ARGUMENT.
   (optional :initarg :optional :accessor llist-optional :type list) ;list, with each element of type OPTIONAL-ARGUMENT.
   (rest :initarg :rest :accessor llist-rest :type (or null argument))
   (key :initarg :key :accessor llist-key :type list) ;list, with each element of type KEY-ARGUMENT.
   (allow-other-keys :initarg :allow-other-keys :accessor llist-allow-other-keys :type boolean)
   (aux :initarg :aux :accessor llist-aux :type list))) ;list, with each element of type AUX-ARGUMENT.
(defclass macro-llist (llist)
  ((whole :initarg :whole :accessor llist-whole :type (or null argument llist))
   (environment :initarg :environment :accessor llist-environment :type (or null argument))
   (required :initarg :required :accessor llist-required :type list) ;list, with each element of type ARGUMENT, or LLIST (for macro-lambda-lists).
   (optional :initarg :optional :accessor llist-optional :type list) ;list, with each element of type OPTIONAL-ARGUMENT, or LLIST (for macro-lambda-lists).
   (rest :initarg :rest :accessor llist-rest :type (or null argument llist))
   (body :initarg :body :accessor llist-body :type (or null argument llist))
   (key :initarg :key :accessor llist-key :type list) ;list, with each element of type KEY-ARGUMENT, or LLIST (for macro-lambda-lists).
   (allow-other-keys :initarg :allow-other-keys :accessor llist-allow-other-keys :type boolean)
   (aux :initarg :aux :accessor llist-aux :type list))) ;list, with each element of type AUX-ARGUMENT.

(defun parse-required-argument (varname)
  ;; probably not TODO (because whether a variable is constant is semantic, not syntax, and therefore not to be checked in a parser): add checking for "constant variable" in: CLHS 3.4.1 Ordinary Lambda Lists says: A var or supplied-p-parameter must be a symbol that is not the name of a constant variable.
  varname)

(defun parse-optional-or-key-or-aux-argument (argform argument-type)
  "Parse argument ARGFORM according to the given ARGUMENT-TYPE, which must be one of &OPTIONAL &KEY &AUX.
Forms allowed for &OPTIONAL, &KEY, &AUX: SYMBOL, (SYMBOL), (SYMBOL FORM)
Forms allowed for &OPTIONAL, &KEY: (SYMBOL FORM SYMBOL)
Forms allowed for &KEY: ((SYMBOL SYMBOL)), ((SYMBOL SYMBOL) FORM), ((SYMBOL SYMBOL) FORM SYMBOL)
Return seven values: indicator whether keyword name is present (NIL for &OPTIONAL and &AUX or if not present, non-NIL otherwise), its keyword name symbol, its variable name symbol (guaranteed to be neither NIL nor T), whether an initialization form is present, the initialization form, indicator whether supplied-p is present or not, the supplied-p symbol."
  (assert (find argument-type '(&OPTIONAL &KEY &AUX)))
  (cond
    ((symbolp argform)
     (values nil nil argform nil nil nil nil))
    ((and (consp argform))
     (let ((keywordp :undef) (keyword nil) (varname nil) (init-form-p nil) (init-form nil) (suppliedp nil) (suppliedp-name nil)
	   (head (car argform))
	   (rest (cdr argform)))
       (cond
	 ((symbolp head)
	  (setf keywordp nil) (setf keyword nil) (setf varname head))
	 ((and (consp head) (symbolp (car head)) (consp (cdr head)) (symbolp (cadr head)) (null (cddr head)))
	  (assert (eq argument-type '&KEY) () "Variable name is ~S, but names of the form (SYMBOL SYMBOL) only allowed for &KEY arguments" head)
	  (setf keywordp t) (setf keyword (car head)) (setf varname (cadr head)))
	 (t (error "Invalid ~S argument name ~S" argument-type varname)))
       (assert (listp rest) () "Invalid ~S argument seems to be of the form (SYMBOL FORM) or (SYMBOL FORM SYMBOL), but is ~S" argument-type argform)
       (when (not (null rest))
	 (let ((init-form2 (car rest))
	       (rest (cdr rest)))
	   (setf init-form-p t)
	   (setf init-form init-form2) ;nothing to check for INIT-FORM
	   (when (eq argument-type '&AUX)
	     (assert (null rest) () "Invalid ~S argument seems to be of the form (SYMBOL FORM), but is ~S which is too long" argument-type argform))
	   (assert (listp rest) () "Invalid ~S argument seems to be of the form (SYMBOL FORM SYMBOL), but is ~S" argument-type argform)
	   (when (not (null rest))
	     (assert (null (cdr rest)) () "Invalid ~S argument ~S seems to be of the form (SYMBOL FORM SYMBOL), but does not match" argument-type argform)
	     (assert (symbolp (car rest)) () "Supplied-p-parameter (of optional argment ~S) must be a symbol, but is ~S" varname suppliedp-name)
	     (setf suppliedp t) (setf suppliedp-name (car rest)))))
       (values keywordp keyword varname init-form-p init-form suppliedp suppliedp-name)))
    (t
     (error "Invalid ~S argument ~S" argument-type argform))))

(assert (equal (multiple-value-list (parse-optional-or-key-or-aux-argument 'a '&optional)) '(nil nil a nil nil nil nil)))
(assert (equal (multiple-value-list (parse-optional-or-key-or-aux-argument 'a '&key)) '(nil nil a nil nil nil nil)))
(assert (equal (multiple-value-list (parse-optional-or-key-or-aux-argument 'a '&aux)) '(nil nil a nil nil nil nil)))
(assert (equal (multiple-value-list (parse-optional-or-key-or-aux-argument '(a) '&optional)) '(nil nil a nil nil nil nil)))
(assert (equal (multiple-value-list (parse-optional-or-key-or-aux-argument '(a) '&key)) '(nil nil a nil nil nil nil)))
(assert (equal (multiple-value-list (parse-optional-or-key-or-aux-argument '(a) '&aux)) '(nil nil a nil nil nil nil)))
(assert (equal (multiple-value-list (parse-optional-or-key-or-aux-argument '(a 1) '&optional)) '(nil nil a t 1 nil nil)))
(assert (equal (multiple-value-list (parse-optional-or-key-or-aux-argument '(a 1) '&key)) '(nil nil a t 1 nil nil)))
(assert (equal (multiple-value-list (parse-optional-or-key-or-aux-argument '(a 1) '&aux)) '(nil nil a t 1 nil nil)))
(assert (equal (multiple-value-list (parse-optional-or-key-or-aux-argument '(a 1 ap) '&optional)) '(nil nil a t 1 t ap)))
(assert (equal (multiple-value-list (parse-optional-or-key-or-aux-argument '(a 1 ap) '&key)) '(nil nil a t 1 t ap)))
(assert (equal (multiple-value-list (parse-optional-or-key-or-aux-argument '((:b a)) '&key)) '(t :b a nil nil nil nil)))
(assert (equal (multiple-value-list (parse-optional-or-key-or-aux-argument '((:b a) 1) '&key)) '(t :b a t 1 nil nil)))
(assert (equal (multiple-value-list (parse-optional-or-key-or-aux-argument '((:b a) 1 ap) '&key)) '(t :b a t 1 t ap)))
(assert (equal (multiple-value-list (parse-optional-or-key-or-aux-argument '((t a) 1 ap) '&key)) '(t t a t 1 t ap)))
(assert (equal (multiple-value-list (parse-optional-or-key-or-aux-argument '((nil a) 1 ap) '&key)) '(t nil a t 1 t ap)))

;; TODO: I should encapsulate all the namesapces (VARIABLES FUNCTIONS BLOCKS) in a class and pass an instance to the custom parser functions so that the user can derive a more specialized class from this class and add additional namespaces (in additional slots), and access these slots in the custom parser functions.

;;Note that passing BLOCKS is not necessary here because REPARSE-FUNCTION has captured BLOCKS, and BLOCKS is not modified in #'PARSE-LAMBDA-LIST: parsing e.g. "(BLOCK TEST (FLET ((F (&OPTIONAL (A (RETURN-FROM TEST 4))) A)) (F)))" works.
(defun parse-lambda-list (lambda-list variables functions parent reparse-function &key (allow-macro-lambda-list nil))
  "Returns two values: an instance of type LLIST (representing the parsed LAMBDA-LIST) and the namespace VARIABLES augmented by the variables created by the arguments in LAMBDA-LIST.
Supported lambda list keywords:
CLHS Figure 3-12. Standardized Operators that use Ordinary Lambda Lists: An ordinary lambda list can contain the lambda list keywords shown in the next figure. &allow-other-keys &key &rest &aux &optional
CLHS Figure 3-18. Lambda List Keywords used by Macro Lambda Lists: A macro lambda list can contain the lambda list keywords shown in the next figure. &allow-other-keys &environment &rest &aux &key &whole &body &optional"
  ;; TODO: come up with a scheme for this function so that it is easily extensible with other types of lambda lists, or replacable (partly or wholly) by user-supplied parsing code.
  ;; TODO: rewrite so that parsing the following is possible: Allow user-defined Lambda Lists (i.e. user-defined subclasses of 'llist), 3.4.1 Ordinary Lambda Lists, 3.4.2 Generic Function Lambda Lists, 3.4.3 Specialized Lambda Lists, 3.4.4 Macro Lambda Lists, 3.4.5 Destructuring Lambda Lists, 3.4.6 Boa Lambda Lists, 3.4.7 Defsetf Lambda Lists, 3.4.8 Deftype Lambda Lists, 3.4.9 Define-modify-macro Lambda Lists, 3.4.10 Define-method-combination Arguments Lambda Lists
  ;; "CLHS 3.4.1 Ordinary Lambda Lists" says: "An init-form can be any form. Whenever any init-form is evaluated for any parameter specifier, that form may refer to any parameter variable to the left of the specifier in which the init-form appears, including any supplied-p-parameter variables, and may rely on the fact that no other parameter variable has yet been bound (including its own parameter variable)." But luckily the order of allowed keywords is fixed for both normal lambda-lists and macro lambda-lists, and a normal lambda-list allows a subset of macro lambda-lists. Maybe I'll have to rewrite this for other lambda-lists.
  (let* ((llist-type (if allow-macro-lambda-list 'macro-llist 'ordinary-llist)) ;TODO: when making this function modular: probably allow passing LLIST-TYPE.
	 (new-llist (make-instance llist-type :parent parent))
	 (new-variables variables))
    (labels ((add-argument (varname)
	       (let* ((new-argument (make-instance 'argument :parent new-llist))
		      (new-var (make-instance 'var :name varname :freep nil :definition new-argument :declspecs nil)))
		 (setf (argument-var new-argument) new-var)
		 (setf new-variables (augment-namespace-with-var new-var new-variables))
		 new-argument))
	     (add-required-argument (varname)
	       (let* ((new-argument (make-instance 'required-argument :parent new-llist))
		      (new-var (make-instance 'var :name varname :freep nil :definition new-argument :declspecs nil)))
		 (setf (argument-var new-argument) new-var)
		 (setf new-variables (augment-namespace-with-var new-var new-variables))
		 new-argument))
	     (add-optional-argument (varname init-form-p init-form suppliedp suppliedp-name)
	       "If INIT-FORM is not given for the &OPTIONAL argument, pass NIL for it. If SUPPLIEDP is non-NIL, INIT-FORM must be non-NIL as well."
	       (assert (if suppliedp (not (null init-form)) t))
	       (let* ((new-argument (make-instance 'optional-argument :parent new-llist))
		      (new-var (make-instance 'var :name varname :freep nil :definition new-argument :declspecs nil)))
		 (setf (argument-var new-argument) new-var)
		 (setf new-variables (augment-namespace-with-var new-var new-variables))
		 (let ((parsed-init-form (if init-form-p (funcall reparse-function init-form new-argument :variables new-variables) nil)))
		   (setf (argument-init new-argument) parsed-init-form)
		   (let ((new-supplied-var (if suppliedp (make-instance 'var :name suppliedp-name :freep nil :definition new-argument :declspecs nil) nil)))
		     (setf (argument-suppliedp new-argument) new-supplied-var)
		     (when new-supplied-var (setf new-variables (augment-namespace-with-var new-supplied-var new-variables)))
		     new-argument))))
	     (add-key-argument (keywordp keyword varname init-form-p init-form suppliedp suppliedp-name)
	       "If KEYWORDNAME or INIT-FORM is not given for the &KEY argument, pass NIL for it. If SUPPLIEDP is non-NIL, INIT-FORM must be non-NIL as well."
	       (assert (if suppliedp (not (null init-form)) t))
	       (let* ((new-argument (make-instance 'key-argument :parent new-llist :keywordp keywordp :keyword keyword))
		      (new-var (make-instance 'var :name varname :freep nil :definition new-argument :declspecs nil)))
		 (setf (argument-var new-argument) new-var)
		 (setf new-variables (augment-namespace-with-var new-var new-variables))
		 (let ((parsed-init-form (if init-form-p (funcall reparse-function init-form new-argument :variables new-variables) nil)))
		   (setf (argument-init new-argument) parsed-init-form)
		   (let ((new-supplied-var (if suppliedp (make-instance 'var :name suppliedp-name :freep nil :definition new-argument :declspecs nil) nil)))
		     (setf (argument-suppliedp new-argument) new-supplied-var)
		     (when new-supplied-var (setf new-variables (augment-namespace-with-var new-supplied-var new-variables)))
		     new-argument))))
	     (add-aux-argument (varname init-form-p init-form)
	       "If INIT-FORM is not given for the &KEY argument, pass NIL for it."
	       (let* ((new-argument (make-instance 'aux-argument :parent new-llist))
		      (new-var (make-instance 'var :name varname :freep nil :definition new-argument :declspecs nil)))
		 (setf (argument-var new-argument) new-var)
		 (setf new-variables (augment-namespace-with-var new-var new-variables))
		 (let ((parsed-init-form (if init-form-p (funcall reparse-function init-form new-argument :variables new-variables) nil)))
		   (setf (argument-init new-argument) parsed-init-form)
		   new-argument))))
      (let* ((remaining lambda-list)
	     (current-keyword nil)
	     whole environment required optional rest body key allow-other-keys aux)
	(assert (listp remaining))
	(loop until (null remaining) do
	     (assert (listp remaining))
	     (let ((head (car remaining)))
	       (cond
		 ((find head '(&whole &environment &optional &rest &body &key &aux)) ;&ALLOW-OTHER-KEYS not included because it does not accept variables
		  (setf current-keyword head))
		 ((eq current-keyword '&whole) ;TODO: allow recursive lambda-list parsing, see CLHS 3.4.4.1.2 Lambda-list-directed Destructuring by Lambda Lists
		  (assert allow-macro-lambda-list () "&WHOLE only allowed in macro lambda lists")
		  (assert (null whole) () "Only one &WHOLE keyword allowed in lambda list ~S" lambda-list)
		  (assert (and (null environment) (null required) (null optional) (null rest) (null body) (null key) (null aux)) () "&WHOLE keyword must be before &ENVIRONMENT, &OPTIONAL, &REST, &BODY, &KEY, &ALLOW-OTHER-KEYS, &AUX, and required arguments in lambda list ~S" lambda-list)
		  (setf whole (add-argument (parse-required-argument head)))
		  (setf current-keyword nil))
		 ((eq current-keyword '&environment)
		  (assert allow-macro-lambda-list () "&ENVIRONMENT only allowed in macro lambda lists")
		  (assert (null environment) () "Only one &ENVIRONMENT keyword allowed in lambda list ~S" lambda-list)
		  (setf environment (add-argument (parse-required-argument head)))
		  (setf current-keyword nil))
		 ((null current-keyword)
		  (assert (and (null optional) (null rest) (null body) (null key) (null aux)) () "Required arguments must be before &OPTIONAL, &REST, &BODY, &KEY, &ALLOW-OTHER-KEYS, &AUX in lambda list ~S" lambda-list)
		  (push (cond
			  ((and (not (null head)) (not (eq head t)) (symbolp head))
			   (add-required-argument (parse-required-argument head)))
			  ((and allow-macro-lambda-list (listp head))
			   (multiple-value-bind (parsed-llist new-variables2)
			       (parse-lambda-list head new-variables functions new-llist reparse-function :allow-macro-lambda-list allow-macro-lambda-list)
			     (setf new-variables new-variables2)
			     parsed-llist))
			  (t (error "Invalid required argument ~S in lambda list ~S" head lambda-list)))
			required))
		 ((eq current-keyword '&optional) ;TODO: allow recursive lambda-list parsing, see CLHS 3.4.4.1.2 Lambda-list-directed Destructuring by Lambda Lists
		  (assert (and (null rest) (null body) (null key) (null aux)) () "Optional arguments must be before &REST, &BODY, &KEY, &ALLOW-OTHER-KEYS, &AUX in lambda list ~S" lambda-list)
		  (push (multiple-value-bind (keywordp keyword varname init-form-p init-form suppliedp suppliedp-name)
			    (parse-optional-or-key-or-aux-argument head '&optional)
			  (declare (ignore keywordp keyword))
			  (add-optional-argument varname init-form-p init-form suppliedp suppliedp-name))
			optional))
		 ;; TODO: in macro lambda lists, allow &REST to be specified as the (CDR (LAST LAMBDA-LIST)), as in '(A &OPTIONAL B . R).
		 ((find current-keyword '(&rest &body)) ;TODO: allow recursive lambda-list parsing, see CLHS 3.4.4.1.2 Lambda-list-directed Destructuring by Lambda Lists
		  (when (eq current-keyword '&BODY) (assert allow-macro-lambda-list () "&BODY only allowed in macro lambda lists"))
		  (assert (and (null rest) (null body)) () "Only either one &REST or one &BODY keyword allowed in lambda list ~S" lambda-list)
		  (assert (and (null key) (null aux)) () "&REST or &BODY keyword must be before &KEY, &AUX in lambda list ~S" lambda-list)
		  (let ((parsed-head (add-argument (parse-required-argument head))))
		    (ecase current-keyword ((&rest) (setf rest parsed-head)) ((&body) (setf body parsed-head))))
		  (setf current-keyword nil))
		 ((eq current-keyword '&key) ;TODO: allow recursive lambda-list parsing, see CLHS 3.4.4.1.2 Lambda-list-directed Destructuring by Lambda Lists
		  (assert (null aux) () "&KEY keyword must be before &AUX in lambda list ~S" lambda-list)
		  (push (multiple-value-bind (keywordp keyword varname init-form-p init-form suppliedp suppliedp-name)
			    (parse-optional-or-key-or-aux-argument head '&key)
			  (add-key-argument keywordp keyword varname init-form-p init-form suppliedp suppliedp-name))
			key))
		 ((eq current-keyword '&allow-other-keys)
		  (assert (not (null key)) () "&ALLOW-OTHER-KEYS not allowed without preceding &KEY keyword in lambda list ~S" lambda-list)
		  (assert (null allow-other-keys) () "Only one &ALLOW-OTHER-KEYS keyword allowed in lambda list ~S" lambda-list)
		  (setf allow-other-keys t)
		  (setf current-keyword nil))
		 ((eq current-keyword '&aux)
		  (push (multiple-value-bind (keywordp keyword varname init-form-p init-form suppliedp suppliedp-name)
			    (parse-optional-or-key-or-aux-argument head '&aux)
			  (declare (ignore keywordp keyword suppliedp suppliedp-name))
			  (add-aux-argument varname init-form-p init-form))
			aux))
		 (t (error "Unhandled argument ~S in lambda-list ~S" head lambda-list))))
	     (setf remaining (cdr remaining)))
	(when (eq llist-type 'macro-llist)
	  (setf (llist-whole new-llist) whole)
	  (setf (llist-environment new-llist) environment)
	  (setf (llist-body new-llist) body))
	(setf (llist-required new-llist) (nreverse required))
	(setf (llist-optional new-llist) (nreverse optional))
	(setf (llist-rest new-llist) rest)
	(setf (llist-key new-llist) (nreverse key))
	(setf (llist-allow-other-keys new-llist) allow-other-keys)
	(setf (llist-aux new-llist) (nreverse aux))))
    (values new-llist new-variables)))

(defun parse-ordinary-lambda-list (lambda-list variables functions parent reparse-function)
  (parse-lambda-list lambda-list variables functions parent reparse-function :allow-macro-lambda-list nil))

(defun parse-macro-lambda-list (lambda-list variables functions parent reparse-function)
  (parse-lambda-list lambda-list variables functions parent reparse-function :allow-macro-lambda-list t))

;;TODO: add some test cases, e.g. (parse-lambda-list '(a &optional (b 1 bp) &rest r &key ((:cdddd c) 1 cp) &aux d) nil nil nil (lambda (form &rest r) (declare (ignore r)) form) :allow-macro-lambda-list nil)

(defmethod print-object ((object required-argument) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (argument-var object))))
(defmethod print-object ((object optional-argument) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cond
      ((and (null (argument-init object)) (null (argument-suppliedp object)))
       (format stream "~S" (argument-var object)))
      ((null (argument-suppliedp object))
       (format stream "(~S ~S)" (argument-var object) (argument-init object)))
      (t
       (format stream "(~S ~S ~S)" (argument-var object) (argument-init object) (argument-suppliedp object))))
    ))
(defmethod print-object ((object key-argument) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cond
      ((and (null (argument-init object)) (null (argument-suppliedp object)))
       (format stream "~S" (argument-var object)))
      (t
       (format stream "(")
       (if (argument-keywordp object)
	   (format stream "(~S ~S) " (argument-keyword object) (argument-var object))
	   (format stream "~S " (argument-var object)))
       (cond
	 ((null (argument-suppliedp object))
	  (format stream "~S" (argument-init object)))
	 (t
	  (format stream "~S ~S" (argument-init object) (argument-suppliedp object))))
       (format stream ")")))))
(defmethod print-object ((object ordinary-llist) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when *print-detailed-walker-objects*
      (format stream "~S" (append
			   (llist-required object)
			   (let ((it (llist-optional object))) (when it (cons '&optional it)))
			   (let ((it (llist-rest object))) (when it (list '&rest it)))
			   (let ((it (llist-key object))) (when it (cons '&key it)))
			   (let ((it (llist-allow-other-keys object))) (when it (list '&allow-other-keys)))
			   (let ((it (llist-aux object))) (when it (cons '&aux it))))))))
(defmethod print-object ((object macro-llist) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when *print-detailed-walker-objects*
      (format stream "~S" (append
			   (let ((it (llist-whole object))) (when it (list '&whole it)))
			   (let ((it (llist-environment object))) (when it (list '&environment it)))
			   (llist-required object)
			   (let ((it (llist-optional object))) (when it (cons '&optional it)))
			   (let ((it (llist-rest object))) (when it (list '&rest it)))
			   (let ((it (llist-body object))) (when it (list '&body it)))
			   (let ((it (llist-key object))) (when it (cons '&key it)))
			   (let ((it (llist-allow-other-keys object))) (when it (list '&allow-other-keys)))
			   (let ((it (llist-aux object))) (when it (cons '&aux it))))))))

;;;; FORMS

(deftype generalform ()
  "A form as described in 'CLHS 3.1.2.1 Form Evaluation'"
  `(or sym ;CLHS 3.1.2.1.1 Symbols as Forms
       form ;CLHS 3.1.2.1.2 Conses as Forms
       selfevalobject ;CLHS 3.1.2.1.3 Self-Evaluating Objects
       ))

(defclass selfevalobject ()
  ((object :initarg :object :accessor selfevalobject-object)) ;TODO: add something like ":type (or number string vector pathname ...)" when I know what types of objects are self-evaluating objects. CLHS 3.1.2.1.3 doesn't seem to have a complete list.
  (:documentation "A self-evaluating object as described in 'CLHS 3.1.2.1.3 Self-Evaluating Objects'"))
(defmethod print-object ((object selfevalobject) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (selfevalobject-object object))))

(defclass form ()
  ((parent :initarg :parent :accessor form-parent))
  (:documentation "A cons form as described in 'CLHS 3.1.2.1.2 Conses as Forms'"))
(defclass body-form () ;Note: objects of this type must never be created, only subtypes of this type.
  ((body :initarg :body :accessor form-body :type list))) ;list of GENERALFORMs
(defclass special-form (form)
  ())
(defclass progn-form (special-form body-form)
  ())
(defclass binding ()
  ((parent :initarg :parent :accessor binding-parent) ;e.g. the LET-form in which it is defined.
   (sym :initarg :sym :accessor binding-sym :type sym)))
(defclass var-binding (binding)
  ((value :initarg :value :accessor binding-value :documentation "The form initializing the variable." :type generalform)))
(defclass bindings-form ()  ;Note: objects of this type must never be created, only subtypes of this type.
  ((bindings :initarg :bindings :accessor form-bindings :type list) ;list of VAR-BINDINGs, or FUN-BINDINGs
   (declspecs :initarg :declspecs :accessor form-declspecs :type list))) ;list of DECLSPECs
(defclass let-form (special-form body-form bindings-form)
  ())
(defclass let*-form (special-form body-form bindings-form)
  ())
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
   (value :initarg :value :accessor form-value :type generalform)))
(defclass locally-form (special-form body-form)
  ((declspecs :initarg :declspecs :accessor form-declspecs :type list)))
(defclass the-form (special-form)
  ((type :initarg :type :accessor form-type :type (or symbol list))
   (value :initarg :value :accessor form-value :type generalform)))
(defclass if-form (special-form)
  ((test :initarg :test :accessor form-test :type generalform)
   (then :initarg :then :accessor form-then :type generalform)
   (else :initarg :else :accessor form-else :type (or null generalform))))
(defclass setq-form (special-form)
  ((vars :initarg :vars :accessor form-vars :type list) ;list of VARs
   (values :initarg :values :accessor form-values :type list))) ;list of GENERALFORMs
(defclass catch-form (special-form body-form)
  ((tag :initarg :tag :accessor form-tag :type generalform)))
(defclass throw-form (special-form)
  ((tag :initarg :tag :accessor form-tag :type generalform)
   (value :initarg :value :accessor form-value :type generalform)))
(defclass eval-when-form (special-form body-form)
  ((situations :initarg :situations :accessor form-situations :type list)))
(defclass load-time-value-form (special-form)
  ((value :initarg :value :accessor form-value :type list)
   (readonly :initarg :readonly :accessor form-readonly :type boolean)))
(defclass quote-form (special-form)
  ((object :initarg :object :accessor form-object :type t)))
(defclass multiple-value-call-form (special-form body-form)
  ((function :initarg :function :accessor form-function :type generalform))) ;note that this slot is not called FUN, because slots named FUN in other FORM-classes mean "of type FUN".
(defclass multiple-value-prog1-form (special-form body-form)
  ((function :initarg :function :accessor form-function :type generalform))) ;note that this slot is not called FUN, because slots named FUN in other FORM-classes mean "of type FUN".
(defclass progv-form (special-form body-form)
  ((symbols :initarg :symbols :accessor form-symbols :type generalform)
   (values :initarg :values :accessor form-values :type generalform)))
(defclass unwind-protect-form (special-form body-form)
  ((protected :initarg :protected :accessor form-protected :type generalform)))
(defclass application-form (form)
  ((fun :initarg :fun :accessor form-fun :type fun)
   (arguments :initarg :arguments :accessor form-arguments :type list))) ;list of GENERALFORMs

;; TODO: In the following functions, wherever (form-body ...) is printed, print "BODY:" before the list so that the user knows that the upper-most list printed is because BODY is a list (and she doesn't think its a function application).
(defmethod print-object ((object progn-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (form-body object))))
(defmethod print-object ((object var-binding) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when *print-detailed-walker-objects*
      (format stream "~S ~S" (binding-sym object) (binding-value object)))))
(defun body-with-declspecs (object)
  (if (null (form-declspecs object))
      (if (= (length (form-body object)) 1)
	  (car (form-body object))
	  (form-body object))
      (cons (cons 'declare (form-declspecs object)) (form-body object))))
(defmethod print-object ((object bindings-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if *print-detailed-walker-objects*
	(format stream "BINDINGS:~S ~S" (form-bindings object) (body-with-declspecs object))
	(format stream "~S" (form-body object)))))
(defmethod print-object ((object fun-binding) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when *print-detailed-walker-objects*
      (format stream "~S ~S ~S" (binding-sym object) (form-llist object) (body-with-declspecs object)))))
(defmethod print-object ((object lambda-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-llist object) (body-with-declspecs object))))
(defmethod print-object ((object block-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-blo object) (form-body object))))
(defmethod print-object ((object return-from-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-blo object) (form-value object))))
(defmethod print-object ((object locally-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (body-with-declspecs object))))
(defmethod print-object ((object the-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-type object) (form-value object))))
(defmethod print-object ((object if-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if (null (form-else object))
	(format stream "~S ~S" (form-test object) (form-then object))
	(format stream "~S ~S ~S" (form-test object) (form-then object) (form-else object)))))
(defmethod print-object ((object setq-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (not (null (form-vars object)))
      (format stream "~S ~S" (car (form-vars object)) (car (form-values object)))
      (loop for var in (cdr (form-vars object)) for value in (cdr (form-values object)) do
	   (format stream " ~S ~S" var value)))))
(defmethod print-object ((object catch-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-tag object) (form-body object))))
(defmethod print-object ((object throw-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-tag object) (form-value object))))
(defmethod print-object ((object eval-when-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-situations object) (form-body object))))
(defmethod print-object ((object load-time-value-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-value object) (form-readonly object))))
(defmethod print-object ((object quote-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (form-object object))))
(defmethod print-object ((object multiple-value-call-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-function object) (form-body object))))
(defmethod print-object ((object multiple-value-prog1-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-function object) (form-body object))))
(defmethod print-object ((object progv-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S ~S" (form-symbols object) (form-values object) (form-body object))))
(defmethod print-object ((object unwind-protect-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-protected object) (form-body object))))
(defmethod print-object ((object application-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (form-fun object))
    (loop for arg in (form-arguments object) do
	 (format stream " ~S" arg))))

;; TODO: maybe rename to PARSE-FORM.
;; TODO: FIXME: if there are multiple references to free namespace objects (like the Xs in (PARSE-WITH-EMPTY-NAMESPACES '(SETF (AREF A 1 2 X) X))), this function creates may create more than one namespace object (with (NSO-FREEP obj)==T) for them. change #'PARSE so that it only creates one free namespace object of the same name in this case.
;; TODO: FIXME: distinguish in all ASSERTs and ERRORs (in all functions, especially the #'PARSE functions) between errors that are recognized as syntax errors because the input form is impossible in Common Lisp, and errors that are due to me having made programming mistakes.
;; Must handle arbitrary lists (also TODO: FIXME: circular lists).
(defun parse (form variables functions blocks parent &key customparsep-function customparse-function customparsedeclspecp-function customparsedeclspec-function)
  (declare (optimize (debug 3)))
  (labels ((varlookup/create* (symbol)
	     (multiple-value-bind (var boundp) (varlookup/create symbol variables)
	       (when (not boundp)
		 (setf variables (augment-namespace-with-var var variables))) ;do not bind :DEFINITION
	       var))
	   (funlookup/create* (symbol)
	     (multiple-value-bind (fun boundp) (funlookup/create symbol functions)
	       (when (not boundp)
		 (setf functions (augment-namespace-with-fun fun functions))) ;do not bind :DEFINITION
	       fun))
	   (blolookup/create* (symbol)
	     (multiple-value-bind (blo boundp) (blolookup/create symbol blocks)
	       (when (not boundp)
		 (setf blocks (augment-namespace-with-blo blo blocks))) ;do not bind :DEFINITION
	       blo))
	   (reparse (form parent &key (variables variables) (functions functions) (blocks blocks))
	     (parse form variables functions blocks parent
		    :customparsep-function customparsep-function
		    :customparse-function customparse-function
		    :customparsedeclspecp-function customparsedeclspecp-function
		    :customparsedeclspec-function customparsedeclspec-function))
	   (parse-functiondef (form variables functions blocks parent)
	     "Parse FORM, which must be of the form (LAMBDA-LIST &BODY BODY) and create a corresponding FUNCTIONDEF-object."
	     (flet ((split-lambda-list-and-body (form)
		      (assert (and (consp form) (listp (car form))) () "Invalid lambda-list ~S" form)
		      (let ((lambda-list (car form))
			    (body (cdr form)))
			(assert (listp body) () "Invalid body ~S; must be a list" body)
			(values lambda-list body))))
	       (multiple-value-bind (lambda-list body) (split-lambda-list-and-body form)
		 (let ((new-functiondef (make-instance 'functiondef :parent parent)))
		   (multiple-value-bind (new-llist variables-in-functiondef)
		       (parse-lambda-list lambda-list variables functions new-functiondef #'reparse :allow-macro-lambda-list nil)
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
       (make-instance 'selfevalobject :object form))
      ((symbolp form)
       (varlookup/create* form)) ;TODO: insert code of #'VARLOOKUP/CREATE* here if it is called only once.
      ((atom form)
       (make-instance 'selfevalobject :object form))
      (t
       (let ((head (car form))
	     (rest (cdr form)))
	 (cond
	   ((eq head 'function)
	    (assert (and (consp rest) (null (cdr rest))) () "Invalid FUNCTION-form ~S" form)
	    (let ((name-form (car rest)))
	      ;;TODO: FIXME: encapsulate the result values (and adapt #'PRINT-OBJECT of a FUN) of the following into a to-be-created FUNCTION-OPERATOR-class (or FUNCTION-FORM-class?) so that (UNPARSE (PARSE form)) == form. Currently (PARSE '(FUNCTION (LAMBDA ())))==(PARSE '(LAMBDA ())), which violates above declaration that package WALKER should have as little semantics as possible.
	      (cond
		((valid-function-name-p name-form)
		 (funlookup/create* name-form))
		((and (consp name-form) (eq (car name-form) 'lambda))
		 (reparse name-form parent))
		(t (error "Invalid FUNCTION-form ~S" form)))))
	   ((eq head 'progn)
	    (let* ((current (make-instance 'progn-form :parent parent :body nil))
		   (body rest)
		   (parsed-body (loop for form in body collect (reparse form current))))
	      (setf (form-body current) parsed-body)
	      current))
	   ;; TODO: FIXME: maybe I have to handle something specially in FLET and LABELS. The CLHS on FLET and LABELS says: "Also, within the scope of flet, global setf expander definitions of the function-name defined by flet do not apply. Note that this applies to (defsetf f ...), not (defmethod (setf f) ...)." What does that mean?
	   ((find head '(let let* flet labels))
	    (assert (and (consp rest) (listp (car rest))) () "cannot parse ~S-form:~%~S" head form)
	    (let* ((definitions (car rest))
		   (body (cdr rest))
		   (form-type (ecase head ((let) 'let-form) ((let*) 'let*-form) ((flet) 'flet-form) ((labels) 'labels-form)))
		   (current (make-instance form-type :parent parent :body nil))) ;:BINDINGS and :DECLSPECS are defined below
	      (labels ((make-var-binding (def variables)
			 (assert (and (consp def) (symbolp (car def)) (not (null (car def))) (not (null (cdr def))) (null (cddr def))) () "cannot parse definition in ~S-form:~%~S" head def)
			 (let* ((name (car def))
				(value-form (cadr def))
				(binding (make-instance 'var-binding :parent current))
				(parsed-value (reparse value-form binding :variables variables))
				(sym (make-instance 'var :name name :freep nil :definition binding :declspecs nil)))
			   (setf (binding-sym binding) sym) (setf (binding-value binding) parsed-value)
			   binding))
		       (make-fun-binding (def functions)
			 (assert (and (consp def) (valid-function-name-p (car def)) (not (null (cdr def)))) () "cannot parse definition in ~S-form:~%~S" head def)
			 (multiple-value-bind (fun-type block-name) (valid-function-name-p (car def)) ;CLHS Glossary function block name: If the function name is a list whose car is setf and whose cadr is a symbol, its function block name is the symbol that is the cadr of the function name.
			   (declare (ignore fun-type))
			   (let* ((name (car def))
				  (body-form (cdr def))
				  (blo (make-instance 'blo :name block-name :freep nil))
				  (binding (make-instance 'fun-binding :parent current :blo blo))
				  (parsed-functiondef (parse-functiondef body-form variables functions (augment-namespace-with-blo blo blocks) current))
				  (sym (make-instance 'fun :name name :freep nil :definition binding :declspecs nil)))
			     (setf (binding-sym binding) sym)
			     (setf (nso-definition blo) binding)
			     (setf-binding-slots-to-functiondef-slots binding parsed-functiondef)
			     binding))))
		(multiple-value-bind (parsed-bindings new-variables new-functions)
		    (let ((parse-value-function (ecase head ((let let*) #'make-var-binding) ((flet labels) #'make-fun-binding)))
			  (namespace (ecase head ((let let*) variables) ((flet labels) functions)))
			  (augment-function (ecase head ((let let*) #'augment-namespace-with-var) ((flet labels) #'augment-namespace-with-fun))))
		      (cond
			((find head '(let flet))
			 (let* ((parsed-bindings (loop for def in definitions collect (funcall parse-value-function def namespace)))
				(parsed-syms (loop for binding in parsed-bindings collect (binding-sym binding))))
			   (ecase head
			     ((let) (let ((new-variables variables))
				      (loop for sym in parsed-syms do (setf new-variables (augment-namespace-with-var sym new-variables)))
				      (values parsed-bindings new-variables functions)))
			     ((flet) (let ((new-functions functions))
				       (loop for sym in parsed-syms do (setf new-functions (augment-namespace-with-fun sym new-functions)))
				       (values parsed-bindings variables new-functions))))))
			((find head '(let* labels))
			 (let* ((new-namespace namespace)
				(parsed-bindings (loop for def in definitions collect
						      (let* ((parsed-binding (funcall parse-value-function def new-namespace))
							     (parsed-sym (binding-sym parsed-binding)))
							(setf new-namespace (funcall augment-function parsed-sym new-namespace))
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
	    (assert (and (consp rest) (symbolp (car rest)) (listp (cdr rest))) () "Cannot parse BLOCK-form ~S" form)
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
	    (assert (and (consp rest) (symbolp (car rest)) (or (null (cdr rest)) (and (consp (cdr rest)) (null (cddr rest))))) () "Cannot parse RETURN-FROM-form ~S" form)
	    (let* ((name (car rest))
		   (value-form (if (null (cdr rest)) nil (cadr rest)))
		   (blo (blolookup/create* name))
		   (current (make-instance 'return-from-form :parent parent :blo blo))
		   (parsed-value (reparse value-form current)))
	      (setf (form-value current) parsed-value)
	      current))
	   ((eq head 'locally)
	    (assert (and (consp rest) (consp (car rest)) (consp (cdr rest))) () "Cannot parse LOCALLY-form ~S" form)
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
	    (assert (and (consp rest) (or (symbolp (car rest)) (consp (car rest))) (consp (cdr rest)) (null (cddr rest))) () "Cannot parse THE-form ~S" form)
	    (let* ((value-type-form (car rest))
		   (value-form (cadr rest))
		   (current (make-instance 'the-form :parent parent :type value-type-form))
		   (parsed-value (reparse value-form current)))
	      (setf (form-value current) parsed-value)
	      current))
	   ((eq head 'if)
	    (assert (and (consp rest) (consp (cdr rest)) (or (null (cddr rest)) (and (consp (cddr rest)) (null (cdddr rest))))) () "Cannot parse IF-form ~S" form)
	    (let* ((test-form (car rest))
		   (then-form (cadr rest))
		   (else-present (not (null (cddr rest))))
		   (else-form (if else-present (caddr rest) nil))
		   (current (make-instance 'if-form :parent parent))
		   (parsed-test (reparse test-form current))
		   (parsed-then (reparse then-form current))
		   (parsed-else (if else-present (reparse else-form current) nil)))
	      (setf (form-test current) parsed-test (form-then current) parsed-then (form-else current) parsed-else)
	      current))
	   ((eq head 'setq)
	    (let ((vars nil)
		  (values nil)
		  (current (make-instance 'setq-form :parent parent)))
	      (loop do
		   (when (null rest) (return))
		   (assert (and (consp rest) (symbolp (car rest)) (consp (cdr rest))) () "Cannot parse SETQ-form part ~S" rest)
		   (let* ((name (car rest))
			  (value-form (cadr rest))
			  (var (varlookup/create* name))
			  (parsed-value (reparse value-form current)))
		     (push var vars) (push parsed-value values))
		   (setf rest (cddr rest)))
	      (setf (form-vars current) (nreverse vars))
	      (setf (form-values current) (nreverse values))
	      current))
	   ((eq head 'catch)
	    (assert (consp rest) () "Cannot parse CATCH-form ~S" form)
	    (let* ((tag (car rest))
		   (body (cdr rest))
		   (current (make-instance 'catch-form :parent parent))
		   (parsed-tag (reparse tag current))
		   (parsed-body (loop for form in body collect (reparse form current))))
	      (setf (form-tag current) parsed-tag (form-body current) parsed-body)
	      current))
	   ((eq head 'throw)
	    (assert (and (consp rest) (consp (cdr rest)) (null (cddr rest))) () "Cannot parse THROW-form ~S" form)
	    (let* ((tag (car rest))
		   (result-form (cadr rest))
		   (current (make-instance 'throw-form :parent parent))
		   (parsed-tag (reparse tag current))
		   (parsed-value (reparse result-form current)))
	      (setf (form-tag current) parsed-tag (form-value current) parsed-value)
	      current))
	   ((eq head 'eval-when)
	    (assert (and (consp rest) (listp (car rest))) () "Cannot parse EVAL-WHEN-form ~S" form)
	    (let* ((situations-form (car rest))
		   (body (cdr rest))
		   (current (make-instance 'eval-when-form :parent parent :situations situations-form))
		   (parsed-body (loop for form in body collect (reparse form current))))
	      (setf (form-body current) parsed-body)
	      current))
	   ((eq head 'load-time-value)
	    (assert (and (consp rest) (or (null (cdr rest)) (and (consp (cdr rest)) (null (cddr rest))))) () "Cannot parse LOAD-TIME-VALUE-form ~S" form)
	    (let* ((value-form (car rest))
		   (readonly (cadr rest)))
	      (assert (position (cadr rest) '(nil t)) () "READ-ONLY-P in LOAD-TIME-VALUE-form ~S must be either NIL or T, but is ~S" form readonly)
	      (let* ((current (make-instance 'load-time-value-form :parent parent :readonly readonly))
		     (parsed-value (reparse value-form current :variables (make-namespace) :functions (make-namespace) :blocks (make-namespace)))) ;TODO: replace (MAKE-NAMESPACE) with (MAKE-NULL-LEXICAL-NAMESPACE) when I have such a function, because dynamic variables must be parsed: in the form (LOAD-TIME-VALUE A), A must refer to the global A.
		(setf (form-value current) parsed-value)
		current)))
	   ((eq head 'quote)
	    (assert (and (consp rest) (null (cdr rest))) () "Cannot parse QUOTE-form ~S" form)
	    (let* ((object (car rest)))
	      (make-instance 'quote-form :parent parent :object object)))
	   ((find head '(multiple-value-call multiple-value-prog1))
	    (assert (and (consp rest) (listp (cdr rest))) () "Cannot parse ~S-form ~S" head form)
	    (let* ((function-form (car rest))
		   (body (cdr rest))
		   (current (make-instance (ecase head ((multiple-value-call) 'multiple-value-call-form) ((multiple-value-prog1) 'multiple-value-prog1-form)) :parent parent))
		   (parsed-function (reparse function-form current))
		   (parsed-body (loop for form in body collect (reparse form current))))
	      (setf (form-function current) parsed-function (form-body current) parsed-body)
	      current))
	   ((eq head 'progv)
	    (assert (and (consp rest) (consp (cdr rest))) () "Cannot parse PROGV-form ~S" form)
	    ;; Note that in (PROGV SYMBOLS VALUES . BODY), both SYMBOLS and VALUES are evaluated, and thus the dynamic variable names are known only at run-time.
	    (let* ((symbols-form (car rest))
		   (values-form (cadr rest))
		   (body (cddr rest))
		   (current (make-instance 'progv-form :parent parent))
		   (parsed-symbols (reparse symbols-form current))
		   (parsed-values (reparse values-form current))
		   (parsed-body (loop for form in body collect (reparse form current))))
	      (setf (form-symbols current) parsed-symbols (form-values current) parsed-values (form-body current) parsed-body)
	      current))
	   ((eq head 'unwind-protect)
	    (assert (consp rest) () "Cannot parse UNWIND-PROTECT-form ~S" form)
	    (let* ((protected-form (car rest))
		   (cleanup-body (cdr rest))
		   (current (make-instance 'unwind-protect-form :parent parent))
		   (parsed-protected (reparse protected-form current))
		   (parsed-cleanup-body (loop for form in cleanup-body collect (reparse form current))))
	      (setf (form-protected current) parsed-protected (form-body current) parsed-cleanup-body)
	      current))
	   ((find head '(go macrolet symbol-macrolet tagbody)) ;TODO: TAGBODY, SYMBOL-MACROLET and MACROLET are tricky to implement, see their CLHS and below.
	    (error "parsing special form ~S not implemented yet" head))
	   (t
	    (assert (symbolp head) () "Invalid function application ~S" form)
	    (let* ((fun-name head)
		   (arg-forms rest)
		   (fun (funlookup/create* fun-name))
		   (current (make-instance 'application-form :parent parent :fun fun))
		   (parsed-arguments nil))
	      (loop do
		   (when (null arg-forms) (return))
		   (assert (and (consp arg-forms) (listp (cdr arg-forms))) () "Invalid argument rest ~S in function application" arg-forms)
		   (push (reparse (car arg-forms) current) parsed-arguments)
		   (setf arg-forms (cdr arg-forms)))
	      (setf (form-arguments current) (nreverse parsed-arguments))
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
      (assert (eq (selfevalobject-object (binding-value (nso-definition b-1))) 5))
      (assert (eq b-1 (binding-value (nso-definition b-2))))
      (assert (not (eq b-1 b-2))))
    (let* ((variables (lexical-namespaces-at '(let* ((b 5)) -here- (let* ((b b)) -here-))))
	   (variables-1 (car variables))
	   (variables-2 (cadr variables))
	   (b-1 (namespace-lookup 'b variables-1))
	   (b-2 (namespace-lookup 'b variables-2)))
      (assert (eq (selfevalobject-object (binding-value (nso-definition b-1))) 5))
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
      (assert (eq b-1 (car (form-body (nso-definition b-2)))))))
  (let* ((ast (parse-with-empty-namespaces '(test #'test 2 3)))
	 (call-fun (form-fun ast))
	 (call-arguments (form-arguments ast)))
    (assert (eq call-fun (car call-arguments))))
  ;; TODO: FIXME:
  ;; (let* ((ast (parse-with-empty-namespaces '(setf (aref a x) x)))
  ;; 	 (args (form-arguments ast))
  ;; 	 (aref-x (cadr (form-arguments (car args))))
  ;; 	 (arg-x (cadr args)))
  ;;   (assert (eq aref-x arg-x)))
  )
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
      (assert (all-equal (binding-sym (car (form-bindings ast))) (car (declspec-funs declspec-ftype)) (car (form-body ast)))))
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

(defun test-load-time-value-form ()
  (declare (optimize (debug 3)))
  (let* ((form '(progn
		 (setf a 2)
		 (let ((a 7))
		   (setf a 8)
		   (load-time-value a))))
	 (ast (parse-with-empty-namespaces form))
	 (progn-body (form-body ast))
	 (a-setf1 (car (form-arguments (car progn-body))))
	 (let-form (cadr progn-body))
	 (let-body (form-body let-form))
	 (a-let (binding-sym (car (form-bindings let-form))))
	 (a-setf2 (car (form-arguments (car let-body))))
	 (a-load-time-value (form-value (cadr let-body))))
    (assert (nso-freep a-setf1))
    (assert (not (nso-freep a-let)))
    (assert (not (nso-freep a-setf2)))
    (assert (nso-freep a-load-time-value))
    (assert (eq a-let a-setf2))
    ;; TODO: (assert (eq a-setf1 a-load-time-value))
    ))
(test-load-time-value-form)

(defun test-quote-form ()
  (declare (optimize (debug 3)))
  (let* ((form '(quote a))
	 (ast (parse-with-empty-namespaces form))
	 (quote-a (form-object ast)))
    (assert (symbolp quote-a))
    (assert (eq quote-a 'a))))
(test-quote-form)

;; TODO:
;; TAGBODY, MACROLET, SYMBOL-MACROLET will be tricky to implement and I don't know what properties an implementation has to fulfill:
;; The CLHS for TAGBODY says:
;; 1. "The determination of which elements of the body are tags and which are statements is made prior to any macro expansion of that element. If a statement is a macro form and its macro expansion is an atom, that atom is treated as a statement, not a tag." What does that mean?
;; The CLHS for SYMBOL-MACROLET says:
;; 0. "The expansion of a symbol macro is subject to further macro expansion in the same lexical environment as the symbol macro invocation": this should be easy to implement if I put the SYMBOL-MACROLETed variables in an lexical namespace. In fact the CLHS on SYMBOL-MACROLET also says "SYMBOL-MACROLET lexically establishes expansion functions for each of the symbol macros named by symbols".
;; 1. "The use of symbol-macrolet can be shadowed by let...", see below.
;; 2. "SYMBOL-MACROLET signals an error if a special declaration names one of the symbols being defined by SYMBOL-MACROLET.": add a testcase that (PARSE '(SYMBOL-MACROLET ((A B)) (DECLARE (SPECIAL A)))) fails.
;; 3. "any use of SETQ to set the value of one of the specified variables is treated as if it were a SETF. PSETQ of a symbol defined as a symbol macro is treated as if it were a PSETF, and MULTIPLE-VALUE-SETQ is treated as if it were a SETF of values.": This could mean that during evaluation of the symbol-macro I have to handle the special forms SETQ, PSETQ, MULTIPLE-VALUE-SETQ specially.
;; TODO: test-case for SYMBOL-MACROLET:
;; (let ((a 1) ;A-LET1
;;       (b 2)) ;B-LET1
;;   (symbol-macrolet ((a b))
;;     (let ((a 5) ;A-LET2
;;           (b 6)) ;B-LET2
;;       (print (list a b)) ;A-LIST1, B-LIST1
;;       (setf a 7 b 8))) ;A-SETF, B-SETF
;;   (list a b)) ;A-LIST2, B-LIST2
;; should print (5 6) and return (1 2). So I should check that (EQ A-LET1 A-LIST2), (EQ B-LET1 B-LIST2), (EQ A-LET2 A-LIST1 A-SETF), (EQ B-LET2 B-LIST1 B-SETF), (NOT (EQ A-LET2 A-LET1)), (NOT (EQ B-LET2 B-LET1)).

;; On TAGBODY:
;; TAGBODY always returns NIL, which means that a single-symbol statement in TAGBODY always means a tag, even as the last element of TAGBODY. The following form returns NIL (and not 1):
;; (let ((a 1))
;;   (tagbody
;;    a))
;; Raises an error that A and B are nonexistant tags:
;; (let ((branch :b)
;;       (a 1)
;;       (b 10))
;;   (tagbody
;;      (ecase branch ((:a) (go a)) ((:b) (go b)))
;;      (symbol-macrolet ((a b))
;;        a
;;        (incf a)
;;        b
;;        (incf b)))
;;   (list a b))
;; Exchanging TAGBODY and SYMBOL-MACROLET compiles:
;; (defun test (branch)
;;   (let ((a 1)
;;         (b 10))
;;     (symbol-macrolet ((a b))
;;       (tagbody
;;          (ecase branch ((:a) (go a)) ((:b) (go b)))
;;        a
;;          (incf a)
;;        b
;;          (incf b)))
;;     (list a b)))
;; WALKER> (test :a)
;; (1 12)
;; WALKER> (test :b)
;; (1 11)
