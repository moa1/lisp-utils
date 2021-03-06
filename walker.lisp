;; Note that the classes VAR, FUN and so on should be named VARIABLE, FUNCTION and so on, but SBCL pollutes the CL package.

;; How to define DEFUN from special forms:
;;CL-USER> (macroexpand '(setf (symbol-function 'test) (lambda () 5)))
;;(LET* ((#:NEW596 (LAMBDA () 5)))
;;  (FUNCALL #'(SETF SYMBOL-FUNCTION) #:NEW596 'TEST))
;;T
;; is accessor #'FDEFINITION redundant to #'SYMBOL-FUNCTION?

;; This file contains the core of the parser, i.e. parsers for all special forms.

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
Its scope is the syntactic handling of arbitrary Common Lisp code, i.e. parsing a list into an abstract syntax tree (e.g. using #'PARSE-WITH-NAMESPACE), and converting an abstract syntax tree into a list (using #'DEPARSE).
It has as little Common Lisp semantics in it as possible (while still being useful as a Common Lisp parser). In particular, there is no notion of constant variables (so NIL and T are ordinary variable names).
Type declarations are parsed, but the contained types are neither parsed nor interpreted.")
  (:use :cl)
  (:export
   ;; for classes: export the class and _all_ slots and their accessors on one line so that deleting a class doesn't have to consider all exports of other classes
   :proper-list-p
   :remove-from-key-list
   ;; NAMESPACES
   :nso :name :nso-name :freep :nso-freep :definition :nso-definition :sites :nso-sites :nso-source :source :user
   :sym :declspecs :nso-declspecs :macrop :nso-macrop
   :var
   :fun
   :blo
   :tag :gopoint :nso-gopoint
   :*print-detailed-walker-objects*
   :valid-function-name-p :fun :setf-fun
   :namespace :var :namespace-var :fun :namespace-fun :blo :namespace-blo :tag :namespace-tag :user
   :lexical-namespace
   :free-namespace
   :namespace-boundp
   :namespace-lookup
   :copy-namespace
   :copy-deep-namespace
   :copy-deep-parser
   ;; PARSERS
   :parser :lexical-namespace :parser-lexical-namespace :free-namespace :parser-free-namespace
   :copy-parser
   :parser-boundp
   :parser-lookup
   ;; NAMESPACES again
   :augment-lexical-namespace
   :augment-free-namespace
   :namespace-lookup/create
   :make-empty-lexical-namespace
   :make-empty-free-namespace
   :+common-lisp-variables+ :+common-lisp-functions+ :+common-lisp-macros+
   :make-parser
   :make-ast
   ;; DECLARATIONS
   :declspec :parent :declspec-parent :source :declspec-source :user
   :form-parent :form-source
   :declspec-type :type :declspec-type :vars :declspec-vars
   :declspec-ftype :type :declspec-type :funs :declspec-funs
   :declspec-optimize :qualities :declspec-qualities
   :declspec-ignore :syms :declspec-syms
   :declspec-ignorable :syms :declspec-syms
   :declspec-dynamic-extent :syms :declspec-syms
   :declspec-inline :funs :declspec-funs
   :declspec-notinline :funs :declspec-funs
   :declspec-special :vars :declspec-vars
   :parse-function-declaration
   :parse-declspec-expr
   :parse-declspec
   :parse-declspecs
   :parse-declaration-in-body
   :parse-declaration-and-documentation-in-body
   :parse
   ;; LAMBDA LISTS
   :parameter :parent :parameter-parent :source :parameter-source :var :parameter-var :user
   :form-parent :form-source
   :parameter-init-mixin
   :whole-parameter
   :environment-parameter
   :required-parameter
   :optional-parameter :init :parameter-init :suppliedp :parameter-suppliedp
   :rest-parameter
   :body-parameter
   :key-parameter :keywordp :parameter-keywordp :keyword :parameter-keyword
   :aux-parameter :init :parameter-init
   :llist :parent :llist-parent :source :llist-source :user
   :form-parent :form-source
   :ordinary-llist :required :llist-required :optional :llist-optional :rest :llist-rest :key :llist-key :allow-other-keys :llist-allow-other-keys :aux :llist-aux
   :macro-llist :whole :llist-whole :environment :llist-environment :required :llist-required :optional :llist-optional :rest :llist-rest :body :llist-body :key :llist-key :allow-other-keys :llist-allow-other-keys :aux :llist-aux
   :parse-required-parameter
   :parse-optional-or-key-or-aux-parameter
   ;;:parse-lambda-list ;do not export this as it should be split into several smaller functions.
   :parse-ordinary-lambda-list
   :parse-macro-lambda-list
   ;; FORMS and Utility Functions
   :form :parent :form-parent :source :form-source :user
   :object-form :object :form-object
   :var-reading :var :form-var
   :body-mixin :body :form-body
   :special-form
   :function-form :object :form-object
   :progn-form
   :binding :parent :form-parent :source :form-source :sym :form-sym :user
   :var-binding :value :form-value
   :bindings-mixin :bindings :form-bindings :declspecs :form-declspecs
   :var-bindings-mixin
   :fun-bindings-mixin
   :let-form
   :let*-form
   :functiondef :parent :form-parent :llist :form-llist :declspecs :form-declspecs :documentation :form-documentation
   :block-naming-mixin :blo :form-blo
   :block-form
   :fun-binding
   :flet-form
   :labels-form
   :lambda-form
   :return-from-form :blo :form-blo :value :form-value
   :locally-form :declspecs :form-declspecs
   :the-form :type :form-type :value :form-value
   :if-form :test :form-test :then :form-then :else :form-else
   :var-writing :parent :form-parent :source :form-source :var :form-var :value :form-value :user
   :setq-form :vars :form-vars
   :catch-form :tag :form-tag
   :throw-form :tag :form-tag :value :form-value
   :eval-when-form :situations :form-situations
   :load-time-value-form :value :form-value :readonly :form-readonly
   :quote-form :object :form-object
   :multiple-value-call-form :function :form-function
   :multiple-value-prog1-form :values :form-values
   :progv-form :symbols :form-symbols :values :form-values
   :unwind-protect-form :protected :form-protected
   :application-form :fun :form-fun :arguments :form-arguments :recursivep :form-recursivep
   :macroapplication-form :lexicalnamespace :form-lexicalnamespace :freenamespace :form-freenamespace
   :symbol-macrolet-form
   :macrolet-form
   :tagbody-form :body :form-body :tags :form-tags
   :tagpoint :parent :form-parent :tag :form-tag
   :go-form :tag :form-tag
   :format-body
   :function-object
   :form-body-1 :form-body-2 :form-body-3 :form-body-4 :form-body-5 :form-body-6 :form-body-7 :form-body-last
   :form-binding-1 :form-binding-2 :form-binding-3 :form-binding-4 :form-binding-5 :form-binding-6 :form-binding-7
   :form-argument-1 :form-argument-2 :form-argument-3 :form-argument-4 :form-argument-5 :form-argument-6 :form-argument-7
   :form-var :form-fun
   :make-nil
   :make-object
   ;; END OF FORMs and Utility Functions
   :parse-and-set-functiondef
   :is-recursive
   :ast-inside-ast-p
   :parse-body
   :parse-form
   :parse-var-binding
   :parse-fun-binding
   :parse-macro-or-function-application
   :parse-with-namespace
   :parser-namespace-at
   :namespace-at
   ;; DEPARSER
   :deparser
   :deparse
   :orderer
   :eval-order
   :eval-order-list
   :deparse-path
   :deparse-path-list
   :deparse-body
   :deparser-map-ast-before :function :deparser-function :result :deparser-result
   :deparser-map-ast-after :function :deparser-function :result :deparser-result
   :map-ast
   ))

(in-package :walker)

;; TODO: FIXME: Package WALKER should handle input containing arbitrary nonsense gracefully, except maybe circular lists. TODO: FIXME: CLHS seems to require allowing circular constants. Related are "Issues/iss079_w.htm", which talks about circular constants, "Issues/iss215_w.htm" talks about #'MAKE-LOAD-FORM. I don't know about circular code (which could be implemented maybe using a JMP/LONGJMP). I would have to splatter the checks for circular lists in the whole code since the user might call any function with a circular list.
(defun proper-list-p (list)
  "Return T if LIST is a proper list, NIL otherwise."
  ;; TODO: FIXME: check for circularity.
  (and (listp list)
       (do ((rest list (cdr rest)))
	   ((or (null rest) (not (listp rest))) (null rest)))))

(defun remove-from-key-list (list keywords)
  "Return the list obtained by removing keywords and their values from KEYWORDS. Does not alter LIST."
  (labels ((rec (list acc)
	     (cond ((null list) acc)
		   ((find (car list) keywords) (rec (cddr list) acc))
		   (t (rec (cdr list) (cons (car list) acc))))))
    (nreverse (rec list nil))))

(assert (equal (remove-from-key-list '(a b :v 1 :x 2 c) '(:v)) '(a b :x 2 c)))
(assert (equal (remove-from-key-list '(a b :v 1 :x 2 c) '(:x :v)) '(a b c)))

;;;; NAMESPACES

(defclass nso ()
  ((name :initarg :name :accessor nso-name :type (or symbol list) :documentation "LIST is allowed for functions named (SETF NAME)")
   (freep :initarg :freep :accessor nso-freep :type boolean
	  :documentation "T if it is a free variable/function, or NIL if bound. Note that this is specific to a namespace, i.e. there may be multiple NSO-instances with the same name and FREEP=T.
For example, in (LET ((X 1)) (DECLARE (SPECIAL X)) (LET ((X 2)) (LET ((OLD-X X) (X 3)) (DECLARE (SPECIAL X)) (LIST OLD-X X)))), there are two VAR-instances with name 'X and :FREEP==T.")
   (definition :initarg :definition :accessor nso-definition :type (or binding llist)
	       :documentation "The parsed object it is defined in, NIL if not known.
For VARs it is of type (OR VAR-BINDING LLIST), for FUNs it is of type FUN-BINDING,
for BLOs it is an instance of a subclass of BLOCK-NAMING-MIXIN,
for TAGs it is an instance of class TAGBODY-FORM.")
   (sites :initform nil :initarg :sites :accessor nso-sites :type list
	  :documentation "List of forms where this NSO is used, excluding the definition. For example, for FUN-nsos it is the list of APPLICATION-FORMs and FUNCTION-forms. For TAG-nsos, it is the list of GO-FORMs.")
   (source :initarg :source :accessor nso-source :type list :documentation "The source of the NSO.")
   (user :initform nil :initarg :user :accessor user
	 :documentation "Arbitrary user-definable slot."))
  (:documentation "a namespace-object (NSO) containing a name and information whether it is free or bound"))
(defclass sym (nso)
  ((declspecs :initarg :declspecs :accessor nso-declspecs :type list
	      :documentation "a list of DECLSPECs that apply to this symbol")
   (macrop :initform nil :initarg :macrop :accessor nso-macrop :type boolean
	   :documentation "NIL for variables and functions, non-NIL for symbol macros and macros"))
  (:documentation "A symbol referring to a variable, function or macro.
Note that symbols are always parsed in a lexical manner, regardless of whether the actual variable this symbol is referring to may be a lexical or special variable. For example, the symbols *A* in the form (LET ((*A* 1)) *A*) may refer to a lexical or special variable."))
(defclass var (sym)
  ()
  (:documentation "The symbol of a variable or symbol macro"))
(defclass fun (sym)
  ()
  (:documentation "The symbol of a function or macro"))
(defclass blo (nso)
  ()
  (:documentation "A named block."))
(defclass tag (nso)
  ((gopoint :initarg :gopoint :accessor nso-gopoint :type list
	    :documentation "The list with elements of type FORM or TAGPOINT that come after (and including) the TAGPOINT in the body of DEFINITION."))
  (:documentation "A tag in a TAGBODY form."))

(defvar *print-detailed-walker-objects* nil "If T, print more details of objects in package WALKER.")

(defmethod print-object ((object sym) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if *print-detailed-walker-objects*
	(format stream "NAME:~S FREEP:~S MACROP:~S USER:~S" (nso-name object) (nso-freep object) (nso-macrop object) (user object))
	(format stream "NAME:~S" (nso-name object)))))
(defmethod print-object ((object blo) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if *print-detailed-walker-objects*
	(format stream "NAME:~S FREEP:~S USER:~S" (nso-name object) (nso-freep object) (user object))
	(format stream "NAME:~S" (nso-name object)))))
(defmethod print-object ((object tag) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if *print-detailed-walker-objects*
	(format stream "NAME:~S FREEP:~S USER:~S" (nso-name object) (nso-freep object) (user object))
	(format stream "NAME:~S" (nso-name object)))))

(defun valid-function-name-p (name)
  "Checks whether NAME is naming a function, i.e. is either of the form SYMBOL or (SETF SYMBOL). In those cases, return as first value 'FUN or 'SETF-FUN, and as second value NAME.
Returns NIL if NAME is not a function name.
Note that CLHS Glossary on \"function name\" defines it as \"A symbol or a list (setf symbol) that is the name of a function in that environment.\""
  (cond
    ((symbolp name)
     (values 'fun name))
    ((and (consp name) (eq (car name) 'setf) (consp (cdr name)) (symbolp (cadr name)) (null (cddr name)))
     (values 'setf-fun name))
    (t
     nil)))

(assert (equal (multiple-value-list (valid-function-name-p nil)) '(fun nil)))
(assert (equal (multiple-value-list (valid-function-name-p t)) '(fun t)))
(assert (equal (multiple-value-list (valid-function-name-p 'a)) '(fun a)))
(assert (equal (multiple-value-list (valid-function-name-p '(setf a))) '(setf-fun (setf a))))
(assert (null (valid-function-name-p '5)))
(assert (null (valid-function-name-p '(setf a b))))
(assert (null (valid-function-name-p '(setf 5))))

;; The three Lisp namespaces (VARIABLES FUNCTIONS BLOCKS) are encapsulated in a class NAMESPACE. The LEXICAL-NAMESPACE-object and the FREE-NAMESPACE-object are both passed to the PARSE*-functions. Instances of LEXICAL-NAMESPACE and FREE-NAMESPACE are passed to the custom parser functions so that the user can derive a more specialized class from LEXICAL-NAMESPACE and FREE-NAMESPACE and add additional namespaces (in additional slots), and access these slots in the custom parser functions. For the LEXICAL-NAMESPACE, the slots VARIABLES FUNCTIONS BLOCKS are implemented as ALISTS like before. For every augmentation (e.g. with a FUN-object) to an instance of the class LEXICAL-NAMESPACE, a new instance of the class LEXICAL-NAMESPACE is created, with the respective slot (e.g. slot FUN) updated to the augmented ALIST. In addition, there is a FREE-NAMESPACE, which contains the slots VARIABLES FUNCTIONS BLOCKS, which are ALISTS. If a free namespace-object should be augmented, then the corresponding slot in the FREE-NAMESPACE object is modified (but no new FREE-NAMESPACE object created). This way, 1. the LEXICAL-NAMESPACE-object that is passed to PARSE*-functions (like PARSE-DECLARATION-IN-BODY) and augmented there can be passed to other PARSE*-functions there and augmented there and so on, and augmented lexical NSO-objects are forgotten automatically when the other PARSE*-functions return without returning their newly created LEXICAL-NAMESPACE-object, but 2. the FREE-NAMESPACE-object passed to the PARSE*-functions is always the same object and thus augmentations to its slots are reflected in all PARSE*-functions.

(defclass namespace ()
  ;; note that the slots are named like NSO-objects, which allows, when given a SYMBOL-TYPE, accessing the slot named SYMBOL-TYPE and creating an instance of type SYMBOL-TYPE.
  ((var :initform nil :initarg :var :accessor namespace-var)
   (fun :initform nil :initarg :fun :accessor namespace-fun)
   (blo :initform nil :initarg :blo :accessor namespace-blo)
   (tag :initform nil :initarg :tag :accessor namespace-tag)
   (user :initform nil :initarg :user :accessor user
	 :documentation "Arbitrary user-definable slot of the namespace, e.g. class- or structure-names."))
  (:documentation "A namespace."))
;; TODO: CLHS Glossary says on "lexical environment": "A lexical environment contains [...] local declarations (see declare)." So I should think about storing DECLSPECs not in VAR, but in NAMESPACE. On the other hand, VARs are stored in NAMESPACE, so the DECLSPECs are already stored in NAMESPACE indirectly.
(defclass lexical-namespace (namespace)
  ()
  (:documentation "A lexical namespace (containing bound namespace objects). This means that (NSO-FREEP OBJECT)==NIL for all OBJECTs in any slot of the namespace."))
(defclass free-namespace (namespace)
  ()
  (:documentation "A namespace of free (i.e. unbound) namespace objects (e.g. variables). This means that (NSO-FREEP OBJECT)==T for all OBJECTs in any slot of the namespace."))

(defun make-empty-lexical-namespace ()
  (make-instance 'lexical-namespace))

(defun make-empty-free-namespace ()
  (make-instance 'free-namespace))

(defmethod print-object ((object namespace) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if *print-detailed-walker-objects*
	(format stream "~%VARs:~S~%FUNs:~S~%BLOs:~S~%TAGs:~S~%" (namespace-var object) (namespace-fun object) (namespace-blo object) (namespace-tag object))
	(flet ((nso-names (namespace)
		 (loop for acons in namespace collect (car acons))))
	  (format stream "VARs:~S FUNs:~S BLOs:~S TAGs:~S" (nso-names (namespace-var object)) (nso-names (namespace-fun object)) (nso-names (namespace-blo object)) (nso-names (namespace-tag object)))))))

(defgeneric namespace-boundp (symbol-type symbol namespace)
  (:documentation "Return non-NIL if SYMBOL is bound in NAMESPACE in the slot SYMBOL-TYPE, NIL otherwise."))
(defmethod namespace-boundp (symbol-type symbol (namespace namespace))
  (let* ((alist (slot-value namespace symbol-type)))
    (not (null (assoc symbol alist :test #'equal))))) ;#'EQUAL for functions named (SETF SYMBOL).

(defgeneric namespace-lookup (symbol-type symbol namespace)
  (:documentation "Look up SYMBOL in NAMESPACE in the slot SYMBOL-TYPE and return it. Signal an error if SYMBOL is not bound."))
(defmethod namespace-lookup (symbol-type symbol (namespace namespace))
  (assert (slot-exists-p namespace symbol-type) () "NSO-type ~S not present in ~S~%(Maybe ~S needs to be in package WALKER?)" symbol-type namespace symbol-type)
  (let* ((alist (slot-value namespace symbol-type))
	 (cons (assoc symbol alist :test #'equal))) ;#'EQUAL for functions named (SETF SYMBOL).
    (assert (not (null cons)) () "SYMBOL ~A with NSO-type ~S not bound in ~S" symbol symbol-type namespace)
    (cdr cons)))

(defgeneric copy-namespace (namespace)
  (:documentation "Make a shallow copy of NAMESPACE and return it."))

(defmethod copy-namespace ((namespace namespace))
  (make-instance (type-of namespace) :var (namespace-var namespace) :fun (namespace-fun namespace) :blo (namespace-blo namespace) :tag (namespace-tag namespace) :user (user namespace)))

(defgeneric copy-deep-namespace (parse namespace-type)
  (:documentation "Return a shallow copy of PARSER where all NAMESPACE-TYPE namespace slots are copied deeply, up to and including the aconses, but excluding the objects stored in the aconses."))

(defgeneric copy-deep-parser (parser)
  (:documentation "Create a new instance of parser that has a deep-copied lexical and free namespace"))

;;;; PARSERS

(defclass parser ()
  ((lexical-namespace :initarg :lexical-namespace :initform (make-empty-lexical-namespace) :accessor parser-lexical-namespace :documentation "The lexical namespace of the parser")
   (free-namespace :initarg :free-namespace :initform (make-empty-free-namespace) :accessor parser-free-namespace :documentation "The free namespace of the parser"))
  (:documentation "The state of the parser. To customize WALKER, inherit from this class."))

(defgeneric copy-parser (parser)
  (:documentation "Shallow-copies PARSER. Must be defined for all user-defined subclasses of PARSER."))
(defmethod copy-parser ((parser parser))
  "Shallow-copies PARSER. Must be defined for all user-defined subclasses of PARSER."
  (make-instance (type-of parser)
		 :lexical-namespace (parser-lexical-namespace parser)
		 :free-namespace (parser-free-namespace parser)))

(defmethod parser-boundp-or-lookup ((parser parser) symbol-type symbol &key error-if-unbound)
  (flet ((try (namespace)
	   (and (namespace-boundp symbol-type symbol namespace)
		(namespace-lookup symbol-type symbol namespace))))
    (or (try (parser-lexical-namespace parser))
	(try (parser-free-namespace parser))
	(if error-if-unbound
	    (error "SYMBOL ~A with NSO-type ~S not bound in ~S" symbol symbol-type parser)
	    nil))))

(defmethod parser-boundp ((parser parser) symbol-type symbol)
  "Return the content bound to symbol SYMBOL, which is of type SYMBOL-TYPE, in the lexical or free namespace in PARSER, or NIL if it is not present."
  (parser-boundp-or-lookup parser symbol-type symbol))

(defmethod parser-lookup ((parser parser) symbol-type symbol)
  "Look up symbol SYMBOL, which is of type SYMBOL-TYPE, in the lexical or free namespace of PARSER, or raise an error otherwise."
  (parser-boundp-or-lookup parser symbol-type symbol :error-if-unbound t))

(defgeneric make-ast (parser type &rest arguments)
  (:documentation "This method is called to create an instance or part of an AST. Override to set e.g. a custom USER slot."))

(defmethod make-ast ((parser parser) type &rest arguments)
  (and (subtypep type '(or walker:nso walker:form))
       (assert (let ((cdr (member :source arguments))) (and (consp cdr) (consp (cdr cdr)))) () "MAKE-AST requires a :SOURCE slot."))
  (apply #'make-instance type arguments))

;; TODO: maybe remove #'COPY-DEEP-NAMESPACE and #'COPY-DEEP-PARSER, because they are false friends, because the free namespaces modified in the child parser copy is not changed in the parent parser copy (maybe only for debugging)
(defmethod copy-deep-namespace ((parser parser) namespace-type)
  (let ((namespace (slot-value parser namespace-type)))
    (make-ast parser namespace-type
	      :var (copy-alist (namespace-var namespace))
	      :fun (copy-alist (namespace-fun namespace))
	      :blo (copy-alist (namespace-blo namespace))
	      :tag (copy-alist (namespace-tag namespace)))))

(defmethod copy-deep-parser ((parser parser))
  (let* ((p (copy-parser parser))
	 (ln (copy-deep-namespace p 'lexical-namespace))
	 (fn (copy-deep-namespace p 'free-namespace)))
    (setf (parser-lexical-namespace p) ln)
    (setf (parser-free-namespace p) fn)
    p))

;;;; NAMESPACES again

(defgeneric augment-lexical-namespace (nso-object parser)
  (:documentation "Create a copy of the PARSER with a copy of the lexical namespace, augment the namespace copy with the NSO-OBJECT (which must be a subtype of NSO), and return the PARSER copy.
The new parser copy shares namespace structure with the original PARSER."))
(defmethod augment-lexical-namespace (nso-object (parser parser))
  (assert (not (nso-freep nso-object)) () "Cannot augment a lexical namespace with free namespace object ~S" nso-object)
  (let* ((parser-copy (copy-parser parser))
	 (namespace-copy (copy-namespace (parser-lexical-namespace parser-copy)))
	 (nso-type (type-of nso-object)))
    (setf (parser-lexical-namespace parser-copy) namespace-copy)
    (setf (slot-value namespace-copy nso-type)
	  (acons (nso-name nso-object) nso-object (slot-value namespace-copy nso-type)))
    parser-copy))

(defgeneric augment-free-namespace (nso-object parser)
  (:documentation "Add NSO-OBJECT (which must be a subtype of NSO) to the free namespace of PARSER. Return NIL."))
(defmethod augment-free-namespace (nso-object (parser parser))
  (let* ((namespace (parser-free-namespace parser))
	 (nso-type (type-of nso-object)))
    (setf (slot-value namespace nso-type)
	  (acons (nso-name nso-object) nso-object (slot-value namespace nso-type)))
    nil))

(defgeneric namespace-lookup/create (symbol-type symbol parser)
  (:documentation "If the SYMBOL is bound in the lexical namespace of PARSER in the slot SYMBOL-TYPE, then return the object bound to SYMBOL. If the SYMBOL is bound in the free namespace of PARSER in the slot SYMBOL-TYPE, then return the object bound to SYMBOL. Otherwise create a free NSO-instance of type SYMBOL-TYPE, modify the free namespace of PARSER so that SYMBOL is bound to new NSO-instance and return the NSO-instance."))
(defun default-namespace-lookup/create (symbol-type symbol parser new-nso)
  (let ((lexical-namespace (parser-lexical-namespace parser))
	(free-namespace (parser-free-namespace parser)))
    (cond
      ((namespace-boundp symbol-type symbol lexical-namespace)
       (namespace-lookup symbol-type symbol lexical-namespace))
      ((namespace-boundp symbol-type symbol free-namespace)
       (namespace-lookup symbol-type symbol free-namespace))
      (t
       (augment-free-namespace new-nso parser)
       new-nso))))
(defmethod namespace-lookup/create ((symbol-type (eql 'var)) symbol (parser parser))
  (assert (symbolp symbol) () "Invalid symbol name ~S" symbol)
  (default-namespace-lookup/create symbol-type symbol parser (make-ast parser 'var :name symbol :freep t :declspecs nil :sites nil :source symbol))) ;do not bind :DEFINITION
(defmethod namespace-lookup/create ((symbol-type (eql 'fun)) symbol (parser parser))
  (assert (not (null (valid-function-name-p symbol))) () "Invalid function name ~S" symbol)
  (default-namespace-lookup/create symbol-type symbol parser (make-ast parser 'fun :name symbol :freep t :declspecs nil :sites nil :source symbol))) ;do not bind :DEFINITION
(defmethod namespace-lookup/create ((symbol-type (eql 'blo)) symbol (parser parser))
  (assert (symbolp symbol) () "Invalid symbol name ~S" symbol)
  (default-namespace-lookup/create symbol-type symbol parser (make-ast parser 'blo :name symbol :freep t :sites nil :source symbol))) ;do not bind :DEFINITION
(defmethod namespace-lookup/create ((symbol-type (eql 'tag)) symbol (parser parser))
  (assert (symbolp symbol) () "Invalid symbol name ~S" symbol)
  (default-namespace-lookup/create symbol-type symbol parser (make-ast parser 'tag :name symbol :freep t :sites nil :source symbol))) ;do not bind :DEFINITION

(let ((parser (make-instance 'parser)))
  (setf parser (augment-lexical-namespace (make-instance 'var :name 'a :freep nil :sites nil) parser))
  (assert (namespace-lookup 'var 'a (parser-lexical-namespace parser)))
  (assert (null (namespace-var (parser-free-namespace parser)))))
(let ((parser (make-instance 'parser)))
  (namespace-lookup/create 'var 'a parser)
  (assert (null (namespace-var (parser-lexical-namespace parser))))
  (assert (namespace-lookup 'var 'a (parser-free-namespace parser))))

;; the list of Common Lisp macros: (determined by looking at differences between SBCL and CLISP of global *LISP-MACROS* computed in file all-lisp-symbols.lisp)
(unless (boundp '+common-lisp-macros+)
  (defconstant +common-lisp-macros+ '(AND ASSERT CALL-METHOD CASE CCASE CHECK-TYPE COND CTYPECASE DECF DECLAIM DEFCLASS DEFCONSTANT DEFGENERIC DEFINE-COMPILER-MACRO DEFINE-CONDITION DEFINE-METHOD-COMBINATION DEFINE-MODIFY-MACRO DEFINE-SETF-EXPANDER DEFINE-SYMBOL-MACRO DEFMACRO DEFMETHOD DEFPACKAGE DEFPARAMETER DEFSETF DEFSTRUCT DEFTYPE DEFUN DEFVAR DESTRUCTURING-BIND DO DO* DO-ALL-SYMBOLS DO-EXTERNAL-SYMBOLS DO-SYMBOLS DOLIST DOTIMES ECASE ETYPECASE FORMATTER HANDLER-BIND HANDLER-CASE IGNORE-ERRORS IN-PACKAGE INCF LAMBDA LOOP LOOP-FINISH MULTIPLE-VALUE-BIND MULTIPLE-VALUE-LIST MULTIPLE-VALUE-SETQ NTH-VALUE OR POP PPRINT-LOGICAL-BLOCK PRINT-UNREADABLE-OBJECT PROG PROG* PROG1 PROG2 PSETF PSETQ PUSH PUSHNEW REMF RESTART-BIND RESTART-CASE RETURN ROTATEF SETF SHIFTF STEP TIME TRACE TYPECASE UNLESS UNTRACE WHEN WITH-ACCESSORS WITH-COMPILATION-UNIT WITH-CONDITION-RESTARTS WITH-HASH-TABLE-ITERATOR WITH-INPUT-FROM-STRING WITH-OPEN-FILE WITH-OPEN-STREAM WITH-OUTPUT-TO-STRING WITH-PACKAGE-ITERATOR WITH-SIMPLE-RESTART WITH-SLOTS WITH-STANDARD-IO-SYNTAX) "The list of macros defined in Common Lisp."))

;; the list of Common Lisp functions: (determined by global *LISP-FUNCTIONS* computed in file all-lisp-symbols.lisp, which are equal between SBCL and CLISP)
(unless (boundp '+common-lisp-functions+)
  (defconstant +common-lisp-functions+ '(* + - / /= 1+ 1- < <= = > >= ABORT ABS ACONS ACOS ACOSH ADD-METHOD ADJOIN ADJUST-ARRAY ADJUSTABLE-ARRAY-P ALLOCATE-INSTANCE ALPHA-CHAR-P ALPHANUMERICP APPEND APPLY APROPOS APROPOS-LIST AREF ARITHMETIC-ERROR-OPERANDS ARITHMETIC-ERROR-OPERATION ARRAY-DIMENSION ARRAY-DIMENSIONS ARRAY-DISPLACEMENT ARRAY-ELEMENT-TYPE ARRAY-HAS-FILL-POINTER-P ARRAY-IN-BOUNDS-P ARRAY-RANK ARRAY-ROW-MAJOR-INDEX ARRAY-TOTAL-SIZE ARRAYP ASH ASIN ASINH ASSOC ASSOC-IF ASSOC-IF-NOT ATAN ATANH ATOM BIT BIT-AND BIT-ANDC1 BIT-ANDC2 BIT-EQV BIT-IOR BIT-NAND BIT-NOR BIT-NOT BIT-ORC1 BIT-ORC2 BIT-VECTOR-P BIT-XOR BOOLE BOTH-CASE-P BOUNDP BREAK BROADCAST-STREAM-STREAMS BUTLAST BYTE BYTE-POSITION BYTE-SIZE CAAAAR CAAADR CAAAR CAADAR CAADDR CAADR CAAR CADAAR CADADR CADAR CADDAR CADDDR CADDR CADR CAR CDAAAR CDAADR CDAAR CDADAR CDADDR CDADR CDAR CDDAAR CDDADR CDDAR CDDDAR CDDDDR CDDDR CDDR CDR CEILING CELL-ERROR-NAME CERROR CHANGE-CLASS CHAR CHAR-CODE CHAR-DOWNCASE CHAR-EQUAL CHAR-GREATERP CHAR-INT CHAR-LESSP CHAR-NAME CHAR-NOT-EQUAL CHAR-NOT-GREATERP CHAR-NOT-LESSP CHAR-UPCASE CHAR/= CHAR< CHAR<= CHAR= CHAR> CHAR>= CHARACTER CHARACTERP CIS CLASS-NAME CLASS-OF CLEAR-INPUT CLEAR-OUTPUT CLOSE CLRHASH CODE-CHAR COERCE COMPILE COMPILE-FILE COMPILE-FILE-PATHNAME COMPILED-FUNCTION-P COMPILER-MACRO-FUNCTION COMPLEMENT COMPLEX COMPLEXP COMPUTE-APPLICABLE-METHODS COMPUTE-RESTARTS CONCATENATE CONCATENATED-STREAM-STREAMS CONJUGATE CONS CONSP CONSTANTLY CONSTANTP CONTINUE COPY-ALIST COPY-LIST COPY-PPRINT-DISPATCH COPY-READTABLE COPY-SEQ COPY-STRUCTURE COPY-SYMBOL COPY-TREE COS COSH COUNT COUNT-IF COUNT-IF-NOT DECODE-FLOAT DECODE-UNIVERSAL-TIME DELETE DELETE-DUPLICATES DELETE-FILE DELETE-IF DELETE-IF-NOT DELETE-PACKAGE DENOMINATOR DEPOSIT-FIELD DESCRIBE DESCRIBE-OBJECT DIGIT-CHAR DIGIT-CHAR-P DIRECTORY DIRECTORY-NAMESTRING DISASSEMBLE DOCUMENTATION DPB DRIBBLE ECHO-STREAM-INPUT-STREAM ECHO-STREAM-OUTPUT-STREAM ED EIGHTH ELT ENCODE-UNIVERSAL-TIME ENDP ENOUGH-NAMESTRING ENSURE-DIRECTORIES-EXIST ENSURE-GENERIC-FUNCTION EQ EQL EQUAL EQUALP ERROR EVAL EVENP EVERY EXP EXPORT EXPT FBOUNDP FCEILING FDEFINITION FFLOOR FIFTH FILE-AUTHOR FILE-ERROR-PATHNAME FILE-LENGTH FILE-NAMESTRING FILE-POSITION FILE-STRING-LENGTH FILE-WRITE-DATE FILL FILL-POINTER FIND FIND-ALL-SYMBOLS FIND-CLASS FIND-IF FIND-IF-NOT FIND-METHOD FIND-PACKAGE FIND-RESTART FIND-SYMBOL FINISH-OUTPUT FIRST FLOAT FLOAT-DIGITS FLOAT-PRECISION FLOAT-RADIX FLOAT-SIGN FLOATP FLOOR FMAKUNBOUND FORCE-OUTPUT FORMAT FOURTH FRESH-LINE FROUND FTRUNCATE FUNCALL FUNCTION-KEYWORDS FUNCTION-LAMBDA-EXPRESSION FUNCTIONP GCD GENSYM GENTEMP GET GET-DECODED-TIME GET-DISPATCH-MACRO-CHARACTER GET-INTERNAL-REAL-TIME GET-INTERNAL-RUN-TIME GET-MACRO-CHARACTER GET-OUTPUT-STREAM-STRING GET-PROPERTIES GET-SETF-EXPANSION GET-UNIVERSAL-TIME GETF GETHASH GRAPHIC-CHAR-P HASH-TABLE-COUNT HASH-TABLE-P HASH-TABLE-REHASH-SIZE HASH-TABLE-REHASH-THRESHOLD HASH-TABLE-SIZE HASH-TABLE-TEST HOST-NAMESTRING IDENTITY IMAGPART IMPORT INITIALIZE-INSTANCE INPUT-STREAM-P INSPECT INTEGER-DECODE-FLOAT INTEGER-LENGTH INTEGERP INTERACTIVE-STREAM-P INTERN INTERSECTION INVALID-METHOD-ERROR INVOKE-DEBUGGER INVOKE-RESTART INVOKE-RESTART-INTERACTIVELY ISQRT KEYWORDP LAST LCM LDB LDB-TEST LDIFF LENGTH LISP-IMPLEMENTATION-TYPE LISP-IMPLEMENTATION-VERSION LIST LIST* LIST-ALL-PACKAGES LIST-LENGTH LISTEN LISTP LOAD LOAD-LOGICAL-PATHNAME-TRANSLATIONS LOG LOGAND LOGANDC1 LOGANDC2 LOGBITP LOGCOUNT LOGEQV LOGICAL-PATHNAME LOGICAL-PATHNAME-TRANSLATIONS LOGIOR LOGNAND LOGNOR LOGNOT LOGORC1 LOGORC2 LOGTEST LOGXOR LONG-SITE-NAME LOWER-CASE-P MACHINE-INSTANCE MACHINE-TYPE MACHINE-VERSION MACRO-FUNCTION MACROEXPAND MACROEXPAND-1 MAKE-ARRAY MAKE-BROADCAST-STREAM MAKE-CONCATENATED-STREAM MAKE-CONDITION MAKE-DISPATCH-MACRO-CHARACTER MAKE-ECHO-STREAM MAKE-HASH-TABLE MAKE-INSTANCE MAKE-INSTANCES-OBSOLETE MAKE-LIST MAKE-LOAD-FORM MAKE-LOAD-FORM-SAVING-SLOTS MAKE-PACKAGE MAKE-PATHNAME MAKE-RANDOM-STATE MAKE-SEQUENCE MAKE-STRING MAKE-STRING-INPUT-STREAM MAKE-STRING-OUTPUT-STREAM MAKE-SYMBOL MAKE-SYNONYM-STREAM MAKE-TWO-WAY-STREAM MAKUNBOUND MAP MAP-INTO MAPC MAPCAN MAPCAR MAPCON MAPHASH MAPL MAPLIST MASK-FIELD MAX MEMBER MEMBER-IF MEMBER-IF-NOT MERGE MERGE-PATHNAMES METHOD-COMBINATION-ERROR METHOD-QUALIFIERS MIN MINUSP MISMATCH MOD MUFFLE-WARNING NAME-CHAR NAMESTRING NBUTLAST NCONC NINTERSECTION NINTH NO-APPLICABLE-METHOD NO-NEXT-METHOD NOT NOTANY NOTEVERY NRECONC NREVERSE NSET-DIFFERENCE NSET-EXCLUSIVE-OR NSTRING-CAPITALIZE NSTRING-DOWNCASE NSTRING-UPCASE NSUBLIS NSUBST NSUBST-IF NSUBST-IF-NOT NSUBSTITUTE NSUBSTITUTE-IF NSUBSTITUTE-IF-NOT NTH NTHCDR NULL NUMBERP NUMERATOR NUNION ODDP OPEN OPEN-STREAM-P OUTPUT-STREAM-P PACKAGE-ERROR-PACKAGE PACKAGE-NAME PACKAGE-NICKNAMES PACKAGE-SHADOWING-SYMBOLS PACKAGE-USE-LIST PACKAGE-USED-BY-LIST PACKAGEP PAIRLIS PARSE-INTEGER PARSE-NAMESTRING PATHNAME PATHNAME-DEVICE PATHNAME-DIRECTORY PATHNAME-HOST PATHNAME-MATCH-P PATHNAME-NAME PATHNAME-TYPE PATHNAME-VERSION PATHNAMEP PEEK-CHAR PHASE PLUSP POSITION POSITION-IF POSITION-IF-NOT PPRINT PPRINT-DISPATCH PPRINT-FILL PPRINT-INDENT PPRINT-LINEAR PPRINT-NEWLINE PPRINT-TAB PPRINT-TABULAR PRIN1 PRIN1-TO-STRING PRINC PRINC-TO-STRING PRINT PRINT-NOT-READABLE-OBJECT PRINT-OBJECT PROBE-FILE PROCLAIM PROVIDE RANDOM RANDOM-STATE-P RASSOC RASSOC-IF RASSOC-IF-NOT RATIONAL RATIONALIZE RATIONALP READ READ-BYTE READ-CHAR READ-CHAR-NO-HANG READ-DELIMITED-LIST READ-FROM-STRING READ-LINE READ-PRESERVING-WHITESPACE READ-SEQUENCE READTABLE-CASE READTABLEP REALP REALPART REDUCE REINITIALIZE-INSTANCE REM REMHASH REMOVE REMOVE-DUPLICATES REMOVE-IF REMOVE-IF-NOT REMOVE-METHOD REMPROP RENAME-FILE RENAME-PACKAGE REPLACE REQUIRE REST RESTART-NAME REVAPPEND REVERSE ROOM ROUND ROW-MAJOR-AREF RPLACA RPLACD SBIT SCALE-FLOAT SCHAR SEARCH SECOND SET SET-DIFFERENCE SET-DISPATCH-MACRO-CHARACTER SET-EXCLUSIVE-OR SET-MACRO-CHARACTER SET-PPRINT-DISPATCH SET-SYNTAX-FROM-CHAR SEVENTH SHADOW SHADOWING-IMPORT SHARED-INITIALIZE SHORT-SITE-NAME SIGNAL SIGNUM SIMPLE-BIT-VECTOR-P SIMPLE-CONDITION-FORMAT-ARGUMENTS SIMPLE-CONDITION-FORMAT-CONTROL SIMPLE-STRING-P SIMPLE-VECTOR-P SIN SINH SIXTH SLEEP SLOT-BOUNDP SLOT-EXISTS-P SLOT-MAKUNBOUND SLOT-MISSING SLOT-UNBOUND SLOT-VALUE SOFTWARE-TYPE SOFTWARE-VERSION SOME SORT SPECIAL-OPERATOR-P SQRT STABLE-SORT STANDARD-CHAR-P STORE-VALUE STREAM-ELEMENT-TYPE STREAM-ERROR-STREAM STREAM-EXTERNAL-FORMAT STREAMP STRING STRING-CAPITALIZE STRING-DOWNCASE STRING-EQUAL STRING-GREATERP STRING-LEFT-TRIM STRING-LESSP STRING-NOT-EQUAL STRING-NOT-GREATERP STRING-NOT-LESSP STRING-RIGHT-TRIM STRING-TRIM STRING-UPCASE STRING/= STRING< STRING<= STRING= STRING> STRING>= STRINGP SUBLIS SUBSEQ SUBSETP SUBST SUBST-IF SUBST-IF-NOT SUBSTITUTE SUBSTITUTE-IF SUBSTITUTE-IF-NOT SUBTYPEP SVREF SXHASH SYMBOL-FUNCTION SYMBOL-NAME SYMBOL-PACKAGE SYMBOL-PLIST SYMBOL-VALUE SYMBOLP SYNONYM-STREAM-SYMBOL TAILP TAN TANH TENTH TERPRI THIRD TRANSLATE-LOGICAL-PATHNAME TRANSLATE-PATHNAME TREE-EQUAL TRUENAME TRUNCATE TWO-WAY-STREAM-INPUT-STREAM TWO-WAY-STREAM-OUTPUT-STREAM TYPE-ERROR-DATUM TYPE-ERROR-EXPECTED-TYPE TYPE-OF TYPEP UNBOUND-SLOT-INSTANCE UNEXPORT UNINTERN UNION UNREAD-CHAR UNUSE-PACKAGE UPDATE-INSTANCE-FOR-DIFFERENT-CLASS UPDATE-INSTANCE-FOR-REDEFINED-CLASS UPGRADED-ARRAY-ELEMENT-TYPE UPGRADED-COMPLEX-PART-TYPE UPPER-CASE-P USE-PACKAGE USE-VALUE USER-HOMEDIR-PATHNAME VALUES VALUES-LIST VECTOR VECTOR-POP VECTOR-PUSH VECTOR-PUSH-EXTEND VECTORP WARN WILD-PATHNAME-P WRITE WRITE-BYTE WRITE-CHAR WRITE-LINE WRITE-SEQUENCE WRITE-STRING WRITE-TO-STRING Y-OR-N-P YES-OR-NO-P ZEROP) "The list of functions defined in Common Lisp."))

(unless (boundp '+common-lisp-variables+)
  (defconstant +common-lisp-variables+ '(* ** *** *BREAK-ON-SIGNALS* *COMPILE-FILE-PATHNAME* *COMPILE-FILE-TRUENAME* *COMPILE-PRINT* *COMPILE-VERBOSE* *DEBUG-IO* *DEBUGGER-HOOK* *DEFAULT-PATHNAME-DEFAULTS* *ERROR-OUTPUT* *FEATURES* *GENSYM-COUNTER* *LOAD-PATHNAME* *LOAD-PRINT* *LOAD-TRUENAME* *LOAD-VERBOSE* *MACROEXPAND-HOOK* *MODULES* *PACKAGE* *PRINT-ARRAY* *PRINT-BASE* *PRINT-CASE* *PRINT-CIRCLE* *PRINT-ESCAPE* *PRINT-GENSYM* *PRINT-LENGTH* *PRINT-LEVEL* *PRINT-LINES* *PRINT-MISER-WIDTH* *PRINT-PPRINT-DISPATCH* *PRINT-PRETTY* *PRINT-RADIX* *PRINT-READABLY* *PRINT-RIGHT-MARGIN* *QUERY-IO* *RANDOM-STATE* *READ-BASE* *READ-DEFAULT-FLOAT-FORMAT* *READ-EVAL* *READ-SUPPRESS* *READTABLE* *STANDARD-INPUT* *STANDARD-OUTPUT* *TERMINAL-IO* *TRACE-OUTPUT* + ++ +++ - / // /// ARRAY-DIMENSION-LIMIT ARRAY-RANK-LIMIT ARRAY-TOTAL-SIZE-LIMIT BOOLE-1 BOOLE-2 BOOLE-AND BOOLE-ANDC1 BOOLE-ANDC2 BOOLE-C1 BOOLE-C2 BOOLE-CLR BOOLE-EQV BOOLE-IOR BOOLE-NAND BOOLE-NOR BOOLE-ORC1 BOOLE-ORC2 BOOLE-SET BOOLE-XOR CALL-ARGUMENTS-LIMIT CHAR-CODE-LIMIT DOUBLE-FLOAT-EPSILON DOUBLE-FLOAT-NEGATIVE-EPSILON INTERNAL-TIME-UNITS-PER-SECOND LAMBDA-LIST-KEYWORDS LAMBDA-PARAMETERS-LIMIT LEAST-NEGATIVE-DOUBLE-FLOAT LEAST-NEGATIVE-LONG-FLOAT LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT LEAST-NEGATIVE-SHORT-FLOAT LEAST-NEGATIVE-SINGLE-FLOAT LEAST-POSITIVE-DOUBLE-FLOAT LEAST-POSITIVE-LONG-FLOAT LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT LEAST-POSITIVE-NORMALIZED-LONG-FLOAT LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT LEAST-POSITIVE-SHORT-FLOAT LEAST-POSITIVE-SINGLE-FLOAT LONG-FLOAT-EPSILON LONG-FLOAT-NEGATIVE-EPSILON MOST-NEGATIVE-DOUBLE-FLOAT MOST-NEGATIVE-FIXNUM MOST-NEGATIVE-LONG-FLOAT MOST-NEGATIVE-SHORT-FLOAT MOST-NEGATIVE-SINGLE-FLOAT MOST-POSITIVE-DOUBLE-FLOAT MOST-POSITIVE-FIXNUM MOST-POSITIVE-LONG-FLOAT MOST-POSITIVE-SHORT-FLOAT MOST-POSITIVE-SINGLE-FLOAT MULTIPLE-VALUES-LIMIT NIL PI SHORT-FLOAT-EPSILON SHORT-FLOAT-NEGATIVE-EPSILON SINGLE-FLOAT-EPSILON SINGLE-FLOAT-NEGATIVE-EPSILON T) "The list of special variables defined in Common Lisp."))

(defun make-parser (&rest rest &key (type 'parser) (variables +common-lisp-variables+) (functions +common-lisp-functions+) (macros +common-lisp-macros+) &allow-other-keys)
  "Return a free namespace in which the list of VARIABLES, FUNCTIONS, and MACROS are defined.
By default, variables, functions, and macros available in package COMMON-LISP are present.
You may pass any keyword option accepted by (MAKE-INSTANCE TYPE ...)."
  ;; the FUN-objects in the returned namespace must have their MACROP-slot bound appropriately, so that parsing '(DEFMACRO BLA (A (IF S) &OPTIONAL C) (PRINT (LIST A IF S C)) NIL) doesn't fail anymore.
  (let* ((rest-filtered (remove-from-key-list rest '(:type :variables :functions :macros)))
	 (parser (apply #'make-instance type rest-filtered)))
    ;; do not bind the :DEFINITION slots.
    (loop for variable in variables do
	 (augment-free-namespace (make-ast parser 'var :name variable :freep t :declspecs nil :macrop nil :sites nil :source variable) parser))
    (loop for function in functions do
	 (augment-free-namespace (make-ast parser 'fun :name function :freep t :declspecs nil :macrop nil :sites nil :source function) parser))
    (loop for macro in macros do
	 (augment-free-namespace (make-ast parser 'fun :name macro :freep t :declspecs nil :macrop t :sites nil :source macro) parser))
    parser))

;;;; DECLARATIONS

(defclass declspec ()
  ((parent :initarg :parent :accessor declspec-parent)
   (source :initarg :source :accessor declspec-source :type list :documentation "The source of the declspec.")
   (user :initarg :user :accessor user
	 :documentation "Arbitrary user-definable slot.")))
(defmethod form-parent ((ast declspec))
  (declspec-parent ast))
(defmethod (setf form-parent) (value (ast declspec))
  (setf (declspec-parent ast) value))
(defmethod form-source ((ast declspec))
  (declspec-source ast))
(defmethod (setf form-source) (value (ast declspec))
  (setf (declspec-source ast) value))
(defclass declspec-type (declspec)
  ((type :initarg :type :accessor declspec-type)
   (vars :initarg :vars :accessor declspec-vars :type list)))
(defclass declspec-ftype (declspec)
  ((type :initarg :type :accessor declspec-type)
   (funs :initarg :funs :accessor declspec-funs :type list)))
(defclass declspec-optimize (declspec)
  ((qualities :initform nil :initarg :qualities :accessor declspec-qualities :type alist :documentation "An ALIST with the optimize quality as CAR and the optimize value, which must be one of NIL,0,1,2,3, as CDR, where NIL means that no value was given")))
(defclass declspec-ignore (declspec)
  ((syms :initarg :syms :accessor declspec-syms :type list :documentation "list of VARs and FUNs")))
(defclass declspec-ignorable (declspec)
  ((syms :initarg :syms :accessor declspec-syms :type list :documentation "list of VARs and FUNs")))
(defclass declspec-dynamic-extent (declspec)
  ((syms :initarg :funs :accessor declspec-syms :type list :documentation "list of VARs and FUNs")))
(defclass declspec-inline (declspec)
  ((funs :initarg :funs :accessor declspec-funs :type list)))
(defclass declspec-notinline (declspec)
  ((funs :initarg :funs :accessor declspec-funs :type list)))
(defclass declspec-special (declspec)
  ((vars :initarg :vars :accessor declspec-vars :type list)))

(defun parse-function-declaration (decl)
  "Given a function declaration, return two values: 1. the alist of parameters, indexed by T (for required), &OPTIONAL, &REST, and &KEY 2. the list of return value types.
Note that this function does not do recursive parsing when an parameter or return value type is a function type."
  (declare (optimize (debug 3)))
  (assert (or (eq decl 'function) (consp decl)) () "DECL must be 'FUNCTION or a list starting with 'FUNCTION, but is ~S" decl)
  (cond
    ((eq decl 'function)
     (values 'function))
    (t
     (assert (eq 'function (car decl)) () "DECL must start with 'FUNCTION, but is ~S" decl)
     (assert (listp (cadr decl)) () "DECL must look like (FUNCTION (PARAMETERS ...) VALUES), but is ~S" decl)
     (let* ((args (cadr decl))
	    (parsed (list (list t) (list '&optional) (list '&rest) (list '&key))) ;T means "required"
	    (current (assoc t parsed)))
       (do () ((null args))
	 (let ((h (pop args)))
	   (if (or (eq h '&optional) (eq h '&rest) (eq h '&key))
	       (let ((order (member (car current) '(t &optional &rest &key))))
		 (assert (find h order) () "PARAMETERS in function declaration must look like (REQUIRED-PARS...~% [&OPTIONAL OPTIONAL-PARS...] [&REST REST-PAR] [&KEY KEY-PARS...]), but~%~A is in the wrong position in~% ~A" h (cadr decl))
		 (setf current (assoc h parsed)))
	       (setf (cdr current) (cons h (cdr current))))))
       (let ((l (assoc t parsed))) (setf (cdr l) (nreverse (cdr l))))
       (let ((l (assoc '&optional parsed))) (setf (cdr l) (nreverse (cdr l))))
       (let ((l (assoc '&key parsed))) (setf (cdr l) (nreverse (cdr l))))
       (assert (<= (length (assoc '&rest parsed)) 2) () "DECL may have only one &REST parameter")
       (let ((values (caddr decl)))
	 (assert (or (symbolp values) (and (consp values) (or (eq (car values) 'values) (eq (car values) 'function)))) () "VALUES in function declaration must be either a type or look like~%(VALUES TYPES...), but is~%~A" values)
	 (if (and (consp values) (eq (car values) 'values))
	     (values parsed (cdr values))
	     (values parsed (list values))))))))

(defgeneric parse-declspec-expr (parser expr parent)
  (:documentation "Parse the declaration expression EXPR using PARSER with PARENT as enclosing form."))
(defgeneric parse-declspec (parser identifier body parent source)
  (:documentation "Parse the declaration expression (CONS IDENTIFIER BODY) using PARSER with PARENT as enclosing form."))

(defmethod parse-declspec-expr ((parser parser) (expr cons) parent)
  (let ((identifier (car expr))
	(body (cdr expr)))
    (assert (symbolp identifier) () "Malformed declaration identifier ~W" identifier)
    (parse-declspec parser identifier body parent expr)))

(defmethod parse-declspec-expr ((parser parser) (expr null) parent)
  nil)

(defmethod parse-declspec-expr ((parser parser) expr parent)
  (error "Malformed declaration specification ~W" expr))

(defun parse-declspec-type-ftype (parser identifier body parent source)
  (assert (and (listp body) (listp (cdr body))) () "Malformed ~W declaration ~W" identifier body)
  (let* ((typespec (car body))
	 (syms (cdr body)))
    ;; will not check TYPESPEC, has to be done in user code.
    (assert (proper-list-p syms) () "Not a proper list in ~W declaration ~W" identifier body)
    (let* ((parsed-syms (loop for sym in syms collect (namespace-lookup/create (ecase identifier ((type) 'var) ((ftype) 'fun)) sym parser)))
	   (parsed-declspec (ecase identifier
			      ((type) (make-ast parser 'declspec-type :parent parent :type typespec :vars parsed-syms :source source))
			      ((ftype) (make-ast parser 'declspec-ftype :parent parent :type typespec :funs parsed-syms :source source)))))
      (loop for sym in parsed-syms do
	   (push parsed-declspec (nso-declspecs sym)) (push parsed-declspec (nso-sites sym)))
      parsed-declspec)))

(defmethod parse-declspec ((parser parser) (identifier (eql 'type)) body parent source)
  (parse-declspec-type-ftype parser identifier body parent source))
(defmethod parse-declspec ((parser parser) (identifier (eql 'ftype)) body parent source)
  (parse-declspec-type-ftype parser identifier body parent source))

(defmethod parse-declspec ((parser parser) (identifier (eql 'optimize)) body parent source)
  (assert (proper-list-p body) () "~W declaration is not a proper list ~W" identifier body)
  (let ((qualities (loop for quality-value in body collect
			(if (consp quality-value)
			    (let ((quality (car quality-value))
				  (rest (cdr quality-value)))
			      (assert (and (symbolp quality) (consp rest) (find (car rest) '(0 1 2 3)) (null (cdr rest))) () "Malformed ~W in ~W declaration" body identifier)
			      (cons quality (car rest)))
			    (let ((quality quality-value))
			      (assert (symbolp quality) () "Malformed ~W in ~W" body identifier)
			      (cons quality nil))))))
    (make-ast parser 'declspec-optimize :parent parent :qualities qualities :source source)))

(defun parse-declspec-ignore-ignorable-dynamic-extent (parser identifier body parent source)
  (assert (listp body) () "Malformed ~W declaration ~W" identifier body)
  (let* ((syms body))
    (assert (proper-list-p syms) () "Not a proper list in ~W declaration ~W" identifier body)
    (let* ((parsed-syms (loop for sym in syms collect
			     (cond
			       ((symbolp sym)
				(namespace-lookup/create 'var sym parser))
			       ((and (consp sym) (eq (car sym) 'function) (valid-function-name-p (cadr sym)))
				(namespace-lookup/create 'fun (cadr sym) parser))
			       (t
				(error "Symbol in ~W declaration must be either a SYMBOL or a function name, but is ~W" identifier sym)))))
	   (parsed-declspec (make-ast parser (ecase identifier ((ignore) 'declspec-ignore) ((ignorable) 'declspec-ignorable) ((dynamic-extent) 'declspec-dynamic-extent)) :parent parent :syms parsed-syms :source source)))
      (loop for sym in parsed-syms do
	   (push parsed-declspec (nso-declspecs sym)) (push parsed-declspec (nso-sites sym)))
      parsed-declspec)))

(defmethod parse-declspec ((parser parser) (identifier (eql 'ignore)) body parent source)
  (parse-declspec-ignore-ignorable-dynamic-extent parser identifier body parent source))
(defmethod parse-declspec ((parser parser) (identifier (eql 'ignorable)) body parent source)
  (parse-declspec-ignore-ignorable-dynamic-extent parser identifier body parent source))
(defmethod parse-declspec ((parser parser) (identifier (eql 'dynamic-extent)) body parent source)
  (parse-declspec-ignore-ignorable-dynamic-extent parser identifier body parent source))

;; probably not TODO (since the parser should contain as little semantics as possible): when adding DYNAMIC-EXTENT and SPECIAL, add an assertion that, as defined in CLHS on DYNAMIC-EXTENT, "The vars and fns named in a dynamic-extent declaration must not refer to symbol macro or macro bindings." and an assertion that, as defined in CLHS on SYMBOL-MACROLET, "Exactly the same declarations are allowed as for let with one exception: symbol-macrolet signals an error if a special declaration names one of the symbols being defined by symbol-macrolet."

(defun parse-declspec-inline-notinline (parser identifier body parent source)
  (assert (listp body) () "Malformed ~W declaration ~W" identifier body)
  (let* ((funs body))
    (assert (proper-list-p funs) () "Not a proper list in ~W declaration ~W" identifier body)
    (let* ((parsed-funs (loop for fun in funs collect
			     (progn
			       (assert (valid-function-name-p fun))
			       (namespace-lookup/create 'fun fun parser))))
	   (parsed-declspec (make-ast parser (ecase identifier ((inline) 'declspec-inline) ((notinline) 'declspec-notinline)) :parent parent :funs parsed-funs :source source)))
      (loop for fun in parsed-funs do
	   (push parsed-declspec (nso-declspecs fun)) (push parsed-declspec (nso-sites fun)))
      parsed-declspec)))

(defmethod parse-declspec ((parser parser) (identifier (eql 'inline)) body parent source)
  (parse-declspec-inline-notinline parser identifier body parent source))
(defmethod parse-declspec ((parser parser) (identifier (eql 'notinline)) body parent source)
  (parse-declspec-inline-notinline parser identifier body parent source))

(defmethod parse-declspec ((parser parser) (identifier (eql 'special)) body parent source)
  (assert (listp body) () "Malformed ~W declaration ~W" identifier body)
  (let* ((vars body))
    (assert (proper-list-p vars) () "Not a proper list in ~W declaration ~W" identifier body)
    (let* ((parsed-vars (loop for var in vars collect (namespace-lookup/create 'var var parser)))
	   (parsed-declspec (make-ast parser 'declspec-special :parent parent :vars parsed-vars :source source)))
      (loop for var in parsed-vars do
	   (setf (nso-freep var) t) ;mark all PARSED-VARS as special
	   (push parsed-declspec (nso-declspecs var)) (push parsed-declspec (nso-sites var)))
      parsed-declspec)))

(defmethod parse-declspec ((parser parser) identifier body parent source)
  (error "Unknown declaration identifier ~W" identifier))

(defgeneric parse-declspecs (parser declspecs parent)
  (:documentation "Parse the DECLSPECS (a list of declaration expressions) using PARSER and PARENT as the enclosing form."))

(defmethod parse-declspecs ((parser parser) declspecs parent)
  "Example: (PARSE-DECLSPECS PARSER '((TYPE FIXNUM A B C) (IGNORE A)) (MAKE-EMPTY-LEXICAL-NAMESPACE) (MAKE-EMPTY-FREE-NAMESPACE) NIL)"
  (assert (proper-list-p declspecs) () "Declaration specifications must be a proper list, but are ~S" declspecs)
  ;; TODO: the CLHS for "Declaration TYPE" says that the syntax for type declarations can be both (TYPE TYPESPEC VAR*) and (TYPESPEC VAR*). Implement this in the fallback method of #'PARSE-DECLSPEC. And since I do not consider interpreting types in package WALKER, any TYPESPEC should be accepted.
  (loop for declspec in declspecs collect
       (parse-declspec-expr parser declspec parent)))

(defgeneric parse-declaration-in-body (parser body parent)
  (:documentation "Parses declarations in the beginning of BODY.
Returns two values: the rest of the BODY that does not start with a DECLARE-expression, and a list of DECLSPEC-objects.
Side-effects: Adds references of the created DECLSPEC-objects to the DECLSPEC-slots of variables or functions in LEXICAL-NAMESPACE and FREE-NAMESPACE. Creates yet unknown free variables and functions, adds references to the created DECLSPEC-objects, and adds the NSO-objects to FREE-NAMESPACE.
Note that this function does not parse types, it just stores them in DECLSPEC-objects. Use #'PARSE-FUNCTION-DECLARATION for function declarations."))

(defmethod parse-declaration-in-body ((parser parser) body parent)
  (declare (optimize (debug 3)))
  (assert (listp body) () "Malformed BODY:~%~S" body)
  (labels ((parse-declare (body collected-declspecs)
	     (let ((head (car body))
		   (rest (cdr body)))
	       (cond
		 ((and (listp head) (eq (car head) 'declare))
		  (let* ((declspecs (cdr head)))
		    (assert (proper-list-p declspecs) () "Not a proper list: ~S" declspecs)
		    (let ((new-declspecs (parse-declspecs parser declspecs parent)))
		      (parse-declare rest (nconc collected-declspecs new-declspecs)))))
		 (t (values body collected-declspecs))))))
    (assert (proper-list-p body) () "Not a proper list: ~S" body)
    (parse-declare body nil)))

(let ((parser (make-instance 'parser)))
  (multiple-value-bind (body declspecs) (parse-declaration-in-body parser '(declare (type fixnum)) nil)
    (assert (and (equal body '(declare (type fixnum))) (null declspecs)))))
;;TODO: test that (parse-declaration-in-body '((declare ()) 5) nil nil nil) throws an error.
(let ((parser (make-instance 'parser)))
  (multiple-value-bind (body declspecs) (parse-declaration-in-body parser '((declare (type fixnum a)) 5) nil)
    (assert (and (equal body '(5)) (typep (car declspecs) 'declspec-type)))
    (assert (nso-freep (car (declspec-vars (car declspecs)))))
    (assert (eq (car (declspec-vars (car declspecs))) (namespace-lookup 'var 'a (parser-free-namespace parser))))
    (assert (not (namespace-boundp 'fun 'a (parser-free-namespace parser))))))
(let ((parser (make-instance 'parser)))
  (multiple-value-bind (body declspecs) (parse-declaration-in-body parser '((declare (ftype (function () fixnum) a)) 5) nil)
    (assert (and (equal body '(5)) (typep (car declspecs) 'declspec-ftype)))
    (assert (nso-freep (car (declspec-funs (car declspecs)))))
    (assert (eq (car (declspec-funs (car declspecs))) (namespace-lookup 'fun 'a (parser-free-namespace parser))))
    (assert (not (namespace-boundp 'var 'a (parser-free-namespace parser))))))
(let ((parser (make-instance 'parser)))
  (multiple-value-bind (body declspecs) (parse-declaration-in-body parser '((declare (ftype (function () fixnum) (setf a))) 5) nil)
    (assert (and (equal body '(5)) (typep (car declspecs) 'declspec-ftype)))
    (assert (nso-freep (car (declspec-funs (car declspecs)))))
    (assert (eq (car (declspec-funs (car declspecs))) (namespace-lookup 'fun '(setf a) (parser-free-namespace parser))))
    (assert (not (namespace-boundp 'var '(setf a) (parser-free-namespace parser))))))
(let ((parser (make-instance 'parser)))
  (multiple-value-bind (body declspecs) (parse-declaration-in-body parser '((declare (optimize (debug 3) speed)) 5) nil)
    (assert (equal body '(5)))
    (assert (let ((d (car declspecs))) (and (typep d 'declspec-optimize) (equal (declspec-qualities d) '((debug . 3) (speed . nil))))))))
(let ((parser (make-instance 'parser)))
  (multiple-value-bind (body declspecs) (parse-declaration-in-body parser '((declare (ignore a (function b))) 5) nil)
    (assert (and (equal body '(5)) (typep (car declspecs) 'declspec-ignore)))
    (assert (let ((syms (declspec-syms (car declspecs)))) (typep (car syms) 'var) (typep (cadr syms) 'fun)))
    (assert (eq (car (declspec-syms (car declspecs))) (namespace-lookup 'var 'a (parser-free-namespace parser))))
    (assert (eq (cadr (declspec-syms (car declspecs))) (namespace-lookup 'fun 'b (parser-free-namespace parser))))))
(multiple-value-bind (body declspecs)
    (parse-declaration-in-body (make-instance 'parser)
			       '((declare (type fixnum) (ftype (function () t))) (declare (optimize (speed 3)) (ignore)))
			       nil)
  (assert (equal body nil))
  (assert (typep (elt declspecs 0) 'declspec-type))
  (assert (typep (elt declspecs 1) 'declspec-ftype))
  (assert (typep (elt declspecs 2) 'declspec-optimize))
  (assert (typep (elt declspecs 3) 'declspec-ignore)))
(multiple-value-bind (body declspecs)
    (parse-declaration-in-body (make-instance 'parser) '((declare (inline f1 f2)) (declare (notinline f1))) nil)
  (assert (equal body nil))
  (assert (let ((inl (elt declspecs 0)))
	    (and (typep inl 'declspec-inline)
		 (and (eq (nso-name (car (declspec-funs inl))) 'f1)
		      (eq (nso-name (cadr (declspec-funs inl))) 'f2)))))
  (assert (let ((notinl (elt declspecs 1)))
	    (and (typep notinl 'declspec-notinline)
		 (eq (nso-name (car (declspec-funs notinl))) 'f1)))))
(multiple-value-bind (body declspecs)
    (parse-declaration-in-body (make-instance 'parser) '((declare (special a))) nil)
  (assert (equal body nil))
  (assert (let ((spec (elt declspecs 0)))
	    (and (typep spec 'declspec-special)
		 (eq (nso-name (car (declspec-vars spec))) 'a)))))

(defgeneric parse-declaration-and-documentation-in-body (parser body parent)
  (:documentation "Parses declarations and documentation in the beginning of BODY.
Returns three values: the rest of the BODY that does not start with a DECLARE-expression or a documentation string, and a list of DECLSPEC-objects, and a documentation string, or NIL if none is present in BODY.
Side-effects: Adds references of the created DECLSPEC-objects to the DECLSPEC-slots of variables or functions in LEXICAL-NAMESPACE and FREE-NAMESPACE. Creates yet unknown free variables and functions, adds references to the created DECLSPEC-objects, and adds the NSO-objects to FREE-NAMESPACE."))

(defmethod parse-declaration-and-documentation-in-body ((parser parser) body parent)
  (let ((documentation nil)
	(declspecs nil))
    (loop do
	 (multiple-value-bind (body-rest declspecs1) (parse-declaration-in-body parser body parent)
	   (setf declspecs (nconc declspecs declspecs1))
	   (cond
	     ((and (consp body) (stringp (car body-rest)))
	      (if documentation
		  (error "Two documentation strings ~S and ~S present, but only one allowed" documentation (car body-rest))
		  (setf documentation (car body-rest)))
	      (setf body (cdr body-rest)))
	     (t
	      (return (values body-rest declspecs documentation))))))))

(multiple-value-bind (body declspecs documentation) (parse-declaration-and-documentation-in-body (make-instance 'parser) '((declare (type number a)) "doc" (declare (type fixnum a)) 5) nil)
  (assert (and (equal body '(5)) (typep (car declspecs) 'declspec-type) (typep (cadr declspecs) 'declspec-type) (equal documentation "doc"))))

(defmethod print-object ((object declspec-type) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (cons 'type (cons (declspec-type object) (declspec-vars object))))))
(defmethod print-object ((object declspec-ftype) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (cons 'ftype (cons (declspec-type object) (declspec-funs object))))))
(defmethod print-object ((object declspec-optimize) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (cons 'optimize (mapcar (lambda (acons) (let ((quality (car acons)) (value (cdr acons))) (if (null value) quality (list quality value)))) (declspec-qualities object))))))
(defmethod print-object ((object declspec-ignore) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (cons 'ignore (declspec-syms object)))))
(defmethod print-object ((object declspec-ignorable) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (cons 'ignorable (declspec-syms object)))))
(defmethod print-object ((object declspec-dynamic-extent) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (cons 'dynamic-extent (declspec-syms object)))))
(defmethod print-object ((object declspec-inline) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (cons 'inline (declspec-funs object)))))
(defmethod print-object ((object declspec-notinline) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (cons 'notinline (declspec-funs object)))))
(defmethod print-object ((object declspec-special) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (cons 'special (declspec-vars object)))))

(defgeneric parse (parser form parent)
  (:documentation "Recursively parse the Common Lisp FORM. During parsing, the lexical namespace of PARSER is augmented with the found namespace objects, but the originally passed PARSER instance is not modified. The free namespace of PARSER is modified by adding namespace objects that are free in FORM. Use PARENT as the :PARENT slot in the returned abstract syntax tree (AST).
PARSE parses the FORM and the elements in it recursively and constructs an AST, i.e. an instance of one of the -FORM classes.
Return the parsed abstract syntax tree (AST)."))

(defgeneric parse-form (parser head rest parent source)
  (:documentation "Helper for #'PARSE"))

;;;; LAMBDA LISTS
;; see CLHS 3.4 Lambda Lists

(defclass parameter ()
  ((parent :initarg :parent :accessor parameter-parent :type functiondef)
   (source :initarg :source :accessor parameter-source :documentation "The source of the parameter.")
   (var :initarg :var :accessor parameter-var :type var)
   (user :initform nil :initarg :user :accessor user
	 :documentation "Arbitrary user-definable slot.")))
(defmethod form-parent ((ast parameter))
  (parameter-parent ast))
(defmethod (setf form-parent) (value (ast parameter))
  (setf (parameter-parent ast) value))
(defmethod form-source ((ast parameter))
  (parameter-source ast))
(defmethod (setf form-source) (value (ast parameter))
  (setf (parameter-source ast) value))
(defclass parameter-init-mixin ()
  ((init :initarg :init :accessor parameter-init :type (or null form) :documentation "NIL means it was not specified, and then the parser does not assign a default initial form. (e.g. DEFTYPE would have * by default instead of NIL, but it's not the parser's job to define semantics.)")))
(defclass whole-parameter (parameter)
  ())
(defclass environment-parameter (parameter)
  ())
(defclass required-parameter (parameter)
  ())
(defclass optional-parameter (parameter parameter-init-mixin)
  ((suppliedp :initarg :suppliedp :accessor parameter-suppliedp :type (or null var) :documentation "NIL means not present")))
(defclass rest-parameter (parameter)
  ())
(defclass body-parameter (parameter)
  ())
(defclass key-parameter (optional-parameter)
  ((keywordp :initarg :keywordp :accessor parameter-keywordp :type boolean :documentation "NIL means not present")
   (keyword :initarg :keyword :accessor parameter-keyword :type symbol :documentation "has no meaning if KEYWORDP==NIL")))
(defclass aux-parameter (parameter parameter-init-mixin)
  ())
(defclass llist ()
  ((parent :initarg :parent :accessor llist-parent
	   :documentation "The FUNCTIONDEF this LLIST is defined in.")
   (source :initarg :source :accessor llist-source :type list :documentation "The source of the lambda list.")
   (user :initform nil :initarg :user :accessor user
	 :documentation "Arbitrary user-definable slot.")))
(defmethod form-parent ((ast llist))
  (llist-parent ast))
(defmethod (setf form-parent) (value (ast llist))
  (setf (llist-parent ast) value))
(defmethod form-source ((ast llist))
  (llist-source ast))
(defmethod (setf form-source) (value (ast llist))
  (setf (llist-source ast) value))
(defclass ordinary-llist (llist)
  ((required :initarg :required :accessor llist-required :type list :documentation "list, with each element of type REQUIRED-PARAMETER")
   (optional :initarg :optional :accessor llist-optional :type list :documentation "list, with each element of type OPTIONAL-PARAMETER")
   (rest :initarg :rest :accessor llist-rest :type (or null rest-parameter))
   (key :initarg :key :accessor llist-key :type list :documentation "list, with each element of type KEY-PARAMETER")
   (allow-other-keys :initarg :allow-other-keys :accessor llist-allow-other-keys :type boolean)
   (aux :initarg :aux :accessor llist-aux :type list :documentation "list, with each element of type AUX-PARAMETER")))
(defclass macro-llist (llist)
  ((whole :initarg :whole :accessor llist-whole :type (or null whole-parameter llist))
   (environment :initarg :environment :accessor llist-environment :type (or null environment-parameter))
   (required :initarg :required :accessor llist-required :type list :documentation "list, with each element of type PARAMETER, or LLIST (for macro-lambda-lists)")
   (optional :initarg :optional :accessor llist-optional :type list :documentation "list, with each element of type OPTIONAL-PARAMETER, or LLIST (for macro-lambda-lists)")
   (rest :initarg :rest :accessor llist-rest :type (or null rest-parameter llist))
   (body :initarg :body :accessor llist-body :type (or null body-parameter llist))
   (key :initarg :key :accessor llist-key :type list :documentation "list, with each element of type KEY-PARAMETER, or LLIST (for macro-lambda-lists)")
   (allow-other-keys :initarg :allow-other-keys :accessor llist-allow-other-keys :type boolean)
   (aux :initarg :aux :accessor llist-aux :type list :documentation "list, with each element of type AUX-PARAMETER")))

(defun parse-required-parameter (varname)
  ;; probably not TODO (because whether a variable is constant is semantic, not syntax, and therefore not to be checked in a parser): add checking for "constant variable" in: CLHS 3.4.1 Ordinary Lambda Lists says: A var or supplied-p-parameter must be a symbol that is not the name of a constant variable.
  varname)

(defun parse-optional-or-key-or-aux-parameter (parform parameter-type)
  "Parse parameter PARFORM according to the given PARAMETER-TYPE, which must be one of &OPTIONAL &KEY &AUX.
Forms allowed for &OPTIONAL, &KEY, &AUX: SYMBOL, (SYMBOL), (SYMBOL FORM)
Forms allowed for &OPTIONAL, &KEY: (SYMBOL FORM SYMBOL)
Forms allowed for &KEY: ((SYMBOL SYMBOL)), ((SYMBOL SYMBOL) FORM), ((SYMBOL SYMBOL) FORM SYMBOL)
Return seven values: indicator whether keyword name is present (NIL for &OPTIONAL and &AUX or if not present, non-NIL otherwise), its keyword name symbol, its variable name symbol (guaranteed to be neither NIL nor T), whether an initialization form is present, the initialization form, indicator whether supplied-p is present or not, the supplied-p symbol."
  (assert (find parameter-type '(&OPTIONAL &KEY &AUX)))
  (cond
    ((symbolp parform)
     (values nil nil parform nil nil nil nil))
    ((and (consp parform))
     (let ((keywordp :undef) (keyword nil) (varname nil) (init-form-p nil) (init-form nil) (suppliedp nil) (suppliedp-name nil)
	   (head (car parform))
	   (rest (cdr parform)))
       (cond
	 ((symbolp head)
	  (setf keywordp nil) (setf keyword nil) (setf varname head))
	 ((and (consp head) (symbolp (car head)) (consp (cdr head)) (symbolp (cadr head)) (null (cddr head)))
	  (assert (eq parameter-type '&KEY) () "Variable name is ~S, but names of the form (SYMBOL SYMBOL) only allowed for &KEY parameters" head)
	  (setf keywordp t) (setf keyword (car head)) (setf varname (cadr head)))
	 (t (error "Invalid ~S parameter name ~S" parameter-type varname)))
       (assert (listp rest) () "Invalid ~S parameter seems to be of the form (SYMBOL FORM) or (SYMBOL FORM SYMBOL), but is ~S" parameter-type parform)
       (when (not (null rest))
	 (let ((init-form2 (car rest))
	       (rest (cdr rest)))
	   (setf init-form-p t)
	   (setf init-form init-form2) ;nothing to check for INIT-FORM
	   (when (eq parameter-type '&AUX)
	     (assert (null rest) () "Invalid ~S parameter seems to be of the form (SYMBOL FORM), but is ~S which is too long" parameter-type parform))
	   (assert (listp rest) () "Invalid ~S parameter seems to be of the form (SYMBOL FORM SYMBOL), but is ~S" parameter-type parform)
	   (when (not (null rest))
	     (assert (null (cdr rest)) () "Invalid ~S parameter ~S seems to be of the form (SYMBOL FORM SYMBOL), but does not match" parameter-type parform)
	     (assert (symbolp (car rest)) () "Supplied-p-parameter (of optional argment ~S) must be a symbol, but is ~S" varname suppliedp-name)
	     (setf suppliedp t) (setf suppliedp-name (car rest)))))
       (values keywordp keyword varname init-form-p init-form suppliedp suppliedp-name)))
    (t
     (error "Invalid ~S parameter ~S" parameter-type parform))))

(flet ((assert-parse-optional-or-key-or-aux-parameter (parform parameter-type desired-result)
	 (let ((actual-result (multiple-value-list (parse-optional-or-key-or-aux-parameter parform parameter-type))))
	   (assert (equal actual-result desired-result)))))
  (assert-parse-optional-or-key-or-aux-parameter 'a '&optional '(nil nil a nil nil nil nil))
  (assert-parse-optional-or-key-or-aux-parameter 'a '&key '(nil nil a nil nil nil nil))
  (assert-parse-optional-or-key-or-aux-parameter 'a '&aux '(nil nil a nil nil nil nil))
  (assert-parse-optional-or-key-or-aux-parameter '(a) '&optional '(nil nil a nil nil nil nil))
  (assert-parse-optional-or-key-or-aux-parameter '(a) '&key '(nil nil a nil nil nil nil))
  (assert-parse-optional-or-key-or-aux-parameter '(a) '&aux '(nil nil a nil nil nil nil))
  (assert-parse-optional-or-key-or-aux-parameter '(a 1) '&optional '(nil nil a t 1 nil nil))
  (assert-parse-optional-or-key-or-aux-parameter '(a 1) '&key '(nil nil a t 1 nil nil))
  (assert-parse-optional-or-key-or-aux-parameter '(a 1) '&aux '(nil nil a t 1 nil nil))
  (assert-parse-optional-or-key-or-aux-parameter '(a 1 ap) '&optional '(nil nil a t 1 t ap))
  (assert-parse-optional-or-key-or-aux-parameter '(a 1 ap) '&key '(nil nil a t 1 t ap))
  (assert-parse-optional-or-key-or-aux-parameter '((:b a)) '&key '(t :b a nil nil nil nil))
  (assert-parse-optional-or-key-or-aux-parameter '((:b a) 1) '&key '(t :b a t 1 nil nil))
  (assert-parse-optional-or-key-or-aux-parameter '((:b a) 1 ap) '&key '(t :b a t 1 t ap))
  (assert-parse-optional-or-key-or-aux-parameter '((t a) 1 ap) '&key '(t t a t 1 t ap))
  (assert-parse-optional-or-key-or-aux-parameter '((nil a) 1 ap) '&key '(t nil a t 1 t ap)))

;;Note that passing BLOCKS is not necessary here because PARSER-FUNCTION has captured BLOCKS, and BLOCKS is not modified in #'PARSE-LAMBDA-LIST: parsing e.g. "(BLOCK TEST (FLET ((F (&OPTIONAL (A (RETURN-FROM TEST 4))) A)) (F)))" works.
(defmethod parse-lambda-list ((parser parser) lambda-list parent &key (allow-macro-lambda-list nil))
  "Returns two values: an instance of type LLIST (representing the parsed LAMBDA-LIST) and a copy of PARSER whose lexical namespace is augmented by the variables created by the parameters in LAMBDA-LIST.
Supported lambda list keywords:
CLHS Figure 3-12. Standardized Operators that use Ordinary Lambda Lists: An ordinary lambda list can contain the lambda list keywords shown in the next figure. &allow-other-keys &key &rest &aux &optional
CLHS Figure 3-18. Lambda List Keywords used by Macro Lambda Lists: A macro lambda list can contain the lambda list keywords shown in the next figure. &allow-other-keys &environment &rest &aux &key &whole &body &optional"
  ;; TODO: come up with a scheme for this function so that it is easily extensible with other types of lambda lists, or replacable (partly or wholly) by user-supplied parsing code.
  ;; TODO: rewrite so that parsing the following is possible: Allow user-defined Lambda Lists (i.e. user-defined subclasses of 'llist), 3.4.1 Ordinary Lambda Lists, 3.4.2 Generic Function Lambda Lists, 3.4.3 Specialized Lambda Lists, 3.4.4 Macro Lambda Lists, 3.4.5 Destructuring Lambda Lists, 3.4.6 Boa Lambda Lists, 3.4.7 Defsetf Lambda Lists, 3.4.8 Deftype Lambda Lists, 3.4.9 Define-modify-macro Lambda Lists, 3.4.10 Define-method-combination Arguments Lambda Lists
  ;; "CLHS 3.4.1 Ordinary Lambda Lists" says: "An init-form can be any form. Whenever any init-form is evaluated for any parameter specifier, that form may refer to any parameter variable to the left of the specifier in which the init-form appears, including any supplied-p-parameter variables, and may rely on the fact that no other parameter variable has yet been bound (including its own parameter variable)." But luckily the order of allowed keywords is fixed for both normal lambda-lists and macro lambda-lists, and a normal lambda-list allows a subset of macro lambda-lists. Maybe I'll have to rewrite this for other lambda-lists.
  (let* ((llist-type (if allow-macro-lambda-list 'macro-llist 'ordinary-llist)) ;TODO: when making this function modular: probably allow passing LLIST-TYPE.
	 (new-llist (make-ast parser llist-type :parent parent :source lambda-list)))
    (labels ((add-parameter (varname parameter-type)
	       (let* ((new-parameter (make-ast parser parameter-type :parent new-llist :source varname))
		      (new-var (make-ast parser 'var :name varname :freep nil :definition new-parameter :declspecs nil :sites nil :source varname)))
		 (setf (parameter-var new-parameter) new-var)
		 (setf parser (augment-lexical-namespace new-var parser))
		 new-parameter))
	     (add-required-parameter (varname)
	       (let* ((new-parameter (make-ast parser 'required-parameter :parent new-llist :source varname))
		      (new-var (make-ast parser 'var :name varname :freep nil :definition new-parameter :declspecs nil :sites nil :source varname)))
		 (setf (parameter-var new-parameter) new-var)
		 (setf parser (augment-lexical-namespace new-var parser))
		 new-parameter))
	     (add-optional-parameter (varname init-form-p init-form suppliedp suppliedp-name source)
	       "If INIT-FORM is not given for the &OPTIONAL parameter, pass NIL for it. If SUPPLIEDP is non-NIL, INIT-FORM must be non-NIL as well."
	       (assert (if suppliedp (not (null init-form)) t))
	       (let* ((new-parameter (make-ast parser 'optional-parameter :parent new-llist :source source))
		      (new-var (make-ast parser 'var :name varname :freep nil :definition new-parameter :declspecs nil :sites nil :source varname)))
		 (setf (parameter-var new-parameter) new-var)
		 (let ((parsed-init-form (if init-form-p (parse parser init-form new-parameter) nil)))
		   (setf parser (augment-lexical-namespace new-var parser))
		   (setf (parameter-init new-parameter) parsed-init-form)
		   (let ((new-supplied-var (if suppliedp (make-ast parser 'var :name suppliedp-name :freep nil :definition new-parameter :declspecs nil :sites nil :source suppliedp-name) nil)))
		     (setf (parameter-suppliedp new-parameter) new-supplied-var)
		     (when new-supplied-var
		       (setf parser (augment-lexical-namespace new-supplied-var parser)))
		     new-parameter))))
	     (add-key-parameter (keywordp keyword varname init-form-p init-form suppliedp suppliedp-name source)
	       "If KEYWORDNAME or INIT-FORM is not given for the &KEY parameter, pass NIL for it. If SUPPLIEDP is non-NIL, INIT-FORM must be non-NIL as well."
	       (assert (if suppliedp (not (null init-form)) t))
	       (let* ((new-parameter (make-ast parser 'key-parameter :parent new-llist :keywordp keywordp :keyword keyword :source source))
		      (new-var (make-ast parser 'var :name varname :freep nil :definition new-parameter :declspecs nil :sites nil :source varname)))
		 (setf (parameter-var new-parameter) new-var)
		 (let ((parsed-init-form (if init-form-p (parse parser init-form new-parameter) nil)))
		   (setf parser (augment-lexical-namespace new-var parser))
		   (setf (parameter-init new-parameter) parsed-init-form)
		   (let ((new-supplied-var (if suppliedp (make-ast parser 'var :name suppliedp-name :freep nil :definition new-parameter :declspecs nil :sites nil :source suppliedp-name) nil)))
		     (setf (parameter-suppliedp new-parameter) new-supplied-var)
		     (when new-supplied-var
		       (setf parser (augment-lexical-namespace new-supplied-var parser)))
		     new-parameter))))
	     (add-aux-parameter (varname init-form-p init-form source)
	       "If INIT-FORM is not given for the &KEY parameter, pass NIL for it."
	       (let* ((new-parameter (make-ast parser 'aux-parameter :parent new-llist :source source))
		      (new-var (make-ast parser 'var :name varname :freep nil :definition new-parameter :declspecs nil :sites nil :source varname)))
		 (setf (parameter-var new-parameter) new-var)
		 (let ((parsed-init-form (if init-form-p (parse parser init-form new-parameter) nil)))
		   (setf parser (augment-lexical-namespace new-var parser))
		   (setf (parameter-init new-parameter) parsed-init-form)
		   new-parameter))))
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
		 ((eql head '&allow-other-keys) ;must be checked before comparing with CURRENT-KEYWORD
		  (assert (not (null key)) () "&ALLOW-OTHER-KEYS not allowed without preceding &KEY keyword in lambda list ~S" lambda-list)
		  (assert (null allow-other-keys) () "Only one &ALLOW-OTHER-KEYS keyword allowed in lambda list ~S" lambda-list)
		  (setf allow-other-keys t)
		  (setf current-keyword nil))
		 ((eq current-keyword '&whole) ;TODO: allow recursive lambda-list parsing, see CLHS 3.4.4.1.2 Lambda-list-directed Destructuring by Lambda Lists
		  (assert allow-macro-lambda-list () "&WHOLE only allowed in macro lambda lists")
		  (assert (null whole) () "Only one &WHOLE keyword allowed in lambda list ~S" lambda-list)
		  (assert (and (null environment) (null required) (null optional) (null rest) (null body) (null key) (null aux)) () "&WHOLE keyword must be before &ENVIRONMENT, &OPTIONAL, &REST, &BODY, &KEY, &ALLOW-OTHER-KEYS, &AUX, and required parameters in lambda list ~S" lambda-list)
		  (setf whole (add-parameter (parse-required-parameter head) 'whole-parameter))
		  (setf current-keyword nil))
		 ((eq current-keyword '&environment)
		  (assert allow-macro-lambda-list () "&ENVIRONMENT only allowed in macro lambda lists")
		  (assert (null environment) () "Only one &ENVIRONMENT keyword allowed in lambda list ~S" lambda-list)
		  (setf environment (add-parameter (parse-required-parameter head) 'environment-parameter))
		  (setf current-keyword nil))
		 ((null current-keyword)
		  (assert (and (null optional) (null rest) (null body) (null key) (null aux)) () "Required parameters must be before &OPTIONAL, &REST, &BODY, &KEY, &ALLOW-OTHER-KEYS, &AUX in lambda list ~S" lambda-list)
		  (push (cond
			  ((and (not (null head)) (not (eq head t)) (symbolp head))
			   (add-required-parameter (parse-required-parameter head)))
			  ((and allow-macro-lambda-list (listp head))
			   (multiple-value-bind (parsed-llist new-parser)
			       (parse-lambda-list parser head new-llist :allow-macro-lambda-list allow-macro-lambda-list)
			     (setf parser new-parser)
			     parsed-llist))
			  (t (error "Invalid required parameter ~S in lambda list ~S" head lambda-list)))
			required))
		 ((eq current-keyword '&optional) ;TODO: allow recursive lambda-list parsing, see CLHS 3.4.4.1.2 Lambda-list-directed Destructuring by Lambda Lists
		  (assert (and (null rest) (null body) (null key) (null aux)) () "Optional parameters must be before &REST, &BODY, &KEY, &ALLOW-OTHER-KEYS, &AUX in lambda list ~S" lambda-list)
		  (push (multiple-value-bind (keywordp keyword varname init-form-p init-form suppliedp suppliedp-name)
			    (parse-optional-or-key-or-aux-parameter head '&optional)
			  (declare (ignore keywordp keyword))
			  (add-optional-parameter varname init-form-p init-form suppliedp suppliedp-name head))
			optional))
		 ;; TODO: in macro lambda lists, allow &REST to be specified as the (CDR (LAST LAMBDA-LIST)), as in '(A &OPTIONAL B . R).
		 ((find current-keyword '(&rest &body)) ;TODO: allow recursive lambda-list parsing, see CLHS 3.4.4.1.2 Lambda-list-directed Destructuring by Lambda Lists
		  (when (eq current-keyword '&BODY) (assert allow-macro-lambda-list () "&BODY only allowed in macro lambda lists"))
		  (assert (and (null rest) (null body)) () "Only either one &REST or one &BODY keyword allowed in lambda list ~S" lambda-list)
		  (assert (and (null key) (null aux)) () "&REST or &BODY keyword must be before &KEY, &AUX in lambda list ~S" lambda-list)
		  (let ((parsed-head (add-parameter (parse-required-parameter head) (if (eq current-keyword '&rest) 'rest-parameter 'body-parameter))))
		    (ecase current-keyword ((&rest) (setf rest parsed-head)) ((&body) (setf body parsed-head))))
		  (setf current-keyword nil))
		 ((eq current-keyword '&key) ;TODO: allow recursive lambda-list parsing, see CLHS 3.4.4.1.2 Lambda-list-directed Destructuring by Lambda Lists
		  (assert (null aux) () "&KEY keyword must be before &AUX in lambda list ~S" lambda-list)
		  (push (multiple-value-bind (keywordp keyword varname init-form-p init-form suppliedp suppliedp-name)
			    (parse-optional-or-key-or-aux-parameter head '&key)
			  (add-key-parameter keywordp keyword varname init-form-p init-form suppliedp suppliedp-name head))
			key))
		 ((eq current-keyword '&aux)
		  (push (multiple-value-bind (keywordp keyword varname init-form-p init-form suppliedp suppliedp-name)
			    (parse-optional-or-key-or-aux-parameter head '&aux)
			  (declare (ignore keywordp keyword suppliedp suppliedp-name))
			  (add-aux-parameter varname init-form-p init-form head))
			aux))
		 (t (error "Unhandled parameter ~S in lambda-list ~S" head lambda-list))))
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
    (values new-llist parser)))

(defun parse-ordinary-lambda-list (parser lambda-list parent)
  (parse-lambda-list parser lambda-list parent :allow-macro-lambda-list nil))

(defun parse-macro-lambda-list (parser lambda-list parent)
  (parse-lambda-list parser lambda-list parent :allow-macro-lambda-list t))

;;TODO: add some test cases, e.g. (let ((lexical-namespace (make-empty-lexical-namespace)) (free-namespace (make-empty-free-namespace))) (parse-lambda-list '(a &optional (b 1 bp) &rest r &key ((:cdddd c) 1 cp) &aux d) lexical-namespace free-namespace nil (lambda (form &rest r) (declare (ignore r)) form) :allow-macro-lambda-list nil))

(defmethod print-object ((object required-parameter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (parameter-var object))))
(defmethod print-object ((object optional-parameter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cond
      ((and (null (parameter-init object)) (null (parameter-suppliedp object)))
       (format stream "~S" (parameter-var object)))
      ((null (parameter-suppliedp object))
       (format stream "(~S ~S)" (parameter-var object) (parameter-init object)))
      (t
       (format stream "(~S ~S ~S)" (parameter-var object) (parameter-init object) (parameter-suppliedp object))))
    ))
(defmethod print-object ((object key-parameter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(")
    (if (parameter-keywordp object)
	(format stream "(~S ~S) " (parameter-keyword object) (parameter-var object))
	(format stream "~S " (parameter-var object)))
    (cond
      ((null (parameter-suppliedp object))
       (format stream "~S" (parameter-init object)))
      (t
       (format stream "~S ~S" (parameter-init object) (parameter-suppliedp object))))
    (format stream ")")))
(defmethod print-object ((object ordinary-llist) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (append
			 (llist-required object)
			 (let ((it (llist-optional object))) (when it (cons '&optional it)))
			 (let ((it (llist-rest object))) (when it (list '&rest it)))
			 (let ((it (llist-key object))) (when it (cons '&key it)))
			 (let ((it (llist-allow-other-keys object))) (when it (list '&allow-other-keys)))
			 (let ((it (llist-aux object))) (when it (cons '&aux it)))))))
(defmethod print-object ((object macro-llist) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (append
			 (let ((it (llist-whole object))) (when it (list '&whole it)))
			 (let ((it (llist-environment object))) (when it (list '&environment it)))
			 (llist-required object)
			 (let ((it (llist-optional object))) (when it (cons '&optional it)))
			 (let ((it (llist-rest object))) (when it (list '&rest it)))
			 (let ((it (llist-body object))) (when it (list '&body it)))
			 (let ((it (llist-key object))) (when it (cons '&key it)))
			 (let ((it (llist-allow-other-keys object))) (when it (list '&allow-other-keys)))
			 (let ((it (llist-aux object))) (when it (cons '&aux it)))))))

;;;; FORMS and Utility Functions

(defclass form ()
  ((parent :initarg :parent :accessor form-parent)
   (source :initarg :source :accessor form-source :documentation "The source of the form.")
   (user :initform nil :initarg :user :accessor user))
  (:documentation "A cons form as described in 'CLHS 3.1.2.1.2 Conses as Forms'"))
(defclass object-form (form)
  ((object :initarg :object :accessor form-object)) ;TODO: add something like ":type (or number string vector pathname ...)" when I know what types of objects are self-evaluating objects. CLHS 3.1.2.1.3 doesn't seem to have a complete list.
  (:documentation "A self-evaluating object as described in 'CLHS 3.1.2.1.3 Self-Evaluating Objects'"))
(defclass var-reading (form)
  ((var :initarg :var :accessor form-var :documentation "The VAR being read"))
  (:documentation "VAR is being accessed for reading. CLHS 3.1.2.1.1 Symbols as Forms. Note that this form is not to be used for variable declarations. This is a subclass of FORM, so that all elements of a correct Lisp form are a type of FORM."))
(defclass body-mixin ()
  ((body :initarg :body :accessor form-body :type list :documentation "list of FORMs"))
  (:documentation "Note: objects of this type must never be created, only subtypes of this type."))
(defclass special-form (form)
  ())
(defclass function-form (special-form)
  ((object :initarg :object :accessor form-object :type (or fun lambda-form))))
(defclass progn-form (special-form body-mixin)
  ())
(defclass binding ()
  ((parent :initarg :parent :accessor form-parent :documentation "the LET-FORM, LET*-FORM, FLET-FORM, LABELS-FORM, SYMBOL-MACROLET-FORM, or MACROLET-FORM in which the binding is defined.")
   (source :initarg :source :accessor form-source :type list :documentation "The source of the binding.")
   (sym :initarg :sym :accessor form-sym :type sym) ;also accessible as #'FORM-VAR and #'FORM-FUN for VAR-BINDINGs and FUN-BINDINGs, respectively
   (user :initform nil :initarg :user :accessor user)))
(defclass var-binding (binding)
  ((value :initarg :value :accessor form-value :documentation "Either NIL if not given, or the form initializing the variable, or the expansion form of the symbol macro." :type (or null form t))))
(defclass bindings-mixin ()
  ((bindings :initarg :bindings :accessor form-bindings :type list :documentation "list of VAR-BINDINGs, or FUN-BINDINGs")
   (declspecs :initarg :declspecs :accessor form-declspecs :type list :documentation "list of DECLSPECs"))
  (:documentation "Note: objects of this type must never be created, only subtypes of this type."))
(defclass var-bindings-mixin (bindings-mixin)
  () (:documentation "Note: objects of this type must never be created, only subtypes of this type."))
(defclass fun-bindings-mixin (bindings-mixin)
  () (:documentation "Note: objects of this type must never be created, only subtypes of this type."))
(defclass let-form (special-form var-bindings-mixin body-mixin)
  ())
(defclass let*-form (special-form var-bindings-mixin body-mixin)
  ())
(defclass functiondef (body-mixin)
  ((parent :initarg :parent :accessor form-parent)
   (llist :initarg :llist :accessor form-llist :type llist)
   (declspecs :initarg :declspecs :accessor form-declspecs :type list)
   (documentation :initarg :documentation :accessor form-documentation :type (or nil string)))
  (:documentation "The definition of a function or macro function without name. The name is provided by class BINDING."))
(defclass block-naming-mixin ()
  ((blo :initarg :blo :accessor form-blo :type blo))
  (:documentation "Note: objects of this type must never be created, only subtypes of this type."))
(defclass block-form (special-form block-naming-mixin body-mixin)
  ())
(defclass fun-binding (binding block-naming-mixin functiondef)
  ())
(defclass flet-form (special-form fun-bindings-mixin body-mixin)
  ())
(defclass labels-form (special-form fun-bindings-mixin body-mixin)
  ())
(defclass lambda-form (special-form functiondef)
  ())
(defclass return-from-form (special-form)
  ((blo :initarg :blo :accessor form-blo :type blo)
   (value :initarg :value :accessor form-value :type (or null form))))
(defclass locally-form (special-form body-mixin)
  ((declspecs :initarg :declspecs :accessor form-declspecs :type list)))
(defclass the-form (special-form)
  ((type :initarg :type :accessor form-type :type (or symbol list))
   (value :initarg :value :accessor form-value :type form)))
(defclass if-form (special-form)
  ((test :initarg :test :accessor form-test :type form)
   (then :initarg :then :accessor form-then :type form)
   (else :initarg :else :accessor form-else :type (or null form))))
(defclass var-writing ()
  ((parent :initarg :parent :accessor form-parent)
   (source :initarg :source :accessor form-source :documentation "The source of the VAR-WRITING.")
   (var :initarg :var :accessor form-var :documentation "The VAR being written")
   (value :initarg :value :accessor form-value :documentation "The value being written to VAR")
   (user :initform nil :initarg :user :accessor user))
  (:documentation "VAR is being accessed for writing. CLHS 3.1.2.1.1 Symbols as Forms"))
(defclass setq-form (special-form)
  ((vars :initarg :vars :accessor form-vars :type list :documentation "list of VAR-WRITINGs")))
(defclass catch-form (special-form body-mixin)
  ((tag :initarg :tag :accessor form-tag :type form)))
(defclass throw-form (special-form)
  ((tag :initarg :tag :accessor form-tag :type form)
   (value :initarg :value :accessor form-value :type form)))
(defclass eval-when-form (special-form body-mixin)
  ((situations :initarg :situations :accessor form-situations :type list)))
(defclass load-time-value-form (special-form)
  ((value :initarg :value :accessor form-value :type list)
   (readonly :initarg :readonly :accessor form-readonly :type boolean)))
(defclass quote-form (special-form)
  ((object :initarg :object :accessor form-object :type t)))
(defclass multiple-value-call-form (special-form body-mixin)
  ((function :initarg :function :accessor form-function :type form))) ;note that this slot is not called FUN, because slots named FUN in other FORM-classes mean "of type FUN".
(defclass multiple-value-prog1-form (special-form body-mixin)
  ((values :initarg :values :accessor form-values :type form)))
(defclass progv-form (special-form body-mixin)
  ((symbols :initarg :symbols :accessor form-symbols :type form)
   (values :initarg :values :accessor form-values :type form)))
(defclass unwind-protect-form (special-form body-mixin)
  ((protected :initarg :protected :accessor form-protected :type form)))
(defclass application-form (form)
  ((fun :initarg :fun :accessor form-fun :type (or fun lambda-form))
   (arguments :initarg :arguments :accessor form-arguments :type list :documentation "list of FORMs")
   (recursivep :initarg :recursivep :accessor form-recursivep :type boolean :documentation "T if the call is inside the function called, NIL otherwise.")))
;; Probably FREENAMESPACE should not be a slot in MACROAPPLICATION-FORM, since CLHS says that the lexical environment is saved, but says nothing about the free environment: CLHS Glossary "environment parameter n. A parameter in a defining form f for which there is no corresponding argument; instead, this parameter receives as its value an environment object which corresponds to the lexical environment in which the defining form f appeared."
(defclass macroapplication-form (application-form)
  ((lexicalnamespace :initarg :lexicalnamespace :accessor form-lexicalnamespace :type lexicalnamespace :documentation "The lexical namespace at the macro application form")
   (freenamespace :initarg :freenamespace :accessor form-freenamespace :type freenamespace :documentation "The free namespace at the macro application form")))
(defclass symbol-macrolet-form (special-form var-bindings-mixin body-mixin)
  ())
(defclass macrolet-form (special-form fun-bindings-mixin body-mixin)
  ())
(defclass tagbody-form (special-form body-mixin)
  ((body :initarg :body :accessor form-body :type list :documentation "list of elements of type (OR FORM TAGPOINT)") ;do not inherit from BODY-MIXIN, otherwise slot DOCUMENTATION would be allowed here
   (tags :initarg :tags :accessor form-tags :type list :documentation "The list of all tags defined in the TAGBODY.")))
(defclass tagpoint (form)
  ((tag :initarg :tag :accessor form-tag :documentation "The TAG"))
  (:documentation "A TAG inside a TAGBODY-FORM. We encapsulate TAG inside a TAGPOINT instance, and make it a subclass of FORM, so that all elements of a correct Lisp form are a type of FORM, and so that it has slots PARENT and USER."))
(defclass go-form (special-form)
  ((tag :initarg :tag :accessor form-tag :type tag)))

(defmethod print-object ((object object-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (form-object object))))
(defmethod print-object ((object var-reading) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (form-var object))))
(defmethod print-object ((object function-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (form-object object))))
(defmethod print-object ((object progn-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "body:~S" (form-body object))))
(defmethod print-object ((object var-binding) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (form-var object))
    (when (form-value object)
      (format stream " ~S" (form-value object)))))
(defun format-body (object declspecsp documentationp)
  (if (and documentationp (form-documentation object))
      (if (or (null declspecsp) (null (form-declspecs object)))
	  (format nil "~S body:~S" (form-documentation object) (form-body object))
	  (format nil "~S ~S body:~S" (form-documentation object) (cons 'declare (form-declspecs object)) (form-body object)))
      (if (or (null declspecsp) (null (form-declspecs object)))
	  (format nil "body:~S" (form-body object))
	  (format nil "~S body:~S" (cons 'declare (form-declspecs object)) (form-body object)))))
(defmethod print-object ((object bindings-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if *print-detailed-walker-objects*
	(format stream "BINDINGS:~S ~A" (form-bindings object) (format-body object t nil))
	(format stream "body:~S" (form-body object)))))
(defmethod print-object ((object fun-binding) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if *print-detailed-walker-objects*
	(format stream "~S ~S ~A" (form-fun object) (if (slot-boundp object 'llist) (form-llist object) 'llist-unbound) (if (slot-boundp object 'body) (format-body object t t) 'body-unbound))
	(format stream "~S" (form-fun object)))))
(defmethod print-object ((object lambda-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~A" (form-llist object) (format-body object t t))))
(defmethod print-object ((object block-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S body:~S" (form-blo object) (form-body object))))
(defmethod print-object ((object return-from-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-blo object) (form-value object))))
(defmethod print-object ((object locally-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (format-body object t nil))))
(defmethod print-object ((object the-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-type object) (form-value object))))
(defmethod print-object ((object if-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if (null (form-else object))
	(format stream "~S ~S" (form-test object) (form-then object))
	(format stream "~S ~S ~S" (form-test object) (form-then object) (form-else object)))))
(defmethod print-object ((object var-writing) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-var object) (form-value object))))
(defmethod print-object ((object setq-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "vars:~S" (form-vars object))))
(defmethod print-object ((object catch-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S body:~S" (form-tag object) (form-body object))))
(defmethod print-object ((object throw-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-tag object) (form-value object))))
(defmethod print-object ((object eval-when-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S body:~S" (form-situations object) (form-body object))))
(defmethod print-object ((object load-time-value-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-value object) (form-readonly object))))
(defmethod print-object ((object quote-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (form-object object))))
(defmethod print-object ((object multiple-value-call-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S body:~S" (form-function object) (form-body object))))
(defmethod print-object ((object multiple-value-prog1-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S body:~S" (form-values object) (form-body object))))
(defmethod print-object ((object progv-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S body:~S" (form-symbols object) (form-values object) (form-body object))))
(defmethod print-object ((object unwind-protect-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S body:~S" (form-protected object) (form-body object))))
(defmethod print-object ((object application-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (form-fun object))
    (loop for arg in (form-arguments object) do
	 (format stream " ~S" arg))))
(defmethod print-object ((object macroapplication-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (form-fun object))
    (loop for arg in (form-arguments object) do
	 (format stream " ~S" arg))
    (format stream " LEXICALNAMESPACE:~S" (form-lexicalnamespace object))
    ;; TODO: FIXME: the following should not override user-settings: maybe add an additional setting *PRINT-DETAILED-WALKER-OBJECTS*==:DEFAULT?
    (let ((*print-detailed-walker-objects* nil))
      (format stream " ~S" (form-freenamespace object)))))
(defmethod print-object ((object tagbody-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "body:~S" (form-body object))))
(defmethod print-object ((object tagpoint) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (form-tag object))))
(defmethod print-object ((object go-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (form-tag object))))

(defun function-object (ast)
  "Return the function object of AST without a FUNCTION-FORM wrapper."
  (cond
    ((typep ast 'lambda-form)
     ast)
    ((and (typep ast 'function-form) (typep (form-object ast) 'lambda-form))
     (form-object ast))
    ((and (typep ast 'function-form) (typep (form-object ast) 'fun))
     (form-object ast))
    ((typep ast 'fun)
     ast)
    (t
     (error "unknown function object ~S" ast))))

(defun form-body-1 (ast)
  (first (form-body ast)))
(defun form-body-2 (ast)
  (second (form-body ast)))
(defun form-body-3 (ast)
  (third (form-body ast)))
(defun form-body-4 (ast)
  (fourth (form-body ast)))
(defun form-body-5 (ast)
  (fifth (form-body ast)))
(defun form-body-6 (ast)
  (sixth (form-body ast)))
(defun form-body-7 (ast)
  (seventh (form-body ast)))
(defun form-body-last (ast)
  (car (last (form-body ast))))
(defun (setf form-body-1) (value ast)
  (setf (first (form-body ast)) value))
(defun (setf form-body-2) (value ast)
  (setf (second (form-body ast)) value))
(defun (setf form-body-3) (value ast)
  (setf (third (form-body ast)) value))
(defun (setf form-body-4) (value ast)
  (setf (fourth (form-body ast)) value))
(defun (setf form-body-5) (value ast)
  (setf (fifth (form-body ast)) value))
(defun (setf form-body-6) (value ast)
  (setf (sixth (form-body ast)) value))
(defun (setf form-body-7) (value ast)
  (setf (seventh (form-body ast)) value))

(defun form-binding-1 (ast)
  (first (form-bindings ast)))
(defun form-binding-2 (ast)
  (second (form-bindings ast)))
(defun form-binding-3 (ast)
  (third (form-bindings ast)))
(defun form-binding-4 (ast)
  (fourth (form-bindings ast)))
(defun form-binding-5 (ast)
  (fifth (form-bindings ast)))
(defun form-binding-6 (ast)
  (sixth (form-bindings ast)))
(defun form-binding-7 (ast)
  (seventh (form-bindings ast)))
(defun (setf form-binding-1) (value ast)
  (setf (first (form-bindings ast)) value))
(defun (setf form-binding-2) (value ast)
  (setf (second (form-bindings ast)) value))
(defun (setf form-binding-3) (value ast)
  (setf (third (form-bindings ast)) value))
(defun (setf form-binding-4) (value ast)
  (setf (fourth (form-bindings ast)) value))
(defun (setf form-binding-5) (value ast)
  (setf (fifth (form-bindings ast)) value))
(defun (setf form-binding-6) (value ast)
  (setf (sixth (form-bindings ast)) value))
(defun (setf form-binding-7) (value ast)
  (setf (seventh (form-bindings ast)) value))

(defun form-argument-1 (ast)
  (first (form-arguments ast)))
(defun form-argument-2 (ast)
  (second (form-arguments ast)))
(defun form-argument-3 (ast)
  (third (form-arguments ast)))
(defun form-argument-4 (ast)
  (fourth (form-arguments ast)))
(defun form-argument-5 (ast)
  (fifth (form-arguments ast)))
(defun form-argument-6 (ast)
  (sixth (form-arguments ast)))
(defun form-argument-7 (ast)
  (seventh (form-arguments ast)))
(defun (setf form-argument-1) (value ast)
  (setf (first (form-arguments ast)) value))
(defun (setf form-argument-2) (value ast)
  (setf (second (form-arguments ast)) value))
(defun (setf form-argument-3) (value ast)
  (setf (third (form-arguments ast)) value))
(defun (setf form-argument-4) (value ast)
  (setf (fourth (form-arguments ast)) value))
(defun (setf form-argument-5) (value ast)
  (setf (fifth (form-arguments ast)) value))
(defun (setf form-argument-6) (value ast)
  (setf (sixth (form-arguments ast)) value))
(defun (setf form-argument-7) (value ast)
  (setf (seventh (form-arguments ast)) value))

;; make slot SYM of BINDINGsa accessible under #'FORM-VAR and #'FORM-FUN.
(defmethod form-var ((ast var-binding))
  (form-sym ast))
(defmethod (setf form-var) (value (ast var-binding))
  (setf (form-sym ast) value))
(defmethod form-fun ((ast fun-binding))
  (form-sym ast))
(defmethod (setf form-fun) (value (ast fun-binding))
  (setf (form-sym ast) value))

(defmethod make-nil ((parser parser) parent &key (user nil))
  (make-ast parser 'walker:object-form  :parent parent :object nil :user user :source nil))
(defmethod make-object ((parser parser) object parent &key (user nil))
  (make-ast parser 'walker:object-form :parent parent :object object :user user :source object))

;;;; END OF FORMs and Utility Functions

(defmethod parse-and-set-functiondef ((parser parser) form parse-lambda-list-function block-name current-functiondef)
  "Parse FORM, which must be of the form (LAMBDA-LIST &BODY BODY) and set the slots of the CURRENT-FUNCTIONDEF-object to the parsed values.
PARSE-LAMBDA-LIST-FUNCTION must be a function that accepts (PARSER LAMBDA-LIST CURRENT-FUNCTIONDEF), parses the LAMBDA-LIST and returns two values: 1. the new LLIST, i.e. an instance of (a subclass of) class LLIST. 2. the lexical namespace of PARSER augmented by the variables in LAMBDA-LIST.
Side-effects: Creates yet unknown free variables and functions and adds them to the free namespace of PARSER."
  (declare (optimize (debug 3)))
  (flet ((split-lambda-list-and-body (form)
	   (assert (and (consp form) (listp (car form))) () "Invalid lambda-list ~S" form)
	   (let ((lambda-list (car form))
		 (body (cdr form)))
	     (assert (listp body) () "Invalid body ~S; must be a list" body)
	     (values lambda-list body))))
    (multiple-value-bind (lambda-list body) (split-lambda-list-and-body form)
      (multiple-value-bind (new-llist parser-in-functiondef)
	  (funcall parse-lambda-list-function parser lambda-list current-functiondef)
	(setf (form-llist current-functiondef) new-llist)
	(unless (null block-name)
	  (let* ((blo (make-ast parser 'blo :name block-name :freep nil :definition current-functiondef :sites nil :source block-name)))
	    (setf (form-blo current-functiondef) blo)
	    (setf parser-in-functiondef (augment-lexical-namespace blo parser-in-functiondef))))
	(multiple-value-bind (body parsed-declspecs parsed-documentation)
	    (parse-declaration-and-documentation-in-body parser-in-functiondef body current-functiondef)
	  (setf (form-declspecs current-functiondef) parsed-declspecs)
	  (setf (form-documentation current-functiondef) parsed-documentation)
	  (assert (proper-list-p body) () "Not a proper list: ~S" body)
	  (let ((parsed-body (loop for form in body collect (parse parser-in-functiondef form current-functiondef))))
	    (setf (form-body current-functiondef) parsed-body))))
      current-functiondef)))

(defun is-recursive (fun parent)
  "Checks if PARENT is a FUNCTIONDEF defining FUN or checks for FUN in the parent path of PARENT otherwise."
  (cond
    ((null parent)
     nil)
    (t
     (if (and (typep parent 'fun-binding) (eq (form-fun parent) fun))
	 t
	 (is-recursive fun (form-parent parent))))))

(defun ast-inside-ast-p (inner-ast outer-ast &optional (eql-means-inside nil))
  "Returns non-NIL if and only if INNER-AST is inside of OUTER-AST.
If EQL-MEANS-INSIDE is non-NIL, then returns T if (EQL INNER-AST OUTER-AST)."
  (cond
    ((null inner-ast)
     nil)
    ((eql (if eql-means-inside inner-ast (form-parent inner-ast)) outer-ast)
     t)
    (t
     (ast-inside-ast-p (form-parent inner-ast) outer-ast eql-means-inside))))

;; TODO: FIXME: distinguish in all ASSERTs and ERRORs (in all functions, especially the #'PARSE functions) between errors that are recognized as syntax errors because the input form is impossible in Common Lisp, and errors that are due to me having made programming mistakes.
;; TODO: FIXME: Must handle arbitrary nonsense input gracefully. (see comment near #'PROPER-LIST-P.)

(defmethod parse-body ((parser parser) body current)
  "Parse the forms in BODY."
  (assert (proper-list-p body) () "Body is not a proper list: ~S" body)
  (loop for form in body collect
       (parse parser form current)))

(defmethod parse ((parser parser) (form (eql nil)) parent)
  (make-ast parser 'object-form :parent parent :source form :object form))

(defmethod parse ((parser parser) (form (eql t)) parent)
  (make-ast parser 'object-form :parent parent :source form :object form))

(defmethod parse ((parser parser) (form symbol) parent)
  (let* ((var (namespace-lookup/create 'var form parser))
	 (read-var (make-ast parser 'var-reading :parent parent :source form :var var)))
    (push read-var (nso-sites var))
    read-var))

(defmethod parse ((parser parser) (form cons) parent)
  (parse-form parser (car form) (cdr form) parent form))

(defmethod parse ((parser parser) form parent)
  (assert (atom form))
  (make-ast parser 'object-form :parent parent :source form :object form))

#|
;; The following types are atoms: taken from CLHS 4.2.2 first star.
array
number
character
hash-table
readtable
package
pathname
stream
random-state
condition
restart
|#

(defmethod parse-form ((parser parser) (head (eql 'function)) rest parent source)
  (assert (and (consp rest) (null (cdr rest))) () "Invalid FUNCTION-form ~S" (cons head rest))
  (let* ((name (car rest))
	 (current (make-ast parser 'function-form :parent parent :source source)))
    (setf (form-object current) (cond
				  ((valid-function-name-p name)
				   (let ((fun (namespace-lookup/create 'fun name parser)))
				     (push current (nso-sites fun))
				     fun))
				  ((and (consp name) (eql (car name) 'lambda))
				   (parse parser name current))
				  (t (error "Invalid FUNCTION-form ~S" source))))
    current))

(defmethod parse-form ((parser parser) (head (eql 'progn)) rest parent source)
  (let* ((current (make-ast parser 'progn-form :parent parent :source source :body nil))
	 (body rest)
	 (parsed-body (parse-body parser body current)))
    (setf (form-body current) parsed-body)
    current))

(defmethod parse-var-binding ((parser parser) head definition parent)
  (ecase head
    ((let let*)
     (assert (or (symbolp definition) (and (consp definition) (symbolp (car definition)) (or (null (cdr definition)) (null (cddr definition))))) () "cannot parse definition in ~S-form:~%~S" head definition))
    ((symbol-macrolet)
     (assert (and (consp definition) (symbolp (car definition)) (not (null (car definition))) (not (null (cdr definition))) (null (cddr definition))) () "cannot parse definition in ~S-form:~%~S" head definition)))
  (let* ((name (if (consp definition) (car definition) definition))
	 (value-form-present-p (consp definition))
	 (value-form (if (consp definition) (cadr definition) nil))
	 (binding (make-ast parser 'var-binding :parent parent :source definition))
	 (parsed-value (ecase head ((let* let) (if value-form-present-p (parse parser value-form binding) nil)) ((symbol-macrolet) value-form)))
	 (macrop (ecase head ((let* let) nil) ((symbol-macrolet) t)))
	 (var (make-ast parser 'var :name name :freep nil :definition binding :declspecs nil :macrop macrop :sites nil :source name)))
    (setf (form-var binding) var
	  (form-value binding) parsed-value)
    binding))

(defmethod parse-fun-binding ((parser parser) head definition parent)
  (assert (and (consp definition) (valid-function-name-p (car definition)) (not (null (cdr definition)))) () "cannot parse definition in ~S-form:~%~S" head definition)
  (multiple-value-bind (fun-type name) (valid-function-name-p (car definition))
    (assert (or (not (eq head 'macrolet)) (symbolp name)) () "macro function name in ~S-definition must be a SYMBOL, but is ~S" head (car definition))
    (let* ((body-form (cdr definition))
	   (block-name (ecase fun-type ((fun) name) ((setf-fun) (cadr name)))) ;CLHS Glossary "function block name" defines "If the function name is a list whose car is setf and whose cadr is a symbol, its function block name is the symbol that is the cadr of the function name."
	   (macrop (ecase head ((flet labels) nil) ((macrolet) t)))
	   (parse-lambda-list-function (if macrop #'parse-macro-lambda-list #'parse-ordinary-lambda-list))
	   (fun (if (eq head 'labels)
		    (namespace-lookup 'fun name (parser-lexical-namespace parser))
		    (make-ast parser 'fun :name name :freep nil :declspecs nil :macrop macrop :sites nil :source definition))) ;for a LABELS-form, the LEXICAL-NAMESPACE already contains a fake FUN, and we want to have it when parsing the body.
	   (binding (make-ast parser 'fun-binding :parent parent :source definition :sym fun))) ;BLO is set by #'PARSE-AND-SET-FUNCTIONDEF
      (setf (nso-definition fun) binding)
      (parse-and-set-functiondef parser body-form parse-lambda-list-function block-name binding)
      binding)))

;; TODO: FIXME: maybe I have to handle something specially in FLET and LABELS. The CLHS on FLET and LABELS says: "Also, within the scope of flet, global setf expander definitions of the function-name defined by flet do not apply. Note that this applies to (defsetf f ...), not (defmethod (setf f) ...)." What does that mean?
(defun parse-let*-flet-labels-symbol-macrolet-macrolet (parser head rest parent source)
  (assert (and (consp rest) (listp (car rest))) () "cannot parse ~S-form:~%~S" head (cons head rest))
  (let* ((definitions (car rest))
	 (body (cdr rest))
	 (form-type (ecase head ((let) 'let-form) ((let*) 'let*-form) ((flet) 'flet-form) ((labels) 'labels-form) ((symbol-macrolet) 'symbol-macrolet-form) ((macrolet) 'macrolet-form)))
	 (current (make-ast parser form-type :source (cons head rest) :parent parent :source source :body nil))) ;:BINDINGS and :DECLSPECS are defined below
    (assert (proper-list-p definitions) () "Not a proper list: ~S" definitions)
    (multiple-value-bind (parsed-bindings new-parser)
	(let ((parse-value-function (ecase head ((let let* symbol-macrolet) #'parse-var-binding) ((flet labels macrolet) #'parse-fun-binding))))
	  (cond
	    ((find head '(let flet symbol-macrolet macrolet))
	     (let* ((parsed-bindings (loop for def in definitions collect
					  (funcall parse-value-function parser head def current)))
		    (parsed-syms (loop for binding in parsed-bindings collect
				      (form-sym binding)))
		    (new-parser parser))
	       (loop for fun in parsed-syms do
		    (setf new-parser (augment-lexical-namespace fun new-parser)))
	       (values parsed-bindings new-parser)))
	    ((eq head 'let*)
	     (let* ((new-parser parser)
		    (parsed-bindings (loop for def in definitions collect
					  (let* ((parsed-binding (funcall parse-value-function new-parser head def current))
						 (parsed-var (form-var parsed-binding)))
					    (setf new-parser (augment-lexical-namespace parsed-var new-parser))
					    parsed-binding))))
	       (values parsed-bindings new-parser)))
	    ((eq head 'labels)
	     (loop for def in definitions do
		  (assert (and (consp def) (valid-function-name-p (car def)) (not (null (cdr def)))) () "cannot parse definition in ~S-form:~%~S" head def))
	     (let* ((fake-funs (loop for def in definitions collect
				    (let ((name (nth-value 1 (valid-function-name-p (car def)))))
				      (make-ast parser 'fun :name name :freep nil :declspecs nil :macrop nil :sites nil :source def)))) ;:DEFINITION will be set in #'PARSE-FUN-BINDING
		    (new-parser (let ((parser parser))
				  (loop for fun in fake-funs do
				       (setf parser (augment-lexical-namespace fun parser)))
				  parser))
		    (parsed-bindings (loop for def in definitions for fun in fake-funs collect
					  (let ((parsed-binding (funcall parse-value-function new-parser head def current)))
					    (setf (nso-definition fun) parsed-binding)
					    (setf (form-fun parsed-binding) fun)
					    parsed-binding))))
	       (values parsed-bindings new-parser)))
	    (t (error "unknown HEAD"))))
      (multiple-value-bind (body parsed-declspecs)
	  (parse-declaration-in-body new-parser body current)
	(setf (form-bindings current) parsed-bindings)
	(setf (form-declspecs current) parsed-declspecs)
	(setf (form-body current) (parse-body new-parser body current))
	current))))

(defmethod parse-form ((parser parser) (head (eql 'let)) rest parent source)
  (parse-let*-flet-labels-symbol-macrolet-macrolet parser head rest parent source))
(defmethod parse-form ((parser parser) (head (eql 'let*)) rest parent source)
  (parse-let*-flet-labels-symbol-macrolet-macrolet parser head rest parent source))
(defmethod parse-form ((parser parser) (head (eql 'flet)) rest parent source)
  (parse-let*-flet-labels-symbol-macrolet-macrolet parser head rest parent source))
(defmethod parse-form ((parser parser) (head (eql 'labels)) rest parent source)
  (parse-let*-flet-labels-symbol-macrolet-macrolet parser head rest parent source))
(defmethod parse-form ((parser parser) (head (eql 'symbol-macrolet)) rest parent source)
  (parse-let*-flet-labels-symbol-macrolet-macrolet parser head rest parent source))
(defmethod parse-form ((parser parser) (head (eql 'macrolet)) rest parent source)
  (parse-let*-flet-labels-symbol-macrolet-macrolet parser head rest parent source))

(defmethod parse-form ((parser parser) (head (eql 'lambda)) rest parent source)
  (let* ((lambda-list-and-body rest)
	 (current (make-ast parser 'lambda-form :parent parent :source source)))
    (parse-and-set-functiondef parser lambda-list-and-body #'parse-ordinary-lambda-list nil current)
    current))

(defmethod parse-form ((parser parser) (head (eql 'block)) rest parent source)
  (assert (and (consp rest) (symbolp (car rest)) (listp (cdr rest))) () "Cannot parse BLOCK-form ~S" (cons head rest))
  (let* ((name (car rest))
	 (body (cdr rest))
	 (blo (make-ast parser 'blo :name name :freep nil :sites nil :source name))
	 (current (make-ast parser 'block-form :parent parent :source source :blo blo))
	 (parsed-body (parse-body (augment-lexical-namespace blo parser) body current)))
    (setf (nso-definition blo) current)
    (setf (form-body current) parsed-body)
    current))

(defmethod parse-form ((parser parser) (head (eql 'return-from)) rest parent source)
  (assert (and (consp rest) (symbolp (car rest)) (or (null (cdr rest)) (and (consp (cdr rest)) (null (cddr rest))))) () "Cannot parse RETURN-FROM-form ~S" (cons head rest))
  (let* ((name (car rest))
	 (value-form-p (not (null (cdr rest))))
	 (value-form (cadr rest))
	 (blo (namespace-lookup/create 'blo name parser))
	 (current (make-ast parser 'return-from-form :parent parent :source source :blo blo))
	 (parsed-value (if value-form-p (parse parser value-form current) nil)))
    (setf (form-value current) parsed-value)
    (push current (nso-sites blo))
    current))

(defmethod parse-form ((parser parser) (head (eql 'locally)) rest parent source)
  (assert (and (consp rest) (consp (car rest))) () "Cannot parse LOCALLY-form ~S" (cons head rest))
  (let ((body rest)
	(current (make-ast parser 'locally-form :parent parent :source source)))
    (multiple-value-bind (body parsed-declspecs)
	(parse-declaration-in-body parser body current)
      (setf (form-declspecs current) parsed-declspecs)
      (setf (form-body current) (parse-body parser body current)))
    current))

(defmethod parse-form ((parser parser) (head (eql 'the)) rest parent source)
  (assert (and (consp rest) (or (symbolp (car rest)) (consp (car rest))) (consp (cdr rest)) (null (cddr rest))) () "Cannot parse THE-form ~S" (cons head rest))
  (let* ((value-type-form (car rest))
	 (value-form (cadr rest))
	 (current (make-ast parser 'the-form :parent parent :source source :type value-type-form))
	 (parsed-value (parse parser value-form current)))
    (setf (form-value current) parsed-value)
    current))

(defmethod parse-form ((parser parser) (head (eql 'if)) rest parent source)
  (assert (and (consp rest) (consp (cdr rest)) (or (null (cddr rest)) (and (consp (cddr rest)) (null (cdddr rest))))) () "Cannot parse IF-form ~S" (cons head rest))
  (let* ((test-form (car rest))
	 (then-form (cadr rest))
	 (else-present (not (null (cddr rest))))
	 (else-form (if else-present (caddr rest) nil))
	 (current (make-ast parser 'if-form :parent parent :source source))
	 (parsed-test (parse parser test-form current))
	 (parsed-then (parse parser then-form current))
	 (parsed-else (if else-present (parse parser else-form current) nil)))
    (setf (form-test current) parsed-test (form-then current) parsed-then (form-else current) parsed-else)
    current))

(defmethod parse-form ((parser parser) (head (eql 'setq)) rest parent source)
  (let ((vars nil)
	(current (make-ast parser 'setq-form :parent parent :source source)))
    (loop do
	 (when (null rest) (return))
	 (assert (and (consp rest) (symbolp (car rest)) (consp (cdr rest))) () "Cannot parse SETQ-form part ~S" rest)
	 (let* ((name (car rest))
		(value-form (cadr rest))
		(var (namespace-lookup/create 'var name parser))
		(parsed-value (parse parser value-form current))
		(write-var (make-ast parser 'var-writing :parent current :var var :value parsed-value :source (subseq rest 0 2))))
	   (push write-var vars)
	   (push write-var (nso-sites var)))
	 (setf rest (cddr rest)))
    (setf (form-vars current) (nreverse vars))
    current))

(defmethod parse-form ((parser parser) (head (eql 'catch)) rest parent source)
  (assert (consp rest) () "Cannot parse CATCH-form ~S" (cons head rest))
  (let* ((tag (car rest))
	 (body (cdr rest))
	 (current (make-ast parser 'catch-form :parent parent :source source))
	 (parsed-tag (parse parser tag current))
	 (parsed-body (parse-body parser body current)))
    (setf (form-tag current) parsed-tag (form-body current) parsed-body)
    current))

(defmethod parse-form ((parser parser) (head (eql 'throw)) rest parent source)
  (assert (and (consp rest) (consp (cdr rest)) (null (cddr rest))) () "Cannot parse THROW-form ~S" (cons head rest))
  (let* ((tag (car rest))
	 (result-form (cadr rest))
	 (current (make-ast parser 'throw-form :parent parent :source source))
	 (parsed-tag (parse parser tag current))
	 (parsed-value (parse parser result-form current)))
    (setf (form-tag current) parsed-tag (form-value current) parsed-value)
    current))

(defmethod parse-form ((parser parser) (head (eql 'eval-when)) rest parent source)
  (assert (and (consp rest) (listp (car rest))) () "Cannot parse EVAL-WHEN-form ~S" (cons head rest))
  (let* ((situations-form (car rest))
	 (body (cdr rest))
	 (current (make-ast parser 'eval-when-form :parent parent :source source :situations situations-form))
	 (parsed-body (parse-body parser body current)))
    (setf (form-body current) parsed-body)
    current))

(defmethod parse-form ((parser parser) (head (eql 'load-time-value)) rest parent source)
  (assert (and (consp rest) (or (null (cdr rest)) (and (consp (cdr rest)) (null (cddr rest))))) () "Cannot parse LOAD-TIME-VALUE-form ~S" (cons head rest))
  (let* ((value-form (car rest))
	 (readonly (cadr rest)))
    (assert (position (cadr rest) '(nil t)) () "READ-ONLY-P in LOAD-TIME-VALUE-form ~S must be either NIL or T, but is ~S" (cons head rest) readonly)
	      (let* ((current (make-ast parser 'load-time-value-form :parent parent :source source :readonly readonly))
		     (parsed-value (parse (make-instance 'parser :free-namespace (parser-free-namespace parser)) value-form current))) ;Note that dynamic variables must be parsed: in the form (LOAD-TIME-VALUE *A*), *A* must refer to the global *A*.
		(setf (form-value current) parsed-value)
		current)))

(defmethod parse-form ((parser parser) (head (eql 'quote)) rest parent source)
  (assert (and (consp rest) (null (cdr rest))) () "Cannot parse QUOTE-form ~S" (cons head rest))
  (let* ((object (car rest)))
    (make-ast parser 'quote-form :parent parent :source source :object object)))

(defmethod parse-form ((parser parser) (head (eql 'multiple-value-call)) rest parent source)
  (assert (and (consp rest) (listp (cdr rest))) () "Cannot parse ~S-form ~S" head (cons head rest))
  (let* ((function-form (car rest))
	 (body (cdr rest))
	 (current (make-ast parser 'multiple-value-call-form :parent parent :source source))
	 (parsed-function (parse parser function-form current))
	 (parsed-body (parse-body parser body current)))
    (setf (form-function current) parsed-function (form-body current) parsed-body)
    current))

(defmethod parse-form ((parser parser) (head (eql 'multiple-value-prog1)) rest parent source)
  (assert (and (consp rest) (listp (cdr rest))) () "Cannot parse ~S-form ~S" head (cons head rest))
  (let* ((function-form (car rest))
	 (body (cdr rest))
	 (current (make-ast parser 'multiple-value-prog1-form :parent parent :source source))
	 (parsed-function (parse parser function-form current))
	 (parsed-body (parse-body parser body current)))
    (setf (form-values current) parsed-function (form-body current) parsed-body)
    current))

(defmethod parse-form ((parser parser) (head (eql 'progv)) rest parent source)
  (assert (and (consp rest) (consp (cdr rest))) () "Cannot parse PROGV-form ~S" (cons head rest))
  ;; Note that in (PROGV SYMBOLS VALUES . BODY), both SYMBOLS and VALUES are evaluated, and thus the dynamic variable names are not known at compile- and load-time, only at run-time.
  (let* ((symbols-form (car rest))
	 (values-form (cadr rest))
	 (body (cddr rest))
	 (current (make-ast parser 'progv-form :parent parent :source source))
	 (parsed-symbols (parse parser symbols-form current))
	 (parsed-values (parse parser values-form current))
	 (parsed-body (parse-body parser body current)))
    (setf (form-symbols current) parsed-symbols (form-values current) parsed-values (form-body current) parsed-body)
    current))

(defmethod parse-form ((parser parser) (head (eql 'unwind-protect)) rest parent source)
  (assert (consp rest) () "Cannot parse UNWIND-PROTECT-form ~S" (cons head rest))
  (let* ((protected-form (car rest))
	 (cleanup-body (cdr rest))
	 (current (make-ast parser 'unwind-protect-form :parent parent :source source))
	 (parsed-protected (parse parser protected-form current))
	 (parsed-cleanup-body (parse-body parser cleanup-body current)))
    (setf (form-protected current) parsed-protected (form-body current) parsed-cleanup-body)
    current))

(defmethod parse-form ((parser parser) (head (eql 'tagbody)) rest parent source)
  (let ((body rest)
	(current (make-ast parser 'tagbody-form :parent parent :source source))
	(tags nil))
    (assert (proper-list-p body) () "Body is not a proper list: ~S" body)
    ;; pass over BODY thrice: in the first pass, establish lexical TAGs, because a GO-form referencing a tag defined after the GO would not know about the tag; in the second, create the parsed body (containing GO-FORMs); in the third, set slot :GOPOINT of the TAGs to the correct position in the parsed body list.
    (loop for form in body do
	 (cond
	   ;; The CLHS for TAGBODY says "The determination of which elements of the body are tags and which are statements is made prior to any macro expansion of that element. If a statement is a macro form and its macro expansion is an atom, that atom is treated as a statement, not a tag.". So we know symbols in the body are always tags, and that later in macro-expansion we cannot ever produce a tag.
	   ((atom form)
	    (assert (symbolp form) () "Cannot parse TAGBODY-form: it must only contain tags (which must be symbols) or conses, but contains ~S" form)
	    (let ((tag (make-ast parser 'tag :name form :freep nil :definition current :sites nil :source form))) ;:gopoint is defined below
	      (setf parser (augment-lexical-namespace tag parser)) ;after this, PARSER is a copy
	      (push tag tags)))))
    (let ((parsed-body (loop for form in body collect
			    (cond
			      ((atom form)
			       (make-ast parser 'tagpoint :parent current :source form :tag (namespace-lookup 'tag form (parser-lexical-namespace parser))))
			      (t
			       (parse parser form current))))))
      (loop for parsed-form-rest on parsed-body do
	   (let ((parsed-form (car parsed-form-rest)))
	     (when (typep parsed-form 'tagpoint)
	       (let ((tag (form-tag parsed-form)))
		 (push parsed-form (nso-sites tag))
		 (setf (nso-gopoint tag) parsed-form-rest)))))
      (setf (form-body current) parsed-body)
      (setf (form-tags current) (nreverse tags)))
    current))

(defmethod parse-form ((parser parser) (head (eql 'go)) rest parent source)
  (assert (and (consp rest) (symbolp (car rest)) (null (cdr rest))) () "Cannot parse GO-form ~S" (cons head rest))
  (let ((tag-name (car rest)))
    (let* ((tag (namespace-lookup/create 'tag tag-name parser))
	   (current (make-ast parser 'go-form :parent parent :source source :tag tag)))
      (push current (nso-sites tag))
      current)))

(defmethod parse-macro-or-function-application ((parser parser) macrop fun arg-forms parent source)
  (let* ((current (if macrop
		      (make-ast parser 'macroapplication-form :parent parent :source source :fun fun :recursivep (is-recursive fun parent) :lexicalnamespace (parser-lexical-namespace parser) :freenamespace (parser-free-namespace parser))
		      (make-ast parser 'application-form :parent parent :source source :fun fun :recursivep (is-recursive fun parent))))
	 (funobj (function-object fun)))
    (unless (typep funobj 'lambda-form)
      (push current (nso-sites funobj)))
    (assert (proper-list-p arg-forms) () "Arguments in function or macro application must be a proper list, but are~%~S" arg-forms)
    (let ((parsed-arguments (loop for arg-form in arg-forms collect
				 (if macrop arg-form (parse parser arg-form current)))))
      (setf (form-arguments current) parsed-arguments))
    current))

(defmethod parse-form ((parser parser) (head symbol) rest parent source)
  (let* ((fun-name head)
	 (arg-forms rest)
	 (fun (namespace-lookup/create 'fun fun-name parser))
	 (macrop (nso-macrop fun)))
    (parse-macro-or-function-application parser macrop fun arg-forms parent source)))

(defmethod parse-form ((parser parser) head rest parent source)
  (cond
    ;; allow '((LAMBDA (X) X) 1), which is legal Common Lisp.
    ((and (consp head) (eql (car head) 'lambda))
     (let ((fun (parse parser head parent))
	   (arg-forms rest))
       (parse-macro-or-function-application parser nil fun arg-forms parent source)))
    (t
     (error "Function or macro application must start with a symbol, but is~%~W" source))))

(defun parse-with-namespace (form &key (parser-type 'parser) (parser (make-parser :type parser-type :variables nil :functions nil :macros nil)))
  "Parse FORM using the PARSER, and any occurring declarations using DECLSPEC-PARSER. Use FREE-NAMESPACE as the free namespace.
If you want the default free Common Lisp namespace, pass ':PARSER (MAKE-PARSER)'."
  (parse parser form nil))

;; tests for PARSE

(defclass parser-namespace-at (parser)
  ((heresymbol :initarg :heresymbol :initform nil :accessor parser-heresymbol :documentation "The marker symbol.")
   (collected-lexical :initarg :collected-lexical :initform (error "asas") :accessor parser-collected-lexical :documentation "The collected lexical namespaces.")
   (collected-free :initarg :collected-free :initform (error "asas") :accessor parser-collected-free :documentation "The collected free namespaces."))
  (:documentation "Parser that collects the namespaces encountered at the symbol HERESYMBOL."))

(defmethod copy-parser ((parser parser-namespace-at))
  (make-instance (type-of parser)
		 :lexical-namespace (parser-lexical-namespace parser)
		 :free-namespace (parser-free-namespace parser)
		 :heresymbol (parser-heresymbol parser)
		 :collected-lexical (parser-collected-lexical parser)
		 :collected-free (parser-collected-free parser)))

(defmethod print-object ((parser parser-namespace-at) stream)
  (print-unreadable-object (parser stream :type t :identity t)
    (let ((*print-circle* t))
      (format stream "lexical: ~W~%free: ~W~%collected-lexical: ~W~%collected-free: ~W~%" (parser-lexical-namespace parser) (parser-free-namespace parser) (parser-collected-lexical parser) (parser-collected-free parser)))))

(defmethod parse :around ((parser parser-namespace-at) (form symbol) parent)
  (if (eql form (parser-heresymbol parser))
      (progn
	(push (parser-lexical-namespace parser) (cdr (parser-collected-lexical parser)))
	(push (parser-free-namespace parser) (cdr (parser-collected-free parser)))
	nil)
      (call-next-method parser form parent)))

(defun namespace-at (form heresymbol &key (variables nil) (functions nil) (macros nil) (parser (make-parser :type 'parser-namespace-at :heresymbol heresymbol :variables variables :functions functions :macros macros :collected-lexical (list nil) :collected-free (list nil))))
  "Parse FORM and collect the namespaces at the positions in FORM marked by the symbol HERESYMBOL.
Returns three values: a list containing the lexical namespaces, a list containing the free namespaces, and the parsed form."
  (let ((parsed-form (parse-with-namespace form :parser parser))
	(collected-lexical (nreverse (cdr (parser-collected-lexical parser))))
	(collected-free (nreverse (cdr (parser-collected-free parser)))))
    (values collected-lexical collected-free parsed-form)))

(defun test-parse-symbol-reference ()
  (declare (optimize (debug 3)))
  (flet ((namespace-at (form)
	   (namespace-at form '-here-)))
    (assert (nso-freep (form-var (parse-with-namespace 'a))))
    (let ((ast (parse-with-namespace 'a)))
      (assert (equal (nso-sites (form-var ast)) `(,ast))))
    (assert (nso-freep (form-object (parse-with-namespace '(function a)))))
    (let ((ast (parse-with-namespace '(function a)))) (assert (equal (nso-sites (form-object ast)) (list ast))))
    (assert (not (nso-freep (form-var (form-body-1 (parse-with-namespace '(let ((a 1)) a)))))))
    (assert (not (nso-freep (form-var (form-body-1 (parse-with-namespace '(let* ((a 1)) a)))))))
    (assert (not (nso-freep (form-object (form-body-1 (parse-with-namespace '(flet ((a ())) #'a)))))))
    (assert (not (nso-freep (form-object (form-body-1 (parse-with-namespace '(labels ((a ())) #'a)))))))
    (assert (parse-with-namespace '(let ((a)) a)))
    (assert (parse-with-namespace '(let (a) a)))
    (assert (parse-with-namespace '(let* ((a)) a)))
    (assert (parse-with-namespace '(let* (a) a)))
    (multiple-value-bind (lexical-namespace free-namespace ast) (namespace-at '(let ((b 5) (c b)) -here- b c))
      (declare (ignore free-namespace))
      (let* ((lexical-namespace (car lexical-namespace))
	     (b-var (namespace-lookup 'var 'b lexical-namespace))
	     (c-var (namespace-lookup 'var 'c lexical-namespace)))
	(assert (not (eq b-var (form-value (nso-definition c-var)))))
	(assert (equal (nso-sites b-var) (list (form-body-2 ast))))
	(assert (equal (nso-sites c-var) (list (form-body-3 ast))))))
    (multiple-value-bind (lexical-namespace free-namespace ast) (namespace-at '(flet ((b ()) (c () #'b)) -here-))
      (declare (ignore free-namespace ast))
      (let* ((lexical-namespace (car lexical-namespace))
	     (b-fun (namespace-lookup 'fun 'b lexical-namespace))
	     (c-fun (namespace-lookup 'fun 'c lexical-namespace)))
	(assert (not (eq b-fun (form-object (form-body-1 (nso-definition c-fun))))))
	(assert (null (nso-sites b-fun)))))
    (multiple-value-bind (lexical-namespace free-namespace ast) (namespace-at '(let* ((b 5) (c b)) -here-))
      (declare (ignore free-namespace))
      (let* ((lexical-namespace (car lexical-namespace))
	     (b-var (namespace-lookup 'var 'b lexical-namespace))
	     (c-var (namespace-lookup 'var 'c lexical-namespace)))
	(assert (eq b-var (form-var (form-value (nso-definition c-var)))))
	(assert (equal (nso-sites b-var) (list (form-value (form-binding-2 ast)))))))
    (multiple-value-bind (lexical-namespace free-namespace ast) (namespace-at '(labels ((b ()) (c () #'b)) -here-))
      (declare (ignore free-namespace))
      (let* ((lexical-namespace (car lexical-namespace))
	     (b-fun (namespace-lookup 'fun 'b lexical-namespace))
	     (c-fun (namespace-lookup 'fun 'c lexical-namespace)))
	(assert (eq b-fun (form-object (form-body-1 (nso-definition c-fun)))))
	(assert (equal (nso-sites b-fun) (list (form-body-1 (form-binding-2 ast)))))))
    (multiple-value-bind (lexical-namespaces free-namespace ast) (namespace-at '(let ((b 5)) -here- (let ((b b)) -here-)))
      (declare (ignore free-namespace))
      (let* ((lexical-namespace-1 (car lexical-namespaces))
	     (lexical-namespace-2 (cadr lexical-namespaces))
	     (b-1 (namespace-lookup 'var 'b lexical-namespace-1))
	     (b-2 (namespace-lookup 'var 'b lexical-namespace-2)))
	(assert (eq (form-object (form-value (nso-definition b-1))) 5))
	(assert (eq b-1 (form-var (form-value (nso-definition b-2)))))
	(assert (not (eq b-1 b-2)))
	(assert (equal (nso-sites b-1) (list (form-value (form-binding-1 (form-body-2 ast))))))))
    (multiple-value-bind (lexical-namespaces free-namespace ast) (namespace-at '(let* ((b 5)) -here- (let* ((b b)) -here-)))
      (declare (ignore free-namespace))
      (let* ((lexical-namespace-1 (car lexical-namespaces))
	     (lexical-namespace-2 (cadr lexical-namespaces))
	     (b-1 (namespace-lookup 'var 'b lexical-namespace-1))
	     (b-2 (namespace-lookup 'var 'b lexical-namespace-2)))
	(assert (eq (form-object (form-value (nso-definition b-1))) 5))
	(assert (eq b-1 (form-var (form-value (nso-definition b-2)))))
	(assert (not (eq b-1 b-2)))
	(assert (equal (nso-sites b-1) (list (form-value (form-binding-1 (form-body-2 ast))))))))
    (multiple-value-bind (lexical-namespaces free-namespace ast) (namespace-at '(flet ((b ())) -here- (flet ((b () #'b)) -here-)))
      (declare (ignore free-namespace))
      (let* ((lexical-namespace-1 (car lexical-namespaces))
	     (lexical-namespace-2 (cadr lexical-namespaces))
	     (b-1 (namespace-lookup 'fun 'b lexical-namespace-1))
	     (b-2 (namespace-lookup 'fun 'b lexical-namespace-2)))
	(assert (not (eq b-1 b-2)))
	(assert (eq b-1 (form-object (form-body-1 (nso-definition b-2)))))
	(assert (equal (nso-sites b-1) (list (form-body-1 (form-binding-1 (form-body-2 ast))))))))
    (multiple-value-bind (lexical-namespaces free-namespace ast) (namespace-at '(labels ((b ())) -here- (labels ((b () -here- #'b)) -here-)))
      (declare (ignore free-namespace))
      (let* ((lexical-namespace-1 (car lexical-namespaces))
	     (lexical-namespace-2 (cadr lexical-namespaces))
	     (lexical-namespace-3 (caddr lexical-namespaces))
	     (b-1 (namespace-lookup 'fun 'b lexical-namespace-1))
	     (b-2 (namespace-lookup 'fun 'b lexical-namespace-2))
	     (b-3 (namespace-lookup 'fun 'b lexical-namespace-3)))
	(assert (not (eq b-1 b-2)))
	(assert (not (eq b-1 b-3)))
	(assert (eq b-2 b-3))
	(assert (eq b-2 (form-object (form-body-2 (nso-definition b-2)))))
	(assert (equal (nso-sites b-2) (list (form-body-2 (form-binding-1 (form-body-2 ast))))))))
    (multiple-value-bind (lexical-namespaces free-namespace ast) (namespace-at '(labels ((a () -here- (b)) (b () -here- (a))) -here-))
      (declare (ignore free-namespace))
      (let* ((lexical-namespace-1 (car lexical-namespaces))
	     (lexical-namespace-2 (cadr lexical-namespaces))
	     (lexical-namespace-3 (caddr lexical-namespaces))
	     (a-2 (namespace-lookup 'fun 'a lexical-namespace-2))
	     (b-1 (namespace-lookup 'fun 'b lexical-namespace-1))
	     (b-2 (namespace-lookup 'fun 'b lexical-namespace-2))
	     (b-3 (namespace-lookup 'fun 'b lexical-namespace-3)))
	(assert (and (eq b-1 b-2) (eq b-1 b-3)))
	(assert (equal (nso-sites a-2) (list (form-body-2 (form-binding-2 ast))))))))
  (let* ((ast (parse-with-namespace '(test #'test 2 3)))
	 (call-fun (form-fun ast)))
    (assert (eq call-fun (form-object (form-argument-1 ast)))))
  (let* ((ast (parse-with-namespace '(bla (aref a x) x)))
	 (aref-x (form-var (form-argument-2 (form-argument-1 ast))))
	 (arg-x (form-var (form-argument-2 ast))))
    (assert (eq aref-x arg-x)))
  (let* ((ast (parse-with-namespace '(labels ((b () #'b (b))) (b))))
	 (b-fun (form-binding-1 ast))
	 (b0-fun (form-fun b-fun))
	 (b1-fun (form-object (form-body-1 b-fun)))
	 (b2-fun (form-fun (form-body-2 b-fun)))
	 (b3-fun (form-fun (form-body-1 ast)))
	 (b-fun-body (form-body (form-binding-1 ast))))
    (assert (and (eq b0-fun b1-fun) (eq b0-fun b2-fun) (eq b0-fun b3-fun)))
    (assert (equal (nso-sites b0-fun) (list (form-body-1 ast) (second b-fun-body) (car b-fun-body))))))
(test-parse-symbol-reference)

(defun test-parse-declaration ()
  (flet ((all-equal (&rest rest)
	   (if (null rest)
	       t
	       (loop for o1 in (butlast rest 1) for o2 in (cdr rest) always (equal o1 o2)))))
    (let* ((form '(let ((a 1)) (declare (type fixnum a)) a))
	   (ast (parse-with-namespace form))
	   (declspec-type (car (form-declspecs ast))))
      (assert (all-equal (form-var (form-binding-1 ast)) (car (declspec-vars declspec-type)) (form-var (form-body-1 ast)))))
    (let* ((form '(flet ((a ())) (declare (ftype fixnum a)) #'a))
	   (ast (parse-with-namespace form))
	   (declspec-ftype (car (form-declspecs ast))))
      (assert (all-equal (form-fun (form-binding-1 ast)) (car (declspec-funs declspec-ftype)) (form-object (form-body-1 ast)))))
    (let* ((ast (parse-with-namespace '(locally (declare (type fixnum a)) a)))
	   (declspec-type (car (form-declspecs ast)))
	   (declspec-type-a (car (declspec-vars declspec-type)))
	   (body-a (form-var (form-body-1 ast))))
      (assert (eq declspec-type-a body-a))
      (assert (nso-freep declspec-type-a)))))
(test-parse-declaration)

(defun test-parse-lambda-list ()
  (declare (optimize (debug 3)))
  (flet ((namespace-at (form)
	   (namespace-at form '-here-)))
    (let* ((lexical-namespaces (namespace-at '(let ((a 1)) -here- (flet ((test (a &optional (b (progn -here- a))) -here-)) -here-))))
	   (a1-var (namespace-lookup 'var 'a (car lexical-namespaces)))
	   (a2-var (namespace-lookup 'var 'a (cadr lexical-namespaces)))
	   (a3-var (namespace-lookup 'var 'a (caddr lexical-namespaces)))
	   (a4-var (namespace-lookup 'var 'a (cadddr lexical-namespaces))))
      (assert (equal a1-var a4-var))
      (assert (equal a2-var a3-var))
      (assert (not (equal a1-var a2-var))))
    (let* ((lexical-namespaces (namespace-at '(let ((a 1)) -here- (macrolet ((test (a (&optional (c (progn -here- a)))) -here-)) -here-))))
	   (a1-var (namespace-lookup 'var 'a (car lexical-namespaces)))
	   (a2-var (namespace-lookup 'var 'a (cadr lexical-namespaces)))
	   (a3-var (namespace-lookup 'var 'a (caddr lexical-namespaces)))
	   (a4-var (namespace-lookup 'var 'a (cadddr lexical-namespaces))))
      (assert (equal a1-var a4-var))
      (assert (equal a2-var a3-var))
      (assert (not (equal a1-var a2-var)))))
  (let* ((ast (parse-with-namespace '(flet ((test (a &optional (b a)) b)) a b)))
	 (test (form-binding-1 ast))
	 (llist (form-llist test))
	 (llist-a (parameter-var (car (llist-required llist))))
	 (llist-b (parameter-var (car (llist-optional llist))))
	 (llist-binit (form-var (parameter-init (car (llist-optional llist)))))
	 (body-a (form-body-1 ast))
	 (body-b (form-body-2 ast)))
    (assert (equal llist-a llist-binit))
    (assert (not (equal llist-a body-a)))
    (assert (not (equal llist-b body-b))))
  (let* ((ast (parse-with-namespace '(flet ((fa (&optional (a (fa))))) (fa))))
	 (binding (form-binding-1 ast))
	 (fa (form-fun binding))
	 (llist-a-init-fun (form-fun (parameter-init (car (llist-optional (form-llist binding)))))))
    (assert (not (eql llist-a-init-fun fa))))
  (let* ((ast (parse-with-namespace '(labels ((fa (&optional (a (fa))))) (fa))))
	 (binding (form-binding-1 ast))
	 (fa (form-fun binding))
	 (llist-a-init-fun (form-fun (parameter-init (car (llist-optional (form-llist binding)))))))
    (assert (eql llist-a-init-fun fa))))

(test-parse-lambda-list)

(defun test-parse-block-reference ()
  (declare (optimize (debug 3)))
  (assert (nso-freep (form-blo (parse-with-namespace '(return-from undef 1)))))
  (let* ((ast (parse-with-namespace '(block test (return-from test 1))))
	 (block-blo (form-blo ast))
	 (return-from-blo (form-blo (form-body-1 ast))))
    (assert (equal block-blo return-from-blo))
    (assert (eq (nso-name block-blo) 'test))
    (assert (not (nso-freep block-blo)))
    (assert (eq (nso-definition block-blo) ast)))
  (let* ((ast (parse-with-namespace '(flet ((test () (return-from test 1))) (return-from test 2))))
	 (test-fun (form-binding-1 ast))
	 (test-fun-blo (form-blo test-fun))
	 (return-from1-blo (form-blo (form-body-1 test-fun)))
	 (return-from2-blo (form-blo (form-body-1 ast))))
    (assert (equal test-fun-blo return-from1-blo))
    (assert (eq (nso-name test-fun-blo) 'test))
    (assert (not (nso-freep test-fun-blo)))
    (assert (eq (nso-definition test-fun-blo) test-fun))
    (assert (not (eq test-fun-blo return-from2-blo)))
    (assert (nso-freep return-from2-blo)))
  (let* ((ast (parse-with-namespace '(flet ((test () (if nil (return-from test 1) (if nil (return-from test 2) (return-from test 3))))) nil)))
	 (test-fun (form-binding-1 ast))
	 (test-fun-blo (form-blo test-fun)))
    (assert (= 3 (length (nso-sites test-fun-blo))))))
(test-parse-block-reference)

(defun test-parent ()
  (declare (optimize (debug 3)))
  (let* ((form '(lambda () (funcall bla)))
	 (ast (parse-with-namespace form))
	 (body (form-body ast)))
    (assert (eq (form-parent (car body)) ast))))
(test-parent)

(defun test-load-time-value-form ()
  (declare (optimize (debug 3)))
  (let* ((form '(progn
		 (bla a 2)
		 (let ((a 7))
		   (bla a 8)
		   (load-time-value a))))
	 (ast (parse-with-namespace form))
	 (a-bla1 (form-var (form-argument-1 (form-body-1 ast))))
	 (let-form (form-body-2 ast))
	 (a-let (form-var (form-binding-1 let-form)))
	 (a-bla2 (form-var (form-argument-1 (form-body-1 let-form))))
	 (a-load-time-value (form-var (form-value (form-body-2 let-form)))))
    (assert (nso-freep a-bla1))
    (assert (not (nso-freep a-let)))
    (assert (not (nso-freep a-bla2)))
    (assert (nso-freep a-load-time-value))
    (assert (eq a-let a-bla2))
    (assert (eq a-bla1 a-load-time-value))
    ))
(test-load-time-value-form)

(defun test-quote-form ()
  (declare (optimize (debug 3)))
  (let* ((form '(quote a))
	 (ast (parse-with-namespace form))
	 (quote-a (form-object ast)))
    (assert (symbolp quote-a))
    (assert (eq quote-a 'a))))
(test-quote-form)

(defun test-macro-form ()
  ;; Test that differentiating between function- and macro-applications works. Macro arguments must not be evaluated, but function arguments must. Otherwise, parsing a macro-call can fail with an incorrect error: (PARSE-WITH-NAMESPACE '(MACROLET ((BLA (A (IF S)) (PRINT (LIST A IF S)) NIL))) :FREE-NAMESPACE NIL), because (IF S) is parsed like an IF-form. Therefore a non-NIL flag MACROP in an instance of FUN, (or maybe a distinction between FUN and a new class MAC as subtypes of SYM), makes that if FUN-NAME is looked up as a macro, then the ARG-FORMS are not parsed at all.
  (parse-with-namespace '(macrolet ((bla (a (if s)) (print (list a if s)) nil))))
  (let* ((form '(symbol-macrolet ((a 2)) a))
	 (ast (parse-with-namespace form))
	 (a-binding (form-var (form-binding-1 ast)))
	 (a-body (form-var (form-body-1 ast))))
    (assert (eq a-binding a-body)))
  ;; I should not add a check that (PARSE-WITH-NAMESPACE '(MACROLET ((A (X &ENVIRONMENT ENV) `(IF ,X 2 3))) (A))) does the right thing since this would require evaluation of macro A, which should not be part of WALKER because it should contain as little semantics as possible. (If you want to add semantics (somewhere else), you might consider passing to #'PARSE a form read using the package FARE-QUASIQUOTE since different Lisps read the backquote (`) differently.)
  )
(test-macro-form)

;; Notes on MACROLET, SYMBOL-MACROLET:
;; The CLHS for SYMBOL-MACROLET says:
;; 0. "The expansion of a symbol macro is subject to further macro expansion in the same lexical environment as the symbol macro invocation": this should be easy to implement if I put the SYMBOL-MACROLETed variables in an lexical namespace. In fact the CLHS on SYMBOL-MACROLET also says "SYMBOL-MACROLET lexically establishes expansion functions for each of the symbol macros named by symbols".
;; 1. "The use of symbol-macrolet can be shadowed by let...", see below. This will be handled automatically since the symbol macros are in the same lexical namespace as normal variables defined by LET,LET*,LAMBDA,LLIST.
;; 2. "SYMBOL-MACROLET signals an error if a special declaration names one of the symbols being defined by SYMBOL-MACROLET.": add a testcase that (PARSE '(SYMBOL-MACROLET ((A B)) (DECLARE (SPECIAL A)))) fails.
;; 3. "any use of SETQ to set the value of one of the specified variables is treated as if it were a SETF. PSETQ of a symbol defined as a symbol macro is treated as if it were a PSETF, and MULTIPLE-VALUE-SETQ is treated as if it were a SETF of values.": This could mean that during evaluation (so not in the parser since it should contain as little semantics as possible) of the symbol-macro I have to handle the special forms SETQ, PSETQ, MULTIPLE-VALUE-SETQ specially.

;; Notes on TAGBODY:
;; The CLHS for TAGBODY says:
;; 1. "The determination of which elements of the body are tags and which are statements is made prior to any macro expansion of that element. If a statement is a macro form and its macro expansion is an atom, that atom is treated as a statement, not a tag."
;; TAGBODY always returns NIL, which means that a single-symbol statement in TAGBODY always means a tag, even as the last element of TAGBODY. The following form returns NIL (and not 1):
;; (let ((a 1))
;;   (tagbody
;;    a))
;; Raises an error that A and B are nonexistant tags. Does not raise an error using #'PARSE-WITH-NAMESPACE and key argument :FREE-NAMESPACE (MAKE-FREE-NAMESPACE), because ECASE is defined as a macro in Common Lisp and so the GO-forms in it are not expanded yet.
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

(defun test-symbol-macrolet ()
  (flet ((namespace-at (form)
	   (namespace-at form '-here-)))
    (let* ((form '(let ((a 1) ;A-LET1
			(b 2)) ;B-LET1
		   -here-
		   (symbol-macrolet ((a b))
		     (values
		      (let ((a 5) ;A-LET2
			    (b 6)) ;B-LET2
			-here-
			(list a b))
		      -here-
		      (list a b))))) ;returns (VALUES (5 6) (2 2)).
	   (lexnss (namespace-at form))
	   (a1 (namespace-lookup 'var 'a (car lexnss)))
	   (a2 (namespace-lookup 'var 'a (cadr lexnss)))
	   (a3 (namespace-lookup 'var 'a (caddr lexnss))))
      (assert (not (or (eq a1 a2) (eq a1 a3) (eq a2 a3)))))))
(test-symbol-macrolet)

(defun test-tagbody ()
  (declare (optimize (debug 3)))
  (let* ((form '(tagbody
		 (go a)
		 a
		 (tagbody
		  a
		    (go a))))
	 (tagbody1 (parse-with-namespace form))
	 (go1-form (form-body-1 tagbody1))
	 (a1-tagpoint (form-body-2 tagbody1))
	 (a1-tag (form-tag a1-tagpoint))
	 (tagbody2 (form-body-3 tagbody1))
	 (a2-tagpoint (form-body-1 tagbody2))
	 (a2-tag (form-tag a2-tagpoint))
	 (go2-form (form-body-2 tagbody2)))
    (assert (eq (form-tag go1-form) a1-tag))
    (assert (eq (form-tag go2-form) a2-tag))
    (assert (not (eq a1-tag a2-tag)))
    (assert (eq (nso-gopoint a1-tag) (nthcdr 1 (form-body tagbody1))))
    (assert (eq (nso-gopoint a2-tag) (nthcdr 0 (form-body tagbody2))))
    (assert (equal (nso-sites a1-tag) (list a1-tagpoint go1-form)))
    (assert (equal (nso-sites a2-tag) (list a2-tagpoint go2-form))))
  (let* ((form '(tagbody
		 (go b)
		 b
		 c
		 (tagbody
		  a
		    (go b))))
	 (ast (parse-with-namespace form))
	 (go1-tag-b (form-tag (form-body-1 ast)))
	 (tag-b (form-tag (form-body-2 ast)))
	 (tag-c (form-tag (form-body-3 ast)))
	 (tagbody2 (form-body-4 ast))
	 (go2-tag-b (form-tag (form-body-2 tagbody2))))
    (assert (and (eq go1-tag-b tag-b) (eq go2-tag-b tag-b)))
    (assert (equal (form-tags ast) (list tag-b tag-c))))
  (let* ((form '(symbol-macrolet ((a 1))
		 (tagbody
		    (go a)
		  a))) ;this A must be parsed as TAG, and the GO above must refer to a known tag. See CLHS for TAGBODY: "The determination of which elements of the body are tags and which are statements is made prior to any macro expansion of that element."
	 (ast (parse-with-namespace form))
	 (tagbody-ast (form-body-1 ast))
	 (go-tag-a (form-tag (form-body-1 tagbody-ast)))
	 (tagbody-tag-a (form-tag (form-body-2 tagbody-ast))))
    (assert (eq go-tag-a tagbody-tag-a))
    (assert (typep tagbody-tag-a 'tag))))
(test-tagbody)

(defun test-recursivep ()
  (let* ((ast (parse-with-namespace '(labels ((b () (b))) (b))))
	 (b-funbind (form-binding-1 ast))
	 (b-fun (form-fun b-funbind))
	 (b1-call (form-body-1 b-funbind))
	 (b2-call (form-body-1 ast)))
    (assert (is-recursive b-fun b1-call))
    (assert (not (is-recursive b-fun b2-call)))
    (assert (form-recursivep b1-call))
    (assert (not (form-recursivep b2-call)))))
(test-recursivep)

;;;; DEPARSER

(defclass deparser ()
  ()
  (:documentation "An instance of this class is the first argument to every #'DEPARSE method. Inherit from it if you want to customize the deparsing process."))

(defgeneric deparse (deparser ast path)
  (:documentation "Generate from the abstract syntax tree AST the corresponding Lisp forms with DEPARSER. PATH is a list describing the position of AST in the abstract syntax tree, whose root form is NIL."))

(defmacro deparse-path (deparser form path)
  "Deparse the form FORM, i.e. call (DEPARSE ,DEPARSER ,FORM ,PATH1) with an appropriate PATH1 describing the position of FORM in the abstract syntax tree, and constructed from PATH and FORM."
  (destructuring-bind (form-path form-ast) form
    (assert (symbolp form-path))
    (assert (symbolp form-ast)) ;(assert (typep form-ast '(member declspec parameter llist ast)))
    (assert (symbolp path))
    `(deparse ,deparser ,form (cons (list ',form-path ,form-ast) ,path))))

(defmacro deparse-path-list (deparser form-list path)
  "Deparse the list of forms FORMS-LIST, i.e. call (MAPCAR (LAMBDA (X) (DEPARSE ,DEPARSER X ,PATH)) ,FORM-LIST), and pass a PATH describing the position of X in the AST (abstract syntax tree), and constructed from PATH and FORM-LIST."
  (destructuring-bind (form-path form-ast) form-list
    (assert (symbolp form-path))
    (assert (symbolp form-ast)) ;(assert (typep form-ast '(member declspec parameter llist ast)))
    (assert (symbolp path))
    `(loop for i from 0 for form in ,form-list collect
	  (deparse ,deparser form (cons (list i ',form-path ,form-ast) ,path)))))

(defmacro deparse-body (deparser ast path declspecsp documentationp)
  (declare (type boolean declspecsp documentationp))
  (flet ((make-cond (documentation declspecs body)
	   `(cond
	      ,@(append
		 (when (and documentationp declspecsp)
		   `(((and ,documentation ,declspecs)
		      (cons ,documentation (cons ,declspecs ,body)))))
		 (when documentationp
		   `((,documentation
		      (list* ,documentation ,body))))
		 (when declspecsp
		   `((,declspecs
		      (list* ,declspecs ,body)))))
	      (t
	       ,body))))
    (let ((ast-sym ast)
	  (ast (gensym "AST")))
      `(let* ((,ast ,ast-sym)
	      ;; TODO: FIXME: (FORM-DOCUMENTATION ,AST) is not passed to DEPARSER (because it is not parsed by #'PARSE)
	      (documentation ,(if documentationp `(if (form-documentation ,ast) (form-documentation ,ast) nil) `nil))
	      (declspecs ,(if declspecsp `(if (form-declspecs ,ast) (cons 'declare (deparse-path-list ,deparser (form-declspecs ,ast) ,path)) nil) `nil))
	      (body (deparse-path-list ,deparser (form-body ,ast) ,path)))
	 (declare (ignorable documentation declspecs))
	 ,(make-cond 'documentation 'declspecs 'body)))))

(defclass orderer ()
  ()
  (:documentation "An instance of this class is the first argument to every #'EVAL-ORDER method. Inherit from it if you want to customize the list of accessors returned by #'EVAL-ORDER."))

(defgeneric eval-order (orderer ast)
  (:documentation "Return the list of accessors of AST describing the order in which slots are defined in AST."))

(defun eval-order-list (orderer ast-list)
  (apply #'nconc (mapcar (lambda (ast) (eval-order orderer ast)) ast-list)))

;; NSOs

(defmethod deparse ((deparser deparser) (nso nso) path)
  (nso-name nso))
(defmethod eval-order ((orderer orderer) (ast nso))
  (error "this cannot happen, since NSOs can be in ASTs only in the form of VAR-READING or VAR-WRITING (and maybe in PROGV or FUNCALL or APPLY)."))

;; DECLSPECs

(defmethod eval-order ((orderer orderer) (ast declspec))
  nil)

(defmethod deparse ((deparser deparser) (declspec declspec-type) path)
  (list* 'type
	 (declspec-type declspec) ;TODO: FIXME: this is not passed to DEPARSER (because it is not parsed by #'PARSE)
	 (deparse-path-list deparser (declspec-vars declspec) path)))

(defmethod deparse ((deparser deparser) (declspec declspec-ftype) path)
  (list* 'ftype
	 (declspec-type declspec) ;TODO: FIXME: this is not passed to DEPARSER (because it is not parsed by #'PARSE)
	 (deparse-path-list deparser (declspec-funs declspec) path)))

(defmethod deparse ((deparser deparser) (declspec declspec-optimize) path)
  (list* 'optimize
	 (mapcar (lambda (acons)
		   (let ((quality (car acons)) (value (cdr acons)))
		     (if (null value) quality (list quality value)))) ;TODO: FIXME: this is not passed to DEPARSER (because it is not parsed by #'PARSE)
		 (declspec-qualities declspec))))

(defmethod deparse ((deparser deparser) (declspec declspec-ignore) path)
  (list* 'ignore
	 (deparse-path-list deparser (declspec-syms declspec) path)))

(defmethod deparse ((deparser deparser) (declspec declspec-ignorable) path)
  (list* 'ignorable
	 (deparse-path-list deparser (declspec-syms declspec) path)))

(defmethod deparse ((deparser deparser) (declspec declspec-dynamic-extent) path)
  (list* 'dynamic-extent
	 (deparse-path-list deparser (declspec-syms declspec) path)))

(defmethod deparse ((deparser deparser) (declspec declspec-inline) path)
  (list* 'inline
	 (deparse-path-list deparser (declspec-funs declspec) path)))

(defmethod deparse ((deparser deparser) (declspec declspec-notinline) path)
  (list* 'notinline
	 (deparse-path-list deparser (declspec-funs declspec) path)))

(defmethod deparse ((deparser deparser) (declspec declspec-special) path)
  (list* 'special
	 (deparse-path-list deparser (declspec-vars declspec) path)))

;; PARAMETERs and LLISTs

(defmethod deparse ((deparser deparser) (parameter required-parameter) path)
  (deparse-path deparser (parameter-var parameter) path))
(defmethod eval-order ((orderer orderer) (ast required-parameter))
  nil)

(defmethod deparse ((deparser deparser) (parameter optional-parameter) path)
  (cons (deparse deparser (parameter-var parameter) path)
	(when (parameter-init parameter)
	  (cons (deparse-path deparser (parameter-init parameter) path)
		(when (parameter-suppliedp parameter)
		  (list (deparse-path deparser (parameter-suppliedp parameter) path)))))))
(defmethod eval-order ((orderer orderer) (ast optional-parameter))
  (when (parameter-init ast)
    `(,#'parameter-init)))

(defmethod deparse ((deparser deparser) (parameter rest-parameter) path)
  (deparse-path deparser (parameter-var parameter) path))
(defmethod eval-order ((orderer orderer) (ast rest-parameter))
  nil)

(defmethod deparse ((deparser deparser) (parameter body-parameter) path)
  (deparse-path deparser (parameter-var parameter) path))
(defmethod eval-order ((orderer orderer) (ast body-parameter))
  nil)

(defmethod deparse ((deparser deparser) (parameter key-parameter) path)
  (cons (if (parameter-keywordp parameter)
	    (list (parameter-keyword parameter)
		  (deparse-path deparser (parameter-var parameter) path))
	    (deparse-path deparser (parameter-var parameter) path))
	(when (parameter-init parameter)
	  (cons (deparse-path deparser (parameter-init parameter) path)
		(when (parameter-suppliedp parameter)
		  (cons (deparse-path deparser (parameter-suppliedp parameter) path)
			nil))))))
(defmethod eval-order ((orderer orderer) (ast key-parameter))
  (when (parameter-init ast)
    `(,#'parameter-init)))

(defmethod deparse ((deparser deparser) (parameter aux-parameter) path)
  (cons (deparse-path deparser (parameter-var parameter) path)
	(when (parameter-init parameter)
	  (deparse-path deparser (parameter-init parameter) path))))
(defmethod eval-order ((orderer orderer) (ast aux-parameter))
  nil)

(defmethod deparse ((deparser deparser) (llist ordinary-llist) path)
  (nconc
   (deparse-path-list deparser (llist-required llist) path)
   (when (llist-optional llist) (cons '&optional (deparse-path-list deparser (llist-optional llist) path)))
   (when (llist-rest llist) (list '&rest (deparse-path deparser (llist-rest llist) path)))
   (when (llist-key llist) (cons '&key (deparse-path-list deparser (llist-key llist) path)))
   (when (llist-allow-other-keys llist) (list '&allow-other-keys))
   (when (llist-aux llist) (cons '&aux (deparse-path-list deparser (llist-aux llist) path)))))
(defmethod eval-order ((orderer orderer) (ast ordinary-llist))
  (nconc
   (eval-order-list orderer (llist-required ast))
   (when (llist-optional ast) (eval-order-list orderer (llist-optional ast)))
   (when (llist-rest ast) (eval-order orderer (llist-rest ast)))
   (when (llist-key ast) (eval-order-list orderer (llist-key ast)))
   (when (llist-aux ast) (eval-order-list orderer (llist-aux ast)))))

(defmethod deparse ((deparser deparser) (llist macro-llist) path)
  (nconc
   (when (llist-whole llist) (list '&whole (deparse-path deparser (llist-whole llist) path)))
   (when (llist-environment llist) (list '&environment (deparse-path deparser (llist-environment llist) path)))
   (deparse-path-list deparser (llist-required llist) path)
   (when (llist-optional llist) (cons '&optional (deparse-path-list deparser (llist-optional llist) path)))
   (when (llist-rest llist) (list '&rest (deparse-path deparser (llist-rest llist) path)))
   (when (llist-body llist) (list '&body (deparse-path deparser (llist-body llist) path)))
   (when (llist-key llist) (cons '&key (deparse-path-list deparser (llist-key llist) path)))
   (when (llist-allow-other-keys llist) (list '&allow-other-keys))
   (when (llist-aux llist) (cons '&aux (deparse-path-list deparser (llist-aux llist) path)))))
(defmethod eval-order ((orderer orderer) (ast macro-llist))
  (nconc
   (when (llist-whole ast) (eval-order orderer (llist-whole ast)))
   (when (llist-environment ast) (eval-order orderer (llist-environment ast)))
   (eval-order-list orderer (llist-required ast))
   (when (llist-rest ast) (eval-order orderer (llist-rest ast)))
   (when (llist-body ast) (eval-order orderer (llist-body ast)))
   (when (llist-key ast) (eval-order-list orderer (llist-key ast)))
   (when (llist-aux ast) (eval-order-list orderer (llist-aux ast)))))

;; FORMS
(defmethod deparse ((deparser deparser) (ast object-form) path)
  (form-object ast))
(defmethod eval-order ((orderer orderer) (ast object-form))
  nil)

(defmethod deparse ((deparser deparser) (ast var-reading) path)
  (nso-name (form-var ast)))
(defmethod eval-order ((orderer orderer) (ast var-reading))
  nil)

(defmethod deparse ((deparser deparser) (ast function-form) path)
  (list 'function
	(deparse-path deparser (form-object ast) path)))
(defmethod eval-order ((orderer orderer) (ast function-form))
  nil)

(defmethod deparse ((deparser deparser) (ast progn-form) path)
  (list* 'progn
	 (deparse-body deparser ast path nil nil)))
(defmethod eval-order ((orderer orderer) (ast progn-form))
  `(,#'form-body))

(defmethod deparse ((deparser deparser) (ast var-binding) path)
  (if (or (form-value ast)
	  (nso-macrop (form-var ast))) ;(SYMBOL-MACROLET ((A NIL)) A) must not be converted to (SYMBOL-MACROLET ((A)) A)
      (list (deparse-path deparser (form-var ast) path)
	    (if (typep (form-value ast) 'form)
		(deparse-path deparser (form-value ast) path)
		(form-value ast)))
      (deparse-path deparser (form-var ast) path)))
(defmethod eval-order ((orderer orderer) (ast var-binding))
  `(,#'form-value))

(defmethod deparse ((deparser deparser) (ast let-form) path)
  (list* 'let
	 (deparse-path-list deparser (form-bindings ast) path)
	 (deparse-body deparser ast path t nil)))
(defmethod eval-order ((orderer orderer) (ast let-form))
  `(,#'form-bindings ,#'form-body))

(defmethod deparse ((deparser deparser) (ast let*-form) path)
  (list* 'let*
	 (deparse-path-list deparser (form-bindings ast) path)
	 (deparse-body deparser ast path t nil)))
(defmethod eval-order ((orderer orderer) (ast let*-form))
  `(#'form-bindings ,#'form-body))

(defmethod deparse ((deparser deparser) (ast block-form) path)
  (list* 'block
	 (deparse-path deparser (form-blo ast) path)
	 (deparse-body deparser ast path nil nil)))
(defmethod eval-order ((orderer orderer) (ast block-form))
  `(,#'form-body))

(defmethod deparse ((deparser deparser) (ast fun-binding) path)
  (list* (deparse-path deparser (form-fun ast) path)
	 (deparse-path deparser (form-llist ast) path)
	 (deparse-body deparser ast path t t)))
(defmethod eval-order ((orderer orderer) (ast fun-binding))
  `(,#'form-llist ,#'form-body))

(defmethod deparse ((deparser deparser) (ast flet-form) path)
  (list* 'flet
	 (deparse-path-list deparser (form-bindings ast) path)
	 (deparse-body deparser ast path t nil)))
(defmethod eval-order ((orderer orderer) (ast flet-form))
  `(,#'form-bindings ,#'form-body))

(defmethod deparse ((deparser deparser) (ast labels-form) path)
  (list* 'labels
	 (deparse-path-list deparser (form-bindings ast) path)
	 (deparse-body deparser ast path t nil)))
(defmethod eval-order ((orderer orderer) (ast labels-form))
  `(,#'form-bindings ,#'form-body))

(defmethod deparse ((deparser deparser) (ast lambda-form) path)
  (list* 'lambda
	 (deparse-path deparser (form-llist ast) path)
	 (deparse-body deparser ast path t t)))
(defmethod eval-order ((orderer orderer) (ast lambda-form))
  `(,#'form-llist ,#'form-body))

(defmethod deparse ((deparser deparser) (ast return-from-form) path)
  (list* 'return-from
	 (deparse-path deparser (form-blo ast) path)
	 (when (form-value ast)
	   (list (deparse-path deparser (form-value ast) path)))))
(defmethod eval-order ((orderer orderer) (ast return-from-form))
  (when (form-value ast)
    `(,#'form-value)))

(defmethod deparse ((deparser deparser) (ast locally-form) path)
  (list* 'locally
	 (deparse-body deparser ast path t nil)))
(defmethod eval-order ((orderer orderer) (ast locally-form))
  `(,#'form-body))

(defmethod deparse ((deparser deparser) (ast the-form) path)
  (list 'the
	(form-type ast) ;TODO: FIXME: this is not passed to DEPARSER (because it is not parsed by #'PARSE)
	(deparse-path deparser (form-value ast) path)))
(defmethod eval-order ((orderer orderer) (ast the-form))
  `(,#'form-value))

(defmethod deparse ((deparser deparser) (ast if-form) path)
  (list* 'if
	 (deparse-path deparser (form-test ast) path)
	 (deparse-path deparser (form-then ast) path)
	 (when (form-else ast)
	   (list (deparse-path deparser (form-else ast) path)))))
(defmethod eval-order ((orderer orderer) (ast if-form))
  `(,#'form-test ,#'form-then ,#'form-else))

(defmethod deparse ((deparser deparser) (ast var-writing) path)
  (list (deparse-path deparser (form-var ast) path)
	(deparse-path deparser (form-value ast) path)))
(defmethod eval-order ((orderer orderer) (ast var-writing))
  `(,#'form-value))

(defmethod deparse ((deparser deparser) (ast setq-form) path)
  (list* 'setq
	 (apply #'nconc (deparse-path-list deparser (form-vars ast) path))))
(defmethod eval-order ((orderer orderer) (ast setq-form))
  (eval-order-list orderer (form-vars ast)))

(defmethod deparse ((deparser deparser) (ast catch-form) path)
  (list* 'catch
	 (deparse-path deparser (form-tag ast) path)
	 (deparse-body deparser ast path nil nil)))
(defmethod eval-order ((orderer orderer) (ast catch-form))
  `(,#'form-body))

(defmethod deparse ((deparser deparser) (ast throw-form) path)
  (list* 'throw
	 (deparse-path deparser (form-tag ast) path)
	 (deparse-path deparser (form-value ast) path)))
(defmethod eval-order ((orderer orderer) (ast throw-form))
  `(,#'form-value))

(defmethod deparse ((deparser deparser) (ast eval-when-form) path)
  (list* 'eval-when
	 (form-situations ast) ;TODO: FIXME: this is not passed to DEPARSER (because it is not parsed by #'PARSE)
	 (deparse-body deparser ast path nil nil)))
(defmethod eval-order ((orderer orderer) (ast eval-when-form))
  `(,#'form-body))

(defmethod deparse ((deparser deparser) (ast load-time-value-form) path)
  (list 'load-time-value
	(deparse-path deparser (form-value ast) path)
	(form-readonly ast))) ;TODO: FIXME: this is not passed to DEPARSER (because it is not parsed by #'PARSE)
(defmethod eval-order ((orderer orderer) (ast load-time-value-form))
  `(,#'form-value))

(defmethod deparse ((deparser deparser) (ast quote-form) path)
  (list 'quote
	(form-object ast))) ;TODO: FIXME: this is not passed to DEPARSER (because it is not parsed by #'PARSE)
(defmethod eval-order ((orderer orderer) (ast quote-form))
  nil)

(defmethod deparse ((deparser deparser) (ast multiple-value-call-form) path)
  (list* 'multiple-value-call
	 (deparse-path deparser (form-function ast) path)
	 (deparse-body deparser ast path nil nil)))
(defmethod eval-order ((orderer orderer) (ast multiple-value-call-form))
  `(,#'form-function ,#'form-body))

(defmethod deparse ((deparser deparser) (ast multiple-value-prog1-form) path)
  (list* 'multiple-value-prog1
	 (deparse-path deparser (form-values ast) path)
	 (deparse-body deparser ast path nil nil)))
(defmethod eval-order ((orderer orderer) (ast multiple-value-prog1-form))
  `(,#'form-values ,#'form-body))

(defmethod deparse ((deparser deparser) (ast progv-form) path)
  (list* 'progv
	 (deparse-path deparser (form-symbols ast) path)
	 (deparse-path deparser (form-values ast) path)
	 (deparse-body deparser ast path nil nil)))
(defmethod eval-order ((orderer orderer) (ast progv-form))
  `(,#'form-symbols ,#'form-values ,#'form-body))

(defmethod deparse ((deparser deparser) (ast unwind-protect-form) path)
  (list* 'unwind-protect
	 (deparse-path deparser (form-protected ast) path)
	 (deparse-body deparser ast path nil nil)))
(defmethod eval-order ((orderer orderer) (ast unwind-protect-form))
  `(,#'form-protected ,#'form-body))

(defmethod deparse ((deparser deparser) (ast application-form) path)
  (list* (deparse-path deparser (form-fun ast) path)
	 (deparse-path-list deparser (form-arguments ast) path)))
(defmethod eval-order ((orderer orderer) (ast application-form))
  `(,#'form-arguments))

(defmethod deparse ((deparser deparser) (ast macroapplication-form) path)
  (list* (deparse-path deparser (form-fun ast) path)
	 (form-arguments ast)))
(defmethod eval-order ((orderer orderer) (ast macroapplication-form))
  `(,#'form-arguments))

(defmethod deparse ((deparser deparser) (ast symbol-macrolet-form) path)
  (list* 'symbol-macrolet
	 (deparse-path-list deparser (form-bindings ast) path)
	 (deparse-body deparser ast path t nil)))
(defmethod eval-order ((orderer orderer) (ast symbol-macrolet-form))
  `(,#'form-body))

(defmethod deparse ((deparser deparser) (ast macrolet-form) path)
  (list* 'macrolet
	 (deparse-path-list deparser (form-bindings ast) path)
	 (deparse-body deparser ast path t nil)))
(defmethod eval-order ((orderer orderer) (ast macrolet-form))
  `(,#'form-body))

(defmethod deparse ((deparser deparser) (ast tagbody-form) path)
  (list* 'tagbody
	 (deparse-body deparser ast path nil nil)))
(defmethod eval-order ((orderer orderer) (ast tagbody-form))
  `(,#'form-body))

(defmethod deparse ((deparser deparser) (ast tagpoint) path)
  (deparse-path deparser (form-tag ast) path))
(defmethod eval-order ((orderer orderer) (ast tagpoint))
  nil)

(defmethod deparse ((deparser deparser) (ast go-form) path)
  (list 'go
	(deparse-path deparser (form-tag ast) path)))
(defmethod eval-order ((orderer orderer) (ast go-form))
  nil)

;; MAP-AST

(defclass deparser-map-ast-before (deparser)
  ((function :initarg :function :initform (constantly nil) :accessor deparser-function :type function :documentation "The function called.")
   (result :initarg :result :accessor deparser-result :type t :documentation "The last result returned by #'DEPARSE."))
  (:documentation "A deparser that calls FUNCTION before every deparsing step."))

(defmethod deparse :before ((deparser deparser-map-ast-before) ast path)
  (setf (deparser-result deparser)
	(multiple-value-list (funcall (deparser-function deparser) ast path))))

(defclass deparser-map-ast-after (deparser)
  ((function :initarg :function :initform (constantly nil) :accessor deparser-function :type function :documentation "The function called.")
   (result :initarg :result :accessor deparser-result :type t :documentation "The last result returned by #'DEPARSE."))
  (:documentation "A deparser that calls FUNCTION after every deparsing step."))

(defmethod deparse :after ((deparser deparser-map-ast-after) ast path)
  (setf (deparser-result deparser)
	(multiple-value-list (funcall (deparser-function deparser) ast path))))

(defun map-ast (function ast &key (deparser-type 'deparser-map-ast-before) path)
  "Recursively call FUNCTION with all objects occurring in the AST in the left-to-right order in which they appear in the original Lisp form (except that documentation and DECLARE-expressions are always visited in this order, but TODO: FIXME: currently documentation is not passed to FUNCTION at all).
FUNCTION is called with one parameter: the current AST1, and a PATH that describes the position of AST1 in AST.
DEPARSER-TYPE can be an instance of classes DEPARSER-MAP-AST-BEFORE or DEPARSER-MAP-AST-AFTER, or a subclass of these. If DEPARSER-TYPE is of type DEPARSER-MAP-AST-BEFORE, then function is called on an encapsulating form before being called on the encapsulated forms; if DEPARSER-TYPE is of type DEPARSER-MAP-AST-AFTER, then after.
PATH is the path leading to AST. Use a value different from NIL if your AST is embedded in another abstract syntax tree, and you want to have the path for AST being passed to FUNCTION have the suffix PATH.
Return the last returned multiple values of FUNCTION."
  (declare (optimize (debug 3)))
  ;;TODO: FIXME: the DEPARSE-* functions above do not call RECURSE-FUNCTION for some slots (those which are not parsed by #'PARSE). Come up with a scheme that allows an adapted RECURSE-FUNCTION to know what type those unparsed slots are (e.g. type specifiers). The slots to which this applies is marked above with "TODO: FIXME: this is not passed to RECURSE-FUNCTION (because it is not parsed by #'PARSE)".
  (let ((deparser (make-instance deparser-type :function function)))
    (deparse deparser ast path)
    (apply #'values (deparser-result deparser))))
