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

;; This file contains the core of the parser, i.e. parsers for TODO all special forms.

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
   ;; for classes: export the class and _all_ accessors on one line so that deletion doesn't have to consider all exports
   :proper-list-p
   ;; NAMESPACES
   :nso :nso-name :nso-freep
   :sym :nso-definition :nso-declspecs
   :var
   :fun :nso-macrop
   :blo :nso-definition
   :*print-detailed-walker-objects*
   :valid-function-name-p
   :namespace :namespace-var :namespace-fun :namespace-blo
   :lexical-namespace
   :free-namespace
   :namespace-boundp
   :namespace-lookup
   :shallow-copy-lexical-namespace
   :augment-lexical-namespace
   :augment-free-namespace
   :namespace-lookup/create
   :default-namespace-lookup/create
   :make-empty-lexical-namespace
   :make-empty-free-namespace
   :make-default-free-common-lisp-namespace
   ;; DECLARATIONS
   :declspec :declspec-parent
   :declspec-type :declspec-vars
   :declspec-ftype :declspec-type :declspec-funs
   :declspec-optimize :declspec-qualities
   :declspec-ignore :declspec-syms
   :declspec-ignorable :declspec-syms
   :parse-declspecs
   :parse-declaration-in-body
   :parse-declaration-and-documentation-in-body
   ;; LAMBDA LISTS
   :argument :argument-parent :argument-var
   :required-argument
   :optional-argument :argument-init :argument-suppliedp
   :key-argument :argument-keywordp :argument-keyword
   :aux-argument :argument-init
   :llist :form-parent
   :ordinary-llist :llist-required :llist-optional :llist-rest :llist-key :llist-allow-other-keys :llist-aux
   :macro-llist :llist-whole :llist-environment :llist-required :llist-optional :llist-rest :llist-body :llist-key :llist-allow-other-keys :llist-aux
   :parse-required-argument
   :parse-optional-or-key-or-aux-argument
   ;;:parse-lambda-list ;do not export this as it should be split into several smaller functions.
   :parse-ordinary-lambda-list
   :parse-macro-lambda-list
   ;; FORMS
   :generalform
   :selfevalobject :selfevalobject-object
   :form :form-parent
   :body-form :form-body
   :special-form
   :progn-form
   :binding :binding-parent :binding-sym
   :var-binding :binding-value
   :bindings-form :form-bindings :form-declspecs
   :let-form
   :let*-form
   :functiondef :form-parent :form-llist :form-declspecs :form-documentation
   :block-naming-form :form-blo
   :block-form
   :fun-binding
   :flet-form
   :labels-form
   :lambda-form
   :return-from-form :form-blo :form-value
   :locally-form :form-declspecs
   :the-form :form-type :form-value
   :if-form :form-test :form-then :form-else
   :setq-form :form-vars :form-values
   :catch-form :form-tag
   :throw-form :form-tag :form-value
   :eval-when-form :form-situations
   :load-time-value-form :form-value :form-readonly
   :quote-form :form-object
   :multiple-value-call-form :form-function
   :multiple-value-prog1-form :form-function
   :progv-form :form-symbols :form-values
   :unwind-protect-form :form-protected
   :application-form :form-fun :form-arguments
   :macroapplication-form :form-lexicalnamespace :form-freenamespace
   :format-body
   :parse-and-set-functiondef
   :parse
   :parse-with-empty-namespaces
   :namespaces-at
   ))

(in-package :walker)

;; TODO: FIXME: Package WALKER should handle input containing arbitrary nonsense gracefully, except maybe circular lists. TODO: FIXME: CLHS seems to require allowing circular constants. Related are "Issues/iss079_w.htm", which talks about circular constants, "Issues/iss215_w.htm" talks about #'MAKE-LOAD-FORM. I don't know about circular code (which could maybe implemented by a JMP/LONGJMP). I would have to splatter the checks for circular lists in the whole code since the user might call any function with a circular list.
(defun proper-list-p (list)
  "Return T if LIST is a proper list, NIL otherwise."
  ;; TODO: FIXME: check for circularity.
  (and (listp list)
       (do ((rest list (cdr rest)))
	   ((or (null rest) (not (listp rest))) (null rest)))))

;;;; NAMESPACES

(defclass nso ()
  ((name :initarg :name :accessor nso-name :type (or symbol list) :documentation "LIST is allowed for functions named (SETF NAME)")
   (freep :initarg :freep :accessor nso-freep :type boolean
	  :documentation "T if it is a free variable/function, or NIL if bound. Note that this is specific to a namespace."))
  (:documentation "a namespace-object (NSO) containing a name and information whether it is free or bound"))
(defclass sym (nso)
  ((definition :initarg :definition :accessor nso-definition :type (or binding llist)
	       :documentation "the parsed object (of type (OR BINDING LLIST)) it is defined in, NIL if not known")
   (declspecs :initarg :declspecs :accessor nso-declspecs :type list
	      :documentation "a list of DECLSPECs that apply to this symbol")
   )
  (:documentation "A symbol referring to a variable, function or macro.
Note that symbols are always parsed in a lexical manner, regardless of whether the actual variable this symbol is referring to may be a lexical or special variable. For example, the symbols *A* in the form (LET ((*A* 1)) *A*) may refer to a lexical or special variable."))
(defclass var (sym)
  ()
  (:documentation "The symbol of a variable"))
(defclass fun (sym)
  ((macrop :initform nil :initarg :macrop :accessor nso-macrop :type boolean
	   :documentation "NIL for functions, non-NIL for macros"))
  (:documentation "The symbol of a function or macro"))
(defclass blo (nso)
  ((definition :initarg :definition :accessor nso-definition
	       :documentation "the parsed object (which should be an instance of a subclass of BLOCK-NAMING-FORM) that it is defined in, NIL if not known"))
  (:documentation "A named block."))

(defvar *print-detailed-walker-objects* t "If T, print more details of objects in package WALKER.")

(defmethod print-object ((object sym) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if *print-detailed-walker-objects*
	(cond
	  ((typep object 'fun)
	   (format stream "NAME:~S FREEP:~S MACROP:~S" (nso-name object) (nso-freep object) (nso-macrop object)))
	  (t
	   (format stream "NAME:~S FREEP:~S" (nso-name object) (nso-freep object))))
	(format stream "NAME:~S" (nso-name object)))))
(defmethod print-object ((object blo) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if *print-detailed-walker-objects*
	(format stream "NAME:~S FREEP:~S" (nso-name object) (nso-freep object))
	(format stream "NAME:~S" (nso-name object)))))

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

;; The three Lisp namespaces (VARIABLES FUNCTIONS BLOCKS) are encapsulated in a class NAMESPACE. The LEXICAL-NAMESPACE-object and the FREE-NAMESPACE-object are both passed to the PARSE*-functions. Instances of LEXICAL-NAMESPACE and FREE-NAMESPACE are passed to the custom parser functions so that the user can derive a more specialized class from LEXICAL-NAMESPACE and FREE-NAMESPACE and add additional namespaces (in additional slots), and access these slots in the custom parser functions. For the LEXICAL-NAMESPACE, the slots VARIABLES FUNCTIONS BLOCKS are implemented as ALISTS like before. For every augmentation (e.g. with a FUN-object) to an instance of the class LEXICAL-NAMESPACE, a new instance of the class LEXICAL-NAMESPACE is created, with the respective slot (e.g. slot FUN) updated to the augmented ALIST. In addition, there is a FREE-NAMESPACE, which contains the slots VARIABLES FUNCTIONS BLOCKS, which are ALISTS. If a free namespace-object should be augmented, then the corresponding slot in the FREE-NAMESPACE object is modified (but no new FREE-NAMESPACE object created). This way, 1. the LEXICAL-NAMESPACE-object that is passed to PARSE*-functions (like PARSE-DECLARATION-IN-BODY) and augmented there can be passed to other PARSE*-functions there and augmented there and so on, and augmented lexical NSO-objects are forgotten automatically when the other PARSE*-functions return without returning their newly created LEXICAL-NAMESPACE-object, but 2. the FREE-NAMESPACE-object passed to the PARSE*-functions is always the same object and thus augmentations to its slots are reflected in all PARSE*-functions.

(defclass namespace ()
  ;; note that the slots are named like NSO-objects, which allows, when given a SYMBOL-TYPE, accessing the slot named SYMBOL-TYPE and creating an instance of type SYMBOL-TYPE.
  ((var :initform nil :initarg :var :accessor namespace-var)
   (fun :initform nil :initarg :fun :accessor namespace-fun)
   (blo :initform nil :initarg :blo :accessor namespace-blo)
   ;; types ;i.e. class- or structure-names
   )
  (:documentation "A namespace"))
(defclass lexical-namespace (namespace)
  ()
  (:documentation "A lexical namespace (containing bound namespace objects). This means that (NSO-FREEP OBJECT)==NIL for all OBJECTS in VARIABLES, FUNCTIONS, BLOCKS."))
(defclass free-namespace (namespace)
  ()
  (:documentation "A namespace of free (i.e. unbound) namespace objects (e.g. variables). This means that (NSO-FREEP OBJECT)==T for all OBJECTS in VARIABLES, FUNCTIONS, BLOCKS."))

(defmethod print-object ((object namespace) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "VARs:~S FUNs:~S BLOs:~S" (namespace-var object) (namespace-fun object) (namespace-blo object))))

(defgeneric namespace-boundp (symbol-type symbol namespace)
  (:documentation "Return non-NIL if SYMBOL is bound in NAMESPACE in the slot SYMBOL-TYPE, NIL otherwise."))
(defmethod namespace-boundp (symbol-type symbol (namespace namespace))
  (let* ((alist (slot-value namespace symbol-type)))
    (not (null (assoc symbol alist)))))

(defgeneric namespace-lookup (symbol-type symbol namespace)
  (:documentation "Look up SYMBOL in NAMESPACE in the slot SYMBOL-TYPE and return it. Signal an error if SYMBOL is not bound."))
(defmethod namespace-lookup (symbol-type symbol (namespace namespace))
  (assert (slot-exists-p namespace symbol-type) () "NSO-type ~S not present in ~S~%(Maybe ~S needs to be in package WALKER?)" symbol-type namespace symbol-type)
  (let* ((alist (slot-value namespace symbol-type))
	 (cons (assoc symbol alist)))
    (assert (not (null cons)) () "SYMBOL ~A with NSO-type ~S not bound in ~S" symbol symbol-type namespace)
    (cdr cons)))

(defgeneric shallow-copy-lexical-namespace (namespace)
  (:documentation "Create a new instance of the type of NAMESPACE, with all slots of NAMESPACE bound to the same objects as in NAMESPACE."))
(defmethod shallow-copy-lexical-namespace ((namespace lexical-namespace))
  (make-instance 'lexical-namespace
		 :var (namespace-var namespace)
		 :fun (namespace-fun namespace)
		 :blo (namespace-blo namespace)))

(defgeneric augment-lexical-namespace (nso-object namespace)
  (:documentation "Create a copy of the lexical namespace NAMESPACE, augment the copy with NSO-OBJECT (which must be a subtype of NSO), and return the copy."))
(defmethod augment-lexical-namespace (nso-object (namespace lexical-namespace))
  (assert (not (nso-freep nso-object)) () "Cannot augment a lexical namespace with free namespace object ~S" nso-object)
  (let* ((new-namespace (shallow-copy-lexical-namespace namespace))
	 (nso-type (type-of nso-object)))
    (setf (slot-value new-namespace nso-type)
	  (acons (nso-name nso-object) nso-object (slot-value new-namespace nso-type)))
    new-namespace))

(defgeneric augment-free-namespace (nso-object namespace)
  (:documentation "Add NSO-OBJECT (which must be a subtype of NSO) to the free NAMESPACE. Return NIL."))
(defmethod augment-free-namespace (nso-object (namespace free-namespace))
  (let* ((nso-type (type-of nso-object)))
    (setf (slot-value namespace nso-type)
	  (acons (nso-name nso-object) nso-object (slot-value namespace nso-type)))
    nil))

(defgeneric namespace-lookup/create (symbol-type symbol lexical-namespace free-namespace)
  (:documentation "If the SYMBOL is bound in LEXICAL-NAMESPACE in the slot SYMBOL-TYPE, then return the object bound to SYMBOL. If the SYMBOL is bound in FREE-NAMESPACE in the slot SYMBOL-TYPE, then return the object bound to SYMBOL. Otherwise create a free NSO-instance of type SYMBOL-TYPE, modify FREE-NAMESPACE so that SYMBOL is bound to new NSO-instance and return the NSO-instance."))
(defun default-namespace-lookup/create (symbol-type symbol lexical-namespace free-namespace new-nso)
  (cond
    ((namespace-boundp symbol-type symbol lexical-namespace)
     (namespace-lookup symbol-type symbol lexical-namespace))
    ((namespace-boundp symbol-type symbol free-namespace)
     (namespace-lookup symbol-type symbol free-namespace))
    (t
     (augment-free-namespace new-nso free-namespace)
     new-nso)))
(defmethod namespace-lookup/create ((symbol-type (eql 'var)) symbol (lexical-namespace lexical-namespace) (free-namespace free-namespace))
  (assert (symbolp symbol) () "Invalid symbol name ~S" symbol)
  (default-namespace-lookup/create symbol-type symbol lexical-namespace free-namespace (make-instance symbol-type :name symbol :freep t :declspecs nil))) ;do not bind :DEFINITION
(defmethod namespace-lookup/create ((symbol-type (eql 'fun)) symbol (lexical-namespace lexical-namespace) (free-namespace free-namespace))
  (multiple-value-bind (fun-type name) (valid-function-name-p symbol)
    (declare (ignore name))
    (assert (not (null fun-type)) () "Invalid function name ~S" symbol)
    (default-namespace-lookup/create symbol-type symbol lexical-namespace free-namespace (make-instance symbol-type :name symbol :freep t :declspecs nil)))) ;do not bind :DEFINITION
(defmethod namespace-lookup/create ((symbol-type (eql 'blo)) symbol (lexical-namespace lexical-namespace) (free-namespace free-namespace))
  (assert (symbolp symbol) () "Invalid symbol name ~S" symbol)
  (default-namespace-lookup/create symbol-type symbol lexical-namespace free-namespace (make-instance symbol-type :name symbol :freep t))) ;do not bind :DEFINITION

(defun make-empty-lexical-namespace ()
  (make-instance 'lexical-namespace))

(defun make-empty-free-namespace ()
  (make-instance 'free-namespace))

(let ((lex (make-empty-lexical-namespace))
      (fre (make-empty-free-namespace)))
  (setf lex (augment-lexical-namespace (make-instance 'var :name 'a :freep nil) lex))
  (assert (namespace-lookup 'var 'a lex))
  (assert (null (namespace-var fre))))
(let ((lex (make-empty-lexical-namespace))
      (fre (make-empty-free-namespace)))
  (namespace-lookup/create 'var 'a lex fre)
  (assert (null (namespace-var lex)))
  (assert (namespace-lookup 'var 'a fre)))

;; TODO: add global variables defined by Common Lisp.
(defun make-default-free-common-lisp-namespace ()
  "Return a free namespace in which all [TODO: variables,] functions, and macros available by default in package COMMON-LISP are present. The FUN-objects in the return namespace must have their MACROP-slot bound appropriately, so that parsing '(DEFMACRO BLA (A (IF S) &OPTIONAL C) (PRINT (LIST A IF S C)) NIL) doesn't fail anymore."
  (let ((free-namespace (make-instance 'free-namespace))
	;; the list of Common Lisp macros: (determined by looking at differences between SBCL and CLISP of global *LISP-MACROS* computed in file all-lisp-symbols.lisp)
	(common-lisp-macros '(AND ASSERT CALL-METHOD CASE CCASE CHECK-TYPE COND CTYPECASE DECF DECLAIM DEFCLASS DEFCONSTANT DEFGENERIC DEFINE-COMPILER-MACRO DEFINE-CONDITION DEFINE-METHOD-COMBINATION DEFINE-MODIFY-MACRO DEFINE-SETF-EXPANDER DEFINE-SYMBOL-MACRO DEFMACRO DEFMETHOD DEFPACKAGE DEFPARAMETER DEFSETF DEFSTRUCT DEFTYPE DEFUN DEFVAR DESTRUCTURING-BIND DO DO* DO-ALL-SYMBOLS DO-EXTERNAL-SYMBOLS DO-SYMBOLS DOLIST DOTIMES ECASE ETYPECASE FORMATTER HANDLER-BIND HANDLER-CASE IGNORE-ERRORS IN-PACKAGE INCF LAMBDA LOOP LOOP-FINISH MULTIPLE-VALUE-BIND MULTIPLE-VALUE-LIST MULTIPLE-VALUE-SETQ NTH-VALUE OR POP PPRINT-LOGICAL-BLOCK PRINT-UNREADABLE-OBJECT PROG PROG* PROG1 PROG2 PSETF PSETQ PUSH PUSHNEW REMF RESTART-BIND RESTART-CASE RETURN ROTATEF SETF SHIFTF STEP TIME TRACE TYPECASE UNLESS UNTRACE WHEN WITH-ACCESSORS WITH-COMPILATION-UNIT WITH-CONDITION-RESTARTS WITH-HASH-TABLE-ITERATOR WITH-INPUT-FROM-STRING WITH-OPEN-FILE WITH-OPEN-STREAM WITH-OUTPUT-TO-STRING WITH-PACKAGE-ITERATOR WITH-SIMPLE-RESTART WITH-SLOTS WITH-STANDARD-IO-SYNTAX))
	;; the list of Common Lisp functions: (determined by global *LISP-FUNCTIONS* computed in file all-lisp-symbols.lisp, which are equal between SBCL and CLISP)
	(common-lisp-functions '(* + - / /= 1+ 1- < <= = > >= ABORT ABS ACONS ACOS ACOSH ADD-METHOD ADJOIN ADJUST-ARRAY ADJUSTABLE-ARRAY-P ALLOCATE-INSTANCE ALPHA-CHAR-P ALPHANUMERICP APPEND APPLY APROPOS APROPOS-LIST AREF ARITHMETIC-ERROR-OPERANDS ARITHMETIC-ERROR-OPERATION ARRAY-DIMENSION ARRAY-DIMENSIONS ARRAY-DISPLACEMENT ARRAY-ELEMENT-TYPE ARRAY-HAS-FILL-POINTER-P ARRAY-IN-BOUNDS-P ARRAY-RANK ARRAY-ROW-MAJOR-INDEX ARRAY-TOTAL-SIZE ARRAYP ASH ASIN ASINH ASSOC ASSOC-IF ASSOC-IF-NOT ATAN ATANH ATOM BIT BIT-AND BIT-ANDC1 BIT-ANDC2 BIT-EQV BIT-IOR BIT-NAND BIT-NOR BIT-NOT BIT-ORC1 BIT-ORC2 BIT-VECTOR-P BIT-XOR BOOLE BOTH-CASE-P BOUNDP BREAK BROADCAST-STREAM-STREAMS BUTLAST BYTE BYTE-POSITION BYTE-SIZE CAAAAR CAAADR CAAAR CAADAR CAADDR CAADR CAAR CADAAR CADADR CADAR CADDAR CADDDR CADDR CADR CAR CDAAAR CDAADR CDAAR CDADAR CDADDR CDADR CDAR CDDAAR CDDADR CDDAR CDDDAR CDDDDR CDDDR CDDR CDR CEILING CELL-ERROR-NAME CERROR CHANGE-CLASS CHAR CHAR-CODE CHAR-DOWNCASE CHAR-EQUAL CHAR-GREATERP CHAR-INT CHAR-LESSP CHAR-NAME CHAR-NOT-EQUAL CHAR-NOT-GREATERP CHAR-NOT-LESSP CHAR-UPCASE CHAR/= CHAR< CHAR<= CHAR= CHAR> CHAR>= CHARACTER CHARACTERP CIS CLASS-NAME CLASS-OF CLEAR-INPUT CLEAR-OUTPUT CLOSE CLRHASH CODE-CHAR COERCE COMPILE COMPILE-FILE COMPILE-FILE-PATHNAME COMPILED-FUNCTION-P COMPILER-MACRO-FUNCTION COMPLEMENT COMPLEX COMPLEXP COMPUTE-APPLICABLE-METHODS COMPUTE-RESTARTS CONCATENATE CONCATENATED-STREAM-STREAMS CONJUGATE CONS CONSP CONSTANTLY CONSTANTP CONTINUE COPY-ALIST COPY-LIST COPY-PPRINT-DISPATCH COPY-READTABLE COPY-SEQ COPY-STRUCTURE COPY-SYMBOL COPY-TREE COS COSH COUNT COUNT-IF COUNT-IF-NOT DECODE-FLOAT DECODE-UNIVERSAL-TIME DELETE DELETE-DUPLICATES DELETE-FILE DELETE-IF DELETE-IF-NOT DELETE-PACKAGE DENOMINATOR DEPOSIT-FIELD DESCRIBE DESCRIBE-OBJECT DIGIT-CHAR DIGIT-CHAR-P DIRECTORY DIRECTORY-NAMESTRING DISASSEMBLE DOCUMENTATION DPB DRIBBLE ECHO-STREAM-INPUT-STREAM ECHO-STREAM-OUTPUT-STREAM ED EIGHTH ELT ENCODE-UNIVERSAL-TIME ENDP ENOUGH-NAMESTRING ENSURE-DIRECTORIES-EXIST ENSURE-GENERIC-FUNCTION EQ EQL EQUAL EQUALP ERROR EVAL EVENP EVERY EXP EXPORT EXPT FBOUNDP FCEILING FDEFINITION FFLOOR FIFTH FILE-AUTHOR FILE-ERROR-PATHNAME FILE-LENGTH FILE-NAMESTRING FILE-POSITION FILE-STRING-LENGTH FILE-WRITE-DATE FILL FILL-POINTER FIND FIND-ALL-SYMBOLS FIND-CLASS FIND-IF FIND-IF-NOT FIND-METHOD FIND-PACKAGE FIND-RESTART FIND-SYMBOL FINISH-OUTPUT FIRST FLOAT FLOAT-DIGITS FLOAT-PRECISION FLOAT-RADIX FLOAT-SIGN FLOATP FLOOR FMAKUNBOUND FORCE-OUTPUT FORMAT FOURTH FRESH-LINE FROUND FTRUNCATE FUNCALL FUNCTION-KEYWORDS FUNCTION-LAMBDA-EXPRESSION FUNCTIONP GCD GENSYM GENTEMP GET GET-DECODED-TIME GET-DISPATCH-MACRO-CHARACTER GET-INTERNAL-REAL-TIME GET-INTERNAL-RUN-TIME GET-MACRO-CHARACTER GET-OUTPUT-STREAM-STRING GET-PROPERTIES GET-SETF-EXPANSION GET-UNIVERSAL-TIME GETF GETHASH GRAPHIC-CHAR-P HASH-TABLE-COUNT HASH-TABLE-P HASH-TABLE-REHASH-SIZE HASH-TABLE-REHASH-THRESHOLD HASH-TABLE-SIZE HASH-TABLE-TEST HOST-NAMESTRING IDENTITY IMAGPART IMPORT INITIALIZE-INSTANCE INPUT-STREAM-P INSPECT INTEGER-DECODE-FLOAT INTEGER-LENGTH INTEGERP INTERACTIVE-STREAM-P INTERN INTERSECTION INVALID-METHOD-ERROR INVOKE-DEBUGGER INVOKE-RESTART INVOKE-RESTART-INTERACTIVELY ISQRT KEYWORDP LAST LCM LDB LDB-TEST LDIFF LENGTH LISP-IMPLEMENTATION-TYPE LISP-IMPLEMENTATION-VERSION LIST LIST* LIST-ALL-PACKAGES LIST-LENGTH LISTEN LISTP LOAD LOAD-LOGICAL-PATHNAME-TRANSLATIONS LOG LOGAND LOGANDC1 LOGANDC2 LOGBITP LOGCOUNT LOGEQV LOGICAL-PATHNAME LOGICAL-PATHNAME-TRANSLATIONS LOGIOR LOGNAND LOGNOR LOGNOT LOGORC1 LOGORC2 LOGTEST LOGXOR LONG-SITE-NAME LOWER-CASE-P MACHINE-INSTANCE MACHINE-TYPE MACHINE-VERSION MACRO-FUNCTION MACROEXPAND MACROEXPAND-1 MAKE-ARRAY MAKE-BROADCAST-STREAM MAKE-CONCATENATED-STREAM MAKE-CONDITION MAKE-DISPATCH-MACRO-CHARACTER MAKE-ECHO-STREAM MAKE-HASH-TABLE MAKE-INSTANCE MAKE-INSTANCES-OBSOLETE MAKE-LIST MAKE-LOAD-FORM MAKE-LOAD-FORM-SAVING-SLOTS MAKE-PACKAGE MAKE-PATHNAME MAKE-RANDOM-STATE MAKE-SEQUENCE MAKE-STRING MAKE-STRING-INPUT-STREAM MAKE-STRING-OUTPUT-STREAM MAKE-SYMBOL MAKE-SYNONYM-STREAM MAKE-TWO-WAY-STREAM MAKUNBOUND MAP MAP-INTO MAPC MAPCAN MAPCAR MAPCON MAPHASH MAPL MAPLIST MASK-FIELD MAX MEMBER MEMBER-IF MEMBER-IF-NOT MERGE MERGE-PATHNAMES METHOD-COMBINATION-ERROR METHOD-QUALIFIERS MIN MINUSP MISMATCH MOD MUFFLE-WARNING NAME-CHAR NAMESTRING NBUTLAST NCONC NINTERSECTION NINTH NO-APPLICABLE-METHOD NO-NEXT-METHOD NOT NOTANY NOTEVERY NRECONC NREVERSE NSET-DIFFERENCE NSET-EXCLUSIVE-OR NSTRING-CAPITALIZE NSTRING-DOWNCASE NSTRING-UPCASE NSUBLIS NSUBST NSUBST-IF NSUBST-IF-NOT NSUBSTITUTE NSUBSTITUTE-IF NSUBSTITUTE-IF-NOT NTH NTHCDR NULL NUMBERP NUMERATOR NUNION ODDP OPEN OPEN-STREAM-P OUTPUT-STREAM-P PACKAGE-ERROR-PACKAGE PACKAGE-NAME PACKAGE-NICKNAMES PACKAGE-SHADOWING-SYMBOLS PACKAGE-USE-LIST PACKAGE-USED-BY-LIST PACKAGEP PAIRLIS PARSE-INTEGER PARSE-NAMESTRING PATHNAME PATHNAME-DEVICE PATHNAME-DIRECTORY PATHNAME-HOST PATHNAME-MATCH-P PATHNAME-NAME PATHNAME-TYPE PATHNAME-VERSION PATHNAMEP PEEK-CHAR PHASE PLUSP POSITION POSITION-IF POSITION-IF-NOT PPRINT PPRINT-DISPATCH PPRINT-FILL PPRINT-INDENT PPRINT-LINEAR PPRINT-NEWLINE PPRINT-TAB PPRINT-TABULAR PRIN1 PRIN1-TO-STRING PRINC PRINC-TO-STRING PRINT PRINT-NOT-READABLE-OBJECT PRINT-OBJECT PROBE-FILE PROCLAIM PROVIDE RANDOM RANDOM-STATE-P RASSOC RASSOC-IF RASSOC-IF-NOT RATIONAL RATIONALIZE RATIONALP READ READ-BYTE READ-CHAR READ-CHAR-NO-HANG READ-DELIMITED-LIST READ-FROM-STRING READ-LINE READ-PRESERVING-WHITESPACE READ-SEQUENCE READTABLE-CASE READTABLEP REALP REALPART REDUCE REINITIALIZE-INSTANCE REM REMHASH REMOVE REMOVE-DUPLICATES REMOVE-IF REMOVE-IF-NOT REMOVE-METHOD REMPROP RENAME-FILE RENAME-PACKAGE REPLACE REQUIRE REST RESTART-NAME REVAPPEND REVERSE ROOM ROUND ROW-MAJOR-AREF RPLACA RPLACD SBIT SCALE-FLOAT SCHAR SEARCH SECOND SET SET-DIFFERENCE SET-DISPATCH-MACRO-CHARACTER SET-EXCLUSIVE-OR SET-MACRO-CHARACTER SET-PPRINT-DISPATCH SET-SYNTAX-FROM-CHAR SEVENTH SHADOW SHADOWING-IMPORT SHARED-INITIALIZE SHORT-SITE-NAME SIGNAL SIGNUM SIMPLE-BIT-VECTOR-P SIMPLE-CONDITION-FORMAT-ARGUMENTS SIMPLE-CONDITION-FORMAT-CONTROL SIMPLE-STRING-P SIMPLE-VECTOR-P SIN SINH SIXTH SLEEP SLOT-BOUNDP SLOT-EXISTS-P SLOT-MAKUNBOUND SLOT-MISSING SLOT-UNBOUND SLOT-VALUE SOFTWARE-TYPE SOFTWARE-VERSION SOME SORT SPECIAL-OPERATOR-P SQRT STABLE-SORT STANDARD-CHAR-P STORE-VALUE STREAM-ELEMENT-TYPE STREAM-ERROR-STREAM STREAM-EXTERNAL-FORMAT STREAMP STRING STRING-CAPITALIZE STRING-DOWNCASE STRING-EQUAL STRING-GREATERP STRING-LEFT-TRIM STRING-LESSP STRING-NOT-EQUAL STRING-NOT-GREATERP STRING-NOT-LESSP STRING-RIGHT-TRIM STRING-TRIM STRING-UPCASE STRING/= STRING< STRING<= STRING= STRING> STRING>= STRINGP SUBLIS SUBSEQ SUBSETP SUBST SUBST-IF SUBST-IF-NOT SUBSTITUTE SUBSTITUTE-IF SUBSTITUTE-IF-NOT SUBTYPEP SVREF SXHASH SYMBOL-FUNCTION SYMBOL-NAME SYMBOL-PACKAGE SYMBOL-PLIST SYMBOL-VALUE SYMBOLP SYNONYM-STREAM-SYMBOL TAILP TAN TANH TENTH TERPRI THIRD TRANSLATE-LOGICAL-PATHNAME TRANSLATE-PATHNAME TREE-EQUAL TRUENAME TRUNCATE TWO-WAY-STREAM-INPUT-STREAM TWO-WAY-STREAM-OUTPUT-STREAM TYPE-ERROR-DATUM TYPE-ERROR-EXPECTED-TYPE TYPE-OF TYPEP UNBOUND-SLOT-INSTANCE UNEXPORT UNINTERN UNION UNREAD-CHAR UNUSE-PACKAGE UPDATE-INSTANCE-FOR-DIFFERENT-CLASS UPDATE-INSTANCE-FOR-REDEFINED-CLASS UPGRADED-ARRAY-ELEMENT-TYPE UPGRADED-COMPLEX-PART-TYPE UPPER-CASE-P USE-PACKAGE USE-VALUE USER-HOMEDIR-PATHNAME VALUES VALUES-LIST VECTOR VECTOR-POP VECTOR-PUSH VECTOR-PUSH-EXTEND VECTORP WARN WILD-PATHNAME-P WRITE WRITE-BYTE WRITE-CHAR WRITE-LINE WRITE-SEQUENCE WRITE-STRING WRITE-TO-STRING Y-OR-N-P YES-OR-NO-P ZEROP)))
    (loop for macro in common-lisp-macros do
	 (augment-free-namespace (make-instance 'fun :name macro :freep t :declspecs nil :macrop t) free-namespace)) ;do not bind :DEFINITION
    (loop for function in common-lisp-functions do
	 (augment-free-namespace (make-instance 'fun :name function :freep t :declspecs nil :macrop nil) free-namespace)) ;do not bind :DEFINITION
    free-namespace))

;;;; DECLARATIONS

(defclass declspec ()
  ((parent :initarg :parent :accessor declspec-parent)))
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

(defun parse-declspecs (declspecs lexical-namespace free-namespace parent &key customparsedeclspecp-function customparsedeclspec-function)
  "Example: (PARSE-DECLSPECS '((TYPE FIXNUM A B C) (IGNORE A)) (MAKE-EMPTY-LEXICAL-NAMESPACE) (MAKE-EMPTY-FREE-NAMESPACE) NIL)"
  (labels ((parse-declspec (expr)
	     (assert (consp expr) () "Malformed declaration specification ~S" expr)
	     (let* ((identifier (car expr))
		    (body (cdr expr)))
	       (assert (symbolp identifier) () "Malformed declaration identifier ~S" identifier)
	       (let ((parsed-declspec
		      (cond
			((and (not (null customparsedeclspecp-function)) (funcall customparsedeclspecp-function identifier body lexical-namespace free-namespace parent))
			 (funcall customparsedeclspec-function identifier body lexical-namespace free-namespace parent))
			((find identifier '(type ftype))
			 (assert (and (listp body) (listp (cdr body))) () "Malformed ~S declaration: ~S" identifier expr)
			 (let* ((typespec (car body))
				(syms (cdr body)))
			   ;; will not check TYPESPEC, has to be done in user code.
			   (assert (proper-list-p syms) () "Not a proper list in ~S declaration: ~S" identifier expr)
			   (let* ((parsed-syms (loop for sym in syms collect (namespace-lookup/create (ecase identifier ((type) 'var) ((ftype) 'fun)) sym lexical-namespace free-namespace)))
				  (parsed-declspec (ecase identifier
						     ((type) (make-instance 'declspec-type :parent parent :type typespec :vars parsed-syms))
						     ((ftype) (make-instance 'declspec-ftype :parent parent :type typespec :funs parsed-syms)))))
			     (loop for sym in parsed-syms do
				  (push parsed-declspec (nso-declspecs sym)))
			     parsed-declspec)))
			((eq identifier 'optimize)
			 (assert (proper-list-p body) () "~S declaration is not a proper list: ~S" identifier expr)
			 (let ((qualities (loop for quality-value in body collect
					       (if (consp quality-value)
						   (let ((quality (car quality-value))
							 (rest (cdr quality-value)))
						     (assert (and (symbolp quality) (consp rest) (find (car rest) '(0 1 2 3)) (null (cdr rest))) () "Malformed ~S in ~S declaration: ~S" body identifier expr)
						     (cons quality (car rest)))
						   (let ((quality quality-value))
						     (assert (symbolp quality) () "Malformed ~S in ~S declaration: ~S" body identifier expr)
						     (cons quality nil))))))
			   (make-instance 'declspec-optimize :parent parent :qualities qualities)))
			((find identifier '(ignore ignorable))
			 (assert (listp body) () "Malformed ~S declaration: ~S" identifier expr)
			 (let* ((syms body))
			   (assert (proper-list-p syms) () "Not a proper list in ~S declaration: ~S" identifier expr)
			   (let* ((parsed-syms (loop for sym in syms collect
						    (cond
						      ((symbolp sym)
						       (namespace-lookup/create 'var sym lexical-namespace free-namespace))
						      ((and (consp sym) (eq (car sym) 'function) (valid-function-name-p (cadr sym)))
						       (namespace-lookup/create 'fun (cadr sym) lexical-namespace free-namespace))
						      (t
						       (error "Symbol in ~S declaration must be either a SYMBOL or a function name, but is ~S" identifier sym)))))
				  (parsed-declspec (make-instance (ecase identifier ((ignore) 'declspec-ignore) ((ignorable) 'declspec-ignorable)) :parent parent :syms parsed-syms)))
			     (loop for sym in parsed-syms do
				  (push parsed-declspec (nso-declspecs sym)))
			     parsed-declspec)))
			((find identifier '(dynamic-extent inline special notinline))
			 (error "declaration identifier ~A not implemented yet" identifier))
			(t (error "Unknown declaration specifier ~S" expr)))))
		 parsed-declspec))))
    (assert (proper-list-p declspecs) () "Declaration specifications must be a proper list, but are ~S" declspecs)
    (loop for declspec in declspecs collect
	 (parse-declspec declspec))))

(defun parse-declaration-in-body (body lexical-namespace free-namespace parent &key customparsedeclspecp-function customparsedeclspec-function)
  "Parses declarations in the beginning of BODY.
Returns two values: the rest of the BODY that does not start with a DECLARE-expression, and a list of DECLSPEC-objects.
Side-effects: Adds references of the created DECLSPEC-objects to the DECLSPEC-slots of variables or functions in LEXICAL-NAMESPACE and FREE-NAMESPACE. Creates yet unknown free variables and functions, adds references to the created DECLSPEC-objects, and adds the NSO-objects to FREE-NAMESPACE."
  (declare (optimize (debug 3)))
  (assert (listp body) () "Malformed BODY:~%~S" body)
  (labels ((parse-declare (body collected-declspecs)
	     (let ((head (car body))
		   (rest (cdr body)))
	       (cond
		 ((and (listp head) (eq (car head) 'declare))
		  (let* ((declspecs (cdr head)))
		    (assert (proper-list-p declspecs) () "Not a proper list: ~S" declspecs)
		    (let ((new-declspecs (parse-declspecs declspecs lexical-namespace free-namespace parent :customparsedeclspecp-function customparsedeclspecp-function :customparsedeclspec-function customparsedeclspec-function)))
		      (parse-declare rest (nconc collected-declspecs new-declspecs)))))
		 (t (values body collected-declspecs))))))
    (assert (proper-list-p body) () "Not a proper list: ~S" body)
    (parse-declare body nil)))

(let ((lexical-namespace (make-empty-lexical-namespace))
      (free-namespace (make-empty-free-namespace)))
  (multiple-value-bind (body declspecs) (parse-declaration-in-body '(declare (type fixnum)) lexical-namespace free-namespace nil)
    (assert (and (equal body '(declare (type fixnum))) (null declspecs)))))
;;TODO: test that (parse-declaration-in-body '((declare ()) 5) nil nil nil) throws an error.
(let ((lexical-namespace (make-empty-lexical-namespace))
      (free-namespace (make-empty-free-namespace)))
  (multiple-value-bind (body declspecs) (parse-declaration-in-body '((declare (type fixnum a)) 5) lexical-namespace free-namespace nil)
    (assert (and (equal body '(5)) (typep (car declspecs) 'declspec-type)))
    (assert (nso-freep (car (declspec-vars (car declspecs)))))
    (assert (eq (car (declspec-vars (car declspecs))) (namespace-lookup 'var 'a free-namespace)))
    (assert (not (namespace-boundp 'fun 'a free-namespace)))))
(let ((lexical-namespace (make-empty-lexical-namespace))
      (free-namespace (make-empty-free-namespace)))
  (multiple-value-bind (body declspecs) (parse-declaration-in-body '((declare (ftype (function () fixnum) a)) 5) lexical-namespace free-namespace nil)
    (assert (and (equal body '(5)) (typep (car declspecs) 'declspec-ftype)))
    (assert (nso-freep (car (declspec-funs (car declspecs)))))
    (assert (eq (car (declspec-funs (car declspecs))) (namespace-lookup 'fun 'a free-namespace)))
    (assert (not (namespace-boundp 'var 'a free-namespace)))))
(let ((lexical-namespace (make-empty-lexical-namespace))
      (free-namespace (make-empty-free-namespace)))
  (multiple-value-bind (body declspecs) (parse-declaration-in-body '((declare (ftype (function () fixnum) (setf a))) 5) lexical-namespace free-namespace nil)
    (assert (and (equal body '(5)) (typep (car declspecs) 'declspec-ftype)))
    (assert (nso-freep (car (declspec-funs (car declspecs)))))
    (assert (eq (car (declspec-funs (car declspecs))) (namespace-lookup 'fun '(setf a) free-namespace)))
    (assert (not (namespace-boundp 'var '(setf a) free-namespace)))))
(let ((lexical-namespace (make-empty-lexical-namespace))
      (free-namespace (make-empty-free-namespace)))
  (multiple-value-bind (body declspecs) (parse-declaration-in-body '((declare (optimize (debug 3) speed)) 5) lexical-namespace free-namespace nil)
    (assert (equal body '(5)))
    (assert (let ((d (car declspecs))) (and (typep d 'declspec-optimize) (equal (declspec-qualities d) '((debug . 3) (speed . nil))))))))
(let ((lexical-namespace (make-empty-lexical-namespace))
      (free-namespace (make-empty-free-namespace)))
  (multiple-value-bind (body declspecs) (parse-declaration-in-body '((declare (ignore a (function b))) 5) lexical-namespace free-namespace nil)
    (assert (and (equal body '(5)) (typep (car declspecs) 'declspec-ignore)))
    (assert (let ((syms (declspec-syms (car declspecs)))) (typep (car syms) 'var) (typep (cadr syms) 'fun)))
    (assert (eq (car (declspec-syms (car declspecs))) (namespace-lookup 'var 'a free-namespace)))
    (assert (eq (cadr (declspec-syms (car declspecs))) (namespace-lookup 'fun 'b free-namespace)))))
(multiple-value-bind (body declspecs)
    (parse-declaration-in-body '((declare (type fixnum) (ftype (function () t))) (declare (optimize (speed 3)) (ignore)))
			       (make-empty-lexical-namespace) (make-empty-free-namespace) nil)
  (assert (equal body nil))
  (assert (typep (elt declspecs 0) 'declspec-type))
  (assert (typep (elt declspecs 1) 'declspec-ftype))
  (assert (typep (elt declspecs 2) 'declspec-optimize))
  (assert (typep (elt declspecs 3) 'declspec-ignore)))

(defun parse-declaration-and-documentation-in-body (body lexical-namespace free-namespace parent &key customparsedeclspecp-function customparsedeclspec-function)
  "Parses declarations and documentation in the beginning of BODY.
Returns three values: the rest of the BODY that does not start with a DECLARE-expression or a documentation string, and a list of DECLSPEC-objects, and a documentation string, or NIL if none is present in BODY.
Side-effects: Adds references of the created DECLSPEC-objects to the DECLSPEC-slots of variables or functions in LEXICAL-NAMESPACE and FREE-NAMESPACE. Creates yet unknown free variables and functions, adds references to the created DECLSPEC-objects, and adds the NSO-objects to FREE-NAMESPACE."
  (let ((documentation nil)
	(declspecs nil))
    (loop do
	 (multiple-value-bind (body-rest declspecs1) (parse-declaration-in-body body lexical-namespace free-namespace parent :customparsedeclspecp-function customparsedeclspecp-function :customparsedeclspec-function customparsedeclspec-function)
	   (setf declspecs (nconc declspecs declspecs1))
	   (cond
	     ((and (consp body) (stringp (car body-rest)))
	      (if documentation
		  (error "Two documentation strings ~S and ~S present, but only one allowed" documentation (car body-rest))
		  (setf documentation (car body-rest)))
	      (setf body (cdr body-rest)))
	     (t
	      (return (values body-rest declspecs documentation))))))))

(let ((lexical-namespace (make-empty-lexical-namespace))
      (free-namespace (make-empty-free-namespace)))
  (multiple-value-bind (body declspecs documentation) (parse-declaration-and-documentation-in-body '((declare (type number a)) "doc" (declare (type fixnum a)) 5) lexical-namespace free-namespace nil)
    (assert (and (equal body '(5)) (typep (car declspecs) 'declspec-type) (typep (cadr declspecs) 'declspec-type) (equal documentation "doc")))))

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

;;;; LAMBDA LISTS
;; see CLHS 3.4 Lambda Lists

(defclass argument ()
  ((parent :initarg :parent :accessor argument-parent :type functiondef)
   (var :initarg :var :accessor argument-var :type var)))
(defclass required-argument (argument)
  ())
(defclass optional-argument (argument)
  ((init :initarg :init :accessor argument-init :type (or null generalform) :documentation "NIL means it was not specified, and then the parser does not assign a default initial form. (e.g. DEFTYPE would have * by default instead of NIL, but it's not the parser's job to define semantics.)")
   (suppliedp :initarg :suppliedp :accessor argument-suppliedp :type (or null var) :documentation "NIL means not present")))
(defclass key-argument (optional-argument)
  ((keywordp :initarg :keywordp :accessor argument-keywordp :type boolean :documentation "NIL means not present")
   (keyword :initarg :keyword :accessor argument-keyword :type symbol :documentation "has no meaning if KEYWORDP==NIL")))
(defclass aux-argument (argument)
  ((init :initarg :init :accessor argument-init :type (or null generalform))))
(defclass llist ()
  ((parent :initarg :parent :accessor form-parent)))
(defclass ordinary-llist (llist)
  ((required :initarg :required :accessor llist-required :type list :documentation "list, with each element of type REQUIRED-ARGUMENT")
   (optional :initarg :optional :accessor llist-optional :type list :documentation "list, with each element of type OPTIONAL-ARGUMENT")
   (rest :initarg :rest :accessor llist-rest :type (or null argument))
   (key :initarg :key :accessor llist-key :type list :documentation "list, with each element of type KEY-ARGUMENT")
   (allow-other-keys :initarg :allow-other-keys :accessor llist-allow-other-keys :type boolean)
   (aux :initarg :aux :accessor llist-aux :type list :documentation "list, with each element of type AUX-ARGUMENT")))
(defclass macro-llist (llist)
  ((whole :initarg :whole :accessor llist-whole :type (or null argument llist))
   (environment :initarg :environment :accessor llist-environment :type (or null argument))
   (required :initarg :required :accessor llist-required :type list :documentation "list, with each element of type ARGUMENT, or LLIST (for macro-lambda-lists)")
   (optional :initarg :optional :accessor llist-optional :type list :documentation "list, with each element of type OPTIONAL-ARGUMENT, or LLIST (for macro-lambda-lists)")
   (rest :initarg :rest :accessor llist-rest :type (or null argument llist))
   (body :initarg :body :accessor llist-body :type (or null argument llist))
   (key :initarg :key :accessor llist-key :type list :documentation "list, with each element of type KEY-ARGUMENT, or LLIST (for macro-lambda-lists)")
   (allow-other-keys :initarg :allow-other-keys :accessor llist-allow-other-keys :type boolean)
   (aux :initarg :aux :accessor llist-aux :type list :documentation "list, with each element of type AUX-ARGUMENT")))

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

;;Note that passing BLOCKS is not necessary here because REPARSE-FUNCTION has captured BLOCKS, and BLOCKS is not modified in #'PARSE-LAMBDA-LIST: parsing e.g. "(BLOCK TEST (FLET ((F (&OPTIONAL (A (RETURN-FROM TEST 4))) A)) (F)))" works.
(defun parse-lambda-list (lambda-list lexical-namespace free-namespace parent reparse-function &key (allow-macro-lambda-list nil))
  "Returns two values: an instance of type LLIST (representing the parsed LAMBDA-LIST) and the LEXICAL-NAMESPACE augmented by the variables created by the arguments in LAMBDA-LIST.
Supported lambda list keywords:
CLHS Figure 3-12. Standardized Operators that use Ordinary Lambda Lists: An ordinary lambda list can contain the lambda list keywords shown in the next figure. &allow-other-keys &key &rest &aux &optional
CLHS Figure 3-18. Lambda List Keywords used by Macro Lambda Lists: A macro lambda list can contain the lambda list keywords shown in the next figure. &allow-other-keys &environment &rest &aux &key &whole &body &optional"
  ;; TODO: come up with a scheme for this function so that it is easily extensible with other types of lambda lists, or replacable (partly or wholly) by user-supplied parsing code.
  ;; TODO: rewrite so that parsing the following is possible: Allow user-defined Lambda Lists (i.e. user-defined subclasses of 'llist), 3.4.1 Ordinary Lambda Lists, 3.4.2 Generic Function Lambda Lists, 3.4.3 Specialized Lambda Lists, 3.4.4 Macro Lambda Lists, 3.4.5 Destructuring Lambda Lists, 3.4.6 Boa Lambda Lists, 3.4.7 Defsetf Lambda Lists, 3.4.8 Deftype Lambda Lists, 3.4.9 Define-modify-macro Lambda Lists, 3.4.10 Define-method-combination Arguments Lambda Lists
  ;; "CLHS 3.4.1 Ordinary Lambda Lists" says: "An init-form can be any form. Whenever any init-form is evaluated for any parameter specifier, that form may refer to any parameter variable to the left of the specifier in which the init-form appears, including any supplied-p-parameter variables, and may rely on the fact that no other parameter variable has yet been bound (including its own parameter variable)." But luckily the order of allowed keywords is fixed for both normal lambda-lists and macro lambda-lists, and a normal lambda-list allows a subset of macro lambda-lists. Maybe I'll have to rewrite this for other lambda-lists.
  (let* ((llist-type (if allow-macro-lambda-list 'macro-llist 'ordinary-llist)) ;TODO: when making this function modular: probably allow passing LLIST-TYPE.
	 (new-llist (make-instance llist-type :parent parent)))
    (labels ((add-argument (varname)
	       (let* ((new-argument (make-instance 'argument :parent new-llist))
		      (new-var (make-instance 'var :name varname :freep nil :definition new-argument :declspecs nil)))
		 (setf (argument-var new-argument) new-var)
		 (setf lexical-namespace (augment-lexical-namespace new-var lexical-namespace))
		 new-argument))
	     (add-required-argument (varname)
	       (let* ((new-argument (make-instance 'required-argument :parent new-llist))
		      (new-var (make-instance 'var :name varname :freep nil :definition new-argument :declspecs nil)))
		 (setf (argument-var new-argument) new-var)
		 (setf lexical-namespace (augment-lexical-namespace new-var lexical-namespace))
		 new-argument))
	     (add-optional-argument (varname init-form-p init-form suppliedp suppliedp-name)
	       "If INIT-FORM is not given for the &OPTIONAL argument, pass NIL for it. If SUPPLIEDP is non-NIL, INIT-FORM must be non-NIL as well."
	       (assert (if suppliedp (not (null init-form)) t))
	       (let* ((new-argument (make-instance 'optional-argument :parent new-llist))
		      (new-var (make-instance 'var :name varname :freep nil :definition new-argument :declspecs nil)))
		 (setf (argument-var new-argument) new-var)
		 (setf lexical-namespace (augment-lexical-namespace new-var lexical-namespace))
		 (let ((parsed-init-form (if init-form-p (funcall reparse-function init-form new-argument :lexical-namespace lexical-namespace) nil)))
		   (setf (argument-init new-argument) parsed-init-form)
		   (let ((new-supplied-var (if suppliedp (make-instance 'var :name suppliedp-name :freep nil :definition new-argument :declspecs nil) nil)))
		     (setf (argument-suppliedp new-argument) new-supplied-var)
		     (when new-supplied-var
		       (setf lexical-namespace (augment-lexical-namespace new-supplied-var lexical-namespace)))
		     new-argument))))
	     (add-key-argument (keywordp keyword varname init-form-p init-form suppliedp suppliedp-name)
	       "If KEYWORDNAME or INIT-FORM is not given for the &KEY argument, pass NIL for it. If SUPPLIEDP is non-NIL, INIT-FORM must be non-NIL as well."
	       (assert (if suppliedp (not (null init-form)) t))
	       (let* ((new-argument (make-instance 'key-argument :parent new-llist :keywordp keywordp :keyword keyword))
		      (new-var (make-instance 'var :name varname :freep nil :definition new-argument :declspecs nil)))
		 (setf (argument-var new-argument) new-var)
		 (setf lexical-namespace (augment-lexical-namespace new-var lexical-namespace))
		 (let ((parsed-init-form (if init-form-p (funcall reparse-function init-form new-argument :lexical-namespace lexical-namespace) nil)))
		   (setf (argument-init new-argument) parsed-init-form)
		   (let ((new-supplied-var (if suppliedp (make-instance 'var :name suppliedp-name :freep nil :definition new-argument :declspecs nil) nil)))
		     (setf (argument-suppliedp new-argument) new-supplied-var)
		     (when new-supplied-var
		       (setf lexical-namespace (augment-lexical-namespace new-supplied-var lexical-namespace)))
		     new-argument))))
	     (add-aux-argument (varname init-form-p init-form)
	       "If INIT-FORM is not given for the &KEY argument, pass NIL for it."
	       (let* ((new-argument (make-instance 'aux-argument :parent new-llist))
		      (new-var (make-instance 'var :name varname :freep nil :definition new-argument :declspecs nil)))
		 (setf (argument-var new-argument) new-var)
		 (setf lexical-namespace (augment-lexical-namespace new-var lexical-namespace))
		 (let ((parsed-init-form (if init-form-p (funcall reparse-function init-form new-argument :lexical-namespace lexical-namespace) nil)))
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
			   (multiple-value-bind (parsed-llist new-lexical-namespace)
			       (parse-lambda-list head lexical-namespace free-namespace new-llist reparse-function :allow-macro-lambda-list allow-macro-lambda-list)
			     (setf lexical-namespace new-lexical-namespace)
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
    (values new-llist lexical-namespace)))

(defun parse-ordinary-lambda-list (lambda-list lexical-namespace free-namespace parent reparse-function)
  (parse-lambda-list lambda-list lexical-namespace free-namespace parent reparse-function :allow-macro-lambda-list nil))

(defun parse-macro-lambda-list (lambda-list lexical-namespace free-namespace parent reparse-function)
  (parse-lambda-list lambda-list lexical-namespace free-namespace parent reparse-function :allow-macro-lambda-list t))

;;TODO: add some test cases, e.g. (let ((lexical-namespace (make-empty-lexical-namespace)) (free-namespace (make-empty-free-namespace))) (parse-lambda-list '(a &optional (b 1 bp) &rest r &key ((:cdddd c) 1 cp) &aux d) lexical-namespace free-namespace nil (lambda (form &rest r) (declare (ignore r)) form) :allow-macro-lambda-list nil))

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
(defclass body-form ()
  ((body :initarg :body :accessor form-body :type list :documentation "list of GENERALFORMs"))
  (:documentation "Note: objects of this type must never be created, only subtypes of this type."))
(defclass special-form (form)
  ())
(defclass progn-form (special-form body-form)
  ())
(defclass binding ()
  ((parent :initarg :parent :accessor binding-parent :documentation "the LET-form in which the binding is defined.")
   (sym :initarg :sym :accessor binding-sym :type sym)))
(defclass var-binding (binding)
  ((value :initarg :value :accessor binding-value :documentation "The form initializing the variable." :type generalform)))
(defclass bindings-form ()
  ((bindings :initarg :bindings :accessor form-bindings :type list :documentation "list of VAR-BINDINGs, or FUN-BINDINGs")
   (declspecs :initarg :declspecs :accessor form-declspecs :type list :documentation "list of DECLSPECs"))
  (:documentation "Note: objects of this type must never be created, only subtypes of this type."))
(defclass let-form (special-form body-form bindings-form)
  ())
(defclass let*-form (special-form body-form bindings-form)
  ())
(defclass functiondef (body-form)
  ((parent :initarg :parent :accessor form-parent)
   (llist :initarg :llist :accessor form-llist :type llist)
   (declspecs :initarg :declspecs :accessor form-declspecs :type list)
   (documentation :initarg :documentation :accessor form-documentation :type (or nil string)))
  (:documentation "Used to define a function without name."))
(defclass block-naming-form ()
  ((blo :initarg :blo :accessor form-blo :type blo))
  (:documentation "Note: objects of this type must never be created, only subtypes of this type."))
(defclass block-form (special-form body-form block-naming-form)
  ())
(defclass fun-binding (binding functiondef block-naming-form)
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
  ((vars :initarg :vars :accessor form-vars :type list :documentation "list of VARs")
   (values :initarg :values :accessor form-values :type list :documentation "list of GENERALFORMs")))
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
   (arguments :initarg :arguments :accessor form-arguments :type list :documentation "list of GENERALFORMs")))
(defclass macroapplication-form (application-form)
  ((lexicalnamespace :initarg :lexicalnamespace :accessor form-lexicalnamespace :type lexicalnamespace :documentation "The lexical namespace at the macro application form")
   (freenamespace :initarg :freenamespace :accessor form-freenamespace :type freenamespace :documentation "The free namespace at the macro application form")))

;; TODO: In the following functions, wherever (form-body ...) is printed, print "BODY:" before the list so that the user knows that the upper-most list printed is because BODY is a list (and she doesn't think its a function application).
(defmethod print-object ((object progn-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (form-body object))))
(defmethod print-object ((object var-binding) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when *print-detailed-walker-objects*
      (format stream "~S ~S" (binding-sym object) (binding-value object)))))
(defun format-body (object declspecsp documentationp)
  (labels ((rec (list)
	     (if (null list)
		 ""
		 (let ((out (format nil "~S" (car list))))
		   (loop for form in (cdr list) do
			(setf out (concatenate 'string out (format nil " ~S" form))))
		   out))))
    (if (and documentationp (form-documentation object))
	(if (or (null declspecsp) (null (form-declspecs object)))
	    (rec (cons (form-documentation object) (form-body object)))
	    (rec (cons (form-documentation object) (cons (cons 'declare (form-declspecs object)) (form-body object)))))
	(if (or (null declspecsp) (null (form-declspecs object)))
	    (rec (form-body object))
	    (rec (cons (cons 'declare (form-declspecs object)) (form-body object)))))))
(defmethod print-object ((object bindings-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if *print-detailed-walker-objects*
	(format stream "BINDINGS:~S ~A" (form-bindings object) (format-body object t nil))
	(format stream "~S" (form-body object)))))
(defmethod print-object ((object fun-binding) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when *print-detailed-walker-objects*
      (format stream "~S ~S ~A" (binding-sym object) (form-llist object) (format-body object t t)))))
(defmethod print-object ((object lambda-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~A" (form-llist object) (format-body object t t))))
(defmethod print-object ((object block-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (form-blo object) (form-body object))))
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
(defmethod print-object ((object macroapplication-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (form-fun object))
    (loop for arg in (form-arguments object) do
	 (format stream " ~S" arg))
    (format stream " ~S ~S" (form-lexicalnamespace object) (form-freenamespace object))))

(defun parse-and-set-functiondef (form lexical-namespace free-namespace current-functiondef &key customparsep-function customparse-function customparsedeclspecp-function customparsedeclspec-function)
  "Parse FORM, which must be of the form (LAMBDA-LIST &BODY BODY) and set the slots of the CURRENT-FUNCTIONDEF-object to the parsed values.
Side-effects: Creates yet unknown free variables and functions and add them to FREE-NAMESPACE."
  (flet ((reparse (form parent &key (lexical-namespace lexical-namespace))
	   (parse form lexical-namespace free-namespace parent
		  :customparsep-function customparsep-function
		  :customparse-function customparse-function
		  :customparsedeclspecp-function customparsedeclspecp-function
		  :customparsedeclspec-function customparsedeclspec-function))
	 (split-lambda-list-and-body (form)
	   (assert (and (consp form) (listp (car form))) () "Invalid lambda-list ~S" form)
	   (let ((lambda-list (car form))
		 (body (cdr form)))
	     (assert (listp body) () "Invalid body ~S; must be a list" body)
	     (values lambda-list body))))
    (multiple-value-bind (lambda-list body) (split-lambda-list-and-body form)
      (multiple-value-bind (new-llist lexical-namespace-in-functiondef)
	  (parse-lambda-list lambda-list lexical-namespace free-namespace current-functiondef #'reparse :allow-macro-lambda-list nil)
	(setf (form-llist current-functiondef) new-llist)
	(multiple-value-bind (body parsed-declspecs parsed-documentation)
	    (parse-declaration-and-documentation-in-body body lexical-namespace-in-functiondef free-namespace current-functiondef :customparsedeclspecp-function customparsedeclspecp-function :customparsedeclspec-function customparsedeclspec-function)
	  (setf (form-declspecs current-functiondef) parsed-declspecs)
	  (setf (form-documentation current-functiondef) parsed-documentation)
	  (assert (proper-list-p body) () "Not a proper list: ~S" body)
	  (let ((parsed-body (loop for form in body collect (reparse form current-functiondef :lexical-namespace lexical-namespace-in-functiondef))))
	    (setf (form-body current-functiondef) parsed-body))))
      current-functiondef)))

;; TODO: maybe rename to PARSE-FORM.
;; TODO: FIXME: distinguish in all ASSERTs and ERRORs (in all functions, especially the #'PARSE functions) between errors that are recognized as syntax errors because the input form is impossible in Common Lisp, and errors that are due to me having made programming mistakes.
;; TODO: FIXME: Must handle arbitrary nonsense input gracefully. (see comment near #'PROPER-LIST-P.)
(defun parse (form lexical-namespace free-namespace parent &key customparsep-function customparse-function customparsedeclspecp-function customparsedeclspec-function)
  (declare (optimize (debug 3)))
  (labels ((reparse (form parent &key (lexical-namespace lexical-namespace))
	     (parse form lexical-namespace free-namespace parent
		    :customparsep-function customparsep-function
		    :customparse-function customparse-function
		    :customparsedeclspecp-function customparsedeclspecp-function
		    :customparsedeclspec-function customparsedeclspec-function))
	   (parse-body (body current &key (lexical-namespace lexical-namespace))
	     (assert (proper-list-p body) () "Body is not a proper list: ~S" body)
	     (loop for form in body collect (reparse form current :lexical-namespace lexical-namespace))))
    (cond
      ((and (not (null customparsep-function)) (funcall customparsep-function form lexical-namespace free-namespace parent))
       (funcall customparse-function form lexical-namespace free-namespace parent))
      ((or (eq form nil) (eq form t))
       (make-instance 'selfevalobject :object form))
      ((symbolp form)
       (namespace-lookup/create 'var form lexical-namespace free-namespace))
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
		 (namespace-lookup/create 'fun name-form lexical-namespace free-namespace))
		((and (consp name-form) (eq (car name-form) 'lambda))
		 (reparse name-form parent))
		(t (error "Invalid FUNCTION-form ~S" form)))))
	   ((eq head 'progn)
	    (let* ((current (make-instance 'progn-form :parent parent :body nil))
		   (body rest)
		   (parsed-body (parse-body body current)))
	      (setf (form-body current) parsed-body)
	      current))
	   ;; TODO: FIXME: maybe I have to handle something specially in FLET and LABELS. The CLHS on FLET and LABELS says: "Also, within the scope of flet, global setf expander definitions of the function-name defined by flet do not apply. Note that this applies to (defsetf f ...), not (defmethod (setf f) ...)." What does that mean?
	   ((find head '(let let* flet labels))
	    (assert (and (consp rest) (listp (car rest))) () "cannot parse ~S-form:~%~S" head form)
	    (let* ((definitions (car rest))
		   (body (cdr rest))
		   (form-type (ecase head ((let) 'let-form) ((let*) 'let*-form) ((flet) 'flet-form) ((labels) 'labels-form)))
		   (current (make-instance form-type :parent parent :body nil))) ;:BINDINGS and :DECLSPECS are defined below
	      (assert (proper-list-p definitions) () "Not a proper list: ~S" definitions)
	      (labels ((make-var-binding (def lexical-namespace)
			 (assert (and (consp def) (symbolp (car def)) (not (null (car def))) (not (null (cdr def))) (null (cddr def))) () "cannot parse definition in ~S-form:~%~S" head def)
			 (let* ((name (car def))
				(value-form (cadr def))
				(binding (make-instance 'var-binding :parent current))
				(parsed-value (reparse value-form binding :lexical-namespace lexical-namespace))
				(sym (make-instance 'var :name name :freep nil :definition binding :declspecs nil)))
			   (setf (binding-sym binding) sym) (setf (binding-value binding) parsed-value)
			   binding))
		       (make-fun-binding (def lexical-namespace)
			 (assert (and (consp def) (valid-function-name-p (car def)) (not (null (cdr def)))) () "cannot parse definition in ~S-form:~%~S" head def)
			 (multiple-value-bind (fun-type block-name) (valid-function-name-p (car def)) ;CLHS Glossary function block name: If the function name is a list whose car is setf and whose cadr is a symbol, its function block name is the symbol that is the cadr of the function name.
			   (declare (ignore fun-type))
			   (let* ((name (car def))
				  (body-form (cdr def))
				  (blo (make-instance 'blo :name block-name :freep nil))
				  (binding (make-instance 'fun-binding :parent current :blo blo))
				  (sym (make-instance 'fun :name name :freep nil :definition binding :declspecs nil)))
			     (parse-and-set-functiondef body-form (augment-lexical-namespace blo lexical-namespace) free-namespace binding :customparsep-function customparsep-function :customparse-function customparse-function :customparsedeclspecp-function customparsedeclspecp-function :customparsedeclspec-function customparsedeclspec-function)
			     (setf (binding-sym binding) sym)
			     (setf (nso-definition blo) binding)
			     binding))))
		(multiple-value-bind (parsed-bindings new-lexical-namespace)
		    (let ((parse-value-function (ecase head ((let let*) #'make-var-binding) ((flet labels) #'make-fun-binding))))
		      (cond
			((find head '(let flet))
			 (let* ((parsed-bindings (loop for def in definitions collect (funcall parse-value-function def lexical-namespace)))
				(parsed-syms (loop for binding in parsed-bindings collect (binding-sym binding)))
				(new-lexical-namespace lexical-namespace))
			   (loop for sym in parsed-syms do (setf new-lexical-namespace (augment-lexical-namespace sym new-lexical-namespace)))
			   (values parsed-bindings new-lexical-namespace)))
			((find head '(let* labels))
			 (let* ((new-lexical-namespace lexical-namespace)
				(parsed-bindings (loop for def in definitions collect
						      (let* ((parsed-binding (funcall parse-value-function def new-lexical-namespace))
							     (parsed-sym (binding-sym parsed-binding)))
							(setf new-lexical-namespace (augment-lexical-namespace parsed-sym new-lexical-namespace))
							parsed-binding))))
			   (values parsed-bindings new-lexical-namespace)))
			(t (error "unknown HEAD"))))
		  (multiple-value-bind (body parsed-declspecs)
		      (parse-declaration-in-body body new-lexical-namespace free-namespace current :customparsedeclspecp-function customparsedeclspecp-function :customparsedeclspec-function customparsedeclspec-function)
		    (setf (form-bindings current) parsed-bindings)
		    (setf (form-declspecs current) parsed-declspecs)
		    (setf (form-body current) (parse-body body current :lexical-namespace new-lexical-namespace))
		    current)))))
	   ((eq head 'lambda)
	    (let* ((lambda-list-and-body (cdr form))
		   (current (make-instance 'lambda-form)))
	      (parse-and-set-functiondef lambda-list-and-body lexical-namespace free-namespace current :customparsep-function customparsep-function :customparse-function customparse-function :customparsedeclspecp-function customparsedeclspecp-function :customparsedeclspec-function customparsedeclspec-function)
	      current))
	   ((eq head 'block)
	    (assert (and (consp rest) (symbolp (car rest)) (listp (cdr rest))) () "Cannot parse BLOCK-form ~S" form)
	    (let* ((name (car rest))
		   (body (cdr rest))
		   (blo (make-instance 'blo :name name :freep nil))
		   (current (make-instance 'block-form :parent parent :blo blo))
		   (parsed-body (parse-body body current :lexical-namespace (augment-lexical-namespace blo lexical-namespace))))
	      (setf (nso-definition blo) current)
	      (setf (form-body current) parsed-body)
	      current))
	   ((eq head 'return-from)
	    (assert (and (consp rest) (symbolp (car rest)) (or (null (cdr rest)) (and (consp (cdr rest)) (null (cddr rest))))) () "Cannot parse RETURN-FROM-form ~S" form)
	    (let* ((name (car rest))
		   (value-form (if (null (cdr rest)) nil (cadr rest)))
		   (blo (namespace-lookup/create 'blo name lexical-namespace free-namespace))
		   (current (make-instance 'return-from-form :parent parent :blo blo))
		   (parsed-value (reparse value-form current)))
	      (setf (form-value current) parsed-value)
	      current))
	   ((eq head 'locally)
	    (assert (and (consp rest) (consp (car rest))) () "Cannot parse LOCALLY-form ~S" form)
	    (let ((body rest)
		  (current (make-instance 'locally-form :parent parent)))
	      (multiple-value-bind (body parsed-declspecs)
		  (parse-declaration-in-body body lexical-namespace free-namespace current :customparsedeclspecp-function customparsedeclspecp-function :customparsedeclspec-function customparsedeclspec-function)
		(setf (form-declspecs current) parsed-declspecs)
		(setf (form-body current) (parse-body body current)))
	      current))
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
			  (var (namespace-lookup/create 'var name lexical-namespace free-namespace))
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
		   (parsed-body (parse-body body current)))
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
		   (parsed-body (parse-body body current)))
	      (setf (form-body current) parsed-body)
	      current))
	   ((eq head 'load-time-value)
	    (assert (and (consp rest) (or (null (cdr rest)) (and (consp (cdr rest)) (null (cddr rest))))) () "Cannot parse LOAD-TIME-VALUE-form ~S" form)
	    (let* ((value-form (car rest))
		   (readonly (cadr rest)))
	      (assert (position (cadr rest) '(nil t)) () "READ-ONLY-P in LOAD-TIME-VALUE-form ~S must be either NIL or T, but is ~S" form readonly)
	      (let* ((current (make-instance 'load-time-value-form :parent parent :readonly readonly))
		     (parsed-value (reparse value-form current :lexical-namespace (make-empty-lexical-namespace)))) ;Note that dynamic variables must be parsed: in the form (LOAD-TIME-VALUE *A*), *A* must refer to the global *A*.
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
		   (parsed-body (parse-body body current)))
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
		   (parsed-body (parse-body body current)))
	      (setf (form-symbols current) parsed-symbols (form-values current) parsed-values (form-body current) parsed-body)
	      current))
	   ((eq head 'unwind-protect)
	    (assert (consp rest) () "Cannot parse UNWIND-PROTECT-form ~S" form)
	    (let* ((protected-form (car rest))
		   (cleanup-body (cdr rest))
		   (current (make-instance 'unwind-protect-form :parent parent))
		   (parsed-protected (reparse protected-form current))
		   (parsed-cleanup-body (parse-body cleanup-body current)))
	      (setf (form-protected current) parsed-protected (form-body current) parsed-cleanup-body)
	      current))
	   ((find head '(go macrolet symbol-macrolet tagbody)) ;TODO: TAGBODY, SYMBOL-MACROLET and MACROLET are tricky to implement, see their CLHS and below.
	    (error "parsing special form ~S not implemented yet" head))
	   (t
	    (assert (symbolp head) () "Invalid function application ~S" form)
	    (let* ((fun-name head)
		   (arg-forms rest)
		   (fun (namespace-lookup/create 'fun fun-name lexical-namespace free-namespace))
		   (macrop (nso-macrop fun))
		   (current (if macrop
				(make-instance 'macroapplication-form :parent parent :fun fun :lexicalnamespace lexical-namespace :freenamespace free-namespace)
				(make-instance 'application-form :parent parent :fun fun)))
		   (parsed-arguments nil))
	      (loop do
		   (when (null arg-forms) (return))
		   (assert (and (consp arg-forms) (listp (cdr arg-forms))) () "Invalid argument rest ~S in function application" arg-forms)
		   (push (let ((arg-form (car arg-forms)))
			   (if macrop arg-form (reparse arg-form current)))
			 parsed-arguments)
		   (setf arg-forms (cdr arg-forms)))
	      (setf (form-arguments current) (nreverse parsed-arguments))
	      current))))))))

(defun parse-with-empty-namespaces (form &key customparsep-function customparse-function customparsedeclspecp-function customparsedeclspec-function free-common-lisp-namespace)
  "If FREE-COMMON-LISP-NAMESPACE is non-NIL, a free namespace created by #'MAKE-DEFAULT-FREE-COMMON-LISP-NAMESPACE is used."
  (let ((lexical-namespace (make-empty-lexical-namespace))
	(free-namespace (if free-common-lisp-namespace (make-default-free-common-lisp-namespace) (make-empty-free-namespace))))
    (parse form lexical-namespace free-namespace nil
	   :customparsep-function customparsep-function
	   :customparse-function customparse-function
	   :customparsedeclspecp-function customparsedeclspecp-function
	   :customparsedeclspec-function customparsedeclspec-function)))

;; tests for PARSE
(defun namespaces-at (form heresymbol)
  "Parse FORM and collect the namespaces at the positions in FORM marked by the symbol HERESYMBOL.
Returns two values: a list containing the lexical namespaces, and a list containing the free namespaces."
  (let* ((lexical-namespace-here-list nil)
	 (free-namespace-here-list nil))
    (parse-with-empty-namespaces form
				 :customparsep-function (lambda (form lexical-namespace free-namespace parent)
							  (declare (ignore lexical-namespace free-namespace parent))
							  (eq form heresymbol))
				 :customparse-function (lambda (form lexical-namespace free-namespace parent)
							 (declare (ignorable form parent))
							 (push lexical-namespace lexical-namespace-here-list)
							 (push free-namespace free-namespace-here-list)))
    (values (nreverse lexical-namespace-here-list) (nreverse free-namespace-here-list))))

(defun test-parse-symbol-reference ()
  (declare (optimize (debug 3)))
  (flet ((namespaces-at (form)
	   (namespaces-at form '-here-)))
    (assert (nso-freep (parse-with-empty-namespaces 'a)))
    (assert (nso-freep (parse-with-empty-namespaces '(function a))))
    (assert (not (nso-freep (car (form-body (parse-with-empty-namespaces '(let ((a 1)) a)))))))
    (assert (not (nso-freep (car (form-body (parse-with-empty-namespaces '(let* ((a 1)) a)))))))
    (assert (not (nso-freep (car (form-body (parse-with-empty-namespaces '(flet ((a ())) #'a)))))))
    (assert (not (nso-freep (car (form-body (parse-with-empty-namespaces '(labels ((a ())) #'a)))))))
    (assert (let* ((lexical-namespace (car (namespaces-at '(let ((b 5) (c b)) -here-))))
		   (b-sym (namespace-lookup 'var 'b lexical-namespace))
		   (c-sym (namespace-lookup 'var 'c lexical-namespace)))
	      (not (eq b-sym (binding-value (nso-definition c-sym))))))
    (assert (let* ((lexical-namespace (car (namespaces-at '(flet ((b ()) (c () #'b)) -here-))))
		   (b-sym (namespace-lookup 'fun 'b lexical-namespace))
		   (c-sym (namespace-lookup 'fun 'c lexical-namespace)))
	      (not (eq b-sym (car (form-body (nso-definition c-sym)))))))
    (assert (let* ((lexical-namespace (car (namespaces-at '(let* ((b 5) (c b)) -here-))))
		   (b-sym (namespace-lookup 'var 'b lexical-namespace))
		   (c-sym (namespace-lookup 'var 'c lexical-namespace)))
	      (eq b-sym (binding-value (nso-definition c-sym)))))
    (assert (let* ((lexical-namespace (car (namespaces-at '(labels ((b ()) (c () #'b)) -here-))))
		   (b-sym (namespace-lookup 'fun 'b lexical-namespace))
		   (c-sym (namespace-lookup 'fun 'c lexical-namespace)))
	      (eq b-sym (car (form-body (nso-definition c-sym))))))
    (let* ((lexical-namespaces (namespaces-at '(let ((b 5)) -here- (let ((b b)) -here-))))
	   (lexical-namespace-1 (car lexical-namespaces))
	   (lexical-namespace-2 (cadr lexical-namespaces))
	   (b-1 (namespace-lookup 'var 'b lexical-namespace-1))
	   (b-2 (namespace-lookup 'var 'b lexical-namespace-2)))
      (assert (eq (selfevalobject-object (binding-value (nso-definition b-1))) 5))
      (assert (eq b-1 (binding-value (nso-definition b-2))))
      (assert (not (eq b-1 b-2))))
    (let* ((lexical-namespaces (namespaces-at '(let* ((b 5)) -here- (let* ((b b)) -here-))))
	   (lexical-namespace-1 (car lexical-namespaces))
	   (lexical-namespace-2 (cadr lexical-namespaces))
	   (b-1 (namespace-lookup 'var 'b lexical-namespace-1))
	   (b-2 (namespace-lookup 'var 'b lexical-namespace-2)))
      (assert (eq (selfevalobject-object (binding-value (nso-definition b-1))) 5))
      (assert (eq b-1 (binding-value (nso-definition b-2))))
      (assert (not (eq b-1 b-2))))
    (let* ((lexical-namespaces (namespaces-at '(flet ((b ())) -here- (flet ((b () #'b)) -here-))))
	   (lexical-namespace-1 (car lexical-namespaces))
	   (lexical-namespace-2 (cadr lexical-namespaces))
	   (b-1 (namespace-lookup 'fun 'b lexical-namespace-1))
	   (b-2 (namespace-lookup 'fun 'b lexical-namespace-2)))
      (assert (not (eq b-1 b-2)))
      (assert (eq b-1 (car (form-body (nso-definition b-2))))))
    (let* ((lexical-namespaces (namespaces-at '(labels ((b ())) -here- (labels ((b () #'b)) -here-))))
	   (lexical-namespace-1 (car lexical-namespaces))
	   (lexical-namespace-2 (cadr lexical-namespaces))
	   (b-1 (namespace-lookup 'fun 'b lexical-namespace-1))
	   (b-2 (namespace-lookup 'fun 'b lexical-namespace-2)))
      (assert (not (eq b-1 b-2)))
      (assert (eq b-1 (car (form-body (nso-definition b-2)))))))
  (let* ((ast (parse-with-empty-namespaces '(test #'test 2 3)))
	 (call-fun (form-fun ast))
	 (call-arguments (form-arguments ast)))
    (assert (eq call-fun (car call-arguments))))
  (let* ((ast (parse-with-empty-namespaces '(bla (aref a x) x)))
  	 (args (form-arguments ast))
  	 (aref-x (cadr (form-arguments (car args))))
  	 (arg-x (cadr args)))
    (assert (eq aref-x arg-x))))
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
  (flet ((namespaces-at (form)
	   (namespaces-at form '-here-)))
    (let* ((lexical-namespaces (namespaces-at '(let ((a 1)) -here- (flet ((test (a &optional (b (progn -here- a))) -here-)) -here-))))
	   (a1-sym (namespace-lookup 'var 'a (car lexical-namespaces)))
	   (a2-sym (namespace-lookup 'var 'a (cadr lexical-namespaces)))
	   (a3-sym (namespace-lookup 'var 'a (caddr lexical-namespaces)))
	   (a4-sym (namespace-lookup 'var 'a (cadddr lexical-namespaces))))
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

(defun test-parent ()
  (declare (optimize (debug 3)))
  (let* ((form '(lambda () (funcall bla)))
	 (ast (parse-with-empty-namespaces form))
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
	 (ast (parse-with-empty-namespaces form))
	 (progn-body (form-body ast))
	 (a-bla1 (car (form-arguments (car progn-body))))
	 (let-form (cadr progn-body))
	 (let-body (form-body let-form))
	 (a-let (binding-sym (car (form-bindings let-form))))
	 (a-bla2 (car (form-arguments (car let-body))))
	 (a-load-time-value (form-value (cadr let-body))))
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
	 (ast (parse-with-empty-namespaces form))
	 (quote-a (form-object ast)))
    (assert (symbolp quote-a))
    (assert (eq quote-a 'a))))
(test-quote-form)

(defun test-macro-application ()
  ;; Test that differentiating between function- and macro-applications works. Macro arguments must not be evaluated, but function arguments must. Otherwise, parsing a macro-call can fail with an incorrect error: (PARSE-WITH-EMPTY-NAMESPACES '(DEFMACRO BLA (A (IF S)) (PRINT (LIST A IF S)) NIL) :FREE-COMMON-LISP-NAMESPACE NIL), because (IF S) is parsed like an IF-form. Therefore a non-NIL flag MACROP in an instance of FUN, (or maybe a distinction between FUN and a new class MAC as subtypes of SYM), makes that if FUN-NAME is looked up as a macro, then the ARG-FORMS are not parsed at all.
  (parse-with-empty-namespaces '(defmacro bla (a (if s)) (print (list a if s)) nil)
			       :free-common-lisp-namespace t))
(test-macro-application)

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
