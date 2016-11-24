(defpackage :walker-plus
  (:documentation "Parsers and deparsers for some more forms.")
  (:use :cl)
  (:export
   ;; for classes: export the class and _all_ accessors on one line so that deleting a class doesn't have to consider all exports of other classes
   ;; FORMS
   :multiple-value-bind-form
   :values-form
   :nth-value-form
   :defun-form
   :declaim-form
   :funcall-form
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
   :deparse-typecase
   :deparse
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

(defun parse-p (form lexical-namespace free-namespace parent)
  (declare (ignore lexical-namespace free-namespace parent))
  (and (listp form)
       (let ((head (car form)))
	 (find head '(multiple-value-bind values nth-value defun declaim funcall)))))

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
	 (assert (and (consp rest) (listp (car rest)) (listp (cadr rest))) () "Cannot parse MULTIPLE-VALUE-BIND-form ~S" form)
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
		  (blo (make-instance 'walker:blo :name block-name :freep nil :jumpers nil))
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
	))))

;;;; DEPARSER

(defun deparse-multiple-value-bind-form (ast parent recurse-function)
  (declare (ignore parent))
  (list* 'multiple-value-bind
	 (mapcar (lambda (var) (funcall recurse-function var ast)) (walker:form-vars ast))
	 (funcall recurse-function (walker:form-values ast) ast)
	 (walker:deparse-body ast recurse-function t nil)))
(defun deparse-values-form (ast parent recurse-function)
  (declare (ignore parent))
  (list* 'values (walker:deparse-body ast recurse-function nil nil)))
(defun deparse-nth-value-form (ast parent recurse-function)
  (declare (ignore parent))
  (list* 'nth-value
	 (funcall recurse-function (walker:form-value ast) ast)
	 (funcall recurse-function (walker:form-values ast) ast)))
(defun deparse-defun-form (ast parent recurse-function)
  (declare (ignore parent))
  (list* 'defun
	 (funcall recurse-function (walker:form-sym ast) ast)
	 (walker:deparse-body ast recurse-function t t)))
(defun deparse-declaim-form (ast parent recurse-function)
  (declare (ignore parent))
  (list* 'declaim
	 (funcall recurse-function (walker:form-declspecs ast) ast)))
(defun deparse-funcall-form (ast parent recurse-function)
  (declare (ignore parent))
  (list* 'funcall
	 (funcall recurse-function (walker:form-var ast) ast)
	 (mapcar (lambda (arg) (funcall recurse-function arg ast)) (walker:form-arguments ast))))

(defun deparse-typecase (ast parent &key &allow-other-keys)
  (declare (ignore parent))
  (typecase ast
    (multiple-value-bind-form #'deparse-multiple-value-bind-form)
    (values-form #'deparse-values-form)
    (nth-value-form #'deparse-nth-value-form)
    (defun-form #'deparse-defun-form)
    (declaim-form #'deparse-declaim-form)
    (funcall-form #'deparse-funcall-form)
    (t nil)))

(defun deparse (ast parent)
  (declare (optimize (debug 3)))
  (if (deparse-typecase ast parent)
      (funcall (deparse-typecase ast parent) ast parent #'deparse)
      (walker:deparse ast parent :customdeparsep-function #'deparse-typecase :customdeparse-function #'deparse)))
