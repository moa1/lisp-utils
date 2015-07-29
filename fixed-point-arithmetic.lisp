;;;; Fixed-point implementation.

(defpackage :fixed-point-arithmetic
  (:nicknames :fixed)
  (:use :common-lisp)
  (:export :to-rational
	   :to-float
	   :*fraction-bits*
	   :*integer-bits*
	   :*fraction-rounding*
	   :to-fixed
	   :most-positive-fixed
	   :least-positive-fixed
	   :least-negative-fixed
	   :most-negative-fixed
	   :f+
	   :f-
	   :f*
	   :f/
	   :overflow))

(in-package :fixed-point-arithmetic)

;;;; I wish LISP would have parameterized types! This would simplify generating efficient code a lot, because we could put the number of fraction and integer bits into the type, and when emitting code to produce an operation check the type of the arguments and emit the code accordingly to the number of bits of the arguments and the result.

;;;; Type definition

(defclass fixed (standard-object)
  ((fraction-bits :initarg :fraction-bits :reader fixed-fraction-bits :type (and fixnum unsigned-byte) :documentation "The (read-only number) of bits of the number after the point. This is an unsigned value.")
   (number :initarg :number :accessor fixed-number :type integer :documentation "The number represented as the integer value, followed by FRACTION-BITS of the fractional value."))
  (:documentation "A fixed-point number, represented by an integer number."))

;; These declaims are to speed up SBCL that ignores the :type declarations in defclass.
(declaim (ftype (function (fixed) (and fixnum unsigned-byte)) fixed-fraction-bits))
(declaim (ftype (function (fixed) integer) fixed-number))
(declaim (ftype (function (fixed integer)) (setf fixed-number)))

#|
;; annotation class used to annotate fixed-point objects with 
(defclass annotation (standard-object)
  ((form :initarg :form) ;the form that is annotated (must have as value a FIXED).
   (fraction-bits :initarg :fraction-bits)))

(defmacro annotate (form &optional fraction-bits)
  "May be used to specify that the form FORM has FRACTION-bits fraction bits.
FRACTION-BITS may be NIL to indicate that the number of bits is unknown."
  (when (and (listp form) (eq (car form) 'to-fixed))
    (destructuring-bind (to-fixed the-number &optional the-fraction-bits the-rounding) form
      (declare (ignore to-fixed the-number the-rounding))
      (when (and (integerp the-fraction-bits) (>= the-fraction-bits 0))
	(setf fraction-bits the-fraction-bits))))
  (make-instance 'annotation :form form :fraction-bits fraction-bits))
|#

;;;; Helper functions

(defun signed-integer-range (bits)
  "Return as first/second value the lowest/highest number representable by a signed integer number with BITS bits."
  (declare (type (and unsigned-byte fixnum) bits))
  (if (= 0 bits)
      (values 0 0)
      (let ((bits-1 (1- bits)))
	(values (- (ash 1 bits-1))
		(1- (ash 1 bits-1))))))

(defun unsigned-integer-range (bits)
  "Return as first/second value the lowest/highest number representable by a unsigned integer number with BITS bits."
  (declare (type (and unsigned-byte fixnum) bits))
  (values 0 (1- (ash 1 bits))))

(defun to-rational (fixed)
  "Convert fixed-point number FIXED to a rational."
  (/ (fixed-number fixed) (ash 1 (fixed-fraction-bits fixed))))

(defun to-float (fixed)
  "Convert fixed-point number FIXED to a float."
  (float (to-rational fixed)))

(when (not (boundp '+print-radix-symbols+))
  (defconstant +print-radix-symbols+ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defun print-integer (integer &optional stream)
  (declare (type integer integer))
  (when (= integer 0)
    (princ "0" stream)
    (return-from print-integer))
  (when (< integer 0)
    (princ "-" stream))
  (do* ((i (abs integer)) (res nil)) ((= i 0) (loop for c in res do (princ (aref +print-radix-symbols+ c) stream)))
    (multiple-value-bind (a b) (floor i *print-base*)
      (setf i a)
      (push b res))))

(defvar *print-precision* 15 "NIL or an integer. An integer means the maximal number of digits to print when a fraction is printed. NIL means that an infinite number of digits is printed.")

(defun print-fraction (fraction &optional stream)
  (declare (type (or rational float) fraction))
  (assert (and (<= 0 fraction) (< fraction 1)))
  (princ "." stream)
  ;; TODO: add a variable *print-rounding* that controls how the last digit of the fraction is rounded, with possible values :truncate (truncate after *print-precision*), :truncate-indicate-rest (truncate after *print-precision* and print "..." if the rest is not 0) or :round (round last digit to the nearest possible digit).
  (if (= 0 fraction)
      (princ "0" stream)
      (do* ((i (* fraction *print-base*)) (n 0 (1+ n))) ((or (= i 0) (and (not (null *print-precision*)) (>= n *print-precision*))))
	(multiple-value-bind (a b) (floor (* i *print-base*) *print-base*)
	  (princ (aref +print-radix-symbols+ a) stream)
	  (setf i b)))))

(defun print-number (number &optional stream)
  (declare (type (or rational float) number))
  (when (< number 0)
    (princ "-" stream))
  (multiple-value-bind (integer fraction) (truncate number)
    (setf integer (abs integer))
    (setf fraction (abs fraction))
    (print-integer integer stream)
    (print-fraction fraction stream)))

(defmethod describe-object ((f fixed) stream)
  (let* ((fraction-bits (fixed-fraction-bits f))
	 (number (fixed-number f))
	 (denom (ash 1 fraction-bits)))
    (format stream "fixed-point Q*.~A number " fraction-bits)
    (multiple-value-bind (integer fraction) (truncate number denom)
      (print-number (+ integer (/ fraction denom)) stream))
    (format stream " (~A)~%" number)))

(defmethod print-object ((f fixed) stream)
  (let* ((fraction-bits (fixed-fraction-bits f))
	 (number (fixed-number f))
	 (denom (ash 1 fraction-bits)))
    (multiple-value-bind (integer fraction) (truncate number denom)
      (print-unreadable-object (f stream :type t)
	(format stream "Q*.~A " fraction-bits)
	(print-number (+ integer (/ fraction denom)) stream)))))

(defparameter *fraction-bits* 16 "The default number of bits for the fraction.")
(defparameter *integer-bits* 16 "The default number of integer bits.")
(defparameter *fraction-rounding* #'round "The default rounding function of fractional numbers.")

(defun to-fixed (number &optional (fraction-bits *fraction-bits*) (fraction-rounding *fraction-rounding*))
  "Convert NUMBER to a fixed-point number with FRACTION-BITS bits after the point and return it.
FRACTION-ROUNDING is a function that rounds the fractional number to the nearest representable number (for example, one of #'round, #'floor, #'ceiling).
Return as 2nd value the rest that was not representable."
  (declare (type (and fixnum unsigned-byte) fraction-bits))
  ;; This function has to check every input, because #'make-instance doesn't check the inputs.
  (etypecase number
    ((or rational float)
     ;;(print (list "number" number))
     (let* ((denom (ash 1 fraction-bits)))
       (multiple-value-bind (integer rest) (funcall fraction-rounding (* number denom))
	 ;;(print (list "integer" integer "fraction" rest))
	 (values
	  (let ((number integer))
	    (make-instance 'fixed :fraction-bits fraction-bits :number number))
	  (/ rest denom)))))))

(define-compiler-macro to-fixed (&whole form number &optional (fraction-bits nil) (fraction-rounding nil) &environment env)
  ;;(prind form)
  ;; The default arguments of FRACTION-BITS etc. may not be the same as in the function #'TO-FIXED, because otherwise we'll use the compile-time values of *FRACTION-BITS* etc. to construct the constant. Instead, make them values that do not fulfill the following conditions, so that we go into the else-branch (which doesn't use the FRACTION-BITS etc. passed to this compiler-macro at all).
  (if (and (typep number '(or rational float) env)
	   (typep fraction-bits '(and fixnum unsigned-byte) env) 
	   (typep fraction-rounding 'function env))
      ;; insert a constant directly
      (multiple-value-bind (number rest) (to-fixed number fraction-bits fraction-rounding)
	`(values ,number ,rest))
      form))

;; negative numbers
(assert (multiple-value-bind (fixed rest) (to-fixed -345/100 1 #'floor)
	  (and (= (to-rational fixed) -350/100) (= rest 5/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed -345/100 1 #'round)
	  (and (= (to-rational fixed) -350/100) (= rest 5/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed -345/100 1 #'ceiling)
	  (and (= (to-rational fixed) -300/100) (= rest -45/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed -315/100 1 #'floor)
	  (and (= (to-rational fixed) -350/100) (= rest 35/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed -315/100 1 #'round)
	  (and (= (to-rational fixed) -300/100) (= rest -15/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed -315/100 1 #'ceiling)
	  (and (= (to-rational fixed) -300/100) (= rest -15/100))))
;; positive-numbers
(assert (multiple-value-bind (fixed rest) (to-fixed 345/100 1 #'floor)
	  (and (= (to-rational fixed) 300/100) (= rest 45/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed 345/100 1 #'round)
	  (and (= (to-rational fixed) 350/100) (= rest -5/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed 345/100 1 #'ceiling)
	  (and (= (to-rational fixed) 350/100) (= rest -5/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed 315/100 1 #'floor)
	  (and (= (to-rational fixed) 300/100) (= rest 15/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed 315/100 1 #'round)
	  (and (= (to-rational fixed) 300/100) (= rest 15/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed 315/100 1 #'ceiling)
	  (and (= (to-rational fixed) 350/100) (= rest -35/100))))
;; this was wrong in TO-FIXED since (round 0.5)==0, but (round 1.5)==2.
(assert (= (to-rational (to-fixed 1/2 0 #'round)) (round 1/2)))
(assert (= (to-rational (to-fixed 3/2 0 #'round)) (round 3/2)))

(defun test-to-fixed (&key (n 1000))
  "Test #'TO-FIXED and #'TO-RATIONAL by creating N random numbers converting them to and from FIXED."
  (loop for i below n do
       (let* ((integer-bits (random 10))
	      (fraction-bits (random 10))
	      (integer (if (> integer-bits 0) (- (random (ash 1 integer-bits)) (ash 1 (1- integer-bits))) 0))
	      (fraction (/ (random (ash 1 fraction-bits)) (ash 1 fraction-bits)))
	      (number (+ integer fraction)) ;NUMBER is exactly representable with INTEGER-BITS and FRACTION-BITS.
	      (fixed0 (to-fixed number fraction-bits #'ceiling))
	      (fixed1 (to-fixed number fraction-bits #'floor))
	      (rational0 (to-rational fixed0))
	      (rational1 (to-rational fixed1)))
	 (assert (= rational0 rational1 number) nil "fraction-bits:~A number:~A fixed0:~A fixed1:~A rational0:~A rational1:~A" fraction-bits number fixed0 fixed1 rational0 rational1))))
(test-to-fixed)

(defun most-positive-fixed (fraction-bits integer-bits)
  "The largest positive fixed-point number with FRACTION-BITS bits for the fractional part and INTEGER-BITS bits for the integer part of the number."
  (nth-value
   0
   (to-fixed 
    (+ (nth-value 1 (signed-integer-range integer-bits))
       (/ (nth-value 1 (unsigned-integer-range fraction-bits)) (ash 1 fraction-bits)))
    integer-bits
    fraction-bits)))

(defun least-positive-fixed (fraction-bits integer-bits)
  "The smallest positive fixed-point number with FRACTION-BITS bits for the fractional part and INTEGER-BITS bits for the integer part of the number."
  (nth-value
   0
   (to-fixed 
    (+ 0
       (/ 1 (ash 1 fraction-bits)))
    integer-bits
    fraction-bits)))

(defun least-negative-fixed (fraction-bits integer-bits)
  "The largest negative fixed-point number with FRACTION-BITS bits for the fractional part and INTEGER-BITS bits for the integer part of the number."
  (nth-value
   0
   (to-fixed 
    (- 0
       (/ 1 (ash 1 fraction-bits)))
    integer-bits
    fraction-bits)))

(defun most-negative-fixed (fraction-bits integer-bits)
  "The largest positive fixed-point number with FRACTION-BITS bits for the fractional part and INTEGER-BITS bits for the integer part of the number."
  ;;TODO check this is correct, for example: CL-USER> (describe (most-negative-fixed 3 2)) gives fixed-point Q3:2 number -4.75 (-19)
  (nth-value
   0
   (to-fixed 
    (- (nth-value 0 (signed-integer-range integer-bits))
       0)
    integer-bits
    fraction-bits)))

;;;; Arithmetic

;;; The idea of the following functions is that you can compute the resulting number by first dividing the fixed numbers by their denominators, then performing the 2-ary operation on the 2 numbers, the result of which is then multiplied with the denominator of the result. For example:
;; (defun f* (a b &optional rf (rounder #'round))
;;   (with-slots ((an number) (af fraction-bits)) a
;;     (with-slots ((bn number) (bf fraction-bits)) b
;;       (let* ((adenom (ash 1 af))
;; 	     (bdenom (ash 1 bf))
;; 	     (rdenom (ash 1 rf))
;; 	     (rn (* (* (/ an adenom) (/ bn bdenom)) rdenom)))
;; 	(multiple-value-bind (number rest) (funcall rounder rn)
;; 	  (values
;; 	   (make-instance 'fixed :fraction-bits rf :number number)
;; 	   (/ rest rdenom)))))))

;;; 2-ary functions

(defun f2+ (a b &optional rf rounder)
  (with-slots ((an number) (af fraction-bits)) a
    (with-slots ((bn number) (bf fraction-bits)) b
      (let* ((adenom (ash 1 af))
	     (bdenom (ash 1 bf))
	     (rdenom (ash 1 rf))
	     (rn (if (= adenom bdenom rdenom)
		     (+ an bn)
		     (* (+ (/ an adenom) (/ bn bdenom)) rdenom))))
	(multiple-value-bind (number rest) (funcall rounder rn)
	  (values
	   (make-instance 'fixed :fraction-bits rf :number number)
	   (/ rest rdenom)))))))

(defun f2- (a b &optional rf rounder)
  (with-slots ((an number) (af fraction-bits)) a
    (with-slots ((bn number) (bf fraction-bits)) b
      (let* ((adenom (ash 1 af))
	     (bdenom (ash 1 bf))
	     (rdenom (ash 1 rf))
	     (rn (if (= adenom bdenom rdenom)
		     (- an bn)
		     (* (- (/ an adenom) (/ bn bdenom)) rdenom))))
	(multiple-value-bind (number rest) (funcall rounder rn)
	  (values
	   (make-instance 'fixed :fraction-bits rf :number number)
	   (/ rest rdenom)))))))

(defun f2* (a b &optional rf rounder)
  (with-slots ((an number) (af fraction-bits)) a
    (with-slots ((bn number) (bf fraction-bits)) b
      (let* ((adenom (ash 1 af))
	     (bdenom (ash 1 bf))
	     (rdenom (ash 1 rf))
	     ;;(rn (* (* (/ an adenom) (/ bn bdenom)) rdenom)))
	     ;;(rn (/ (* an bn rdenom) (* adenom bdenom))))
	     (rn (if (= adenom bdenom rdenom)
		     (/ (* an bn) adenom)
		     (/ (* an bn rdenom) (* adenom bdenom)))))
	(multiple-value-bind (number rest) (funcall rounder rn)
	  (values
	   (make-instance 'fixed :fraction-bits rf :number number)
	   (/ rest rdenom)))))))

(defun f2/ (a b &optional rf rounder)
  (with-slots ((an number) (af fraction-bits)) a
    (with-slots ((bn number) (bf fraction-bits)) b
      (let* ((adenom (ash 1 af))
	     (bdenom (ash 1 bf))
	     (rdenom (ash 1 rf))
	     ;;(rn (* (/ (/ an adenom) (/ bn bdenom)) rdenom)))
	     (rn (if (= adenom bdenom rdenom)
		     (/ (* an bdenom) bn)
		     (/ (* an bdenom rdenom) (* adenom bn)))))
	(multiple-value-bind (number rest) (funcall rounder rn)
	  (values
	   (make-instance 'fixed :fraction-bits rf :number number)
	   (/ rest rdenom)))))))

(defun f+ (&rest fixed-numbers)
  "Return the sum of FIXED-NUMBERS, or the FIXED 0 if none are given. The result's FRACTION-BITS is those of the first argument, or *FRACTION-BITS* if no argument is given. The result is rounded using function *FRACTION-ROUNDING*."
  (let* ((rf (if (null fixed-numbers) *fraction-bits* (fixed-fraction-bits (car fixed-numbers))))
	 (rr *fraction-rounding*)
	 (res (to-fixed 0 rf rr)))
    (loop for i in fixed-numbers do
	 (setf res (f2+ res i rf rr)))
    res))

(defun f- (fixed-number &rest fixed-numbers)
  "Subtract FIXED-NUMBERS from FIXED-NUMBER. The result's FRACTION-BITS is those of the first argument, or *FRACTION-BITS* if no argument is given. The result is rounded using function *FRACTION-ROUNDING*."
  (let* ((rf (fixed-fraction-bits fixed-number))
	 (rr *fraction-rounding*)
	 (res fixed-number))
    (loop for i in fixed-numbers do
	 (setf res (f2- res i rf rr)))
    res))

(defun f* (&rest fixed-numbers)
  "Return the product of FIXED-NUMBERS, or the FIXED 1 if none are given. The result's FRACTION-BITS is those of the first argument, or *FRACTION-BITS* if no argument is given. The result is rounded using function *FRACTION-ROUNDING*."
  (let* ((rf (if (null fixed-numbers) *fraction-bits* (fixed-fraction-bits (car fixed-numbers))))
	 (rr *fraction-rounding*)
	 (res (to-fixed 1 rf rr)))
    (loop for i in fixed-numbers do
	 (setf res (f2* res i rf rr)))
    res))

(defun f/ (fixed-number &rest fixed-numbers)
  "Divide FIXED-NUMBER by FIXED-NUMBERS. The result's FRACTION-BITS is those of the first argument, or *FRACTION-BITS* if no argument is given. The result is rounded using function *FRACTION-ROUNDING*."
  (let* ((rf (fixed-fraction-bits fixed-number))
	 (rr *fraction-rounding*)
	 (res fixed-number))
    (loop for i in fixed-numbers do
	 (setf res (f2/ res i rf rr)))
    res))

(defun test-arithmetic (function-fixed function-rational n-arguments &key (n 1000) (no-zeros nil))
  (loop for i below n do
       (let* ((f (random 10)) ;precision of arguments
	      (f-result (random 10)) ;precision of result
	      (args (loop for j below n-arguments collect (let ((x (to-fixed (+ -100 (random 200) (random 1.0)) f))) (if (and (= 0 (to-rational x)) no-zeros) (to-fixed 1 f) x))))
	      (args* (if (null args)
			 args
			 (cons (to-fixed (to-rational (car args)) f-result) (cdr args))))
	      (args-rational (mapcar #'to-rational args*))
	      (result (apply function-fixed
			     args*))
	      (result-rational (reduce (lambda (&rest r) (to-rational (to-fixed (apply function-rational r) f-result)))
				       args-rational)))
	 ;;(print (list "f" f "args*" args* "args-rational" args-rational "f-result" f-result "result" result "result-rational" result-rational))
	 (assert (= (to-rational result) result-rational)))))
(test-arithmetic #'f+ #'+ 0 :n 1)
(test-arithmetic #'f+ #'+ 5)
(test-arithmetic #'f- #'- 5)
(test-arithmetic #'f* #'* 0 :n 1)
(test-arithmetic #'f* #'* 5)
(test-arithmetic #'f/ #'/ 5 :no-zeros t)

#|
;; Probably I should use compiler-macros for the arithmetic functions, and if they detect an ANNOTATION object, emit different code than normal.

;; Example: (unwrap (annotate (to-fixed 3 3 1)))
(defmacro unwrap (body)
  ;;(let ((body (list 'progn
  (prind body (macroexpand body) (describe (macroexpand body)))
  (with-slots (fixed fraction-bits number) (macroexpand body)
    fixed))
|#
;; TODO: write a function that defines many functions f+,f-,... which all accept between 0(for +,*) or 1(for -,/) and infinity arguments in two versions: those which perform type-checking and type-conversion (maybe also allow specifying the resuling precision), and those who don't.

(defun overflow (fixed &optional (integer-bits *integer-bits*))
  "Return the FIXED number that would result if FIXED were stored in a machine word with INTEGER-BITS bits for the integer number.
You can use this function to obtain the same result as when doing fixed-point computations in C, for example
  (overflow (f* (to-fixed A 16) (to-fixed B 16)) 16)
will give the same result as a Q16.16 fixed-point multiplication of A and B."
  (with-slots (number fraction-bits) fixed
    (if (< number 0)
	(make-instance 'fixed
		       :fraction-bits fraction-bits
		       :number (- (logand (- number) (1- (ash 1 (+ -1 fraction-bits integer-bits))))))
	fixed)))

(assert (= (to-rational (overflow (f* (to-fixed 55 0) (to-fixed -5 0)) 8)) (to-rational (overflow (f* (to-fixed 55 2) (to-fixed -5 2)) 8)) -19))
