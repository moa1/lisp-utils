;; Print and parse floating-point numbers in any radix and with any precision. This is useful for persistent exact floats.
;; The function to return a string readable by C is (hex-c-readable FLOAT-NUMBER).
;; The function to parse a string printed by hex-c-readable or printf in C is (parse-hex-c-readable STRING).

(defpackage :floating
  (:use :common-lisp)
  (:export :*print-precision*
	   :*print-rounding*
	   :print-real
	   :floating
	   :floating-hex-to-c-readable
	   :hex-c-readable
	   :parse-float-as-rational
	   :parse-float
	   :parse-hex-c-readable))

(in-package :floating)

(defun digitize-integer (integer)
  (declare (type (and unsigned-byte integer) integer))
  (let ((output-list nil))
    (flet ((output-digit (d)
	     (push d output-list)))
      (when (= integer 0)
	(output-digit 0)
	(return-from digitize-integer))
      (do* ((i (abs integer))) ((= i 0))
	(multiple-value-bind (int frac) (floor i *print-base*)
	  (setf i int)
	  (output-digit frac)))
      output-list)))

(defvar *print-precision* 10 "NIL or an integer. An integer means the maximal number of digits to print when a fraction is printed. NIL means that an infinite number of digits may be printed.")
(defvar *print-rounding* :round "*PRINT-ROUNDING* controls how the last digit of the fraction is rounded, with possible values :TRUNCATE (truncate after *PRINT-PRECISION*), :ELLIPSIS (truncate after *PRINT-PRECISION* and print \"...\" if the rest is not 0) or :ROUND (round last digit to the nearest possible digit).")

(defun propagate-rest! (l)
  (if (null l)
      t
      (let ((new-digit (1+ (car l))))
	(if (>= new-digit *print-base*)
	    (progn
	      (setf (car l) 0)
	      (propagate-rest! (cdr l)))
	    (progn
	      (setf (car l) new-digit)
	      nil)))))

(assert (equal (let* ((*print-base* 10) (l '(2 1 3 4 5))) (list (floating::propagate-rest! l) l)) '(nil (3 1 3 4 5))))
(assert (equal (let* ((*print-base* 10) (l '(9 9 3 4 5))) (list (floating::propagate-rest! l) l)) '(nil (0 0 4 4 5))))
(assert (equal (let* ((*print-base* 10) (l '(9 9 9 9 9))) (list (floating::propagate-rest! l) l)) '(t (0 0 0 0 0))))

(defun digitize-fraction (fraction)
  "Return three values (OVERFLOW DIGITS SUFFIX) with the following meaning:
OVERFLOW is a boolean that is T iff rounding resulted in an overflow of the first fractional digit.
DIGITS is a list with digits D ready to be printed, with all D satisfying (and (<= 0 D) (< D *PRINT-BASE*)).
SUFFIX is a string that is to be appended to the digits."
  (declare (type real fraction))
  (assert (and (<= 0 fraction) (< fraction 1)))
  ;; TODO: add a variable *print-ellipsis* (maybe *print-rounding*=:repetend is better, since displaying the exact value, which :ellipsis is, doesn't need any type of rounding.) with possible values NIL and non-NIL, and if non-NIL, then #'print-fraction detects repeating sequences of fractional digits and prints for (PRINT-FRACTION .55551234123412341234...) the string ".5555#1=1234#1#". Only print *print-precision* digits, followed by "...", if (one repetition of the repeating fraction + the fraction before the repeating fraction) has a length greater than *print-precision*. Test with 1/7, 1/14, 1/28. See [https://en.wikipedia.org/wiki/Repeating_decimal].
  (if (= 0 fraction)
      (if (> *print-precision* 0)
	  (values nil '(0) "")
	  (values nil '() ""))
      (let ((output-list nil))
	(flet ((output-digit (d)
		 (push d output-list)))
	  (multiple-value-bind (rest digits)
	      (do* ((rest (* fraction *print-base*)) (n 0 (1+ n)))
		   ((or (= rest 0)
			(and (not (null *print-precision*))
			     (>= n (1- *print-precision*))))
		    (values rest n))
		(multiple-value-bind (a b) (floor (* rest *print-base*) *print-base*)
		  (output-digit a)
		  (setf rest b)))
	    (if (= rest 0)
		(progn
		  (values nil (nreverse output-list) ""))
		(progn
		  (assert (= digits (max 0 (1- *print-precision*))))
		  (multiple-value-bind (integer fraction)
		      (ecase *print-rounding*
			((:truncate :ellipsis) (truncate rest))
			((:round) (round rest)))
		    (output-digit integer)
		    (if (>= integer *print-base*)
			(values (propagate-rest! output-list) (nreverse output-list) "")
			(values nil (nreverse output-list) (if (and (eq *print-rounding* :ellipsis) (/= fraction 0)) "..." "")))))))))))

(assert (equal (let ((*print-precision* 3) (*print-base* 10) (*print-rounding* :round)) (multiple-value-list (floating::digitize-fraction 0.999))) '(nil (9 9 9) "")))
(assert (equal (let ((*print-precision* 3) (*print-base* 10) (*print-rounding* :round)) (multiple-value-list (floating::digitize-fraction 0.9999))) '(t (0 0 0) "")))
(assert (equal (let ((*print-precision* 3) (*print-base* 10) (*print-rounding* :truncate)) (multiple-value-list (floating::digitize-fraction 0.9999))) '(nil (9 9 9) "")))
(assert (equal (let ((*print-precision* 3) (*print-base* 10) (*print-rounding* :ellipsis)) (multiple-value-list (floating::digitize-fraction 0.9999))) '(nil (9 9 9) "...")))
(assert (equal (let ((*print-precision* 3) (*print-base* 10) (*print-rounding* :ellipsis)) (multiple-value-list (floating::digitize-fraction 0.54))) '(nil (5 4) "")))

(when (not (boundp '+print-radix-symbols+))
  (defconstant +print-radix-symbols+ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defun print-real (number &optional stream)
  (declare (type real number))
  ;;(princ (aref +print-radix-symbols+ d) stream))))
  (when (minusp number)
    (princ "-" stream))
  (multiple-value-bind (integer fraction) (truncate number)
    (setf integer (abs integer))
    (setf fraction (abs fraction))
    (let ((integer-digits (digitize-integer integer)))
      (multiple-value-bind (overflow fraction-digits suffix)
	  (digitize-fraction fraction)
	(when overflow
	  (setf integer-digits (nreverse integer-digits))
	  (if (propagate-rest! integer-digits)
	      (setf integer-digits (cons 1 (nreverse integer-digits)))
	      (setf integer-digits (nreverse integer-digits))))
	(labels ((print-list (l)
		   (when (not (null l))
		     (princ (aref +print-radix-symbols+ (car l)) stream)
		     (print-list (cdr l)))))
	  (print-list integer-digits)
	  (princ #\. stream)
	  (print-list fraction-digits)
	  (princ suffix stream)))))
  number)

(defun floating (stream number colonp atp 
                 &optional (*print-base* *print-base*) (*print-precision* *print-precision*)
                 &rest args)
  (declare (ignore colonp atp args))
  ;; TODO: check how I have to use COLONP and ATP: See [file:///home/toni/text/soft/HyperSpec/Body/22_ced.htm].
  (print-real number stream))

(defparameter +test-numbers+ '(-1024.0 -4.0 -3.0 -2.0 -1.0 -0.4 -0.3 -0.2 -0.1 0.0 0.1 0.2 0.3 0.4 1.0 2.0 3.0 4.0 1024.0))

(defun test-print ()
  (let ((tests +test-numbers+))
    (loop for f in tests do
	 (format t "~A == ~16/floating/ ~A~%" f f (multiple-value-list (integer-decode-float f))))
    (loop for f in tests do
	 (format t "\"~16/floating/\", " f))))

(defun floating-hex-to-c-readable (hex-string)
  "Given a floating-point hexadecimal number represented by HEX-STRING, return a string S-IN-C that is readable by 'sscanf(S-IN-C, \"%a\", &f)'."
  (if (string= "-" (subseq hex-string 0 1))
      (concatenate 'string "-0x" (subseq hex-string 1))
      (concatenate 'string "0x" hex-string)))

(defun hex-c-readable (number)
  (floating-hex-to-c-readable (with-output-to-string (stream) (floating stream number nil nil 16))))

(defun test-print-c-readable ()
  (let ((tests +test-numbers+))
    (loop for i in tests do
	 (let ((c-readable (hex-c-readable i)))
	   (format t "\"~A\", " c-readable)))))

(defun parse-float-as-rational (string &optional (radix 10))
  "Return the floating point number encoded by STRING.
Return the exact rational number encoded by STRING."
  ;; TODO: this also parses (parse-float-as-rational "  -2.   123")==-2000123/1000000. Make it throw an error instead.
  (let* ((.-pos (position #\. string))
	 (--pos (position #\- string)))
    (if (null .-pos)
	(parse-integer string :radix radix :junk-allowed nil)
	(let* ((intstr (subseq string (if (null --pos) 0 (1+ --pos)) .-pos))
	       (fracstr (subseq string (1+ .-pos))))
	  (* (if (null --pos) 1 -1)
	     (+ (parse-integer (if (string= intstr "") "0" intstr) :radix radix :junk-allowed nil)
		(let ((frac (if (string= fracstr "") "0" fracstr)))
		  (/ (parse-integer frac :radix radix :junk-allowed nil)
		     (expt radix (length frac))))))))))

(defun parse-float (string &optional (radix 10))
  (float (parse-float-as-rational string radix)))

(defun parse-hex-c-readable (string)
  "Given a STRING as output by '(hex-c-readable NUMBER)', or as output by 'printf(\"%a\", NUMBER)' in C, return the floating-point number encoded by that string.
Two values are returned: the floating-point number, and the exact parsed rational number."
  ;; Examples
  ;; by (hex-c-readable NUMBER): "-0x4." == -4.0, "0x0.3333334" == 0.2, "0x400." == 1024.
  ;; by printf("%a", NUMBER) in C: "-0x1p+2" == -4.0, "0x1.99999ap-3" == 0.2, "0x1p+10" == 1024.
  (let ((rest string)
	sign factor exp)
    (let ((s (subseq string 0 1)))
      (if (string= s "-")
	  (progn
	    (setf sign -1)
	    (setf rest (subseq rest 1)))
	  (if (string= s "+")
	      (progn
		(setf sign 1)
		(setf rest (subseq rest 1)))
	      (progn
		(setf sign 1)))))
    ;;(print rest)
    (let ((0x (subseq rest 0 2)))
      (if (string= 0x "0x")
	  (setf rest (subseq rest 2))
	  (error "float as hex parsing error: \"0x\" expected")))
    ;;(print (list "sign" sign "rest" rest))
    (setf factor (let ((p-pos (position-if (lambda (c) (or (eq #\p c) (eq #\P c))) rest)))
		   (if (null p-pos)
		       (prog1
			   (parse-float-as-rational rest 16)
			 (setf rest "0"))
		       (prog1
			   (parse-float-as-rational (subseq rest 0 p-pos) 16)
			 (setf rest (subseq rest (1+ p-pos)))))))
    (setf exp (if (string= "" rest)
		  1
		  (parse-integer rest :radix 10 :junk-allowed nil)))
    ;;(print (list "sign" sign "factor" factor "exp" exp))
    (let ((number (* sign factor (expt 2 exp))))
      (values (float number) number))))

(defun test-parse-hex-c-readable ()
  (let ((numbers '(-1024.0 -4.0 -3.0 -2.0 -1.0 -0.4 -0.3 -0.2 -0.1 0.0 0.1 0.2 0.3 0.4 1.0 2.0 3.0 4.0 1024.0))
	(hex-floating-numbers '("-0x400." "-0x4." "-0x3." "-0x2." "-0x1." "-0x0.6666668" "-0x0.4CCCCD" "-0x0.3333334" "-0x0.199999A" "0x0." "0x0.199999A" "0x0.3333334" "0x0.4CCCCD" "0x0.6666668" "0x1." "0x2." "0x3." "0x4." "0x400."))
	(printf-numbers '("-0x1p+10" "-0x1p+2" "-0x1.8p+1" "-0x1p+1" "-0x1p+0" "-0x1.99999ap-2" "-0x1.333334p-2" "-0x1.99999ap-3" "-0x1.99999ap-4" "0x0p+0" "0x1.99999ap-4" "0x1.99999ap-3" "0x1.333334p-2" "0x1.99999ap-2" "0x1p+0" "0x1p+1" "0x1.8p+1" "0x1p+2" "0x1p+10")))
    (loop
       for number in numbers
       for hex-floating-number in hex-floating-numbers
       for printf-number in printf-numbers
       do
	 (let ((n1 (parse-hex-c-readable hex-floating-number))
	       (n2 (parse-hex-c-readable printf-number)))
	   (assert (= number n1 n2))))))

(defun test-parse-hex-c-readable-2 (&optional (tests 1000))
  (loop for i below tests do
       (let* ((number (* (if (= 0 (random 2)) -1 1) (random 1e37)))
	      (hex-floating-number (hex-c-readable number)))
	 (let ((number-parsed (parse-hex-c-readable hex-floating-number)))
	   (assert (= number number-parsed))))))
