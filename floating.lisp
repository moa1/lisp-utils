;; Print and parse floating-point numbers in any radix and with any precision. This is useful for persistent exact floats.
;; The function to return a string readable by C is (floating-hex-c-readable FLOAT-NUMBER).
;; The function to parse a string printed by floating-hex-c-readable or printf in C is (parse-hex-c-readable STRING).

(defpackage :floating
  (:use :common-lisp)
  (:export :floating
	   :floating-hex-to-c-readable
	   :floating-hex-c-readable
	   :parse-float-as-rational
	   :parse-float
	   :parse-hex-c-readable))

(in-package :floating)


(defun floating (stream number colonp atp 
                 &optional (*print-base* 10) (num-digits 10)
                 &rest args)
  "Taken from [https://stackoverflow.com/questions/21103985/floating-point-formatted-to-base-16] by Joshua Taylor."
  (declare (ignore colonp args))
  ;; If the number is negative, print the #\- and invert the number.
  ;; Otherwise, the number is non-negative, and if an @ was provided
  ;; we print a leading #\+.
  (cond
    ((minusp number)
     (write-char #\- stream)
     (setq number (- number)))
    (atp
     (write-char #\+ stream)))
  ;; Print number, which is now guaranteed to be positive.  Begin by
  ;; taking its integer part and printing it, followed by a point.
  ;; Then, pull individual places and write them.  This continues,
  ;; updating quotient and remainder by multiplying the remainder by
  ;; the base and taking the floor again until either the remainder
  ;; becomes zero, or we've reached the maximum number of digits.
  (multiple-value-bind (quotient remainder) (floor number 1.0)
    (write quotient :stream stream)
    (write-char #\. stream)
    (do ((num-digits num-digits (1- num-digits)))
        ((or (zerop remainder) (zerop num-digits)))
      (multiple-value-setq (quotient remainder)
        (floor (* *print-base* remainder)))
      (write quotient :stream stream))))

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

(defun floating-hex-c-readable (number)
  (floating-hex-to-c-readable (with-output-to-string (stream) (floating stream number nil nil 16 100))))

(defun test-print-c-readable ()
  (let ((tests +test-numbers+))
    (loop for i in tests do
	 (let ((c-readable (floating-hex-c-readable i)))
	   (format t "\"~A\", " c-readable)))))

(defun parse-float-as-rational (string &optional (radix 10))
  "Return the floating point number encoded by STRING.
Return the exact rational number encoded by STRING."
  (let* ((.-pos (position #\. string)))
    (if (null .-pos)
	(parse-integer string :radix radix :junk-allowed nil)
	(let* ((intstr (subseq string 0 .-pos))
	       (fracstr (subseq string (1+ .-pos))))
	  (+ (parse-integer (if (string= intstr "") "0" intstr) :radix radix :junk-allowed nil)
	     (let ((frac (if (string= fracstr "") "0" fracstr)))
	       (/ (parse-integer frac :radix radix :junk-allowed nil)
		  (expt radix (length frac)))))))))

(defun parse-float (string &optional (radix 10))
  (float (parse-float-as-rational string radix)))

(defun parse-hex-c-readable (string)
  "Given a STRING as output by '(floating-hex-c-readable NUMBER)', or as output by 'printf(\"%a\", NUMBER)' in C, return the floating-point number encoded by that string.
Two values are returned: the floating-point number, and the exact parsed rational number."
  ;; Examples
  ;; by (floating-hex-c-readable NUMBER): "-0x4." == -4.0, "0x0.3333334" == 0.2, "0x400." == 1024.
  ;; by printf in C: "-0x1p+2" == -4.0, "0x1.99999ap-3" == 0.2, "0x1p+10" == 1024.
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
	      (hex-floating-number (floating-hex-c-readable number)))
	 (let ((number-parsed (parse-hex-c-readable hex-floating-number)))
	   (assert (= number number-parsed))))))
