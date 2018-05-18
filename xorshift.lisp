(defpackage xorshift
  (:documentation "Random number generators from the \"xorshift\" family.")
  (:shadow :random)
  (:use :cl)
  (:export
   :make-xorshift1024*-random-state :xorshift1024*-random-state-s :xorshift1024*-random-state-q
   :*xorshift1024*-random-state*
   :xorshift1024*
   :make-xorshift128+-random-state :xorshift128+-random-state-s0 :xorshift128+-random-state-s1
   :*xorshift128+-random-state*
   :xorshift128+
   :random
   :random-gaussian-2
   :random-gaussian
   ))

(in-package :xorshift)

;;;; The paper can be downladed from http://www.jstatsoft.org/v08/i14/.
;;;; Also see the article "xorshift* / xorshift+ generators and the PRNG shootout".

(defstruct (xorshift1024*-random-state (:constructor %make-xorshift1024*-random-state))
  (s (make-array 16 :element-type '(unsigned-byte 64) :initial-contents '(0 1 2 3  4 5 6 7  8 9 10 11  12 13 14 15) :fill-pointer nil :adjustable nil) :type (vector (unsigned-byte 64) 16))
  (q 0 :type (integer 0 15)))

(defun make-xorshift1024*-random-state (&optional (s '(0 1 2 3  4 5 6 7  8 9 10 11  12 13 14 15)) (p 0))
  (declare (type list s)
	   (type (integer 0 15) p))
  (assert (every (lambda (x) (typep x '(unsigned-byte 64))) s) () "S must have elements of type (UNSIGNED-BYTE 64).")
  (assert (notevery (lambda (x) (= x 0)) s) () "S must have at least one element different from 0.")
  (let ((sreal (make-array 16 :element-type '(unsigned-byte 64) :initial-contents s :fill-pointer nil :adjustable nil)))
    (declare (type (vector (unsigned-byte 64) 16) sreal))
    (%make-xorshift1024*-random-state :s sreal :q p)))

(defvar *xorshift1024*-random-state* (make-xorshift1024*-random-state) "A random state of the xorshift1024* random number generator.")

(defun xorshift1024* (random-state)
  (declare (optimize (speed 3) (safety 0))
	   (type xorshift1024*-random-state random-state))
  (let* ((s (xorshift1024*-random-state-s random-state))
	 (p (xorshift1024*-random-state-q random-state))
	 (s0 (aref s p))
	 (s1 (aref s (setf p (logand (1+ p) 15)))))
    (declare (type (unsigned-byte 64) p s0 s1) (type (simple-vector 16) s))
    (setf s1 (logxor s1 (logand (ash s1 31) (1- (expt 2 64)))))
    (setf s1 (logxor s1 (logand (ash s1 -11) (1- (expt 2 64)))))
    (setf s0 (logxor s0 (logand (ash s1 -30) (1- (expt 2 64)))))
    (let ((r (logxor s0 s1)))
      (setf (aref s p) r)
      (setf (xorshift1024*-random-state-q random-state) p)
      (logand (* r 1181783497276652981) (1- (expt 2 64))))))

(defstruct (xorshift128+-random-state (:constructor %make-xorshift128+-random-state))
  (s0 1 :type (unsigned-byte 64))
  (s1 0 :type (unsigned-byte 64)))

(defun make-xorshift128+-random-state (&optional (s0 1) (s1 0))
  (declare (type (unsigned-byte 64) s0 s1))
  (assert (not (= 0 s0 s1)) () "S0 and S1 must not both be 0.")
  (%make-xorshift128+-random-state :s0 s0 :s1 s1))

(defvar *xorshift128+-random-state* (make-xorshift128+-random-state) "A random state of the xorshift128+ random number generator.")

(defun xorshift128+ (random-state)
  (declare (optimize (speed 3) (safety 0))
	   (type xorshift128+-random-state random-state))
  (let ((s1 (xorshift128+-random-state-s0 random-state))
	(s0 (xorshift128+-random-state-s1 random-state)))
    (declare (type (unsigned-byte 64) s0 s1))
    (setf (xorshift128+-random-state-s0 random-state) s0)
    (setf s1 (logxor s1 (logand (ash s1 23) (1- (expt 2 64)))))
    (let ((r (logxor s1 s0 (ash s1 -17) (ash s0 -26))))
      (setf (xorshift128+-random-state-s1 random-state) r)
      (logand (+ r s0) (1- (expt 2 64))))))

(defmethod random-64bits ((random-state xorshift1024*-random-state))
  (xorshift1024* random-state))

(defmethod random-64bits ((random-state xorshift128+-random-state))
  (xorshift128+ random-state))

(defmethod random (arg &optional (random-state *xorshift1024*-random-state*))
  "Return a random number below ARG.
ARG must be either a positive integer or a positive float."
  (declare (type (or (integer 1 *) float) arg))
  (assert (> arg 0) () "ARG is neither a positive integer nor a positive float, but has value ~A" arg)
  ;; TODO: add random number generator for floats.
  (cond
    ((typep arg 'single-float)
     (cond
       ((= arg 1.0)
	(/ (float (random-64bits random-state)) (expt 2 64)))
       (t
	(error "TODO random float"))))
    ((typep arg 'double-float)
     (cond
       ((= arg 1.0)
	(/ (float (random-64bits random-state)) (expt 2 64)))
       (t
	(error "TODO random float"))))
    ((< arg (expt 2 64))
     (mod (random-64bits random-state) arg))
    (t
     (do ((i (ash arg -64) (ash i -64))
	  (r (random-64bits random-state) (+ (* r (expt 2 64)) (random-64bits random-state))))
	 ((<= i 1) (mod r arg))))))


(defun random-gaussian-2 (&optional (random-state *xorshift1024*-random-state*))
  "Return two with mean 0 and standard deviation 1 normally distributed random v
ariables."
  (declare (optimize (speed 3) (safety 3)))
  (flet ((xinit ()
           (the single-float (- (* 2.0 (xorshift:random 1.0 random-state)) 1))))
    (do* ((x1 (xinit) (xinit))
          (x2 (xinit) (xinit))
          (w (+ (* x1 x1) (* x2 x2)) (+ (* x1 x1) (* x2 x2))))
         ((< w 1.0)
          (let* ((wlog (the single-float (log (the (single-float 0.0 *) w))))
                 (v (the single-float (sqrt (the (single-float 0.0 *) (/ (* -2.0 wlog) w))))))
	    (declare (type single-float wlog))
            (values (* x1 v) (* x2 v)))))))

(let ((temp nil))
  (defun random-gaussian (&optional (random-state *xorshift1024*-random-state*))
    (if temp
	(prog1 temp
	  (setf temp nil))
	(multiple-value-bind (a b) (random-gaussian-2 random-state)
	  (setf temp b)
	  a))))
