;;(asdf:oos 'asdf:test-op 'utils)


(defpackage :utils-tests
  (:use :cl :utils
        #+sbcl :sb-rt
        #-sbcl :regression-test))

(in-package :utils-tests)


;; (defmacro stupid-check (check &key (msg "stupid-check-error:"))
;;   ;; this macro is full of brain-deadness
;;   (with-gensyms (result)
;;     (let ((msg-r msg))
;;       `(let ((,result ,check))
;; 	 (if ,result
;; 	     t
;; 	     (error ,msg-r))))))

;; (defun sreplace-test ()
;;   ;; this function is full of brain-deadness
;;   (dolist (foo
;; 	    '(((sreplace "abc123" "xxx") "xxx")
;; 	      ((sreplace "abc123" "xxx" :start1 0 :end1 3) "xxx123")
;; 	      ((sreplace "abc123" "xxx" :start1 0 :end1 3 :start2 1) "xx123")
;; 	      ((sreplace "abc123" "xxx" :start1 3 :end1 3) "abcxxx123")
;; 	      ((sreplace "abc123" "xyz" :start1 3 :end1 3 :start2 1 :end2 2)
;; 	       "abcy123")
;; 	      ((sreplace "abc123" "xxx" :start1 3 :end1 3) "abcxxx123")))
;;     (stupid-check (equal (eval (car foo)) (eval (cadr foo)))
;; 		  :msg (format nil "~A" foo))))

;; (sreplace-test)

;; ;; (sreplace "abc123" "xxx") "xxx"
;; ;; (sreplace "abc123" "xxx" :start1 0 :end1 3) "xxx123"
;; ;; (sreplace "abc123" "xxx" :start1 0 :end1 3 :start2 1) "xx123"
;; ;; (sreplace "abc123" "xxx" :start1 1 :end1 1) "abcxxx123"
;; ;; (sreplace "abc123" "xyz" :start1 3 :end1 3 :start2 1 :end2 2) "abcy123"
;; ;; (sreplace "abc123" "xxx" :start1 3 :end1 3) "abcxxx123"


(rem-all-tests)

;;     If continue-testing is evaluated in a break
;; generated during testing, it aborts the current
;; test (which remains pending) and forces the
;; processing of tests to continue. Note that in
;; such a breakpoint, *test* is bound to the name
;; of the test being performed and (get-test) can
;; be used to look at the test.

(deftest unique.1
    (sort (unique '(1 2 1 2 3 2 1 2)) #'<)
  (1 2 3))

(deftest unique.2
    (sort (unique '(1 2 1 2 3 2 1 2) :only t) #'<)
  (3))

(deftest unique.2a
    (sort (unique '(1 2 1 2 2 1 2) :only t) #'<)
  nil)

(deftest unique.3
    (sort (unique '(1 2 1 2 3 2 1 2) :not t) #'<)
  (1 2))

(deftest unique.4
    (sort (unique '(1 2 1 2 3 2 1 2) :countp nil) #'<)
  (1 2 3))

(deftest unique.5
    (sort (unique '(1 2 1 2 3 2 1 2) :countp (lambda (c) (= c 3))) #'<)
  (1))

(deftest unique.6
    (sort (unique '(1 2 1 2 3 2 1 2) :countp (lambda (c) (>= c 3))) #'<)
  (1 2))

(deftest unique.7
    (sort (unique '(1 2 1 2 3 2 1 2) :countp (lambda (c) (>= c 4))) #'<)
  (2))

(deftest unique.8
    (sort (unique '(1 2 1 2 3 2 1 2) :countp (lambda (c) (< c 4))) #'<)
  (1 3))

(deftest unique.8a
    (sort (unique '(1 2 1 2 3 2 1 2) :countp (lambda (c) (> c 55))) #'<)
  nil)

(defparameter +list1+ (mapcar (lambda (x) (cons x (1+ x))) '(1 2 3 2 1 1)))

(deftest unique.9
    (sort (unique +list1+) #'< :key #'car)
  ((1 . 2) (1 . 2) (1 . 2) (2 . 3) (2 . 3) (3 . 4)))

(deftest unique.10
    (sort (unique +list1+ :test #'equal) #'< :key #'car)
  ((1 . 2) (2 . 3) (3 . 4)))

(deftest unique.11
    (sort (unique +list1+ :test #'equalp) #'< :key #'car)
  ((1 . 2) (2 . 3) (3 . 4)))

(deftest unique.12
    (sort (unique +list1+ :only t :test #'equalp) #'< :key #'car)
  ((3 . 4)))

(deftest unique.13
    (sort (unique +list1+ :not t :test #'equalp) #'< :key #'car)
  ((1 . 2) (2 . 3)))

(deftest unique.14
    (sort (unique +list1+ :countp nil :test #'equalp) #'< :key #'car)
  ((1 . 2) (2 . 3) (3 . 4)))

(deftest unique.15
    (sort (unique +list1+ :countp (lambda (c) (= c 3)) :test #'equalp)
	  #'< :key #'car)
  ((1 . 2)))

(deftest unique.16
    (sort (unique +list1+ :countp (lambda (c) (= c 2)) :test #'equalp)
	  #'< :key #'car)
  ((2 . 3)))

(deftest unique.17
    (sort (unique +list1+ :countp (lambda (c) (<= c 2)) :test #'equalp)
	  #'< :key #'car)
  ((2 . 3) (3 . 4)))

(deftest unique.18
    (sort (unique '(3 4 5 6 6 7) :key (lambda (x) (mod x 3))) #'<)
  (3 4 5))

(deftest unique.19
    (sort (unique '(3 4 5 6 6 7) :only t :key (lambda (x) (mod x 3))) #'<)
  (5))

(deftest unique.19a
    (sort (unique '(3 4 6 6 7) :only t :key (lambda (x) (mod x 3))) #'<)
  nil)

(deftest unique.20
    (sort (unique '(3 4 5 6 6 7) :not t :key (lambda (x) (mod x 3))) #'<)
  (3 4))

(deftest unique.21
    (sort (unique '(3 4 5 6 6 7) :countp nil :key (lambda (x) (mod x 3))) #'<)
  (3 4 5))

(deftest unique.22
    (sort (unique '(3 4 5 6 6 7) :countp (lambda (c) (= c 2))
		  :key (lambda (x) (mod x 3)))
	  #'<)
  (4))

(deftest unique.23
    (sort (unique '(3 4 5 6 6 7) :countp (lambda (c) (= c 3))
		  :key (lambda (x) (mod x 3)))
	  #'<)
  (3))

(deftest unique.24
    (sort (unique '(3 4 5 6 6 7) :countp (lambda (c) (<= c 2))
		  :key (lambda (x) (mod x 3)))
	  #'<)
  (4 5))

(deftest unique.24a
    (sort (unique '(3 4 5 6 6 7) :countp (lambda (c) (>= c 32))
		  :key (lambda (x) (mod x 3)))
	  #'<)
  nil)

(defparameter +list2+ '((1 . a) (3 . a) (1 . b) (2 . a) (1 . b) (2 . b)))

(deftest unique.25
    (sort (unique +list2+ :key #'car) #'< :key #'car)
  ((1 . a) (2 . a) (3 . a)))

(deftest unique.26
    (sort (unique +list2+ :only t :key #'car) #'< :key #'car)
  ((3 . a)))

(deftest unique.27
    (sort (unique +list2+ :not t :key #'car) #'< :key #'car)
  ((1 . a) (2 . a)))

(deftest unique.28
    (sort (unique +list2+ :countp nil :key #'car) #'< :key #'car)
  ((1 . a) (2 . a) (3 . a)))

(deftest unique.29
    (sort (unique +list2+ :countp (lambda (c) (= c 3)) :key #'car)
	  #'< :key #'car)
  ((1 . a)))

(deftest unique.30
    (sort (unique +list2+ :countp (lambda (c) (= c 2)) :key #'car)
	  #'< :key #'car)
  ((2 . a)))

(deftest unique.31
    (sort (unique +list2+ :countp (lambda (c) (<= c 2)) :key #'car)
	  #'< :key #'car)
  ((2 . a) (3 . a)))

(deftest unique.32
    (sort (unique +list2+ :countp (lambda (c) (>= c 32)) :key #'car)
	  #'< :key #'car)
  nil)
