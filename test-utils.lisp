
(defmacro stupid-check (check &key (msg "stupid-check-error:"))
  ;; this macro is full of brain-deadness
  (with-gensyms (result)
    (let ((msg-r msg))
      `(let ((,result ,check))
	 (if ,result
	     t
	     (error ,msg-r))))))

(defun sreplace-test ()
  ;; this function is full of brain-deadness
  (dolist (foo
	    '(((sreplace "abc123" "xxx") "xxx")
	      ((sreplace "abc123" "xxx" :start1 0 :end1 3) "xxx123")
	      ((sreplace "abc123" "xxx" :start1 0 :end1 3 :start2 1) "xx123")
	      ((sreplace "abc123" "xxx" :start1 3 :end1 3) "abcxxx123")
	      ((sreplace "abc123" "xyz" :start1 3 :end1 3 :start2 1 :end2 2)
	       "abcy123")
	      ((sreplace "abc123" "xxx" :start1 3 :end1 3) "abcxxx123")))
    (stupid-check (equal (eval (car foo)) (eval (cadr foo)))
		  :msg (format nil "~A" foo))))

(sreplace-test)

;; (sreplace "abc123" "xxx") "xxx"
;; (sreplace "abc123" "xxx" :start1 0 :end1 3) "xxx123"
;; (sreplace "abc123" "xxx" :start1 0 :end1 3 :start2 1) "xx123"
;; (sreplace "abc123" "xxx" :start1 1 :end1 1) "abcxxx123"
;; (sreplace "abc123" "xyz" :start1 3 :end1 3 :start2 1 :end2 2) "abcy123"
;; (sreplace "abc123" "xxx" :start1 3 :end1 3) "abcxxx123"
