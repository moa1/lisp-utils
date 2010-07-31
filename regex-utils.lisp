(asdf:oos 'asdf:load-op 'regex)


(defun match (matcher str)
  ;; (declare (optimize (debug 3)))
  (let ((groups (list)))
    (multiple-value-bind (matched-p start len regs)
	(regex:match-str matcher str)
      (declare (ignore start len))
      (or matched-p
	  (error 'parse-geoip-match-error :str str))
      (let ((num-groups (array-dimension regs 0)))
	(dotimes (i num-groups)
	  (let* ((g-start (regex:register-start regs i))
		 (g-stop (regex:register-end regs i))
		 (group (subseq str g-start g-stop)))
	    (push group groups)))))
    (nreverse groups)))


(defun scan-replace (matcher modify-sequence replace-sequence)
  "replace in modify-sequence all matches of matcher with replace-sequence"
  (labels ((helper (seq start)
	     (multiple-value-bind (matched-p start len)
		 (regex:scan-str matcher seq :start start)
	       (if matched-p
		   (let* ((end1 (+ start len))
			  (modified (sreplace seq replace-sequence
					      :start1 start :end1 end1))
			  (damn (coerce modified 'simple-string)))
		     (helper damn end1))
		   seq))))
    (helper modify-sequence 0)))

