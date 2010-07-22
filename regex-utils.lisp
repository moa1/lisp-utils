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


(defun match-replace (matcher modify-sequence replace-sequence
		      &optional (start 0))
  "replace in modify-sequence all matches of matcher with replace-sequence"
  (error "doesnt work yet")
  (multiple-value-bind (matched-p start len)
      (regex:scan-str matcher modify-sequence :start start)
    (if matched-p
	(let ((modified
	       (delete-if (const-fun t)
			  (replace modify-sequence replace-sequence
				   :start1 start)
			  :start (+ start (length replace-sequence))
			  :count (- len (length replace-sequence)))))
	  (match-replace matcher modified replace-sequence
			 (at-least (+ start (length replace-sequence) (- len))
				   0)))
	modify-sequence)))
