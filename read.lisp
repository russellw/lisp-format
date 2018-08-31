(defconstant comment (gensym))

(defun semicolon-reader (st char)
  (list comment
        (concatenate 'string (string char)
                     (read-line st nil #\Newline t))))

(defun read-file (file)
	(with-open-file (st file)
		(set-macro-character (char ";" 0) #'semicolon-reader)
		(loop
			for x = (read st nil st)
			until (eq x st)
			collect x)))
