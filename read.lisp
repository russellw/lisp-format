(defun line-comment-reader (st c)
  (list +line-comment+
    (concatenate 'string
                     (string c)
                     (read-line st nil #\Newline t))))

(defun feature-test-reader (st c x)
   (declare (ignore c x))
   (list +feature-test+
    (read st t nil t)
    (read st t nil t)))

(defun read-file (file)
  (with-open-file (st file)
    (set-macro-character (char ";" 0) #'line-comment-reader)
    (set-dispatch-macro-character #\# #\+ #'feature-test-reader)
    (loop
      for x = (read st nil st)
      until (eq x st)
      collect x)))
