(defconstant comment (gensym))
(defconstant feature-test (gensym))

(defun comment-reader (st c)
  (declare (ignore c))
  (list comment
                     (read-line st nil #\Newline t)))

(defun feature-test-reader (st c x)
   (declare (ignore c x))
   (list feature-test
    (read st t nil t)
    (read st t nil t)))

(defun read-file (file)
  (with-open-file (st file)
    (set-macro-character (char ";" 0) #'comment-reader)
    (set-dispatch-macro-character #\# #\+ #'feature-test-reader)
    (loop
      for x = (read st nil st)
      until (eq x st)
      collect x)))
