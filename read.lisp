(defun line-comment-reader (*standard-input* c)
  (list +line-comment+
    (concatenate 'string
                     (string c)
                     (read-line *standard-input* nil #\Newline t))))

(defun feature-test-reader (*standard-input* c x)
   (declare (ignore c x))
   (list +feature-test+
    (read *standard-input* t nil t)
    (read *standard-input* t nil t)))

(defun read-file (file)
  (with-open-file (*standard-input* file)
    (set-macro-character (char ";" 0) #'line-comment-reader)
    (set-dispatch-macro-character #\# #\+ #'feature-test-reader)
    (loop
      for x = (read *standard-input* nil *standard-input*)
      until (eq x *standard-input*)
      collect x)))
