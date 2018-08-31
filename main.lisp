(defun args ()
   #+CCL CCL:*UNPROCESSED-COMMAND-LINE-ARGUMENTS*
   #+SBCL (cdr *posix-argv*))

(load "read")

(format t "~A~%" (read-file (car (args))))
(quit)
