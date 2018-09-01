(defun args ()
   #+CCL *UNPROCESSED-COMMAND-LINE-ARGUMENTS*
   #+SBCL (cdr *posix-argv*))

(load "read")

(let ((s (read-file (car (args)))))
  (mapcar #'pprint s))
(quit)
