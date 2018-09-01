(defun args ()
   #+CCL *UNPROCESSED-COMMAND-LINE-ARGUMENTS*
   #+SBCL (cdr *posix-argv*))

(load "read")

(let ((s (read-file (car (args)))))
  (mapc #'pprint s))
(quit)
