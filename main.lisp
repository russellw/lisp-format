(load "read")
(load "write")

(defconstant +args+
   #+CCL *UNPROCESSED-COMMAND-LINE-ARGUMENTS*
   #+SBCL (cdr *posix-argv*))

(dolist (file +args+)
  (let ((s (read-file file)))
    (write-file file s)))
(quit)
