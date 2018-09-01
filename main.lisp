;extra forms
(defconstant +comment+ (gensym))
(defconstant +feature-test+ (gensym))

;modules
(load "read")
(load "write")

;command line
(defconstant +args+
   #+CCL *UNPROCESSED-COMMAND-LINE-ARGUMENTS*
   #+SBCL (cdr *posix-argv*))

;files
(dolist (file +args+)
  (let ((s (read-file file)))
    (write-file file s)))
(quit)
