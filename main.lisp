;extra forms
(defconstant +line-comment+ (gensym))
(defconstant +feature-test+ (gensym))

;modules
(load "read")
(load "write")

;command line
(defconstant
  +args+
  #+ccl *unprocessed-command-line-arguments*
  #+sbcl (cdr *posix-argv*)
  )

;files
(dolist (file +args+)
  (princ file)
  (terpri)
  (let ((s (read-file file))
        (backup (make-pathname :defaults file :directory "/tmp/")))
    (ignore-errors
      (delete-file backup))
    (ignore-errors
      (rename-file file backup))
    (write-file file s)))
(quit)
