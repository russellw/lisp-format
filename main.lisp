;extra forms
(defconstant +line-comment+ (gensym))
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
(princ file)
(terpri)
  (let (
            (s (read-file file))
            (backup(make-pathname :defaults file :directory "/tmp/" ))
        )
    (handler-case
    	(progn
        (delete-file  backup)
      )
  	  (error (c)
  	  )
  	)
    (handler-case
    	(progn
        (rename-file file backup)
      )
  	  (error (c)
  	  )
  	)
    (write-file file s)))
(quit)
