;modules
(load "main")

;test
  (let ((args))
    (setf
      args
      #+ccl *unprocessed-command-line-arguments*
      #+sbcl (cdr *posix-argv*)
      )
    (dolist (file args)
      (princ file)
      (terpri)
      (let ((s (read-file file)))
      (print s)
)))
;end
(quit)
