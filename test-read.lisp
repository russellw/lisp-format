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
      (pprint
        (with-open-file (*standard-input* file)
          (lex)
          (loop
            while *tok*
            collect *tok*
            do
            (lex)
          )
        )
      )
    )
  )
;end
(quit)
