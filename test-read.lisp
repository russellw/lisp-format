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
          (if nil
            (progn
              (loop
                while *tok*
                collect *tok*
                do
                (lex)
              )
            )
            (read-all)
          )
        )
      )
    )
  )
;end
(quit)
