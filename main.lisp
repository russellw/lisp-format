;boilerplate
#+sbcl (declaim (muffle-conditions style-warning))

;extra forms
(defconstant +line-comment+ (gensym))
(defconstant +feature-test+ (gensym))

;modules
(load "etc")
(load "read")
(load "write")

(defun main ()
  (let ((args))
    (setf
      args
      #+ccl *unprocessed-command-line-arguments*
      #+sbcl (cdr *posix-argv*)
      )
    (dolist (file args)
      (princ file)
      (terpri)
      (let ((s (read-file file))
            (backup (make-pathname :defaults file :directory "/tmp/")))
        (ignore-errors
          (delete-file backup))
        (ignore-errors
          (rename-file file backup))

        ;TODO: on error, restore backup
        (write-file file s)))
    (quit)))
