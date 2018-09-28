;boilerplate
#+sbcl (declaim (muffle-conditions style-warning))

;macros
(load "macros")

;modules
(load "etc")
(load "read")
(load "write")

;globals
(defvar *right-margin* 40)

(defun main ()
  (let ((args))
    (setf
      args
      #+ccl *unprocessed-command-line-arguments*
      #+sbcl (cdr *posix-argv*)
      )
    (dolist (file args)
      (format t "~a~%" file)
      (let ((s (read-file file))
            (backup (make-pathname :defaults file :directory "/tmp/")))
        (ignore-errors
          (delete-file backup))
        (ignore-errors
          (rename-file file backup))

        ;TODO: on error, restore backup
        (write-file file s)))
    (quit)))
