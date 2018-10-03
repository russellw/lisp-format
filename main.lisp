;boilerplate
#+sbcl (declaim (muffle-conditions style-warning))

;globals
(defconstant +special+ (gensym))
(defvar *right-margin* 40)

;macros
(load "macros")

;modules
(load "etc")
(load "read")
(load "write")

(defun canonical-option(s)
  (when(and(search"Win"(software-type))
           (subseqp"/"s 0))
    (when(eql s"/?")
      (setf s "-h")
    )
    (setf(elt s 0 )#\-)
  )
  (loop while(and   (>(length s)2)
                    (subseqp "--"s 0)
             )
    do
    (setf s(subseq s 1))
  )
  s
)

(defun help ()
(format t"General options:~%")
(format t"-help     Show help~%")
(format t"-version  Show version~%")
)

(defun version ()
(format t"lisp-format version 0~%")
)

(defun main ()
  (let ((args)
        (files)
        (options t)
       )
    (setf
      args
      #+ccl *unprocessed-command-line-arguments*
      #+sbcl (cdr *posix-argv*)
      )
      (format t"~a~%"args)
      ;options
      (setf files
      (loop while args
        for s =(canonical-option (pop args))
        if (and options (subseqp"-"s 0))
          do
          (case s
            ("h" (help))
            (("V""v")(version))
            (t s)
          )
        else
           collect s
      )
      )

      ;files
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
