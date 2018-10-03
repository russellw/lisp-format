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

;Code transformations
(load "comment-space")

(defun canonical-option(s)
  (when(and(search"Win"(software-type))
           (subseqp"/"s ))
    (when(equal s"/?")
      (setf s "-h")
    )
    (setf(elt s 0 )#\-)
  )
  (loop while(and   (>(length s)2)
                    (subseqp "--"s )
             )
    do
    (setf s(subseq s 1))
  )
  s
)

(defun help ()
(format t"General options:~%")
(format t"-help           Show help~%")
(format t"-version        Show version~%")
(format t"~%")
(format t"Code transformations:~%")
(format t"-comment-case   ; foo -> ; Foo~%")
(format t"-comment-space  ;foo -> ; foo~%")
(format t"-all            All the above~%")
)

(defun version ()
(format t"lisp-format version 0~%")
)

(defun main ()
  (let ((args)
        (files)
        (options t)
              ;Code transformations
              (comment-case )
              (comment-space )
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
        do
      (format t"~a~%"s)
        if (and options (subseqp"-"s ))
          do
          (cond
          ;General options
            ((member s '("h" "help" ) :test #'string=)
              (help))
            ((member s '("V" "v" "version") :test #'string=)
              (version))

              ;Code transformations
              ((equal s"comment-space")
                (setf comment-space t))

              ;error
            (t (format t "~a: unknown option~%"s))
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
