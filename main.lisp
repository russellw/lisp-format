; Boilerplate
#+sbcl (declaim (muffle-conditions style-warning))

; Globals
(defconstant +special+ (gensym))
(defvar *right-margin* 80)

; Macros
(load "macros")

; Modules
(load "etc")
(load "fmt")
(load "read")

; Optional transformations
(load "comment-case")
(load "comment-space")

; Necessary transformations
(load "add-blanks")

; Options
(defvar *comment-case* nil)
(defvar *comment-space* nil)
(defvar *all* nil)

(defun args ()
  #+ccl *unprocessed-command-line-arguments*
  #+sbcl (cdr *posix-argv*))

(defun canonical-option (s)
  (when (and (search "Win" (software-type)) (subseqp "/" s))
    (when (equal s "/?")
      (setf s "-h"))
    (setf (elt s 0) #\-))
  (loop
   while (and (> (length s) 2) (subseqp "--" s))
   do
   (setf s (subseq s 1)))
  s)

(defun help ()
  (format t "General options:~%")
  (format t "-help           Show help~%")
  (format t "-version        Show version~%")
  (format t "~%")
  (format t "Code transformations:~%")
  (format t "-comment-case   ;; foo -> ;; Foo~%")
  (format t "-comment-space  ;;foo -> ;; foo~%")
  (format t "-all            All the above~%"))

(defun version ()
  (format t "lisp-format version 0~%"))

(defun parse-args (args)
  (let ((options t))
    (loop
     while args
     for s
     = (canonical-option (pop args))
     if (and options (subseqp "-" s))
     do
     (cond

       ; General options
       ((member s '("-h" "-help") :test #'string=)
        (help))
       ((member s '("-V" "-v" "-version") :test #'string=)
        (version))

       ; Code transformations
       ((equal s "-comment-case")
        (setf *comment-case* t))
       ((equal s "-comment-space")
        (setf *comment-space* t))
       ((equal s "-all")
        (setf *all* t))

       ; Error
       (t
        (format t "~a: unknown option~%"
          s)))
     else collect
     s)))

(defun transform (s)

  ; Optional transformations
  (when (or *comment-case* *all*)
    (setf s (comment-case s)))
  (when (or *comment-space* *all*)
    (setf s (comment-space s)))

  ; Necessary transformations
  (add-blanks s))

(defun do-file (file)
  (let ((new-text (fmt-all (transform (read-file file))))
        (old-text (read-text-file file))
        (backup (make-pathname :defaults file :directory "/tmp/")))
    (unless (string= old-text new-text)
      (format t "~a~%"
        file)
      (ignore-errors (delete-file backup))
      (ignore-errors (rename-file file backup))
      (write-text-file file new-text))))

(defun main ()
  (let ((files (parse-args (args))))
    (dolist (file files)
      (do-file file))))
