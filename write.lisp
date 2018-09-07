(defvar *indent* 0)
(defvar *newlines* 0)

(defun next-line()
  (setf *newlines*(max *newlines* 1)))

(defun blank-line()
  (setf *newlines* 2))

(defun pp-position()
  (unless(= *newlines* 0)
  (unless(= (file-position *standard-output*) 0)
  (dotimes (i *newlines*)
    (terpri))
    )
  (dotimes (i *indent*)
    (write-char #\space))
  (setf *newlines* 0)
    )
  )

(defun pp-write(a)
  (pp-position)
  (write a))

(defun pp-list(s)
  (if s
    (pp-write s)
    (pp-string"()")))

(defun pp-string(s)
  (pp-position)
  (princ s))

(defun pp-feature-test(a)
  (pp-string"#+")
  (pp(cadr a))
  (pp-string" ")
  (pp(caddr a)))

(defun pp-line-comment(a)
      (blank-line)
      (pp-string (cadr a))
      (next-line))

(defun pp-defun(a)
  (blank-line)
      (pp-string "(")
      (pp-write(pop a))
      (pp-string " ")
      (pp-write(pop a))
      (pp-string " ")
      (pp-list(pop a))
      (let ((*indent*(+ *indent* 2)))
        (loop
          while a do
          (next-line)
          (pp(pop a)))
        (pp-string ")"))
  (blank-line)
        )

(defun writer(a)
  (cond
    ((atom a)
      #'pp-write)
    ((eq(car a)+feature-test+)
      #'pp-feature-test)
    ((eq(car a)+line-comment+)
      #'pp-line-comment)
    ((member (car a)'(defun))
      #'pp-defun)
    (t
      #'pp-write)))

(defun pp(a)
  (funcall(writer a)a))

(defun write-file (file s)
  (with-open-file (*standard-output* file
    :direction :output
    :if-exists :supersede)
  (let((*print-case* :downcase))
        (loop
          while s do
          (next-line)
          (pp(pop s)))
  (terpri)
  )))
