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
  (cond
    (a
      (pp-position)
      (write a))
    (t
      (pp-string"()"))))

(defun pp-string(s)
  (pp-position)
  (princ s))

(defun pp-special(a n)
      (pp-string "(")
      (pp-write(pop a))
      (dotimes(i n)
      (pp-string " ")
      (pp-write(pop a)))
      (let ((*indent*(+ *indent* 2)))
        (loop
          while a do
          (next-line)
          (pp(pop a)))
        (pp-string ")"))
)

(defun pp(a)
  (cond
  ;atom
    ((atom a)
      (pp-write a))

      ;comment
    ((eq (car a) +line-comment+)
      (blank-line)
      (pp-string (cadr a))
      (next-line)
      )

;reader macros
    ((eq (car a) +feature-test+)
      (next-line)
  (pp-string"#+")
  (pp(cadr a))
  (pp-string" ")
  (pp(caddr a))
      (next-line)
  )

  ;special forms
    ((eq  (car a) 'defun)
  (blank-line)
  (pp-special a 2)
  (blank-line)
        )

        ;1 special arg
    ((member  (car a) '(dolist dotimes))
  (pp-special a 1)
        )

        ;multiline
        ((multiline a)
          (next-line )
      (pp-string "(")
      (pp(pop a))
      (let ((*indent*(+ *indent* 2)))
        (loop
          while a do
          (next-line)
          (pp(pop a)))
        (pp-string ")"))
  (next-line)
        )

        ;inline
        (t
      (pp-string "(")
      (pp(pop a))
        (loop
          while a do
          (pp-string" ")
          (pp(pop a)))
        (pp-string ")"))

))

(defun multiline(a)
  (cond
  ;atom
    ((atom a)
      nil)
      ;comment
    ((eq (car a) +line-comment+)
      t)
;reader macros
    ((eq (car a) +feature-test+)
    t)
    ;special forms
    ((member  (car a) '(defun))
      t)
      ;etc
    (t
      (some #'multiline a))))

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
