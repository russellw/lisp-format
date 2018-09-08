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
        (pp-lines a)
        (pp-string ")"))
)

(defun pp-lines(s)
        (dolist(a s)
          (next-line)
          (pp a)))

(defun pp-spaces(s)
        (dolist(a s)
          (pp-string" ")
          (pp a)))


(defun loop-keyword(a)
(or(not a)
(member a '(for until collect))))

(defun pp-loop(a)
      (pp-string "(")
      (pp-write(pop a))
      (let ((*indent*(+ *indent* 2)))
        (loop
          while a
          do

          (cond
            ((loop-keyword(car a))
              (next-line)
              (pp (pop a))
              (loop
                until (loop-keyword(car a))
                do
                (pp-string" ")
                (pp (pop a))))
            (t
              (pp-lines a)
              (setf a nil)))
        )


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
    ((eq (car a) 'function)
  (pp-string"#'")
  (pp(cadr a))
  )
    ((eq (car a) 'quote)
  (pp-string"'")
  (pp-write(cadr a))
  )

  ;special forms
    ((eq  (car a) 'defun)
  (blank-line)
  (pp-special a 2)
  (blank-line)
        )
    ((eq  (car a) 'let)
      (pp-string "(")
      (pp-write(pop a))
      (pp-string " (")
      (let (  (vars(pop a))
              (*indent*(+ *indent* 6)))
             (pp-write(pop vars))
             (dolist (v vars)
              (next-line)
              (pp-write v))
      )
      (pp-string")")
      (let        ((*indent*(+ *indent* 2)))
        (pp-lines a)
        (pp-string ")"))
        )
    ((eq  (car a) 'loop)
      (pp-loop a))

        ;0 special args
    ((member  (car a) '(ignore-errors))
  (pp-special a 0)
        )

        ;1 special arg
    ((member  (car a) '(dolist dotimes with-open-file))
  (pp-special a 1)
        )

        ;multiline
        ((multiline a)
      (pp-string "(")
      (pp(pop a))
      (let ((*indent*(+ *indent* 2)))
        (pp-lines a)
        (pp-string ")"))
        )

        ;inline
        (t
      (pp-string "(")
      (pp(pop a))
        (pp-spaces a)
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
    ((member  (car a) '(defun let loop))
      t)
        ;0 special args
    ((member  (car a) '(ignore-errors))
 t
        )

        ;1 special arg
    ((member  (car a) '(dolist dotimes with-open-file))
  t
        )
      ;etc
    ((eq (car a) 'quote)
    nil)

    (t
      (some #'multiline a))))

(defun write-file (file s)
  (with-open-file (*standard-output* file
    :direction :output
    :if-exists :supersede)
  (let((*print-case* :downcase))
    (pp-lines s)
  (terpri)
  )))
