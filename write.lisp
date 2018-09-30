(defvar *indent* 0)
(defvar *newlines* 0)

(defun next-line ()
  (setf *newlines* (max *newlines* 1)))

(defun blank-line ()
  (setf *newlines* 2))

(defun pp-position ()
  (unless
    (= *newlines* 0)
    (unless
      (= (file-position *standard-output*) 0)
      (dotimes (i *newlines*)
        (terpri)))
    (dotimes (i *indent*)
      (write-char #\Space))
    (setf *newlines* 0)))

(defun pp-write (a)
  (pp-position)
  (write a))

(defun pp-list (a)
  (pp-position)
  (if a (write a) (princ "()")))

(defun pp-string (s)
  (pp-position)
  (princ s))

(defun pp-special (a n)
  (pp-string "(")
  (pp-write (pop a))
  (dotimes (i n)
    (pp-string " ")
    (pp-list (pop a)))
  (let ((*indent* (+ *indent* 2)))
    (pp-lines a)
    (pp-string ")")))

(defun pp-lines (s)
  (dolist (a s)
    (next-line)
    (pp a)))

(defun pp-spaces (s)
  (dolist (a s)
    (pp-string " ")
    (pp a)))

(defun loop-keyword (a)
  (or (not a) (member a '(named initially finally for as with do collect
                          collecting append appending nconc nconcing into count
                          counting sum summing maximize return maximizing
                          minimize minimizing doing thereis always never if
                          when unless repeat while until))))

(defun pp-loop (a)
  (pp-string "(")
  (pp-write (pop a))
  (let ((*indent* (+ *indent* 2)))
    (loop
      while a
      do
      (cond
        ((eq (car a) 'do)
         (pp-lines a)
         (setf a nil))
        ((loop-keyword (car a))
         (next-line)
         (pp (pop a))
         (loop
           until (loop-keyword (car a))
           do
           (pp-string " ")
           (pp (pop a))))
        (t
         (pp-lines a)
         (setf a nil))))
    (pp-string ")")))

(defun pp (a)
  (cond

    ;atom
    ((characterp a)
     (format t "#\\~:c" a))
    ((atom a)
     (pp-write a))

    ;comment
    ((eq (car a) +special+)
     (blank-line)
     (pp-string (cadr a))
     (next-line))

    ;reader macros
    ((eq (car a) 'function)
     (pp-string "#'")
     (pp (cadr a)))
    ((eq (car a) 'quote)
     (pp-string "'")
     (pp-write (cadr a)))

    ;special forms
    ((eq (car a) 'cond)
     (pp-string "(")
     (pp-write (pop a))
     (let ((*indent* (+ *indent* 2)))
       (dolist (b a)
         (cond
           ((eq (car b) +special+)
            (blank-line)
            (pp-string (cadr b))
            (next-line))
           (t
            (next-line)
            (pp-string "(")
            (let ((*indent* (+ *indent* 1)))
              (pp (pop b))
              (pp-lines b)
              (pp-string ")")))))
       (pp-string ")")))
    ((eq (car a) 'defun)
     (blank-line)
     (pp-special a 2)
     (blank-line))
    ((member (car a) (list 'let 'let*))
     (pp-string "(")
     (let* ((op (pop a))
            (vars (pop a))
            (*indent* (+ *indent* 1 (length (string op)) 2)))
       (pp-write op)
       (pp-string " (")
       (pp-write (pop vars))
       (dolist (v vars)
         (next-line)
         (pp-write v)))
     (pp-string ")")
     (let ((*indent* (+ *indent* 2)))
       (pp-lines a)
       (pp-string ")")))
    ((eq (car a) 'loop)
     (pp-loop a))

    ;0 special args
    ((member (car a) '(ignore-errors))
     (pp-special a 0))

    ;1 special arg
    ((member (car a) '(dolist dotimes with-open-file))
     (pp-special a 1))

    ;multiline
    ((multiline a)
     (pp-string "(")
     (pp (pop a))
     (let ((*indent* (+ *indent* 2)))
       (pp-lines a)
       (pp-string ")")))

    ;inline
    (t
     (pp-string "(")
     (pp (pop a))
     (pp-spaces a)
     (pp-string ")"))))

(defun multiline (a)
  (cond

    ;atom
    ((atom a)
     nil)

    ;comment
    ((eq (car a) +special+)
     t)

    ;special forms
    ((member (car a) '(defun let loop))
     t)

    ;0 special args
    ((member (car a) '(ignore-errors))
     t)

    ;1 special arg
    ((member (car a) '(dolist dotimes with-open-file))
     t)

    ;etc
    ((eq (car a) 'quote)
     nil)
    (t
     (some #'multiline a))))




(defun fmt-atom(a)
(if    (characterp a)
     (format nil "#\\~:c" a)

      (let((*print-case* :downcase))
        (prin1-to-string a)))
)

(defun fmt-inline(a)
  (cond
    ((atom a)
      (fmt-atom a))
    ((eq(car a)+special+)
      (concatenate 'string (cadr a) (format nil "~{~a~^ ~}" (mapcar #'fmt-inline (cddr a)))))
    (t
      (format nil "(~{~a~^ ~})" (mapcar #'fmt-inline a)))
  )
)

(defun fmt-params(col params)
  (format nil "~{~a~^ ~}" (mapcar #'fmt-inline params))
)

(defun fmt-lines-indent-prefix(col s)
  (apply #'concatenate 'string
    (loop for a in s
      collect #.(format nil "~%")
      collect(make-array col :initial-element #\space)
      collect (fmt col a)
    )
  )
)

(defun fmt-lines-indent-separator(col s)
  (apply #'concatenate 'string
    (loop for (a . more) on s
      collect (fmt col a)
      if more
      collect #.(format nil "~%")and
      collect(make-array col :initial-element #\space)
    )
  )
)

(defun fmt-let(col a)
  (destructuring-bind (op vars &rest body) a
    (setf op(fmt-inline op))
    (format nil "(~a (~a)~a)"
      op
      (fmt-lines-indent-separator  (+ col 1 (length op) 2) vars)
      (fmt-lines-indent-prefix (+ col 2) body)
    )
  )
)

(defun fmt-defun(col a)
  (destructuring-bind (op name params &rest body) a
    (setf op(fmt-inline op))
    (format nil "(~a ~a (~a)~a)"
      op
      (fmt-inline name)
      (fmt-params (+ col 1 (length op) 1) params)
      (fmt-lines-indent-prefix (+ col 2) body)
    )
  )
)

(defun fmt0(col a)
  (destructuring-bind (op &rest body) a
    (setf op(fmt-inline op))
    (format nil "(~a~a)"
      op
      (fmt-lines-indent-prefix (+ col 2) body)
    )
  )
)

(defun fmt1(col a)
  (destructuring-bind (op param1 &rest body) a
    (setf op(fmt-inline op))
    (format nil "(~a ~a~a)"
      op
      (fmt-inline param1)
      (fmt-lines-indent-prefix (+ col 2) body)
    )
  )
)

(defun fmt2(col a)
  (destructuring-bind (op param1 param2 &rest body) a
    (setf op(fmt-inline op))
    (format nil "(~a ~a ~a~a)"
      op
      (fmt-inline param1)
      (fmt-inline param2)
      (fmt-lines-indent-prefix (+ col 2) body)
    )
  )
)

(defun fmt-special(col a)
  (cond
    ((member (cadr a) '("#+" "#-") :test #'string=)
      (concatenate 'string (cadr a) (fmt-inline(caddr a)) (fmt-lines-indent-prefix (+ col 2) (cdddr a)))
    )
    (t
      (concatenate 'string (cadr a) (fmt-lines-indent-prefix (+ col 2) (cddr a)))
    )
  )
)

(defun fmt(col a)
  (cond
    ((atom a)
      (fmt-atom a))

      ;known forms
    ((eq(car a)'defun)
      (fmt-defun col a))
    ((member(car a)'(let let*))
      (fmt-let col a))
    ((member(car a)'(dolist dotimes with-open-file when unless))
      (fmt1 col a))
    ((member(car a)'(format))
      (fmt2 col a))

      ;fits on one line
    ((<=(+ col(length(fmt-inline a)))*right-margin*)
      (fmt-inline a)
    )
    ;needs multiple lines
    ((eq(car a)+special+)
      (fmt-special col a))
    (t
      (fmt0 col a)
    )
  )
)

(defun write-all (s)
    (dolist (a s)
      (format t "~a~%" (fmt 0 a))
    )
)

(defun write-file (file s)
  (with-open-file (*standard-output* file :direction :output :if-exists
                   :supersede)
          (write-all s)))
