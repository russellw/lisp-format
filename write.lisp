

(defun fmt-loop-body(col s)
  (cond
    ((not s)
      nil)
    ((consp(car s))
      (list*
        (indent col s)
        (fmt col(car s))
        (fmt-loop-body col(cdr s))
      )
    )
    (t
      (let((k(fmt-inline(car s))))
        (list*
          (indent col s)
          k
          " "
          (fmt(+ col(length k)1)(cadr s))
          (fmt-loop-body col(cddr s))
        )
      )
    )
  )
)

(defun fmt-loop(col a)
  (destructuring-bind (op &rest body) a
    (setf op(fmt-inline op))
    (format nil "(~a~a)"
      op
      (apply #'concatenate 'string
        (fmt-loop-body(1+ col)body)
      )
    )
  )
)

(defun fmt-clause(col a)
    (if(eq(car a)+special+)
      (fmt-special col a)
      (format nil"(~a)"(fmt-lines-indent-separator(1+ col)a)))
)
(defun fmt-clauses(col s)
  (apply #'concatenate 'string
    (loop for a in s
      collect (indent col (list a))
      collect (fmt-clause col a)
    )
  )
)
(defun fmt-cond(col a)
  (destructuring-bind (op &rest clauses) a
    (setf op(fmt-inline op))
    (format nil "(~a~a)"
      op
      (fmt-clauses (+ col 2) clauses)
    )
  )
)


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

(defun indent(col s)
  (cond
    ((not s)
      ""
    )
    ((blankp (car s))
      #.(format nil "~%")
    )
    (t
      (concatenate 'string
        #.(format nil "~%")
        (make-array col :initial-element #\space)
      )
    )
  )
)

(defun fmt-lines-indent-prefix(col s)
  (apply #'concatenate 'string
    (loop while s
      for a =(pop s)
      collect (indent col (list a))
      collect (fmt col a)
      if(and(keywordp a)s)
        collect" "and
        collect(fmt(+ col 1(length(fmt-atom a)))(pop s))
    )
  )
)

(defun fmt-lines-indent-separator(col s)
  (apply #'concatenate 'string
    (loop for (a . more) on s
      collect (fmt col a)
      collect (indent col more)
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
  (if(cddr a)
      (concatenate 'string (cadr a) (fmt (+ col (length(cadr a))) (caddr a)) (fmt-lines-indent-prefix (+ col 2) (cdddr a)))
      (cadr a))
)

(defun fmt(col a)
  (cond
    ((atom a)
      (fmt-atom a))

      ;known forms
    ((eq(car a)'cond)
      (fmt-cond col a))
    ((member(car a)'(defmacro defun))
      (fmt-defun col a))
    ((member(car a)'(let let*))
      (fmt-let col a))
    ((eq(car a)'loop)
      (fmt-loop col a))
    ((member(car a)'(dolist dotimes with-open-file when unless if))
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
