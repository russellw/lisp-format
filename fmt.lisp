(defun fmt-loop-clause-n (col s n)
  (values
    (let ((u
            (apply
              #'concatenate
              'string
              (loop
               for i below n
               collect (fmt-inline (pop s))
               collect " "))))
      (concatenate 'string (indent col s) u (fmt (+ col (length u)) (pop s))))
    s))

(defun fmt-loop-clause (col s)
  (values
    (let ((indent (indent col s))
          (u (when (atom (car s)) (fmt-atom (car s)))))
      (cond
        ((not s)
         nil)
        ((consp (car s))
         (concatenate 'string indent (fmt col (pop s))))

        ; 6.1.2.1.1 The for-as-arithmetic subclause
        ; 6.1.2.1.2 The for-as-in-list subclause
        ; 6.1.2.1.3 The for-as-on-list subclause
        ; 6.1.2.1.5 The for-as-across subclause
        ((and (member (car s) '(for as)) (preposition (caddr s)))
         (pop s)
         (apply
           #'concatenate
           'string
           indent
           u
           " "
           (fmt-inline (pop s))
           (loop
            while (preposition (car s))
            collect " "
            collect (fmt-atom (pop s))
            collect " "
            collect (fmt-inline (pop s)))))

        ; 6.1.2.1.4 The for-as-equals-then subclause
        ; 6.1.2.1.6 The for-as-hash subclause
        ; 6.1.2.1.7 The for-as-package subclause
        ((and (member (car s) '(for as)) (eq (caddr s) 'being))
         (pop s)
         (concatenate
           'string
           indent
           u
           " "
           (fmt-inline (pop s))
           " "
           (fmt-inline (pop s))))

        ; 6.1.2.2 Local Variable Initializations
        ((and (eq (car s) 'with) (eq (caddr s) '=))
         (setf (values u s) (fmt-loop-clause-n col s 3))
         (apply
           #'concatenate
           'string
           u
           (loop
            while (eq (car s) 'and)
            do
            (setf (values u s) (fmt-loop-clause-n col s 3))
            collect u)))

        ; 6.1.5 Unconditional Execution Clauses
        ((member (car s) '(do doing))
         (pop s)
         (concatenate 'string indent u))

        ; 6.1.6 Conditional Execution Clauses
        ((member (car s) '(if when unless))
         (pop s)
         (concatenate 'string indent u " " (fmt (+ col (length u) 1) (pop s))))

        ; Other keyword
        (t
         (pop s)
         (if s
           (concatenate 'string indent u " " (fmt (+ col (length u) 1) (pop s)))
           (concatenate 'string indent u)))

        ; For item = (length stack) then (pop stack)
        ; For i fixnum from 3
))
    s))

(defun preposition (a)
  (member
    a
    '(; 6.1.2.1.1 The for-as-arithmetic subclause
       from
       downfrom
       upfrom
       to
       downto
       upto
       below
       above
       by

       ; 6.1.2.1.2 The for-as-in-list subclause
       in

       ; 6.1.2.1.3 The for-as-on-list subclause
       on

       ; 6.1.2.1.5 The for-as-across subclause
       across)))

(defun fmt-loop (col a)
  (destructuring-bind
    (op &rest body)
    a
    (setf op (fmt-inline op))
    (format nil "(~a~a)"
      op
      (apply
        #'concatenate
        'string
        (let ((u))
          (loop
           while body
           do
           (setf (values u body) (fmt-loop-clause (1+ col) body))
           collect u))))))

(defun fmt-clause (col a)
  (if (eq (car a) +special+)
    (fmt-special col a)
    (format nil "(~a)"
      (fmt-lines-indent-separator (1+ col) a))))

(defun fmt-clauses (col s)
  (apply
    #'concatenate
    'string
    (loop
     for a in s
     collect (indent col (list a))
     collect (fmt-clause col a))))

(defun fmt-cond (col a)
  (destructuring-bind
    (op &rest clauses)
    a
    (setf op (fmt-inline op))
    (format nil "(~a~a)"
      op
      (fmt-clauses (+ col 2) clauses))))

(defun fmt-case (col a)
  (destructuring-bind
    (op key &rest clauses)
    a
    (setf op (fmt-inline op))
    (format nil "(~a ~a~a)"
      op
      (fmt-inline key)
      (fmt-clauses (+ col 2) clauses))))

(defun fmt-atom (a)
  (if (characterp a)
    (format nil "#\\~:c"
      a)
    (let ((*print-case* :downcase))
      (prin1-to-string a))))

(defun fmt-inline (a)
  (cond
    ((atom a)
     (fmt-atom a))
    ((eq (car a) +special+)
     (concatenate
       'string
       (cadr a)
       (format nil "~{~a~^ ~}"
         (mapcar #'fmt-inline (cddr a)))))
    (t
     (format nil "(~{~a~^ ~})"
       (mapcar #'fmt-inline a)))))

(defun fmt-params (col params)
  (format nil "~{~a~^ ~}"
    (mapcar #'fmt-inline params)))

(defun indent (col s)
  (cond
    ((not s)
     "")
    ((blankp (car s))
     #.(format nil "~%"))
    (t
     (concatenate
       'string
       #.(format nil "~%")
       (make-array col :initial-element #\Space)))))

(defun fmt-lines-indent-prefix (col s)
  (apply
    #'concatenate
    'string
    (loop
     while s
     for a
     = (pop s)
     collect (indent col (list a))
     collect (fmt col a)
     if (and (keywordp a) s)
     collect " "
     and collect
     (fmt (+ col 1 (length (fmt-atom a))) (pop s)))))

(defun fmt-lines-indent-separator (col s)
  (apply
    #'concatenate
    'string
    (loop
     for (a . more) on s
     collect (fmt col a)
     collect (indent col more))))

(defun fmt-let (col a)
  (destructuring-bind
    (op vars &rest body)
    a
    (setf op (fmt-inline op))
    (format nil "(~a (~a)~a)"
      op
      (fmt-lines-indent-separator (+ col 1 (length op) 2) vars)
      (fmt-lines-indent-prefix (+ col 2) body))))

(defun fmt-defun (col a)
  (destructuring-bind
    (op name params &rest body)
    a
    (setf op (fmt-inline op))
    (format nil "(~a ~a (~a)~a)"
      op
      (fmt-inline name)
      (fmt-params (+ col 1 (length op) 1) params)
      (fmt-lines-indent-prefix (+ col 2) body))))

(defun fmt0 (col a)
  (destructuring-bind
    (op &rest body)
    a
    (setf op (fmt-inline op))
    (format nil "(~a~a)"
      op
      (fmt-lines-indent-prefix (+ col 2) body))))

(defun fmt1 (col a)
  (destructuring-bind
    (op param1 &rest body)
    a
    (setf op (fmt-inline op))
    (format nil "(~a ~a~a)"
      op
      (fmt-inline param1)
      (fmt-lines-indent-prefix (+ col 2) body))))

(defun fmt2 (col a)
  (destructuring-bind
    (op param1 param2 &rest body)
    a
    (setf op (fmt-inline op))
    (format nil "(~a ~a ~a~a)"
      op
      (fmt-inline param1)
      (fmt-inline param2)
      (fmt-lines-indent-prefix (+ col 2) body))))

(defun fmt-special (col a)
  (if (cddr a)
    (concatenate
      'string
      (cadr a)
      (fmt (+ col (length (cadr a))) (caddr a))
      (fmt-lines-indent-prefix (+ col 2) (cdddr a)))
    (cadr a)))

(defun fmt (col a)
  (cond
    ((atom a)
     (fmt-atom a))

    ; Known forms
    ((member (car a) '(case))
     (fmt-case col a))
    ((member (car a) '(acond cond))
     (fmt-cond col a))
    ((member (car a) ' (defmacro defun))
     (fmt-defun col a))
    ((member (car a) '(let let*))
     (fmt-let col a))
    ((eq (car a) 'loop)
     (fmt-loop col a))

    ; Known forms, grouped
    ((member (car a) '(dolist dotimes with-open-file when unless if lambda))
     (fmt1 col a))
    ((member (car a) '(format))
     (fmt2 col a))

    ; Fits on one line
    ((<= (+ col (length (fmt-inline a))) *right-margin*)
     (fmt-inline a))

    ; Needs multiple lines
    ((eq (car a) +special+)
     (fmt-special col a))
    (t
     (fmt0 col a))))

(defun fmt-all (s)
  (format nil "~a~%"
    (fmt-lines-indent-separator 0 s)))
