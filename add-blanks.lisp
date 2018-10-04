(defun want-blank-before(a)
  (cond
    ((atom a)
      nil)

   ((member(car a)'(defmacro defun)))
   ((and
      (line-comment-p a)
      (subseqp";"(cadr a))))
  )
)

(defun want-blank-after(a more)
  (cond
    ((atom a)
      nil)

   ((member(car a)'(defmacro defun)))
   ((and
      (line-comment-p a)
      (not more)))
  )
)

(defun add-blanks(a)
  (if(atom a)
    a
    (loop
      for (b . more) on a
      if(want-blank-before b)
        collect(list +special+"")
      collect b
      if(want-blank-after b more)
        collect(list +special+"")
    )
  )
)
