(defun comment-space(a)
  (cond
    ((atom a)
      a)
    ((and(eq(car a)+special+)
         (eql(elt(cadr a)0)#.(elt";"0))
         (find-if-not #'(lambda(c)(eql c #.(elt";"0)))(cadr a))
     )
     (let*(
                (s(cadr a))
                (i(position-if-not #'(lambda(c)(eql c #.(elt";"0)))s))
          )
      (if(eql(elt s i)#\space)
        a
        (list
          +special+
          (concatenate 'string
            (subseq s 0 i)
            " "
            (subseq s i)
          )
        )
      )
     )
    )
    (t
      (mapcar #'comment-space a))
  )
)
