(defun comment-case(a)
  (cond
    ((atom a)
      a)
    ((line-comment-p a)
     (let*(
                (s(cadr a))
                (i(position-if-not #'(lambda(c)(eql c #.(elt";"0)))s))
          )
      (unless i
        (setf i(length s)))
      (if(eql(elt* s i)#\space)
        (incf i))
      (cond
        ((= i(length s))
          a)
        ((subseqp "http"s i)
          a)
        ((lower-case-p(elt s i))
          (list
            +special+
            (concatenate 'string
              (subseq s 0 i)
              (string(char-upcase(elt s i)))
              (subseq s (1+ i))
            )
          )
        )
        (t a)
      )
     )
    )
    (t
      (mapcar #'comment-case a))
  )
)
