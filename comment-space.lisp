(defun comment-space (a)
  (cond
    ((atom a)
     a)
    ((line-comment-p a)
     (let* ((s (cadr a))
            (i (position-if-not #'(lambda (c) (eql c #.(elt ";" 0))) s)))
       (cond
         ((not i)
          a)
         ((eql (elt s i) #\Space)
          a)
         (t
          (list +special+ (concatenate 'string (subseq s 0 i) " " (subseq s i)))))))
    (t
     (mapcar #'comment-space a))))
