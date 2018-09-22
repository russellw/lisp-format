(defun flatten (a)
  (let (r)
    (labels
      ((rec (a)
        (cond
          ((not a))
          ((atom a)
            (push a r))
          (t
           (rec (car a))
           (rec (cdr a))))))
      (rec a))
    (nreverse r)))
