(defmacro acond (&rest clauses)
  (when clauses
    (let ((clause (car clauses))
          (name (gensym)))
      `(let ((,name ,(car clause)))
         (if ,name
           (let ((it ,name))
             ,@(cdr clause))
           (acond ,@(cdr clauses)))))))

(defmacro pr (a)
  (let ((name (gensym)))
    `(let ((,name ,a))
       (format t "~s = ~s~%"
         ',a
         ,name)
       ,name)))
