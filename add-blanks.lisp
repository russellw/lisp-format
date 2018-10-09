
(defun remove-extra-blanks (s)
  ;remove leading blanks
  (loop
   while (blankp (car s))
   do
   (pop s))
   ;remove consecutive blanks
  (setf s
  (loop
   for (a . more) on s
   when (not (blankp a))
   collect a
   when (and (blankp a) (not (blankp (car more))))
   collect a)
   )
  ;remove trailing blanks, except after line comments
  (setf  s(reverse s))
  (loop
   while(and (blankp (car s))(not(line-comment-p(cadr s))))
   do
   (pop s))
  )
  (reverse s)
   )

(defun add-blanks (a)
  (if (atom a)
    a
    (let(( s (mapcar #'add-blanks a)))
      (remove-extra-blanks
        (loop
         for (a . more) on s
         with  b = (car more)

         if (and(consp a)(member (car a) ' (defmacro defun)))
          collect (list +special+ "")

         collect a

         if(and(not(line-comment-p a))(line-comment-p b))
          collect (list +special+ "")
         if (and(consp a)(member (car a) ' (defmacro defun)))
          collect (list +special+ "")

         )))))
