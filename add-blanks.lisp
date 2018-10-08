(defun want-blank-before (a)
  (cond
    ((atom a)
     nil)
    ((member (car a) ' (defmacro defun)))
    ((and (line-comment-p a) (subseqp ";" (cadr a))))))

(defun want-blank-after (a more)
  (cond
    ((atom a)
     nil)
    ((member (car a) ' (defmacro defun)))
    ((and (line-comment-p a) (not more)))))

(defun remove-extra-blanks (s)
  (loop
   while (blankp (car s))
   do
   (pop s))
  (loop
   for (a . more) on s
   when (not (blankp a))
   collect a
   when (and (blankp a) more (not (blankp (car more))))
   collect a))

(defun add-blanks (a)
  (if (atom a)
    a
    (progn
      (setf a (mapcar #'add-blanks a))
      (remove-extra-blanks
        (loop
         for (b . more) on a
         if (want-blank-before b)
         collect (list +special+ "")
         collect b
         if (want-blank-after b more)
         collect (list +special+ ""))))))
