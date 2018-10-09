(defun remove-extra-blanks (s)

  ; Remove leading blanks
  (loop
   while (blankp (car s))
   do
   (pop s))

  ; Remove consecutive blanks
  (setf
    s
    (loop
     for (a . more) on s
     when (not (blankp a))
     collect a
     when (and (blankp a) (not (blankp (car more))))
     collect a))

  ; Remove trailing blanks, except after line comments
  (setf s (reverse s))
  (loop
   while (and (blankp (car s)) (not (line-comment-p (cadr s))))
   do
   (pop s))
  (reverse s))

(defun add-blanks (a)
  (if (atom a)
    a
    (let ((s (mapcar #'add-blanks a)))
      (remove-extra-blanks
        (loop
         for (a . more) on s
         for b
         = (car more)

         ; Blank around big top-level definition
         if (and (consp a) (member (car a) ' (defmacro defun)))
         collect (list +special+ "")

         ; Current form
         collect a

         ; Blank before line comments
         if (and (not (line-comment-p a)) (line-comment-p b))
         collect (list +special+ "")

         ; Blank after last line comment
         if (and (line-comment-p a) (not more))
         collect (list +special+ "")

         ; Blank around big top-level definition
         if (and (consp a) (member (car a) ' (defmacro defun)))
         collect (list +special+ ""))))))
