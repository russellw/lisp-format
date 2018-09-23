(defun subseqp(needle haystack i)
  (when(<=(+ i (length needle))(length haystack))
    (dotimes (j (length needle) t)
      (unless(eql(elt needle j)(elt haystack(+ i j)))
        (return nil)))))


(defun split-string(delimiter s)
  (let((i 0))
    (labels
      (
        (skip-delimiters ()
          (loop
            while(subseqp delimiter s i)
            do
            (incf i(length delimiter))
          )
        )
        (next-delimiter ()
          (let((j
              (search delimiter s :start2 i)))
             (if j
              j
              (length s)))
        )
        (next-word ()
          (skip-delimiters)
          (let*((j(next-delimiter))
                (word(subseq s i  j)))
             (setf i j)
             (if(equal word "")nil word))
        )
      )
      (loop
        while(< i(length s))
        if(next-word)
        collect it
      )
    )
  )
)
