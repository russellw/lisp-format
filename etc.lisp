(defun substringp(needle haystack i)
  (when(<=(+ i (length needle))(length haystack))
    (dotimes (j (length needle) t)
      (unless(eql(elt needle j)(elt haystack(+ i j)))
        (return nil)))))

