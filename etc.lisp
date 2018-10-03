(defun subseqp(needle haystack &optional (i 0))
  (when(<=(+ i (length needle))(length haystack))
    (dotimes (j (length needle) t)
      (unless(eql(elt needle j)(elt haystack(+ i j)))
        (return nil)))))

(defun last-elt (s) (elt s (1- (length s))))

(defun elt*(s i)
  (when(< i(length s))
    (elt s i)))

(defun blankp(a)
  (and
    (consp a)
    (eq(car a)+special+)
    (eql(cadr a)"")))
