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
    (string=(cadr a)"")))

(defun line-comment-p(a)
  (and
    (consp a)
    (eq(car a)+special+)
    (eql(elt(cadr a)0)#.(elt";"0))))

(defun flatten (s)
  (cond
    ((not s)
      nil)
    ((atom s)
      (list s))
    (t
      (loop
        for a in s
        append (flatten a)))))

(defun write-text-file (file s)
  (with-open-file (*standard-output* file :direction :output :if-exists
                   :supersede :if-does-not-exist :create)
          (princ s)))

(defun read-text-file (file)
  (with-open-file (st file)
    (let ((s (make-string (file-length st))))
      (read-sequence s st)
      s)))
