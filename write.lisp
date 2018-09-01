(defun print-comment (st s)
  (princ (cadr s) st))

(defun write-file (file s)
  (set-pprint-dispatch `(cons (eql ,+comment+)) #'print-comment)
  (mapc #'pprint s))
