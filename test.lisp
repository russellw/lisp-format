(load "main")

(let ((files (parse-args (args))))
  (dolist (file files)
    (format t "~a~%"
      file)
    (let ((s (read-file file)))
      (setf s (transform s))
      (princ (fmt-all s)))))
