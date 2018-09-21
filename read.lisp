;http://www.lispworks.com/documentation/lw50/CLHS/Body/02_ad.htm
(defun syntax-type(c)
  (cond
    ((eql c #\backspace)
      'constituent)
    ((eql c #\tab)
      'whitespace)
    ((eql c #\newline)
      'whitespace)
    ((eql c #\linefeed)
      'whitespace)
    ((eql c #\page)
      'whitespace)
    ((eql c #\return)
      'whitespace)
    ((eql c #\space)
      'whitespace)
    ((eql c #\!)
      'constituent)
    ((eql c (char "\"" 0))
      'terminating-macro-char)
    ((eql c #\#)
      'non-terminating-macro-char)
    ((eql c #\$)
      'constituent)
    ((eql c #\%)
      'constituent)
    ((eql c #\&)
      'constituent)
    ((eql c (char "'" 0))
      'terminating-macro-char)
    ((eql c (char "(" 0))
      'terminating-macro-char)
    ((eql c (char ")" 0))
      'terminating-macro-char)
    ((eql c #\*)
      'constituent)
    ((eql c #\+)
      'constituent)
    ((eql c (char "," 0))
      'terminating-macro-char)
    ((eql c #\-)
      'constituent)
    ((eql c #\.)
      'constituent)
    ((eql c #\/)
      'constituent)
    ((char<= #\0 c #\9)
      'constituent)
    ((eql c #\:)
      'constituent)
    ((eql c (char ";" 0))
      'terminating-macro-char)
    ((eql c #\<)
      'constituent)
    ((eql c #\=)
      'constituent)
    ((eql c #\>)
      'constituent)
    ((eql c #\?)
      'constituent)
    ((eql c #\@)
      'constituent)
    ((char<= #\A c #\Z)
      'constituent)
    ((eql c #\[)
      'constituent)
    ((eql c (char "\\" 0))
      'single-escape)
    ((eql c #\])
      'constituent)
    ((eql c #\^)
      'constituent)
    ((eql c #\_)
      'constituent)
    ((eql c (char "`" 0))
      'terminating-macro-char)
    ((char<= #\a c #\z)
      'constituent)
    ((eql c #\{)
      'constituent)
    ((eql c (char "|" 0))
      'multiple-escape)
    ((eql c #\})
      'constituent)
    ((eql c #\~)
      'constituent)
    ((eql c #\rubout)
      'constituent)
  )
)

(defun line-comment-reader (*standard-input* c)
  (list +line-comment+ (concatenate 'string (string c) (read-line *standard-input* nil #\Newline t))))

(defun feature-test-reader (*standard-input* c x)
  (declare (ignore c x))
  (list +feature-test+ (read *standard-input* t nil t) (read *standard-input* t nil t)))

(defun read-file (file)
  (with-open-file (*standard-input* file)
    ;(set-macro-character (char ";" 0) #'line-comment-reader)
    ;(set-dispatch-macro-character #\# #\+ #'feature-test-reader)
    (loop
      for x = (read *standard-input* nil *standard-input*)
      until (eq x *standard-input*)
      collect x)))
