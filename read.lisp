(defconstant +line-comment+ (gensym))
(defconstant +package-marker+ (gensym))
(defconstant +package-marker-2+ (gensym))
(defconstant +feature-test+ (gensym))
(defun err(msg)
  (princ msg)
  (quit))

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
    ((eql c (elt "\"" 0))
      'terminating-macro-char)
    ((eql c #\#)
      'non-terminating-macro-char)
    ((eql c #\$)
      'constituent)
    ((eql c #\%)
      'constituent)
    ((eql c #\&)
      'constituent)
    ((eql c (elt "'" 0))
      'terminating-macro-char)
    ((eql c (elt "(" 0))
      'terminating-macro-char)
    ((eql c (elt ")" 0))
      'terminating-macro-char)
    ((eql c #\*)
      'constituent)
    ((eql c #\+)
      'constituent)
    ((eql c (elt "," 0))
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
    ((eql c (elt ";" 0))
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
    ((eql c (elt "\\" 0))
      'single-escape)
    ((eql c #\])
      'constituent)
    ((eql c #\^)
      'constituent)
    ((eql c #\_)
      'constituent)
    ((eql c (elt "`" 0))
      'terminating-macro-char)
    ((char<= #\a c #\z)
      'constituent)
    ((eql c #\{)
      'constituent)
    ((eql c (elt "|" 0))
      'multiple-escape)
    ((eql c #\})
      'constituent)
    ((eql c #\~)
      'constituent)
    ((eql c #\rubout)
      'constituent)
  )
)

;tokenizer
(defvar *tok* nil)

(defun single-escape()
                                  (if (eql(peek-char)(elt"\\"0))
                                    (list(read-char)(read-char))
                                    (list(read-char))))

(defun multiple-escape()
          (if (eql(peek-char)(elt"\\"0))
            (append
                        (list(read-char))
                        (loop
                          until (eql(peek-char)(elt"|"0))
                          append(single-escape)
                        )
                        (list(read-char))
            )
            (single-escape)
          )
)

(defun lex()
  (cond
    ((not(peek-char t *standard-input* nil))
      (setf *tok* nil))

    ;comment
    ((eql(peek-char)(elt";"0))
    (setf *tok* (read-line))
    )

    ;number or symbol
    ((member(syntax-type(peek-char))(list 'constituent 'single-escape 'multiple-escape))
      (setf *tok*
                (coerce
                    (loop
                      while(member(syntax-type(peek-char nil *standard-input* nil))
                                  (list 'constituent 'single-escape 'multiple-escape 'non-terminating-macro-char))
                      append(multiple-escape)
                    )
                  'string
                )
      )
    )

    ;other
    (t
      (setf *tok* (string(read-char)))
    )
  )
)

;parser

(defun read*()
  (cond
    ((not *tok*)
      (err "unexpected end of file"))
    ((equal *tok* "'" )
      (lex)
      (list 'quote (read*))
    )
    (t
      (let ((s *tok*))
        (lex)
        (cond
          ((=(length(split-string"::"s))2)
              (cons +package-marker-2+
                (loop
                  for x in (split-string"::"s)
                  collect(read-from-string x)
                )
              )
          )
          ((=(length(split-string":"s))2)
              (cons +package-marker+
                (loop
                  for x in (split-string":"s)
                  collect(read-from-string x)
                )
              )
          )
          (t
            (read-from-string s))
        )
      )
    )
  )
)
(defun read-all()
  (lex)
  (loop
    while *tok*
    collect (read*)
  )
)

(defun line-comment-reader (*standard-input* c)
  (list +line-comment+ (concatenate 'string (string c) (read-line *standard-input* nil #\Newline t))))

(defun feature-test-reader (*standard-input* c x)
  (declare (ignore c x))
  (list +feature-test+ (read *standard-input* t nil t) (read *standard-input* t nil t)))

(defun read-file (file)
  (with-open-file (*standard-input* file)
    ;(set-macro-character (elt ";" 0) #'line-comment-reader)
    ;(set-dispatch-macro-character #\# #\+ #'feature-test-reader)
    (loop
      for x = (read *standard-input* nil *standard-input*)
      until (eq x *standard-input*)
      collect x)))
