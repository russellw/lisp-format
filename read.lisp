(defconstant +atom+ (gensym))
(defconstant +prefix+ (gensym))

(defconstant +backquote+ (gensym))
(defconstant +comma+ (gensym))
(defconstant +comma-at+ (gensym))

(defconstant +feature-plus+ (gensym))
(defconstant +feature-minus+ (gensym))

(defconstant +array+ (gensym))
(defconstant +structure+ (gensym))

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
    ((and c(char<= #\0 c #\9))
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
    ((and c(char<= #\A c #\Z))
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
    ((and c(char<= #\a c #\z))
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
          (if (eql(peek-char)(elt"|"0))
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

(defun digits()
                (coerce
                    (loop
                      while(and(peek-char nil *standard-input* nil)
                               (digit-char-p(peek-char)))
                      collect(read-char)
                    )
                  'string
                )
)

(defun token()
                (coerce
                    (loop
                      while(member(syntax-type(peek-char nil *standard-input* nil))
                                  (list 'constituent 'single-escape 'multiple-escape 'non-terminating-macro-char))
                      append(multiple-escape)
                    )
                  'string
                )
)

(defun block-comment()
  (concatenate 'string
    (list(read-char))
    (loop
      with c = (read-char)
      collect c
      until (and(eql c (elt"|"0))(eql(peek-char)(elt"#"0)))
      if (and(eql c (elt"#"0))(eql(peek-char)(elt"|"0)))
        append(concatenate 'list (list(read-char)) (block-comment))
      do
      (setf c(read-char))
    )
    (list(read-char))
  )
)

;http://www.lispworks.com/documentation/lw50/CLHS/Body/02_.htm
(defun lex()
(setf *tok*
  (cond
    ((not(peek-char t *standard-input* nil))
     nil)

    ;number or symbol
    ((member(syntax-type(peek-char))(list 'constituent 'single-escape 'multiple-escape))
      (token)
    )

    ;semicolon
    ((eql(peek-char)(elt";"0))
     (read-line)
    )

    ;double-quote
    ((eql(peek-char)(elt"\""0))
      (concatenate 'string
        (list(read-char))
        (loop
          until(eql(peek-char nil *standard-input* nil)(elt"\""0))
          unless(peek-char nil *standard-input* nil)
            do
            (err"\" unmatched")
          append(single-escape)
        )
        (list(read-char))
      )
    )

    ;comma
    ((eql(peek-char)(elt","0))
    (read-char)
    (if(eql(peek-char nil *standard-input* nil)(elt"@"0))
      (progn
        (read-char)
         ",@")
      ","
    )
    )

    ;sharpsign
    ;http://www.lispworks.com/documentation/lw50/CLHS/Body/02_dh.htm
    ((eql(peek-char)(elt"#"0))
      (read-char)
      (concatenate 'string
        "#"
        (digits)
        (cond
          ((not(peek-char nil *standard-input* nil))
            (err "unexpected end of file"))

          ;Sharpsign Backslash
          ((eql(peek-char)(elt"\\"0))
            (concatenate 'string
              (list(read-char))
              (if(eq(syntax-type(peek-char nil *standard-input* nil))'constituent)
                (token)
                (list(read-char)))
            )
          )

          ;Sharpsign Asterisk
          ((eql(peek-char)(elt"*"0))
            (concatenate 'string
              (list(read-char))
              (token)
            )
          )

          ;Sharpsign Colon
          ((eql(peek-char)(elt":"0))
            (concatenate 'string
              (list(read-char))
              (token)
            )
          )

          ;Sharpsign Vertical-Bar
          ((eql(peek-char)(elt"|"0))
            (block-comment)
          )

          ;other
          (t
            (list(read-char)))
        )
      )
    )

    ;other
    (t
       (string(read-char))
    )
  )
)
)
;parser
(defun read*()
  ;http://www.lispworks.com/documentation/lw50/CLHS/Body/02_d.htm
  (cond
    ((not *tok*)
      (err "unexpected end of file"))

    ;Left-Parenthesis
    ((equal *tok* "(" )
      (lex)
      (prog1
      (loop
        until(equal *tok*")")
        collect(read*)
      )
      (lex)
      )
    )

    ;Right-Parenthesis
    ((equal *tok* ")" )
      (err"unexpected ')'"))

    ;Single-Quote
    ((equal *tok* "'" )
      (lex)
      `(quote ,(read*))
    )

    ;Semicolon
    ((eql(elt *tok* 0)(elt ";" 0))
      (prog1
        (list +atom+ *tok*)
        (lex))
    )

    ;Double-Quote
    ((eql(elt *tok* 0)(elt "\"" 0))
      (prog1
        (read-from-string *tok*)
        (lex))
    )

    ;Backquote
    ((equal *tok* "`" )
      (lex)
      (list +backquote+ (read*))
    )

    ;Comma
    ((equal *tok* "," )
      (lex)
      (list +comma+ (read*))
    )
    ((equal *tok* ",@" )
      (lex)
      (list +comma-at+ (read*))
    )

    ;Sharpsign
    ;http://www.lispworks.com/documentation/lw50/CLHS/Body/02_dh.htm
    ((eql(elt *tok* 0)(elt "#" 0))
      (let*(
              (arg(subseq *tok* 1(position-if-not #'digit-char-p *tok* :start 1)))
              (n
                (when
                  (> (length arg) 0)
                  (parse-integer arg))
              )
              (dispatch(elt* *tok* (1+(length arg))))
           )
        (cond

          ;Sharpsign Backslash, character
          ;http://www.lispworks.com/documentation/lw50/CLHS/Body/13_ag.htm
          ((eql dispatch(elt "\\" 0))
            (cond
              ((string-equal *tok*"#\\newline")
                #\newline)
              ((string-equal *tok*"#\\space")
                #\space)
              ((string-equal *tok*"#\\rubout")
                #\rubout)
              ((string-equal *tok*"#\\page")
                #\page)
              ((string-equal *tok*"#\\tab")
                #\tab)
              ((string-equal *tok*"#\\backspace")
                #\backspace)
              ((string-equal *tok*"#\\return")
                #\return)
              ((string-equal *tok*"#\\linefeed")
                #\linefeed)
              ((>(length *tok*)3)
                (err "unknown character name"))
              (t
                (elt *tok* 2))
            )
          )

          ;Sharpsign Single-Quote, function
          ((equal *tok*"#'")
            (lex)
            `(function ,(read*))
          )

          ;Sharpsign Left-Parenthesis, vector
          ((eql dispatch(elt"("0))
            (lex)
            (let*(
                  (s      (loop
                            until(equal *tok*")")
                            collect(read*)
                          )
                  )
                  (v(make-array (if n n (length s))))
                )
                (fill-vector v s)
                v
            )
          )

          ;Sharpsign Asterisk, bit vector
          ((eql dispatch(elt"*"0))
            (prog1
              (list
                +atom+
                *tok*)
              (lex))
          )

          ;Sharpsign Colon, keyword
          ((eql dispatch(elt":"0))
            (prog1
              (read-from-string *tok*)
              (lex))
          )

          ;Sharpsign Plus, feature expression
          ((eql dispatch(elt"+"0))
            (lex)
            (list +feature-plus+(read*)(read*))
          )

          ;Sharpsign Minus, feature expression
          ((eql dispatch(elt"-"0))
            (lex)
            (list +feature-minus+(read*)(read*))
          )

          ;Sharpsign Vertical-Bar, comment
          ((eql dispatch(elt"|"0))
            (prog1
              (list +atom+ *tok*)
              (lex))
          )

          ;general prefix
          (t
            (list
              +prefix+
              (prog1
                *tok*
                (lex)
              )
              (read*)
            )
          )

        )
      )
    )

    ;number or symbol
    ;http://www.lispworks.com/documentation/lw50/CLHS/Body/02_c.htm
    (t
      (prog1
        (if(and(not(eql(elt *tok* 0)#\:))
               (find #\: *tok*))
          (list +atom+ *tok*)
          (read-from-string *tok*))
        (lex)
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
