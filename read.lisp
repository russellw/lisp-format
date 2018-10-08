(defun err (msg)
  (princ msg)
  (quit))

; http://www.lispworks.com/documentation/lw50/CLHS/Body/02_ad.htm

(defun syntax-type (c)
  (cond
    ((eql c #\Backspace)
     'constituent)
    ((eql c #\Tab)
     'whitespace)
    ((eql c #\Newline)
     'whitespace)
    ((eql c #\Newline)
     'whitespace)
    ((eql c #\Page)
     'whitespace)
    ((eql c #\Return)
     'whitespace)
    ((eql c #\Space)
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
    ((and c (char<= #\0 c #\9))
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
    ((and c (char<= #\A c #\Z))
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
    ((and c (char<= #\a c #\z))
     'constituent)
    ((eql c #\{)
     'constituent)
    ((eql c (elt "|" 0))
     'multiple-escape)
    ((eql c #\})
     'constituent)
    ((eql c #\~)
     'constituent)
    ((eql c #\Rubout)
     'constituent)))

; Tokenizer
(defvar *tok* nil)

(defun single-escape ()
  (if (eql (peek-char) (elt "\\" 0))
    (list (read-char) (read-char))
    (list (read-char))))

(defun multiple-escape ()
  (if (eql (peek-char) (elt "|" 0))
    (append
      (list (read-char))
      (loop
       until (eql (peek-char) (elt "|" 0))
       append (single-escape))
      (list (read-char)))
    (single-escape)))

(defun digits ()
  (coerce
    (loop
     while (and (peek-char nil *standard-input* nil) (digit-char-p (peek-char)))
     collect (read-char))
    'string))

(defun token ()
  (coerce
    (loop
     while (member
             (syntax-type (peek-char nil *standard-input* nil))
             (list
               'constituent
               'single-escape
               'multiple-escape
               'non-terminating-macro-char))
     append (multiple-escape))
    'string))

(defun block-comment ()
  (assert (eql (peek-char) (elt "|" 0)))
  (concatenate
    'string
    (list (read-char))
    (loop
     with c = (read-char)
     collect c
     until (and (eql c (elt "|" 0)) (eql (peek-char) (elt "#" 0)))
     if (and (eql c (elt "#" 0)) (eql (peek-char) (elt "|" 0)))
     append (coerce (block-comment) 'list)
     do
     (setf c (read-char)))
    (list (read-char))))

; http://www.lispworks.com/documentation/lw50/CLHS/Body/02_.htm

(defun lex ()
  (setf
    *tok*
    (cond
      ((not (peek-char t *standard-input* nil))
       nil)

      ; Number or symbol
      ((member
         (syntax-type (peek-char))
         (list 'constituent 'single-escape 'multiple-escape))
       (token))

      ; Semicolon
      ((eql (peek-char) (elt ";" 0))
       (read-line))

      ; Double-quote
      ((eql (peek-char) (elt "\"" 0))
       (concatenate
         'string
         (list (read-char))
         (loop
          until (eql (peek-char nil *standard-input* nil) (elt "\"" 0))
          unless (peek-char nil *standard-input* nil)
          do
          (err "\" unmatched")
          append (single-escape))
         (list (read-char))))

      ; Comma
      ((eql (peek-char) (elt "," 0))
       (read-char)
       (if (eql (peek-char nil *standard-input* nil) (elt "@" 0))
         (progn (read-char) ",@")
         ","))

      ; Sharpsign

      ; http://www.lispworks.com/documentation/lw50/CLHS/Body/02_dh.htm
      ((eql (peek-char) (elt "#" 0))
       (read-char)
       (concatenate
         'string
         "#"
         (digits)
         (cond
           ((not (peek-char nil *standard-input* nil))
            (err "unexpected end of file"))

           ; Sharpsign Backslash
           ((eql (peek-char) (elt "\\" 0))
            (concatenate
              'string
              (list (read-char))
              (if (eq (syntax-type (peek-char nil *standard-input* nil)) 'constituent)
                (token)
                (list (read-char)))))

           ; Sharpsign Left-Parenthesis
           ((eql (peek-char) (elt "(" 0))
            "")

           ; Sharpsign Asterisk
           ((eql (peek-char) (elt "*" 0))
            (concatenate 'string (list (read-char)) (token)))

           ; Sharpsign Colon
           ((eql (peek-char) (elt ":" 0))
            (concatenate 'string (list (read-char)) (token)))

           ; Sharpsign Vertical-Bar
           ((eql (peek-char) (elt "|" 0))
            (block-comment))

           ; Other
           (t
            (list (read-char))))))

      ; Other
      (t
       (string (read-char))))))

; Parser

(defun read* ()

  ; http://www.lispworks.com/documentation/lw50/CLHS/Body/02_d.htm
  (cond
    ((not *tok*)
     (err "unexpected end of file"))

    ; Left-Parenthesis
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

    ;dot
    ((equal *tok* ".")
     (prog1 (list +special+ *tok*) (lex)))

    ; Right-Parenthesis
    ((equal *tok* ")")
     (err "unexpected ')'"))

    ; Single-Quote
    ((member *tok* '("'" "`" "," ",@") :test #'string=)
     (list +special+ (prog1 *tok* (lex)) (read*)))

    ; Semicolon
    ((eql (elt *tok* 0) (elt ";" 0))
     (prog1 (list +special+ *tok*) (lex)))

    ; Double-Quote
    ((eql (elt *tok* 0) (elt "\"" 0))
     (prog1 (read-from-string *tok*) (lex)))

    ; Sharpsign

    ; http://www.lispworks.com/documentation/lw50/CLHS/Body/02_dh.htm
    ((eql (elt *tok* 0) (elt "#" 0))
     (let* ((arg
              (subseq *tok* 1 (position-if-not #'digit-char-p *tok* :start 1)))
            (dispatch (elt* *tok* (1+ (length arg)))))
       (cond

         ; Sharpsign Backslash, character

         ; http://www.lispworks.com/documentation/lw50/CLHS/Body/13_ag.htm
         ((eql dispatch (elt "\\" 0))
          (prog1
            (cond
              ((string-equal *tok* "#\\newline")
               #\Newline)
              ((string-equal *tok* "#\\space")
               #\Space)
              ((string-equal *tok* "#\\rubout")
               #\Rubout)
              ((string-equal *tok* "#\\page")
               #\Page)
              ((string-equal *tok* "#\\tab")
               #\Tab)
              ((string-equal *tok* "#\\backspace")
               #\Backspace)
              ((string-equal *tok* "#\\return")
               #\Return)
              ((string-equal *tok* "#\\linefeed")
               #\Newline)
              ((> (length *tok*) 3)
               (err "unknown character name"))
              (t
               (elt *tok* 2)))
            (lex)))

         ; Sharpsign Asterisk, bit vector
         ((eql dispatch (elt "*" 0))
          (prog1 (list +special+ *tok*) (lex)))

         ; Sharpsign Colon, keyword
         ((eql dispatch (elt ":" 0))
          (prog1 (read-from-string *tok*) (lex)))

         ; Feature expression
         ((member dispatch '(#\+ #\-))
          (list +special+ (prog1 *tok* (lex)) (read*) (read*)))

         ; Sharpsign Vertical-Bar, comment
         ((eql dispatch (elt "|" 0))
          (prog1 (list +special+ *tok*) (lex)))

         ; General prefix
         (t
          (list +special+ (prog1 *tok* (lex)) (read*))))))

    ; Number or symbol

    ; http://www.lispworks.com/documentation/lw50/CLHS/Body/02_c.htm
    (t
     (prog1
       (if (and (not (eql (elt *tok* 0) #\:)) (find #\: *tok*))
         (list +special+ *tok*)
         (read-from-string *tok*))
       (lex)))))

(defun read-all ()
  (lex)
  (loop
   while *tok*
   collect (read*)))

(defun read-file (file)
  (with-open-file (*standard-input* file)
    (read-all)))
