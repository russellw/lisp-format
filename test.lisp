(load "main")
;syntax-type
(assert(eq(syntax-type #\a) 'constituent))
(assert(eq(syntax-type nil) nil))
;subseqp
(assert(subseqp "a" "abc" 0))
(assert(not(subseqp "a" "abc" 1)))
(assert(subseqp "quick" "the quick brown fox jumped over the lazy dog" 4))
(assert(not(subseqp "quick" "the quick brown fox jumped over the lazy dog" 5)))
(assert(not(subseqp "quickened" "the quick brown fox jumped over the lazy dog" 4)))
(assert(not(subseqp "quick" "the quick brown fox jumped over the lazy dog" 400)))
;split-string
(assert(equal(split-string " " "one two three")'("one" "two" "three")))
(assert(equal(split-string " " " one two three")'("one" "two" "three")))
(assert(equal(split-string " " "one two three ")'("one" "two" "three")))

;read*
(defun read-tokens()
(loop
while *tok*
collect *tok*
do(lex)))

(defun read1(s)
(when t
(format t "~a~40T~a~%" s
(with-input-from-string(*standard-input* s)
(lex)
(read-tokens)
)
)
)

(with-input-from-string(*standard-input* s)
(lex)
(read*)))
;numbers
(assert(equal(read1 "123")123))
(assert(equal(read1 "+123")+123))
(assert(equal(read1 "-123")-123))
(assert(equal(read1 "123/456")123/456))
(assert(equal(read1 "-123/456")-123/456))
(assert(equal(read1 "123.5")123.5))
(assert(equal(read1 "-123.5")-123.5))
(assert(equal(read1 "123.5e6")123.5e6))
(assert(equal(read1 "-123.5e6")-123.5e6))
;symbols
(assert(equal(read1 "a")'a))
(assert(equal(read1 "\\a")'\a))
(assert(equal(read1 "|one two three|")'|one two three|))
(assert(equal(read1 ":abc"):abc))
(assert(equal(read1 "abc:xyz")(list +package-marker+ 'abc 'xyz)))
(assert(equal(read1 "abc::xyz")(list +package-marker-2+ 'abc 'xyz)))
;Left-Parenthesis
(assert(equal(read1 "(a b c (d e f) g h i)")'(a b c (d e f) g h i)))
;Single-Quote
(assert(equal(read1 "'a")' 'a))
;semicolon
(assert(equal(read1 "; comment")(list +line-comment+ "; comment")))
;Double-Quote
(assert(equal(read1 "\"Foo\"")"Foo"))
(assert(equal(read1 "\"\"")""))
(assert(equal(read1 "\"\\\"APL\\\\360?\\\" he cried.\"")"\"APL\\360?\" he cried."))
(assert(equal(read1 "\"|x| = |-x|\"")"|x| = |-x|"))
;backquote
(assert(equal(read1 "`a")(list +backquote+ 'a)))
;comma
(assert(equal(read1 ",a")(list +comma+ 'a)))
(assert(equal(read1 ",@a")(list +comma-at+ 'a)))
;sharpsign Backslash
(assert(equal(read1 "#\\newline")#\newline))
(assert(equal(read1 "#\\a")#\a))
(assert(equal(read1 "#\\'")#\'))
;Sharpsign Single-Quote
(assert(equal(read1 "#'+")'(function +)))
        ;Sharpsign Left-Parenthesis
(assert(equalp(read1 "#(a b c c c c)") #(a b c c c c) ))
(assert(equalp(read1 "#6(a b c c c c)") #(a b c c c c) ))
(assert(equalp(read1 "#6(a b c)") #(a b c c c c) ))
(assert(equalp(read1 "#6(a b c c)") #(a b c c c c) ))
(assert(equalp(read1 "#(a b c)") #(a b c) ))
(assert(equalp(read1 "#(2 3 5 7 11 13 17 19)") #(2 3 5 7 11 13 17 19 ) ))
(assert(equalp(read1 "#()") #() ))
(assert(equalp(read1 "#0()") #() ))
          ;Sharpsign Asterisk
(assert(equalp(read1 "#*10000") #*10000 ))
(assert(equalp(read1 "#8*10000") #8*10000 ))
(assert(equalp(read1 "#*") #*))
(assert(equalp(read1 "#0*") #*))
