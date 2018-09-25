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

;read
(defun read-tokens()
(loop
while *tok*
collect *tok*
do(lex)))

(defun read1(s)
(when nil
(let((*print-right-margin* 200))
(format t "~a~99T~a~%" s
(with-input-from-string(*standard-input* s)
(lex)
(read-tokens)
)
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
(assert(equalp(read1 "#*") #*))
(assert(equalp(read1 "#0*") #*))
(assert(equalp(read1 "#*101111") #*101111 ))
(assert(equalp(read1 "#6*101111") #*101111 ))
(assert(equalp(read1 "#6*101") #*101111 ))
(assert(equalp(read1 "#6*1011") #*101111 ))
;Sharpsign Colon
(assert(symbolp(read1 "#:abc") ))
;Sharpsign Dot
(assert(equal(read1 "#.123")(list +read-eval+ 123)))
(assert(equal(read1 "#.abc")(list +read-eval+ 'abc)))
;Sharpsign B
(assert(equal(read1 "#B1101")13))
(assert(equal(read1 "#b101/11")5/3))
(assert(equal(read1 "#b 101/11")5/3))
;Sharpsign O
(assert(equal(read1 "#o37/15")31/13))
(assert(equal(read1 "#o777")511))
(assert(equal(read1 "#o105")69))
;Sharpsign X
(assert(equal(read1 "#xF00")3840))
(assert(equal(read1 "#x105")261))
;Sharpsign R
(assert(equal(read1 "#2r11010101") 213   ))
(assert(equal(read1 "#b11010101")   213 ))
(assert(equal(read1 "#b+11010101")   213 ))
(assert(equal(read1 "#o325")   213 ))
(assert(equal(read1 "#xD5")   213 ))
(assert(equal(read1 "#16r+D5")   213 ))
(assert(equal(read1 "#o-300")    -192))
(assert(equal(read1 "#3r-21010")   -192 ))
(assert(equal(read1 "#25R-7H") -192   ))
(assert(equal(read1 "#xACCEDED")181202413    ))
;Sharpsign C
(assert(equal(read1 "#C(3.0s1 2.0s-1)") #C(3.0s1 2.0s-1)   ))
(assert(equal(read1 "#C(5 -3) ")  #C(5 -3)   ))
(assert(equal(read1 "#C(5/3 7.0)")    #C(5/3 7.0) ))
(assert(equal(read1 "#C(0 1)") #C(0 1)   ))
;Sharpsign A
(assert(equal(read1 "#2A((0 1 5) (foo 2 (hot dog)))") (list +array+ 2 '((0 1 5) (foo 2 (hot dog)))   )))
(assert(equal(read1 "#1A((0 1 5) (foo 2 (hot dog)))") (list +array+ 1 '((0 1 5) (foo 2 (hot dog)))   )))
(assert(equal(read1 "#0A((0 1 5) (foo 2 (hot dog)))") (list +array+ 0 '((0 1 5) (foo 2 (hot dog)))   )))
(assert(equal(read1 "#0A foo") (list +array+ 0 'foo   )))
;Sharpsign S
(assert(equal(read1 "#s(name slot1 value1 slot2 value2)") (list +structure+ '(name slot1 value1 slot2 value2)   )))
;Sharpsign P
(assert(equal(read1 "#p\"abc\"") #p"abc"))
;Sharpsign Plus
(assert(equal(read1 "(cons #+spice \"Spice\" #-spice \"Lispm\" x)") `(cons (,+feature-plus+ spice "Spice") (,+feature-minus+ spice "Lispm") x)))
;Sharpsign Vertical-Bar
(assert(equal(read1
                   "(defun add3 (n) #|(format t \"~&Adding 3 to ~D.\" n)|# (+ n 3))")
`(defun add3 (n) (,+block-comment+ "#|(format t \"~&Adding 3 to ~D.\" n)|#") (+ n 3))
))

(assert(equal(read1
"(defun mention-fun-fact-1a () (format t \"CL uses ; and #|...|# in comments.\"))")
`(defun mention-fun-fact-1a () (format t "CL uses ; and #|...|# in comments."))
))

(assert(equal(read1
"#|(defun mention-fun-fact-1b () (format t \"CL uses ; and #|...|# in comments.\"))|#")
`(,+block-comment+ "#|(defun mention-fun-fact-1b () (format t \"CL uses ; and #|...|# in comments.\"))|#")
))

(assert(equal(read1
"(defun mention-fun-fact-2a () (format t \"Don't use |\\# unmatched or you'll get in trouble!\"))")
`(defun mention-fun-fact-2a () (format t "Don't use |# unmatched or you'll get in trouble!"))
))

(assert(equal(read1
"#|(defun mention-fun-fact-2b () (format t \"Don't use |\\# unmatched or you'll get in trouble!\"))|#")
`(,+block-comment+ "#|(defun mention-fun-fact-2b () (format t \"Don't use |\\# unmatched or you'll get in trouble!\"))|#")
))

;write
(defun write1(a)
  (assert(equal(read1(fmt 0 a))a)))

(write1'abc)
(write1 123)
(write1 +123)
(write1 -123)
(write1 123/456)
(write1 "abc")
(write1 "abc\\def")
(write1 #\a)
(write1 '(foo bar))
