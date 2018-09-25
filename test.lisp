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

(defun read-string(s)
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
(assert(equal(read-string "123")123))
(assert(equal(read-string "+123")+123))
(assert(equal(read-string "-123")-123))
(assert(equal(read-string "123/456")123/456))
(assert(equal(read-string "-123/456")-123/456))
(assert(equal(read-string "123.5")123.5))
(assert(equal(read-string "-123.5")-123.5))
(assert(equal(read-string "123.5e6")123.5e6))
(assert(equal(read-string "-123.5e6")-123.5e6))
;symbols
(assert(equal(read-string "a")'a))
(assert(equal(read-string "\\a")'\a))
(assert(equal(read-string "|one two three|")'|one two three|))
(assert(equal(read-string ":abc"):abc))
(assert(equal(read-string "abc:xyz")(list +package-marker+ 'abc 'xyz)))
(assert(equal(read-string "abc::xyz")(list +package-marker-2+ 'abc 'xyz)))
;Left-Parenthesis
(assert(equal(read-string "(a b c (d e f) g h i)")'(a b c (d e f) g h i)))
;Single-Quote
(assert(equal(read-string "'a")' 'a))
;semicolon
(assert(equal(read-string "; comment")(list +comment+ "; comment")))
;Double-Quote
(assert(equal(read-string "\"Foo\"")"Foo"))
(assert(equal(read-string "\"\"")""))
(assert(equal(read-string "\"\\\"APL\\\\360?\\\" he cried.\"")"\"APL\\360?\" he cried."))
(assert(equal(read-string "\"|x| = |-x|\"")"|x| = |-x|"))
;backquote
(assert(equal(read-string "`a")(list +backquote+ 'a)))
;comma
(assert(equal(read-string ",a")(list +comma+ 'a)))
(assert(equal(read-string ",@a")(list +comma-at+ 'a)))
;sharpsign Backslash
(assert(equal(read-string "#\\newline")#\newline))
(assert(equal(read-string "#\\a")#\a))
(assert(equal(read-string "#\\'")#\'))
;Sharpsign Single-Quote
(assert(equal(read-string "#'+")'(function +)))
        ;Sharpsign Left-Parenthesis
(assert(equalp(read-string "#(a b c c c c)") #(a b c c c c) ))
(assert(equalp(read-string "#6(a b c c c c)") #(a b c c c c) ))
(assert(equalp(read-string "#6(a b c)") #(a b c c c c) ))
(assert(equalp(read-string "#6(a b c c)") #(a b c c c c) ))
(assert(equalp(read-string "#(a b c)") #(a b c) ))
(assert(equalp(read-string "#(2 3 5 7 11 13 17 19)") #(2 3 5 7 11 13 17 19 ) ))
(assert(equalp(read-string "#()") #() ))
(assert(equalp(read-string "#0()") #() ))
          ;Sharpsign Asterisk
(assert(equalp(read-string "#*") #*))
(assert(equalp(read-string "#0*") #*))
(assert(equalp(read-string "#*101111") #*101111 ))
(assert(equalp(read-string "#6*101111") #*101111 ))
(assert(equalp(read-string "#6*101") #*101111 ))
(assert(equalp(read-string "#6*1011") #*101111 ))
;Sharpsign Colon
(assert(symbolp(read-string "#:abc") ))
;Sharpsign Dot
(assert(equal(read-string "#.123")(list +read-eval+ 123)))
(assert(equal(read-string "#.abc")(list +read-eval+ 'abc)))
;Sharpsign B
(assert(equal(read-string "#B1101")13))
(assert(equal(read-string "#b101/11")5/3))
(assert(equal(read-string "#b 101/11")5/3))
;Sharpsign O
(assert(equal(read-string "#o37/15")31/13))
(assert(equal(read-string "#o777")511))
(assert(equal(read-string "#o105")69))
;Sharpsign X
(assert(equal(read-string "#xF00")3840))
(assert(equal(read-string "#x105")261))
;Sharpsign R
(assert(equal(read-string "#2r11010101") 213   ))
(assert(equal(read-string "#b11010101")   213 ))
(assert(equal(read-string "#b+11010101")   213 ))
(assert(equal(read-string "#o325")   213 ))
(assert(equal(read-string "#xD5")   213 ))
(assert(equal(read-string "#16r+D5")   213 ))
(assert(equal(read-string "#o-300")    -192))
(assert(equal(read-string "#3r-21010")   -192 ))
(assert(equal(read-string "#25R-7H") -192   ))
(assert(equal(read-string "#xACCEDED")181202413    ))
;Sharpsign C
(assert(equal(read-string "#C(3.0s1 2.0s-1)") #C(3.0s1 2.0s-1)   ))
(assert(equal(read-string "#C(5 -3) ")  #C(5 -3)   ))
(assert(equal(read-string "#C(5/3 7.0)")    #C(5/3 7.0) ))
(assert(equal(read-string "#C(0 1)") #C(0 1)   ))
;Sharpsign A
(assert(equal(read-string "#2A((0 1 5) (foo 2 (hot dog)))") (list +array+ 2 '((0 1 5) (foo 2 (hot dog)))   )))
(assert(equal(read-string "#1A((0 1 5) (foo 2 (hot dog)))") (list +array+ 1 '((0 1 5) (foo 2 (hot dog)))   )))
(assert(equal(read-string "#0A((0 1 5) (foo 2 (hot dog)))") (list +array+ 0 '((0 1 5) (foo 2 (hot dog)))   )))
(assert(equal(read-string "#0A foo") (list +array+ 0 'foo   )))
;Sharpsign S
(assert(equal(read-string "#s(name slot1 value1 slot2 value2)") (list +structure+ '(name slot1 value1 slot2 value2)   )))
;Sharpsign P
(assert(equal(read-string "#p\"abc\"") #p"abc"))
;Sharpsign Plus
(assert(equal(read-string "(cons #+spice \"Spice\" #-spice \"Lispm\" x)") `(cons (,+feature-plus+ spice "Spice") (,+feature-minus+ spice "Lispm") x)))
;Sharpsign Vertical-Bar
(assert(equal(read-string
                   "(defun add3 (n) #|(format t \"~&Adding 3 to ~D.\" n)|# (+ n 3))")
`(defun add3 (n) (,+comment+ "#|(format t \"~&Adding 3 to ~D.\" n)|#") (+ n 3))
))

(assert(equal(read-string
"(defun mention-fun-fact-1a () (format t \"CL uses ; and #|...|# in comments.\"))")
`(defun mention-fun-fact-1a () (format t "CL uses ; and #|...|# in comments."))
))

(assert(equal(read-string
"#|(defun mention-fun-fact-1b () (format t \"CL uses ; and #|...|# in comments.\"))|#")
`(,+comment+ "#|(defun mention-fun-fact-1b () (format t \"CL uses ; and #|...|# in comments.\"))|#")
))

(assert(equal(read-string
"(defun mention-fun-fact-2a () (format t \"Don't use |\\# unmatched or you'll get in trouble!\"))")
`(defun mention-fun-fact-2a () (format t "Don't use |# unmatched or you'll get in trouble!"))
))

(assert(equal(read-string
"#|(defun mention-fun-fact-2b () (format t \"Don't use |\\# unmatched or you'll get in trouble!\"))|#")
`(,+comment+ "#|(defun mention-fun-fact-2b () (format t \"Don't use |\\# unmatched or you'll get in trouble!\"))|#")
))
;fmt
(assert(equal(fmt-inline 'abc)"abc"))
(assert(equal(fmt-inline 123)"123"))
(assert(equal(fmt-inline -123)"-123"))
(assert(equal(fmt-inline '(foo bar))"(foo bar)"))
(assert(equal(fmt-inline ''(foo bar))"'(foo bar)"))
(assert(equal(fmt-inline `(,+backquote+ (foo bar)))"`(foo bar)"))
(assert(equal(fmt-inline `(,+comma+ (foo bar)))",(foo bar)"))
(assert(equal(fmt-inline `(,+comma-at+ (foo bar)))",@(foo bar)"))
(assert(equal(fmt-inline `(,+read-eval+ (foo bar)))"#.(foo bar)"))

;read then write
(defun read-write(s)(assert(equal(fmt 0(read-string s))s)))

(read-write"abc")
(read-write"123")
(read-write"-123")
(read-write"|camelCase|")
(read-write"(foo bar)")
(read-write"'(foo bar)")
(read-write"`(foo bar)")
(read-write",(foo bar)")
(read-write",@(foo bar)")
(read-write"#.(foo bar)")
(read-write"#+sbcl (foo bar)")
(read-write"#-sbcl (foo bar)")
(read-write"; text")
(read-write";; text")
(read-write";;; text")
(read-write";;;; text")
(read-write"#| text |#")

;write then read
(defun write-read (a) (assert (equal (read-string (fmt 0 a)) a)))

(write-read'abc)
(write-read 123)
(write-read +123)
(write-read -123)
(write-read 123/456)
(write-read "abc")
(write-read "abc\\def")
(write-read #\a)
(write-read '(foo bar))
(write-read ''(foo bar))
(write-read `(,+backquote+ (foo bar)))
(write-read `(,+comma+ (foo bar)))
(write-read `(,+comma-at+ (foo bar)))
(write-read `(,+read-eval+ (foo bar)))
