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
;Left-Parenthesis
(assert(equal(read-string "(a b c (d e f) g h i)")'(a b c (d e f) g h i)))
;semicolon
(assert(equal(read-string "; comment")(list +special+ "; comment")))
;Double-Quote
(assert(equal(read-string "\"Foo\"")"Foo"))
(assert(equal(read-string "\"\"")""))
(assert(equal(read-string "\"\\\"APL\\\\360?\\\" he cried.\"")"\"APL\\360?\" he cried."))
(assert(equal(read-string "\"|x| = |-x|\"")"|x| = |-x|"))
;sharpsign Backslash
(assert(equal(read-string "#\\newline")#\newline))
(assert(equal(read-string "#\\a")#\a))
(assert(equal(read-string "#\\'")#\'))
;Sharpsign Colon
(assert(symbolp(read-string "#:abc") ))
;Sharpsign Vertical-Bar
(assert(equal(read-string
                   "(defun add3 (n) #|(format t \"~&Adding 3 to ~D.\" n)|# (+ n 3))")
`(defun add3 (n) (,+special+ "#|(format t \"~&Adding 3 to ~D.\" n)|#") (+ n 3))
))

(assert(equal(read-string
"(defun mention-fun-fact-1a () (format t \"CL uses ; and #|...|# in comments.\"))")
`(defun mention-fun-fact-1a () (format t "CL uses ; and #|...|# in comments."))
))

(assert(equal(read-string
"#|(defun mention-fun-fact-1b () (format t \"CL uses ; and #|...|# in comments.\"))|#")
`(,+special+ "#|(defun mention-fun-fact-1b () (format t \"CL uses ; and #|...|# in comments.\"))|#")
))

(assert(equal(read-string
"(defun mention-fun-fact-2a () (format t \"Don't use |\\# unmatched or you'll get in trouble!\"))")
`(defun mention-fun-fact-2a () (format t "Don't use |# unmatched or you'll get in trouble!"))
))

(assert(equal(read-string
"#|(defun mention-fun-fact-2b () (format t \"Don't use |\\# unmatched or you'll get in trouble!\"))|#")
`(,+special+ "#|(defun mention-fun-fact-2b () (format t \"Don't use |\\# unmatched or you'll get in trouble!\"))|#")
))
;fmt
(assert(equal(fmt-inline 'abc)"abc"))
(assert(equal(fmt-inline 123)"123"))
(assert(equal(fmt-inline -123)"-123"))
(assert(equal(fmt-inline '(foo bar))"(foo bar)"))

;read then write
(defun read-write(s)(assert(equal(fmt 0(read-string s))s)))

(read-write"abc")
(read-write"123")
(read-write"-123")
(read-write"|camelCase|")
(read-write"(foo bar)")
(read-write"'(foo bar)")
(read-write"#'foo")
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
(read-write"\"abc\"")
(read-write"#b100")
(read-write"#o100")
(read-write"#x100")
(read-write"#26r100")
(read-write"#c(0 1)")
(read-write"#(0 1)")
(read-write"#a(0 1)")
(read-write"#P\"abc\"")
(read-write"#*1111")
(read-write"#:abc")
(read-write":abc")
(read-write"abc:def")
(read-write"abc::def")

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
(write-read #\newline)
(write-read #\space)
(write-read '(foo bar))
