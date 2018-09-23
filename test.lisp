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
(defun read-from(s)
(with-input-from-string(*standard-input* s)
(lex)
(read*)))

(assert(equal(read-from "123")123))
(assert(equal(read-from "a")'a))
(assert(equal(read-from ":abc"):abc))
(assert(equal(read-from "abc:xyz")(list +package-marker+ 'abc 'xyz)))
(assert(equal(read-from "abc::xyz")(list +package-marker-2+ 'abc 'xyz)))
