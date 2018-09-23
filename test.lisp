(load "etc")
;substringp
(assert(substringp "a" "abc" 0))
(assert(not(substringp "a" "abc" 1)))
