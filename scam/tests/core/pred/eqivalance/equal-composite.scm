(import checks
	(lib test narc))

(narc-label "Equal for Composites")

(narc-expect
 ('(#t #t #f) (check-3 test-equal "bunny" "bunny" "carlos"))
 ('(#t #t #f) (check-3 test-equal '(1 2 3) '(1 2 3) '(5 4 3)))
 ('(#t #t #f) (check-3 test-equal #(1 2 3) #(1 2 3) #(5 4 3)))
 ('(#t #t #f) (check-3 test-equal #u8(1 2 3) #u8(1 2 3) #u8(5 4 3)))
 ('(#t #t #f) (check-3 test-equal { :a 1 :b 2 } { :a 1 :b 2 } { :x 99 :u 98 })))

(narc-report)
