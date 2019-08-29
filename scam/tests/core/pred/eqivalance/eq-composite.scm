(import checks
	(lib test narc))

(narc-label "Eq for Composites")

(narc-expect
 ('(#t #f #f) (check-3 test-eq "bunny" "bunny" "carlos"))
 ('(#t #f #f) (check-3 test-eq '(1 2 3) '(1 2 3) '(5 4 3)))
 ('(#t #f #f) (check-3 test-eq #(1 2 3) #(1 2 3) #(5 4 3)))
 ('(#t #f #f) (check-3 test-eq #u8(1 2 3) #u8(1 2 3) #u8(5 4 3)))
 ('(#t #f #f) (check-3 test-eq { :a 1 :b 2 } { :a 1 :b 2 } { :x 99 :u 98 })))

(narc-report)
