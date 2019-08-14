(import checks)

(narc-label "Eqv for Composites")

(narc-expect
 ('(#t #f #f) (check-3 test-eqv "bunny" "bunny" "carlos"))
 ('(#t #f #f) (check-3 test-eqv '(1 2 3) '(1 2 3) '(5 4 3)))
 ('(#t #f #f) (check-3 test-eqv #(1 2 3) #(1 2 3) #(5 4 3)))
 ('(#t #f #f) (check-3 test-eqv #u8(1 2 3) #u8(1 2 3) #u8(5 4 3)))
 ('(#t #f #f) (check-3 test-eqv { :a 1 :b 2 } { :a 1 :b 2 } { :x 99 :u 98 })))

(narc-report)
