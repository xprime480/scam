(narc-label "CMP = Numbers")

(narc-expect
 (#t (= 3))
 (#t (= 3 3))
 (#f (= 3 6))
 (#t (= 3/4 6/8))
 (#t (= 3/4 0.75))
 (#t (= 3 3 3 3 3 3))
 (#f (= 3 3 3 3 3 5))
 (#t (= 2+3i 2+3i 2+3i)))

(narc-report)

