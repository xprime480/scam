(narc-label "CMP >= Numbers")

(narc-expect
 (#t (>= 3))
 (#t (>= 3 3))
 (#f (>= 2 3))
 (#t (>= 51 3 3 -4))
 (#f (>= 1 1 1 10))
 (#f (>= 2+i 1+i))
 (#f (>= 1+i 2+i))
 (#t (>= 1+i 1+i)))

(narc-report)

