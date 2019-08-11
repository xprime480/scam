(narc-label "CMP <> Numbers")

(narc-expect
 (#t (<> 3))
 (#f (<> 3 3))
 (#t (<> 3 6))
 (#t (<> 3 4 5 3))
 (#f (<> 3 3 5))
 (#t (<> 7-5i 1.111+0.00001i)))

(narc-report)

