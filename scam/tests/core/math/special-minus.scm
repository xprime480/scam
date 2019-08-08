;;; Test operator - on infinity and NaN
;;;

(narc-label "Special Minus")

(narc-expect
 (+inf.0  (- 2 -inf.0))
 (-inf.0  (- 2 +inf.0))
 (#t      (nan? (- 2 -nan.0)))
 (-inf.0  (- -inf.0 +inf.0))
 (#t      (nan? (- +inf.0 +inf.0)))
 (#t      (nan? (- -inf.0 -inf.0))))

(narc-report)
