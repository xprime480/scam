;;; Ensure bad values are reported
;;;

(narc-label "Negative Tests for Tokenizing")

(narc-skip
 (#t (read #tt)))

(narc-expect
 (1 1))

(narc-report)
