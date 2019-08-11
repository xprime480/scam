;;; Test comparison operations with zero forms
;;;

(narc-label "CMP Zero Forms")

(narc-expect
 (#t (=))
 (#t (<>))
 (#t (<))
 (#t (<=))
 (#t (>))
 (#t (>=)))

(narc-report)

