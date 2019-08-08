;;; define persists a value
;;;

(narc-label "Define")

(define x (- 3 2))

(narc-expect
 (1 x))

(narc-report)
