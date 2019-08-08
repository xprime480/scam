;;; Assign to symbol
;;;

(narc-label "Assign")

(define x (- 3 2))
(set! x 77)

(narc-expect
 (77 x))

(narc-report)
