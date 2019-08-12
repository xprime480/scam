(narc-label "Let Env")

(load "let-suite.scm")

(define x 2)
(define y 0)

(let-suite-expect 2.0 ((y 1.0)) (/ x y))

(narc-expect
 (0   y))

(narc-report)

