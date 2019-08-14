(narc-label "Let Dependent Bindings")

(import lib/test/let-suite)

(define x 2)
(define y 0)

(let-suite-expect (2 1 2) ((x 1) (y x)) y)

(narc-report)

