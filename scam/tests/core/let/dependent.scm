(import (test let-suite)
        (test narc))

(narc-label "Let Dependent Bindings")

(define x 2)
(define y 0)

(let-suite-expect (2 1 2) ((x 1) (y x)) y)

(narc-report)

