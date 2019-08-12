(narc-label "Let Dependent Bindings")

(load "lib/test/let-suite.scm")

(define x 2)
(define y 0)

(let-suite-expect (2 1 2) ((x 1) (y x)) y)

(narc-report)

