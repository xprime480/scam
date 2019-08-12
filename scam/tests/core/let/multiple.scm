(narc-label "Let Multiple Bindings")

(load "let-suite.scm")

(let-suite-expect 15 ((a 3) (b 5)) (* a b))

(narc-report)

