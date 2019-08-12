(narc-label "Let")

(load "let-suite.scm")

(let-suite-expect 2 ((x 1)) (* x 2))

(narc-report)

