(narc-label "Let Multiple Forms")

(load "let-suite.scm")

(let-suite-expect 9 () 3 5 9)

(narc-report)

