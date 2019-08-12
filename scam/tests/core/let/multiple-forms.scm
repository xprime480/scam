(narc-label "Let Multiple Forms")

(load "lib/test/let-suite.scm")

(let-suite-expect 9 () 3 5 9)

(narc-report)

