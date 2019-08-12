(narc-label "Let No Bindings")

(load "lib/test/let-suite.scm")

(let-suite-expect 2 () 2)

(narc-report)

