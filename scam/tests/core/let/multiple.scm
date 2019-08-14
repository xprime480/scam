(narc-label "Let Multiple Bindings")

(import lib/test/let-suite)

(let-suite-expect 15 ((a 3) (b 5)) (* a b))

(narc-report)

