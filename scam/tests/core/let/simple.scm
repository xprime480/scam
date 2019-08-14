(narc-label "Let")

(import lib/test/let-suite)

(let-suite-expect 2 ((x 1)) (* x 2))

(narc-report)

