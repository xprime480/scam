(import lib/test/let-suite
        (lib test narc))

(narc-label "Let")

(let-suite-expect 2 ((x 1)) (* x 2))

(narc-report)

