(import lib/test/let-suite
	(lib test narc))

(narc-label "Let No Bindings")

(let-suite-expect 2 () 2)

(narc-report)

