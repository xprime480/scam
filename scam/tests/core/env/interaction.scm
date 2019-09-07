(import (scheme repl)
	(only (scam misc)
	      environment?)
	(test narc))

(narc-label "Interaction Environment")

(narc-expect
 (#t (environment? (interaction-environment))))

(narc-report)
