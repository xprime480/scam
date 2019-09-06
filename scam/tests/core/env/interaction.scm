(import (test narc))

(narc-label "Interaction Environment")

(narc-expect
 (#t (environment? (interaction-environment))))

(narc-report)
