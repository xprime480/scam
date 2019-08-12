(narc-label "Include")

(load "lib/prelude.scm")

(narc-expect
 (66 (include "lib/test/data/includetest.scm")))

(narc-report)
