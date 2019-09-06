(import (test narc))

(narc-label "Include")

(narc-expect
 (66 (include "lib/test/data/includetest.scm")))

(narc-report)
