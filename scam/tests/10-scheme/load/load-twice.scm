(import (scheme load)
        (test narc))

(load "lib/test/data/foo.scm")

(narc-label "Load Twice")

(narc-catch
 (:file (load "lib/test/data/foo.scm")))

(narc-report)
