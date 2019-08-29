(import (lib test narc))

(load "lib/prelude.scm")

(narc-label "Load Twice")

(narc-catch
 (:file (load "lib/prelude.scm")))

(narc-report)
