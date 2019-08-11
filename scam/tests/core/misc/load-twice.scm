(narc-label "Load Twice")

(load "lib/prelude.scm")

(narc-catch
 (:file (load "lib/prelude.scm")))

(narc-report)
