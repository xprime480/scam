(import (test narc))

(narc-label "Import Library Except")

(narc-catch
 (:eval (sample))
 (:eval (example)))

(import (except sample sample))

(narc-expect
 ("howdy" (example)))

(narc-catch
 (:eval (sample)))

(narc-report)
